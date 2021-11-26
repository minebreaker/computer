(ns rip.deadcode.computer.component.compiler
  (:require [rip.deadcode.computer.component.extensional :refer :all]
            [rip.deadcode.computer.component.hardware :refer [console-idx]]
            [clojure.string :refer [blank? split]]
            [clojure.core.match :refer [match]]))

(defn lex [^String code]
  (filter #(not (blank? %))
    (re-seq #"[\w\"]+|[\s\(\)\{\}\;]|\+\+|<=|==|=|%" code))
  )

(declare parse)

(defn parse-for [code]
  (let [
        res-init (match [code]
                   [(["for" "(" & res] :seq)] (parse res))
        res-cond (parse (:rest res-init))
        res-loop (parse (:rest res-cond))
        res-body (match [(:rest res-loop)]
                   [([")" "{" & res] :seq)] (parse res))
        res (match [(:rest res-body)]
              [(["}" & res] :seq)] res)
        ]
    {:type "for"
     :init res-init
     :cond res-cond
     :loop res-loop
     :body res-body
     :rest res}))

(defn parse-if [code]
  (let [
        res-cond (match [code]
                   [(["if" "(" & res] :seq)] (parse res))
        res-then (match [(:rest res-cond)]
                   [([")" "{" & res] :seq)] (parse res))
        res-then' (:rest res-then)
        [res-else res] (match [res-then']
                         [(["}" "else" "{" & res] :seq)] (let [res' (parse res)]
                                                           (match [(:rest res')]
                                                             [(["}" & res''] :seq)] [res' res'']))
                         [(["}" "else" "if" & _] :seq)] (let [res' (parse (drop 2 res-then'))]
                                                          [res' (:rest res')]))
        ]
    {:type "if"
     :cond res-cond
     :then res-then
     :else res-else
     :rest res}))

(defn parse-value [v]
  (cond
    (re-matches #"\d+" v) {:type "int" :value (Integer/parseInt v)}
    (and (.startsWith v "\"") (.endsWith v "\"")) {:type "str" :value (.substring v 1 (- (.length v) 1))}
    :else {:type "ref" :name v}))

(defn drop-semi [code]
  (if (= (first code) ";") (rest code) code))

(defn parse
  [code]
  (let [[t0 t1 t2 t3 t4] code]
    (cond
      (= t2 "=") {:type "assign" :name t1 :value (parse-value t3) :rest (drop-semi (drop 4 code))}
      (= t1 "<=") {:type "le" :x (parse-value t0) :y (parse-value t2) :rest (drop-semi (drop 3 code))}
      (= t1 "++") {:type "inc" :name t0 :rest (drop-semi (drop 2 code))}
      (and (= t1 "%") (= t3 "==")) {:type "rem" :x (parse-value t0) :y (parse-value t2) :z (parse-value t4) :rest (drop-semi (drop 5 code))}
      (= t0 "for") (parse-for code)
      (= t0 "if") (parse-if code)
      (and (re-matches #"[\w]+" t0) (= t1 "(") (= t3 ")")) {:type "call" :name t0 :arg (parse-value t2) :rest (drop-semi (drop 4 code))}
      :else (let [v (parse-value t0)] (merge v {:rest (rest code)})))))

(declare -compile)

(defn compile-assign [code var]
  (let [{name :name value :value} code
        {local :local ptr :next-local} var
        [op-value] (-compile value var)]
    [(vec (concat
            op-value                                        ; D=value
            [
             (a-inst (bit15 (i2ba ptr)))                    ; @ptr
             (c-inst op-d op-dest-m)                        ; M=D
             ]))
     (merge var {:local (merge local {name ptr}) :next-local (inc ptr)})])
  )

(defn compile-int [code]
  [(a-inst (bit15 (i2ba (:value code))))                    ; @i
   (c-inst false op-a op-dest-d op-no-jump)                 ; D=A
   ])

(defn compile-str [code var]
  (let [{obj-ptr :next-const t-ptr :next-temp} var
        str (:value code)
        str-len (.length str)
        out-code (merge [
                         (a-inst (bit15 (i2ba str-len)))    ; @length of the str
                         (c-inst false op-a op-dest-d)      ; D=A
                         (a-inst (bit15 (i2ba obj-ptr)))    ; @obj-pt
                         (c-inst false op-d op-dest-m)      ; M=D
                         ]
                   (vec (map-indexed                        ; for each char
                          (fn [n c]
                            [
                             (a-inst obj-ptr)               ; @obj-ptr
                             (c-inst false op-a op-dest-d)  ; D=A
                             (a-inst (bit15 (i2ba t-ptr)))  ; @0
                             (c-inst true op-d op-dest-m)   ; M=D ; t0 = obj-ptr
                             (a-inst (bit15 (i2ba (+ n 1)))) ; @(n+1)
                             (c-inst false op-a op-dest-d)  ; D=A
                             (a-inst (bit15 (i2ba t-ptr)))  ; @0
                             (c-inst true op-d+a op-dest-m) ; M=M+D ; t0 = obj-ptr + n
                             (a-inst (bit15 (i2ba (int c)))) ; @char
                             (c-inst false op-a op-dest-d)  ; D=A
                             (a-inst (bit15 (i2ba t-ptr)))  ; @0
                             (c-inst true op-a op-dest-a)   ; A=M ; A = t0
                             (c-inst true op-d op-dest-m)   ; M=D ; [t0] = char
                             ])
                          str
                          ))
                   [
                    (a-inst obj-ptr)                        ; @obj-ptr
                    (c-inst false op-a op-dest-d)           ; D=A
                    ]
                   )
        out-var (merge
                  var
                  {:const (merge (:const var) {str obj-ptr}) :next-const (+ obj-ptr str-len 1)})
        ]
    [out-code out-var]))

(defn compile-ref [code var]
  (let [ptr (get (:local var) (:name code))]
    [[
      (a-inst (bit15 (i2ba (int ptr))))                     ; @ptr
      (c-inst true op-a op-dest-d op-no-jump)               ; D=M
      ]
     var]))

(defn inc-temp [var]
  (assoc var :next-temp (inc (:next-temp var))))

(defn compile-le [code var]
  (let [{offset :offset t-ptr :next-temp} var
        {x :x y :y} code
        [op-x] (-compile x (inc-temp (assoc var :offset (+ offset))))
        c-op-x (count op-x)
        [op-y] (-compile y (inc-temp (assoc var :offset (+ offset c-op-x 2))))
        c-op-y (count op-y)]
    [(vec (concat
            op-x                                            ; D=x
            [(a-inst (bit15 (i2ba t-ptr)))                  ; @t0
             (c-inst false op-d op-dest-m)                  ; M=D ; t0=x
             ]
            op-y                                            ; D=y
            [(a-inst (bit15 (i2ba t-ptr)))                  ; @t0
             (c-inst true op-a-d op-dest-d)                 ; D=M-D ; x-y
             (a-inst (bit15 (i2ba (+ offset c-op-x 2 c-op-y 6))))
             (c-inst false op-d op-dest-null op-jle)        ; D,JLE ; jump if x-y <= 0
             (a-inst (bit15 (i2ba (+ offset c-op-x 2 c-op-y 7))))
             (c-inst false op-0 op-dest-d op-jmp)           ; D=0,JMP ; false when x-y > 0 jump to last
             (c-inst false op-1 op-dest-d)                  ; D=1 ; true when x-y <= 0
             (c-inst op-d op-dest-null)                     ; D ; TODO: can be shorten
             ]))
     var]))


(defn compile-inc [code var]
  (let [{name :name} code
        ptr (get (:local var) name)]
    [[(a-inst (bit15 (i2ba ptr)))                           ; @ptr
      (c-inst true op-a+1 op-dest-md)                       ; MD=M+1
      ]
     var]))

(defn compile-rem [code var]
  (let [{x :x y :y z :z} code
        {offset :offset t-ptr :next-temp} var
        [op-x] (-compile x (inc-temp var))
        c-op-x (count op-x)
        [op-y] (-compile y (inc-temp (assoc var :offset (+ offset c-op-x 2))))
        c-op-y (count op-y)
        [op-y'] (-compile y (inc-temp (assoc var :offset (+ offset c-op-x 2 c-op-y 5)))) ; FIXME: this is dumb
        [op-z] (-compile z (inc-temp (assoc var :offset (+ offset c-op-x 2 c-op-y 5 c-op-y 1))))
        c-op-z (count op-z)]
    [(vec (concat
            ; calculate remainder
            op-x                                            ; D=x
            [(a-inst (bit15 (i2ba t-ptr)))                  ; @t0
             (c-inst true op-d op-dest-m)                   ; M=D ; t0=x ; rem-y
             ]
            op-y                                            ; D=y
            [(a-inst (bit15 (i2ba t-ptr)))                  ; @t0
             (c-inst true op-a-d op-dest-md)                ; MD=M-D ; x=x-y
             (a-inst (bit15 (i2ba (+ offset c-op-x 2))))    ; @ point of op-y
             (c-inst false op-d op-dest-null op-jge)        ; D,JGE ; repeat while x-y >= 0
             ]
            op-y'                                           ; D=y
            [(a-inst (bit15 (i2ba t-ptr)))                  ; @t0
             (c-inst true op-d+a op-dest-m)]                ; M=D+M ; t0=(rem-y)+y
            ; check if rem equals to z
            op-z                                            ; D=z
            [(a-inst (bit15 (i2ba t-ptr)))                  ; @t0
             (c-inst true op-d-a op-dest-d)                 ; D=D-M ; z-rem
             (a-inst (bit15 (i2ba (+ offset c-op-x 2 c-op-y 4 c-op-y 2 c-op-z 6))))
             (c-inst false op-d op-dest-null op-jeq)        ; D,JEQ ; jump if rem=z
             (a-inst (bit15 (i2ba (+ offset c-op-x 2 c-op-y 4 c-op-y 2 c-op-z 7))))
             (c-inst false op-0 op-dest-d op-jmp)           ; D=0,JMP ; when rem != z jump to last
             (c-inst op-1 op-dest-d)                        ; D=1 ; when rem=z
             ]))
     var]
    )
  )

(defn compile-call [code var]
  (let [{name :name arg :arg} code
        {arg-type :type} arg
        {offset :offset t0-ptr :next-temp} var
        t1-ptr (inc t0-ptr)
        ]
    [(cond
       (= name "println") (let [write-int (fn [add-offset]
                                            [; assuming  D=x
                                             (a-inst (bit15 (i2ba t0-ptr))) ; @t0
                                             (c-inst true op-d op-dest-m) ; M=D ; t0=x ; rem-y
                                             (a-inst (bit15 (i2ba t1-ptr))) ; @t1 ; q
                                             (c-inst false op--1 op-dest-m) ; M=-1 ; t1=-1 ; quotient
                                             (a-inst (bit15 (i2ba t1-ptr))) ; @t1
                                             (c-inst true op-a+1 op-dest-m) ; M=M+1 ; increment q
                                             (a-inst (bit15 (i2ba 10))) ; @10
                                             (c-inst op-a op-dest-d) ; D=A ; D=10
                                             (a-inst (bit15 (i2ba t0-ptr))) ; @t0
                                             (c-inst true op-a-d op-dest-md) ; MD=M-D ; x=x-10
                                             (a-inst (bit15 (i2ba (+ offset add-offset 4))))
                                             (c-inst false op-d op-dest-null op-jge) ; D,JGE ; repeat while x-10 >= 0
                                             (a-inst (bit15 (i2ba 10))) ; @10
                                             (c-inst op-a op-dest-d) ; D=A
                                             (a-inst (bit15 (i2ba t0-ptr))) ; @t0
                                             (c-inst true op-d+a op-dest-m) ; M=D+M ; t0=(rem-y)+y
                                             ; prints second digit if not 0
                                             (a-inst (bit15 (i2ba t1-ptr))) ; @t1
                                             (c-inst true op-a op-dest-d) ; D=M
                                             (a-inst (bit15 (i2ba (+ offset add-offset 26))))
                                             (c-inst false op-d op-dest-null op-jeq) ; jump if q=0
                                             (a-inst (bit15 (i2ba t1-ptr))) ; @t1
                                             (c-inst true op-a op-dest-d) ; D=M ; D=q
                                             (a-inst (bit15 (i2ba 48))) ; @48 ; ascii digit
                                             (c-inst op-d+a op-dest-d) ; D=D+A
                                             (a-inst console-idx) ; @console
                                             (c-inst op-d op-dest-m) ; M=D
                                             ; print first digit
                                             (a-inst (bit15 (i2ba t0-ptr))) ; @t0
                                             (c-inst true op-a op-dest-d) ; D=M ; D=r
                                             (a-inst (bit15 (i2ba 48))) ; @48
                                             (c-inst op-d+a op-dest-d) ; D=D+A
                                             (a-inst console-idx) ; @console
                                             (c-inst op-d op-dest-m) ; M=D
                                             (a-inst (bit15 (i2ba 10))) ; @10 ; lf
                                             (c-inst op-a op-dest-d) ; D=A
                                             (a-inst console-idx) ; @console
                                             (c-inst op-d op-dest-m) ; M=D
                                             ])
                                write-str (fn [out]
                                            (vec (reduce
                                                   #(concat %1 %2)
                                                   (map
                                                     (fn [c]
                                                       [(a-inst (bit15 (i2ba (int c)))) ; @char
                                                        (c-inst false op-a op-dest-d) ; D=A
                                                        (a-inst console-idx) ; @console
                                                        (c-inst true op-d op-dest-m) ; M=D
                                                        ])
                                                     (str out "\n")))))
                                ]
                            (cond
                              (= arg-type "str") (write-str (:value arg))
                              (= arg-type "int") (write-str (Integer/toString (:value arg)))
                              (= arg-type "ref") (let [[op] (-compile arg var)]
                                                   (vec (concat op (write-int (count op)))))
                              )
                            )
       )
     var
     ]))


(defn compile-for [code var]
  (let [offset (:offset var)
        {init :init cond :cond loop :loop body :body} code
        [op-init var'] (-compile init var)
        c-op-init (count op-init)
        [op-cond] (-compile cond (assoc var' :offset (+ offset c-op-init)))
        c-op-cond (count op-cond)
        [op-body] (-compile body (assoc var' :offset (+ offset c-op-init c-op-cond 2)))
        c-op-body (count op-body)
        [op-loop] (-compile loop (assoc var' :offset (+ offset c-op-init c-op-cond 2 c-op-body)))
        c-op-loop (count op-loop)
        ]
    [(vec (concat
            op-init
            op-cond
            [(a-inst (bit15 (i2ba (+ offset c-op-init c-op-cond 2 c-op-body c-op-loop 2)))) ; @ the end of the loop
             (c-inst false op-d op-dest-null op-jeq)        ; D,JEQ ; if cond=false jump
             ]
            op-body
            op-loop
            [(a-inst (bit15 (i2ba (+ offset c-op-init))))   ; @cond
             (c-inst false op-0 op-dest-null op-jmp)        ; JMP
             ]))
     var]))

(defn compile-if [code var]
  (let [offset (:offset var)
        {cond :cond then :then else :else} code
        [op-cond] (-compile cond var)
        c-op-cond (count op-cond)
        [op-then] (-compile then (merge var {:offset (+ offset c-op-cond 2)}))
        c-op-then (count op-then)
        [op-else] (-compile else (merge var {:offset (+ offset c-op-cond c-op-then 4)}))
        c-op-else (count op-else)
        ]
    [(vec (concat
            op-cond
            [(a-inst (bit15 (i2ba (+ offset c-op-cond 2 c-op-then 2)))) ; A = offset + len(op-cond) + 2 +len(op-then) + 2
             (c-inst false op-d op-dest-null op-jeq)        ; D,JNE ; if cond = false jump to op-else
             ]
            op-then
            [(a-inst (bit15 (i2ba (+ offset c-op-cond 2 c-op-then 2 c-op-else)))) ; A = offset + len(op-cond) + 2 + len(op-then) + 2 + len(op-else)
             (c-inst false op-0 op-dest-null op-jmp)]       ; D,JMP ; jump to last
            op-else
            ))
     var]))

(defn increment-offset [[code var]]
  [code (merge var {:offset (+ (:offset var) (count code))})])

(defn -compile [code var]
  "var - Variable table
    :offset     - beginning index of the next code
    :local      - variable map
      key       - string name of variable
      value     - pointer to the variable
    :const      - strings index map
      key       - string itself is a key
      value     - pointer to the string
    :next-temp  - next pointer to the temporal (per-operation) variable
    :next-local - next pointer to the variable
    :next-const - next pointer to save objects"
  (let [type (:type code)]
    (increment-offset
      (cond
        (= type "assign") (compile-assign code var)
        (= type "int") [(compile-int code) var]
        (= type "str") (compile-str code var)
        (= type "ref") (compile-ref code var)
        (= type "le") (compile-le code var)
        (= type "inc") (compile-inc code var)
        (= type "rem") (compile-rem code var)
        (= type "call") (compile-call code var)
        (= type "for") (compile-for code var)
        (= type "if") (compile-if code var)
        ))
    )
  )

(defn compile [code]
  (let [[op] (-compile code {:offset 0 :local {} :const {} :next-temp 0 :next-local 4 :next-const 12})]
    (vec (concat
           op
           [exit-inst
            (a-inst zero15)]))))
