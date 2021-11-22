(ns rip.deadcode.computer.component.compiler
  (:require [rip.deadcode.computer.component.extensional :refer :all]
            [rip.deadcode.computer.component.hardware :refer [console-idx]]
            [clojure.string :refer [blank? split]]
            [clojure.core.match :refer [match]]))

; #include <stdio.h>;
; int main() {
;   for (i = 0; i <= 100; i++) {
;     if (i % 15 == 0) {
;       println("FizzBuzz");
;     } else if (i % 5 == 0) {
;       println("Buzz");
;     } else if (i % 3 == 0) {
;       println("Fizz");
;     } else {
;       println(i);
;     }
;   }
; }

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
                         [(["}" "else" "if" & _] :seq)] [(parse (drop 2 res-then')) nil])
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
      (= t1 "++") {:type "inc" :x (parse-value t0) :rest (drop-semi (drop 2 code))}
      (and (= t1 "%") (= t3 "==")) {:type "rem" :x (parse-value t0) :y (parse-value t2) :z (parse-value t4) :rest (drop-semi (drop 5 code))}
      (= t0 "for") (parse-for code)
      (= t0 "if") (parse-if code)
      (and (re-matches #"[\w]+" t0) (= t1 "(") (= t3 ")")) {:type "call" :name t0 :arg (parse-value t2) :rest (drop-semi (drop 4 code))}
      :else (let [v (parse-value t0)] (merge v {:rest (rest code)})))))

(declare -compile)

(defn compile-int [code]
  [(a-inst (bit15 (i2ba (:value code))))                    ; @i
   (c-inst false op-a op-dest-d op-no-jump)                 ; D=A
   ])

(defn compile-str [code var]
  (let [obj-ptr (:next var)
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
                             (a-inst zero15)                ; @0
                             (c-inst true op-d op-dest-m)   ; M=D ; t0 = obj-ptr
                             (a-inst (bit15 (i2ba (+ n 1)))) ; @(n+1)
                             (c-inst false op-a op-dest-d)  ; D=A
                             (a-inst zero15)                ; @0
                             (c-inst true op-d+a op-dest-m) ; M=M+D ; t0 = obj-ptr + n
                             (a-inst (bit15 (i2ba (int c)))) ; @char
                             (c-inst false op-a op-dest-d)  ; D=A
                             (a-inst zero15)                ; @0
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
  (let [var-ptr (get (:var var) (:name code))]
    [[
      (a-inst (bit15 (i2ba (int var-ptr))))                 ; @var-ptr
      (c-inst false op-a op-dest-d op-no-jump)              ; D=M
      ]
     var]))

(defn compile-le [code var]
  (let [[op] (-compile (:x code) var)
        [op'] (-compile (:y op) var)]
    [(merge
       op                                                   ; D=x
       [(a-inst zero15)                                     ; @0
        (c-inst false op-d op-dest-m)                       ; M=D ; t0=x
        ]
       op'                                                  ; D=y
       [(a-inst zero15)                                     ; @0
        (c-inst false op-d-a op-dest-d)                     ; D=M-D ; x-y
        (a-inst (bit15 (i2ba (+ (:offset var) 7))))         ; @jump ; i+7
        (c-inst false op-d op-dest-null op-jle)             ; JLE,D
        (c-inst false op-0 op-dest-d)                       ; D=0
        (c-inst false op-1 op-dest-d)                       ; D=1
        ])
     var]))


(defn compile-inc [code var]
  ; i++
  (let [name (:name code)
        ptr (get (:var var) name)]
    ; @ptr
    ; MD=M+1
    [, var]
    )
  )

(defn compile-rem [code var])

(defn compile-call [code var]
  (let [{name :name arg :arg} code
        arg-type (:type arg)]
    [(cond
       (= name "println") (let [out (cond
                                      (= arg-type "str") (:value arg)
                                      (= arg-type "int") (Integer/toString (:value arg)))
                                ]
                            (vec (reduce
                                   #(concat %1 %2)
                                   (map
                                     (fn [c]
                                       [(a-inst (bit15 (i2ba (int c)))) ; @char
                                        (c-inst false op-a op-dest-d) ; D=A
                                        (a-inst console-idx) ; @console
                                        (c-inst true op-d op-dest-m) ; M=D
                                        ])
                                     (str out "\n"))))
                            ))
     var
     ]))


(defn compile-for [code var])

(defn compile-if [code var]
  (let [offset (:offset var)
        {cond :cond then :then else :else} code
        [op-cond] (-compile cond var)
        c-op-cond (count op-cond)
        [op-then] (-compile then (merge var {:offset (+ c-op-cond 2)}))
        c-op-then (count op-then)
        [op-else] (-compile else (merge var {:offset (+ c-op-cond c-op-then 4)}))
        c-op-else (count op-else)
        ]
    [(vec (concat
            op-cond
            [(a-inst (bit15 (i2ba (+ offset c-op-cond c-op-then 4)))) ; A = offset + len(op-cond) + 2 +len(op-then) + 2
             (c-inst false op-d op-dest-null op-jeq)        ; D,JNE ; if cond = false jump to op-else
             ]
            op-then
            [(a-inst (bit15 (i2ba (+ offset c-op-cond c-op-then c-op-else 4)))) ; A = offset + len(op-cond) + 2 + len(op-then) + 2 + len(op-else)
             (c-inst false op-0 op-dest-null op-jmp)]       ; D,JMP ; jump to last
            op-else
            ))
     var]))

(defn increment-code-index [[code var]]
  [code (merge var {:offset (+ (:offset var) (count code))})])

(defn -compile [code var]
  "var - Variable table
    ::offset    - beginning index of the next code
    :var        - variable map
      key       - string name of variable
      value     - pointer to the variable
    :const      - strings index map
      key       - string itself is a key
      value     - pointer to the string
    :next-var   - next pointer to the variable
    :next-const - next pointer to save objects"
  (let [type (:type code)]
    (increment-code-index
      (cond
        (= type "assign") nil
        (= type "int") [(compile-int code) var]
        (= type "str") (compile-str code var)
        (= type "ref") (compile-ref code var)
        (= type "le") (compile-le code var)
        (= type "inc") nil
        (= type "rem") nil
        (= type "call") (compile-call code var)
        (= type "for") nil
        (= type "if") (compile-if code var)
        ))
    )
  )

(defn compile [code]
  (let [[op var] (-compile code {:offset 0 :var {} :const {} :next-var 4 :next-const 12})]
    (vec (concat
           op
           [(a-inst (bit15 (i2ba (+ (:offset var) 1))))
            (c-inst false op-0 op-dest-null op-jmp)         ; Infinite jump to here
            (a-inst zero15)]))))
