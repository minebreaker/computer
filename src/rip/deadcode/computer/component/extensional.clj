(ns rip.deadcode.computer.component.extensional)

; Functions to cooperate with Hardware-world and Clojure world
; Also contains some utility functions for readability

(defn i2ba [i]
  (reduce
    #(assoc %1 %2 (not=
                    (bit-and i (bit-shift-left 1 %2))
                    0))
    []
    (range 0 16)))

(defn ba2i [ba]
  (let [convert (fn [ba] (reduce
                           #(bit-or (bit-shift-left %1 1) (if (true? %2) 1 0))
                           0
                           (reverse ba)))]
    (if (false? (nth ba 15))                                ; 2's complement?
      (convert ba)
      (- (+ 1 (convert (map #(not %) ba)))))))

(def false16 (vec (repeat 16 false)))
(def true16 (vec (repeat 16 true)))
(def zero16 false16)
(def one16 (assoc zero16 0 true))
(def m-one16 true16)
(def two16 (assoc zero16 1 true))
(def three16 (assoc two16 0 true))
(def max16 (assoc true16 15 false))
(def min16 (assoc false16 15 true))

(defn expand16 [in]
  "Expands 1bit input to 16bit"
  [in in in in in in in in in in in in in in in in])

(defn bit15 [in] (subvec in 0 15))
(def zero15 (bit15 zero16))
(def one15 (bit15 one16))
(def two15 (bit15 two16))
(def three15 (bit15 three16))

(def dont-care false16)                                     ; just a convention

(defn i2s [i] "Converts given int code point to String" (new String (Character/toChars i)))
(defn s2ba [s] "Converts a first letter of given string to bytes" (i2ba (int (nth s 0))))
(def console-idx (bit15 (i2ba 16384)))

; comp mnemonics
(def op-0 [true false true false true false])
(def op-1 [true true true true true true])
(def op--1 [true true true false true false])
(def op-d [false false true true false false])
(def op-a [true true false false false false])
(def op-!d [false false true true false true])
(def op-!a [true true false false false true])
(def op--d [false false true true true true])
(def op--a [true true false false true true])
(def op-d+1 [false true true true true true])
(def op-a+1 [true true false true true true])
(def op-d-1 [false false true true true false])
(def op-a-1 [true true false false true false])
(def op-d+a [false false false false true false])
(def op-d-a [false true false false true true])
(def op-a-d [false false false true true true])
(def op-a&d [false false false false false false])
(def op-d|a [false true false true false true])

; dest mnemonics
(def op-dest-null [false false false])
(def op-dest-m [false false true])
(def op-dest-d [false true false])
(def op-dest-md [false true true])
(def op-dest-a [true false false])
(def op-dest-am [true false true])
(def op-dest-ad [true true false])
(def op-dest-amd [true true true])

; jump mnemonics
(def op-no-jump [false false false])
(def op-jgt [false false true])
(def op-jeq [false true false])
(def op-jge [false true true])
(def op-jlt [true false false])
(def op-jne [true false true])
(def op-jle [true true false])
(def op-jmp [true true true])

(defn a-inst [in]
  "Creates a-instruction from bool array[15]"
  (into [false]
    (cond
      (int? in) (bit15 (i2ba in))
      (string? in) (bit15 (s2ba in))
      (= 15 (count in)) in                                  ; bit array[15]
      (= 16 (count in)) (bit15 in))                         ; bit array[16]
    )
  )

(defn c-inst
  "Creates c-instruction from extension-code, a-code, comp[6], dest[3], jump[3]"
  ([comp dest] (c-inst false comp dest))
  ([a comp dest] (c-inst a comp dest op-no-jump))
  ([a comp dest jump] (c-inst [false false] a comp dest jump))
  ([ext a comp dest jump] (vec (concat [true] ext [a] comp dest jump))))

(def exit-inst
  "Signals to exit"
  (vec (concat [true true false] op-0 op-dest-null op-no-jump)))
