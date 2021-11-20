(ns rip.deadcode.computer.component.debug
  (:require [rip.deadcode.computer.component.hardware :refer :all]
            [rip.deadcode.computer.component.opcode :refer :all]
            [rip.deadcode.computer.component.extensional :refer :all]
            [clojure.core.match :refer [match]]))


(defn decode [[i0 i1 i2 i3 i4 i5 i6 i7 i8 i9 i10 i11 i12 i13 i14 i15]]
  (if (false? i0)
    (let [i (ba2i [i1 i2 i3 i4 i5 i6 i7 i8 i9 i10 i11 i12 i13 i14 i15 false])]
      (str "IA " i " " (i2s i)))
    (str
      "IC ["
      (let [A (if (false? i3) "A" "M")]
        (match [i4 i5 i6 i7 i8 i9]
          [true false true false true false] "0"
          [true true true true true true] "1"
          [true true true false true false] "-1"
          [false false true true false false] "D"
          [true true false false false false] A
          [false false true true false true] "!D"
          [true true false false false true] (str "!" A)
          [false false true true true true] "-D"
          [true true false false true true] (str "-" A)
          [false true true true true true] "D+1"
          [true true false true true true] (str A "+1")
          [false false true true true false] "D-1"
          [true true false false true false] (str A "-1")
          [false false false false true false] (str "D+" A)
          [false true false false true true] (str "D-" A)
          [false false false true true true] (str A "-D")
          [false false false false false false] (str "D&" A)
          [false true false true false true] (str "D|" A))) "] "
      "[" (match [i10 i11 i12]
            [false false false] "null"
            [false false true] "M"
            [false true false] "D"
            [false true true] "MD"
            [true false false] "A"
            [true false true] "AM"
            [true true false] "AD"
            [true true true] "AMD") "] "
      "[" (match [i13 i14 i15]
            [false false false] "null"
            [false false true] "JGT"
            [false true false] "JEQ"
            [false true true] "JGE"
            [true false false] "JLT"
            [true false true] "JNE"
            [true true false] "JLE"
            [true true true] "JMP") "]")))
