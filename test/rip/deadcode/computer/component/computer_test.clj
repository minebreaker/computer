(ns rip.deadcode.computer.component.computer-test
  (:require [clojure.test :refer :all]
            [rip.deadcode.computer.component.hardware :refer :all]
            [rip.deadcode.computer.component.computer :refer :all]
            [rip.deadcode.computer.component.extensional :refer :all]))

(deftest computer-test
  (testing "computer"
    (computer
      (make-program-memory
        [
         (a-inst (bit15 (s2ba "a")))
         (c-inst false [true true false false false false] [false true false] [false false false]) ; D=A
         (a-inst console-idx)                               ; @console
         (c-inst false [false false true true false false] [false false true] [false false false]) ; M=D
         (a-inst (bit15 (i2ba 5)))                          ; @5
         (c-inst false [true false true false true false] [false false false] [true true true]) ; JMP
         (a-inst zero15)                                    ; null
         ]
        )
      )
    )
  )
