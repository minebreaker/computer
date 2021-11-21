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
         (c-inst false op-a op-dest-d op-no-jump)           ; D=A
         (a-inst console-idx)                               ; @console
         (c-inst false op-d op-dest-m op-no-jump)           ; M=D
         (a-inst (bit15 (i2ba 5)))                          ; @5
         (c-inst false op-0 op-dest-null op-jmp)            ; JMP
         (a-inst zero15)                                    ; null
         ]
        )
      )
    )
  )
