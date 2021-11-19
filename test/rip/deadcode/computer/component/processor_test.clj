(ns rip.deadcode.computer.component.processor-test
  (:require [clojure.test :refer :all]
            [rip.deadcode.computer.component.extensional :refer :all]
            [rip.deadcode.computer.component.processor :refer :all]))

(defn a-inst [bits] (into [false] bits))
(defn c-inst [a comp dest jump]
  (->
    [true]
    (into [true true])
    (into [a])
    (into comp)
    (into dest)
    (into jump)))
(defn bit15 [in] (subvec in 0 15))
(def zero15 (bit15 zero16))
(def one15 (bit15 one16))
(def two15 (bit15 two16))
(def three15 (bit15 three16))

(deftest cpu-test-a-instruction
  (testing "a-instruction"
    (let [
          cpu (make-cpu)
          memset5 [false false true false false false false false false false false false false false false]
          memset6 [false false false false false false false false false false false false false false true]
          _ (cpu (into [false] zero15) false16 false)
          [_ _ addressM-1 _] (cpu (a-inst one15) false16 false)
          [_ _ addressM-2 _] (cpu (a-inst two15) false16 false)
          [_ _ addressM-3 _] (cpu (a-inst three15) false16 false)
          [_ _ addressM-4 _] (cpu (a-inst memset5) false16 false)
          [_ _ addressM-5 _] (cpu (a-inst memset6) false16 false)
          [_ _ addressM-6 _] (cpu (a-inst zero15) false16 false)
          ]
      (is (= zero15 (bit15 addressM-1)))
      (is (= one15 (bit15 addressM-2)))
      (is (= two15 (bit15 addressM-3)))
      (is (= three15 (bit15 addressM-4)))
      (is (= memset5 (bit15 addressM-5)))
      (is (= memset6 (bit15 addressM-6)))
      )))

(deftest cpu-test-c-instruction-a-comp
  (testing "a-comp"
    (let [
          cpu (make-cpu)
          _ (cpu (a-inst one15) false16 false)              ; @1
          _ (cpu (c-inst false [true true false false false false] [false true false] [false false false]) false16 false) ; D=A
          _ (cpu (a-inst two15) false16 false)              ; @2
          [outM1] (cpu (c-inst false [false false false false true false] [false true false] [false false false]) false16 false) ; D=D+A
          [outM2] (cpu (c-inst false [false false true true true false] [false true false] [false false false]) false16 false) ; D=D-1
          [outM3] (cpu (c-inst false [true true true false true false] [false true false] [false false false]) false16 false) ; D=-1
          ]
      (is (= three16 outM1))
      (is (= two16 outM2))
      (is (= m-one16 outM3)))))

(deftest cpu-test-c-instruction-c-comp
  (testing "a-comp"
    (let [
          cpu (make-cpu)
          [outM] (cpu (c-inst true [true true false true true true] [false true false] [false false false]) one16 false) ; M+1
          ]
      (is (= two16 outM)))))

(deftest cpu-test-c-instruction-dest
  (testing "dest"
    (let [
          cpu (make-cpu)
          _ (cpu (a-inst one15) false16 false)              ; @1
          [outM1 writeM1 addressM1] (cpu (c-inst false [true true false false false false] [false true true] [false false false]) one16 false) ; MD=A
          [outM2 writeM2 addressM2] (cpu (c-inst false [false true true true true true] [true false false] [false false false]) false16 false) ; A=D+1
          [outM3 writeM3 addressM3] (cpu (c-inst true [true true false false false false] [true true false] [false false false]) one16 false) ; AD=M

          ]
      (is (= one16 outM1))
      (is (= true writeM1))
      (is (= one16 addressM1))
      (is (= two16 outM2))
      (is (= false writeM2))
      (is (= one16 addressM2))
      (is (= one16 outM3))
      (is (= false writeM3))
      (is (= two16 addressM3)))))

(deftest cpu-test-jump
  (testing "jump"
    (let [
          cpu (make-cpu)
          [_ _ _ pc1] (cpu (a-inst one15) false16 false)    ; @1
          [_ _ _ pc2] (cpu (a-inst (bit15 (i2ba 10))) false16 false) ; @10
          [_ _ _ pc3] (cpu (c-inst false [true true true false true false] [false true false] [false false false]) false16 false) ; D=-1
          [_ _ _ pc4] (cpu (c-inst false [false false true true false false] [false false false] [false false true]) false16 false) ; D,JGT ; false
          [_ _ _ pc5] (cpu (c-inst false [false false true true false false] [false false false] [false true false]) false16 false) ; D,JEQ ; false
          [_ _ _ pc6] (cpu (c-inst false [false false true true false false] [false false false] [false true true]) false16 false) ; D,JGE ; false
          [_ _ _ pc7] (cpu (c-inst false [false false true true false false] [false false false] [true false false]) false16 false) ; D,JLT ; true
          [_ _ _ pc8] (cpu (c-inst false [false false true true false false] [false false false] [true false true]) false16 false) ; D,JNE ; true
          [_ _ _ pc9] (cpu (c-inst false [false false true true false false] [false false false] [true true false]) false16 false) ; D,JLE ; true
          [_ _ _ pc10] (cpu (c-inst false [false false true true false false] [false false false] [true true true]) false16 false) ; JMP ; true
          [_ _ _ pc11] (cpu (c-inst false [true false true false true false] [false true false] [false false false]) false16 false) ; D=0
          [_ _ _ pc12] (cpu (c-inst false [false false true true false false] [false false false] [false false true]) false16 false) ; D,JGT ; false
          [_ _ _ pc13] (cpu (c-inst false [false false true true false false] [false false false] [false true false]) false16 false) ; D,JEQ ; true
          [_ _ _ pc14] (cpu (c-inst false [false false true true false false] [false false false] [false true true]) false16 false) ; D,JGE ; true
          [_ _ _ pc15] (cpu (c-inst false [false false true true false false] [false false false] [true false false]) false16 false) ; D,JLT ; false
          [_ _ _ pc16] (cpu (c-inst false [false false true true false false] [false false false] [true false true]) false16 false) ; D,JNE ; false
          [_ _ _ pc17] (cpu (c-inst false [false false true true false false] [false false false] [true true false]) false16 false) ; D,JLE ; true
          [_ _ _ pc18] (cpu (c-inst false [false false true true false false] [false false false] [true true true]) false16 false) ; JMP ; true
          [_ _ _ pc19] (cpu (c-inst false [true true true true true true] [false true false] [false false false]) false16 false) ; D=1
          [_ _ _ pc20] (cpu (c-inst false [false false true true false false] [false false false] [false false true]) false16 false) ; D,JGT ; true
          [_ _ _ pc21] (cpu (c-inst false [false false true true false false] [false false false] [false true false]) false16 false) ; D,JEQ ; false
          [_ _ _ pc22] (cpu (c-inst false [false false true true false false] [false false false] [false true true]) false16 false) ; D,JGE ; true
          [_ _ _ pc23] (cpu (c-inst false [false false true true false false] [false false false] [true false false]) false16 false) ; D,JLT ; false
          [_ _ _ pc24] (cpu (c-inst false [false false true true false false] [false false false] [true false true]) false16 false) ; D,JNE ; true
          [_ _ _ pc25] (cpu (c-inst false [false false true true false false] [false false false] [true true false]) false16 false) ; D,JLE ; false
          [_ _ _ pc26] (cpu (c-inst false [false false true true false false] [false false false] [true true true]) false16 false) ; JMP ; true
          [_ _ _ pc27] (cpu (a-inst one15) false16 false)   ; @1
          ]
      (is (= zero16 pc1))                                   ; since inc of pc at the first clock is zero, our cpu runs ProgMem[0] twice
      (is (= one16 pc2))
      (is (= two16 pc3))
      (is (= three16 pc4))
      (is (= (i2ba 4) pc5))
      (is (= (i2ba 5) pc6))
      (is (= (i2ba 6) pc7))
      (is (= (i2ba 10) pc8))
      (is (= (i2ba 10) pc9))
      (is (= (i2ba 10) pc10))
      (is (= (i2ba 10) pc11))
      (is (= (i2ba 11) pc12))
      (is (= (i2ba 12) pc13))
      (is (= (i2ba 10) pc14))
      (is (= (i2ba 10) pc15))
      (is (= (i2ba 11) pc16))
      (is (= (i2ba 12) pc17))
      (is (= (i2ba 10) pc18))
      (is (= (i2ba 10) pc19))
      (is (= (i2ba 11) pc20))
      (is (= (i2ba 10) pc21))
      (is (= (i2ba 11) pc22))
      (is (= (i2ba 10) pc23))
      (is (= (i2ba 11) pc24))
      (is (= (i2ba 10) pc25))
      (is (= (i2ba 11) pc26))
      (is (= (i2ba 10) pc27))
      )))
