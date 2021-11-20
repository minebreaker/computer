(ns rip.deadcode.computer.component.hardware-test
  (:require [clojure.test :refer :all]
            [rip.deadcode.computer.component.extensional :refer :all]
            [rip.deadcode.computer.component.opcode :refer :all]
            [rip.deadcode.computer.component.hardware :refer :all]))

(deftest console-test
  (testing "console output"
    (with-redefs [print identity]
      (is (= " " (console (i2ba 32) true)))
      (is (= "0" (console (i2ba 48) true)))
      (is (= "A" (console (i2ba 65) true)))
      (is (= "a" (console (i2ba 97) true)))
      (is (nil? (console zero16 false))))))

(deftest program-memory-test
  (testing "works fine"
    (let [
          op0 (a-inst (bit15 (i2ba 10)))                    ; @10
          op1 (c-inst false op-a+1 op-dest-d op-no-jump) ; D=A+1
          pm (make-program-memory [op0 op1])
          ]
      (is (= op0 (pm (i2ba 0))))
      (is (= op1 (pm (i2ba 1)))))))
