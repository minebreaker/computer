(ns rip.deadcode.computer.component.hardware-test
  (:require [clojure.test :refer :all]
            [rip.deadcode.computer.component.extensional :refer :all]
            [rip.deadcode.computer.component.hardware :refer :all]))

(deftest console-test
  (testing "console output"
    (with-redefs [print identity]
      (is (= " " (console (i2ba 32))))
      (is (= "0" (console (i2ba 48))))
      (is (= "A" (console (i2ba 65))))
      (is (= "a" (console (i2ba 97)))))))
