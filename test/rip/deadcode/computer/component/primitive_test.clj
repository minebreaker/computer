(ns rip.deadcode.computer.component.primitive-test
  (:require [clojure.test :refer :all]
            [rip.deadcode.computer.component.primitive :refer :all]))

(deftest nand-test
  (testing "nand"
    (is (true? (nand false false)))
    (is (true? (nand false true)))
    (is (true? (nand true false)))
    (is (false? (nand true true)))))

(deftest make-register-test
  (testing "default value"
    (is (false? (let [r (make-register)]
                  (r false false))))
    (is (false? (let [r (make-register)]
                  (r true false))))
    )
  (testing "remembering"
    (is (false? (let [r (make-register)]
                  (r false true)
                  (r false false))))
    (is (false? (let [r (make-register)]
                  (r false true)
                  (r true false))))
    (is (true? (let [r (make-register)]
                 (r true true)
                 (r false true))))
    (is (true? (let [r (make-register)]
                 (r true true)
                 (r true true))))))
