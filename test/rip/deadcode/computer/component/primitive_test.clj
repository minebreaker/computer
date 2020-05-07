(ns rip.deadcode.computer.component.primitive-test
  (:require [clojure.test :refer :all]
            [rip.deadcode.computer.component.primitive :refer :all]))

(deftest nand-test
  (testing "nand"
    (is (true? (nand false false)))
    (is (true? (nand false true)))
    (is (true? (nand true false)))
    (is (false? (nand true true)))))

(deftest make-flipflop-test
  (testing "flipflop"
    (is (false? (let [ff (make-flipflop)]
                  (ff false))))
    (is (false? (let [ff (make-flipflop)]
                  (ff true)))))
  (testing "remembering"
    (is (false? (let [ff (make-flipflop)]
                  (ff false)
                  (ff false))))
    (is (false? (let [ff (make-flipflop)]
                  (ff false)
                  (ff true))))
    (is (true? (let [ff (make-flipflop)]
                 (ff true)
                 (ff false))))
    (is (true? (let [ff (make-flipflop)]
                 (ff true)
                 (ff true))))))
