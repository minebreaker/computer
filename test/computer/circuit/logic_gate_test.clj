(ns computer.circuit.logic_gate_test
  (:require [clojure.test :refer :all]
            [computer.circuit.logic_gate :as l]))

(deftest nand-test
  (testing "NAND"
    (is (true? (l/nand false false)))
    (is (true? (l/nand false true)))
    (is (true? (l/nand true false)))
    (is (false? (l/nand true true)))))

(deftest not-test
  (testing "NOT"
    (is (true? (l/not false)))
    (is (false? (l/not true)))))

(deftest and-test
  (testing "AND"
    (is (false? (l/and false false)))
    (is (false? (l/and false true)))
    (is (false? (l/and true false)))
    (is (true? (l/and true true)))))

(deftest or-test
  (testing "OR"
    (is (false? (l/or false false)))
    (is (true? (l/or false true)))
    (is (true? (l/or true false)))
    (is (true? (l/or true true)))))

(deftest xor-test
  (testing "XOR"
    (is (false? (l/xor false false)))
    (is (true? (l/xor false true)))
    (is (true? (l/xor true false)))
    (is (false? (l/xor true true)))))
