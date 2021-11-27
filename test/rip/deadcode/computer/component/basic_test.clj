(ns rip.deadcode.computer.component.basic-test
  (:require [clojure.test :refer :all]
            [rip.deadcode.computer.component.basic :refer :all]
            [rip.deadcode.computer.component.extensional :refer :all]))

(deftest not-test
  (testing "not"
    (is (true? (not false)))
    (is (false? (not true)))))

(deftest not-test
  (testing "not"
    (is (true? (not false)))
    (is (false? (not true)))))

(deftest not16-16-test
  (testing "not16-16"
    (is (= m-one16 (not16-16 zero16)))))

(deftest and-test
  (testing "and"
    (is (false? (and false false)))
    (is (false? (and false true)))
    (is (false? (and true false)))
    (is (true? (and true true)))))

(deftest and16-16-test
  (testing "and16-16"
    (is (= zero16 (and16-16 zero16 zero16)))
    (is (= zero16 (and16-16 zero16 m-one16)))
    (is (= zero16 (and16-16 m-one16 zero16)))
    (is (= m-one16 (and16-16 m-one16 m-one16)))))

(deftest or-test
  (testing "or"
    (is (false? (or false false)))
    (is (true? (or false true)))
    (is (true? (or true false)))
    (is (true? (or true true)))))

(deftest or16-16-test
  (testing "or16-16"
    (is (= zero16 (or16-16 zero16 zero16)))
    (is (= m-one16 (or16-16 zero16 m-one16)))
    (is (= m-one16 (or16-16 m-one16 zero16)))
    (is (= m-one16 (or16-16 m-one16 m-one16)))))

(deftest or16-1-test
  (testing "or16-1"
    (is (= false (or16-1 zero16)))
    (is (= true (or16-1 [false false false false false false false false false false false false false false false true])))
    (is (= true (or16-1 [true false false false false false false false false false false false false false false false])))
    (is (= true (or16-1 m-one16)))))

(deftest xor-test
  (testing "xor"
    (is (false? (xor false false)))
    (is (true? (xor false true)))
    (is (true? (xor true false)))
    (is (false? (xor true true)))))

(deftest xor16-16-test
  (testing "xor16-16"
    (is (= zero16 (xor16-16 zero16 zero16)))
    (is (= m-one16 (xor16-16 zero16 m-one16)))
    (is (= m-one16 (xor16-16 m-one16 zero16)))
    (is (= zero16 (xor16-16 m-one16 m-one16)))))

(deftest mux-test
  (testing "mux"
    (is (false? (mux false false false)))
    (is (true? (mux true false false)))
    (is (false? (mux false true false)))
    (is (true? (mux true true false)))
    (is (false? (mux false false true)))
    (is (false? (mux true false true)))
    (is (true? (mux false true true)))
    (is (true? (mux true true true)))))

(deftest mux16-test
  (testing "mux16"
    (is (= zero16 (mux16 zero16 zero16 false)))
    (is (= m-one16 (mux16 m-one16 zero16 false)))
    (is (= zero16 (mux16 zero16 m-one16 false)))
    (is (= m-one16 (mux16 m-one16 m-one16 false)))
    (is (= zero16 (mux16 zero16 zero16 true)))
    (is (= zero16 (mux16 m-one16 zero16 true)))
    (is (= m-one16 (mux16 zero16 m-one16 true)))
    (is (= m-one16 (mux16 m-one16 m-one16 true)))))

(deftest mux8-test
  (testing "mux8"
    (is (false? (mux8 false false false false false false false false [false false false])))
    (is (true? (mux8 true false false false false false false false [false false false])))
    (is (true? (mux8 false true false false false false false false [true false false])))
    (is (true? (mux8 false false true false false false false false [false true false])))
    (is (true? (mux8 false false false true false false false false [true true false])))
    (is (true? (mux8 false false false false true false false false [false false true])))
    (is (true? (mux8 false false false false false true false false [true false true])))
    (is (true? (mux8 false false false false false false true false [false true true])))
    (is (true? (mux8 false false false false false false false true [true true true])))))

(deftest mux16-8-test
  (testing "mux16-8"
    (is (= false16 (mux16-8 false16 false16 false16 false16 false16 false16 false16 false16 [false false false])))
    (is (= true16 (mux16-8 true16 false16 false16 false16 false16 false16 false16 false16 [false false false])))
    (is (= true16 (mux16-8 false16 true16 false16 false16 false16 false16 false16 false16 [true false false])))
    (is (= true16 (mux16-8 false16 false16 true16 false16 false16 false16 false16 false16 [false true false])))
    (is (= true16 (mux16-8 false16 false16 false16 true16 false16 false16 false16 false16 [true true false])))
    (is (= true16 (mux16-8 false16 false16 false16 false16 true16 false16 false16 false16 [false false true])))
    (is (= true16 (mux16-8 false16 false16 false16 false16 false16 true16 false16 false16 [true false true])))
    (is (= true16 (mux16-8 false16 false16 false16 false16 false16 false16 true16 false16 [false true true])))
    (is (= true16 (mux16-8 false16 false16 false16 false16 false16 false16 false16 true16 [true true true])))))

(deftest dmux-test
  (testing "dmux"
    (is (= [false false] (dmux false false)))
    (is (= [true false] (dmux true false)))
    (is (= [false false] (dmux false true)))
    (is (= [false true] (dmux true true)))))

(deftest dmux8-test
  (testing "dmux8"
    (is (= [false false false false false false false false]
          (dmux8 false [false false false])))
    (is (= [true false false false false false false false]
          (dmux8 true [false false false])))
    (is (= [false true false false false false false false]
          (dmux8 true [true false false])))
    (is (= [false false true false false false false false]
          (dmux8 true [false true false])))
    (is (= [false false false true false false false false]
          (dmux8 true [true true false])))
    (is (= [false false false false true false false false]
          (dmux8 true [false false true])))
    (is (= [false false false false false true false false]
          (dmux8 true [true false true])))
    (is (= [false false false false false false true false]
          (dmux8 true [false true true])))
    (is (= [false false false false false false false true]
          (dmux8 true [true true true])))))
