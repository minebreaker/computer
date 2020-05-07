(ns rip.deadcode.computer.component.basic-test
  (:require [clojure.test :refer :all]
            [rip.deadcode.computer.component.basic :refer :all]
            [rip.deadcode.computer.component.extensional :refer [i2ba zero16 m-one16]]))

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
    (is (= (not16-16 zero16) m-one16))))

(deftest and-test
  (testing "and"
    (is (false? (and false false)))
    (is (false? (and false true)))
    (is (false? (and true false)))
    (is (true? (and true true)))))

(deftest and16-16-test
  (testing "and16-16"
    (is (= (and16-16 zero16 zero16) zero16))
    (is (= (and16-16 zero16 m-one16) zero16))
    (is (= (and16-16 m-one16 zero16) zero16))
    (is (= (and16-16 m-one16 m-one16) m-one16))))

(deftest or-test
  (testing "or"
    (is (false? (or false false)))
    (is (true? (or false true)))
    (is (true? (or true false)))
    (is (true? (or true true)))))

(deftest or16-16-test
  (testing "or16-16"
    (is (= (or16-16 zero16 zero16) zero16))
    (is (= (or16-16 zero16 m-one16) m-one16))
    (is (= (or16-16 m-one16 zero16) m-one16))
    (is (= (or16-16 m-one16 m-one16) m-one16))))

(deftest or16-1-test
  (testing "or16-1"
    (is (= (or16-1 zero16) false))
    (is (= (or16-1 [false false false false false false false false false false false false false false false true]) true))
    (is (= (or16-1 [true false false false false false false false false false false false false false false false]) true))
    (is (= (or16-1 m-one16) true))))

(deftest xor-test
  (testing "xor"
    (is (false? (xor false false)))
    (is (true? (xor false true)))
    (is (true? (xor true false)))
    (is (false? (xor true true)))))

(deftest xor16-16-test
  (testing "xor16-16"
    (is (= (xor16-16 zero16 zero16) zero16))
    (is (= (xor16-16 zero16 m-one16) m-one16))
    (is (= (xor16-16 m-one16 zero16) m-one16))
    (is (= (xor16-16 m-one16 m-one16) zero16))))

(deftest mux-test
  (testing "multiplexer"
    (is (false? (mux false false false)))
    (is (true? (mux true false false)))
    (is (false? (mux false true false)))
    (is (true? (mux true true false)))
    (is (false? (mux false false true)))
    (is (false? (mux true false true)))
    (is (true? (mux false true true)))
    (is (true? (mux true true true)))))

(deftest mux16-16-test
  (testing "mux16-16"
    (is (= (mux16-16 zero16 zero16 false) zero16))
    (is (= (mux16-16 m-one16 zero16 false) m-one16))
    (is (= (mux16-16 zero16 m-one16 false) zero16))
    (is (= (mux16-16 m-one16 m-one16 false) m-one16))
    (is (= (mux16-16 zero16 zero16 true) zero16))
    (is (= (mux16-16 m-one16 zero16 true) zero16))
    (is (= (mux16-16 zero16 m-one16 true) m-one16))
    (is (= (mux16-16 m-one16 m-one16 true) m-one16))))

(deftest dmux-test
  (testing "demultiplexer"
    (is (= (dmux false false) [false false]))
    (is (= (dmux true false) [true false]))
    (is (= (dmux false true) [false false]))
    (is (= (dmux true true) [false true]))))
