(ns rip.deadcode.computer.component.alu-test
  (:require [clojure.test :refer :all]
            [rip.deadcode.computer.component.alu :refer :all]
            [rip.deadcode.computer.component.extensional :refer :all]))

(deftest half-adder-test
  (testing "half-adder"
    (is (= (half-adder false false) [false false]))
    (is (= (half-adder false true) [true false]))
    (is (= (half-adder true false) [true false]))
    (is (= (half-adder true true) [false true]))))

(deftest full-adder-test
  (testing "full-adder"
    (is (= (full-adder false false false) [false false]))
    (is (= (full-adder false true false) [true false]))
    (is (= (full-adder true false false) [true false]))
    (is (= (full-adder true true false) [false true]))
    (is (= (full-adder false false true) [true false]))
    (is (= (full-adder false true true) [false true]))
    (is (= (full-adder true false true) [false true]))
    (is (= (full-adder true true true) [true true]))))

(deftest adder16-test
  (testing "adder16"
    (is (=
          (adder16 zero16 zero16 false)
          [zero16 false]))
    (is (=
          (adder16 zero16 zero16 true)
          [one16 false]))
    (is (=
          (adder16 zero16 one16 false)
          [one16 false]))
    (is (=
          (adder16 one16 zero16 false)
          [one16 false]))
    (is (=
          (adder16 zero16 one16 true)
          [(i2ba 2) false]))
    (is (=
          (adder16 one16 zero16 true)
          [(i2ba 2) false]))
    (is (=
          (adder16 one16 one16 true)
          [(i2ba 3) false]))
    (is (=
          (adder16 zero16 m-one16 false)
          [m-one16 false]))
    (is (=
          (adder16 m-one16 zero16 false)
          [m-one16 false]))
    (is (=
          (adder16 m-one16 m-one16 false)
          [(i2ba -2) true]))))

(deftest inc16-test
  (testing "inc16"
    (is (= (inc16 zero16) one16))
    (is (= (inc16 one16) (i2ba 2)))
    (is (= (inc16 (i2ba 32766)) max16))))

(deftest alu-test
  (testing "alu 0"
    (is (= (alu zero16 zero16 true false true false true false) [zero16 true false]))
    (is (= (alu zero16 m-one16 true false true false true false) [zero16 true false]))
    (is (= (alu m-one16 zero16 true false true false true false) [zero16 true false]))
    (is (= (alu m-one16 m-one16 true false true false true false) [zero16 true false])))
  (testing "alu 1"
    (is (= (alu zero16 zero16 true true true true true true) [one16 false false]))
    (is (= (alu zero16 m-one16 true true true true true true) [one16 false false]))
    (is (= (alu m-one16 zero16 true true true true true true) [one16 false false]))
    (is (= (alu m-one16 m-one16 true true true true true true) [one16 false false])))
  (testing "alu -1"
    (is (= (alu zero16 zero16 true true true false true false) [m-one16 false true]))
    (is (= (alu zero16 m-one16 true true true false true false) [m-one16 false true]))
    (is (= (alu m-one16 zero16 true true true false true false) [m-one16 false true]))
    (is (= (alu m-one16 m-one16 true true true false true false) [m-one16 false true])))
  (testing "alu x"
    (is (= (alu false16 false16 false false true true false false) [false16 true false]))
    (is (= (alu false16 true16 false false true true false false) [false16 true false]))
    (is (= (alu true16 false16 false false true true false false) [true16 false true]))
    (is (= (alu true16 true16 false false true true false false) [true16 false true])))
  (testing "alu y"
    (is (= (alu false16 false16 true true false false false false) [false16 true false]))
    (is (= (alu false16 true16 true true false false false false) [true16 false true]))
    (is (= (alu true16 false16 true true false false false false) [false16 true false]))
    (is (= (alu true16 true16 true true false false false false) [true16 false true])))
  (testing "alu !x"
    (is (= (alu false16 false16 false false true true false true) [true16 false true]))
    (is (= (alu false16 true16 false false true true false true) [true16 false true]))
    (is (= (alu true16 false16 false false true true false true) [false16 true false]))
    (is (= (alu true16 true16 false false true true false true) [false16 true false])))
  (testing "alu !y"
    (is (= (alu false16 false16 true true false false false true) [true16 false true]))
    (is (= (alu false16 true16 true true false false false true) [false16 true false]))
    (is (= (alu true16 false16 true true false false false true) [true16 false true]))
    (is (= (alu true16 true16 true true false false false true) [false16 true false])))
  (testing "alu -x"
    (is (= (alu one16 one16 false false true true true true) [m-one16 false true]))
    (is (= (alu m-one16 one16 false false true true true true) [one16 false false])))
  (testing "alu -y"
    (is (= (alu one16 one16 true true false false true true) [m-one16 false true]))
    (is (= (alu one16 m-one16 true true false false true true) [one16 false false])))
  (testing "alu x+1"
    (is (= (alu m-one16 one16 false true true true true true) [zero16 true false]))
    (is (= (alu (i2ba -2) one16 false true true true true true) [m-one16 false true])))
  (testing "alu y+1"
    (is (= (alu one16 m-one16 true true false true true true) [zero16 true false]))
    (is (= (alu one16 (i2ba -2) true true false true true true) [m-one16 false true])))
  (testing "alu x-1"
    (is (= (alu one16 one16 false false true true true false) [zero16 true false]))
    (is (= (alu zero16 one16 false false true true true false) [m-one16 false true])))
  (testing "alu y-1"
    (is (= (alu one16 one16 true true false false true false) [zero16 true false]))
    (is (= (alu one16 zero16 true true false false true false) [m-one16 false true])))
  (testing "alu x+y"
    (is (= (alu zero16 zero16 false false false false true false) [zero16 true false]))
    (is (= (alu zero16 one16 false false false false true false) [one16 false false]))
    (is (= (alu one16 zero16 false false false false true false) [one16 false false]))
    (is (= (alu one16 one16 false false false false true false) [(i2ba 2) false false]))
    (is (= (alu one16 m-one16 false false false false true false) [zero16 true false]))
    (is (= (alu m-one16 one16 false false false false true false) [zero16 true false]))
    (is (= (alu m-one16 m-one16 false false false false true false) [(i2ba -2) false true])))
  (testing "alu x-y"
    (is (= (alu zero16 zero16 false true false false true true) [zero16 true false]))
    (is (= (alu zero16 one16 false true false false true true) [m-one16 false true]))
    (is (= (alu one16 zero16 false true false false true true) [one16 false false]))
    (is (= (alu one16 one16 false true false false true true) [zero16 true false]))
    (is (= (alu one16 m-one16 false true false false true true) [(i2ba 2) false false]))
    (is (= (alu m-one16 one16 false true false false true true) [(i2ba -2) false true]))
    (is (= (alu m-one16 m-one16 false true false false true true) [zero16 true false])))
  (testing "alu y-x"
    (is (= (alu zero16 zero16 false false false true true true) [zero16 true false]))
    (is (= (alu zero16 one16 false false false true true true) [one16 false false]))
    (is (= (alu one16 zero16 false false false true true true) [m-one16 false true]))
    (is (= (alu one16 one16 false false false true true true) [zero16 true false]))
    (is (= (alu one16 m-one16 false false false true true true) [(i2ba -2) false true]))
    (is (= (alu m-one16 one16 false false false true true true) [(i2ba 2) false false]))
    (is (= (alu m-one16 m-one16 false false false true true true) [zero16 true false])))
  (testing "alu x&y"
    (is (= (alu false16 false16 false false false false false false) [false16 true false]))
    (is (= (alu false16 true16 false false false false false false) [false16 true false]))
    (is (= (alu true16 false16 false false false false false false) [false16 true false]))
    (is (= (alu true16 true16 false false false false false false) [true16 false true])))
  (testing "alu x|y"
    (is (= (alu false16 false16 false true false true false true) [false16 true false]))
    (is (= (alu false16 true16 false true false true false true) [true16 false true]))
    (is (= (alu true16 false16 false true false true false true) [true16 false true]))
    (is (= (alu true16 true16 false true false true false true) [true16 false true]))))
