(ns rip.deadcode.computer.component.alu-test
  (:require [clojure.test :refer :all]
            [rip.deadcode.computer.component.alu :refer :all]
            [rip.deadcode.computer.component.extensional :refer :all]))

(deftest half-adder-test
  (testing "half-adder"
    (is (= [false false] (half-adder false false)))
    (is (= [true false] (half-adder false true)))
    (is (= [true false] (half-adder true false)))
    (is (= [false true] (half-adder true true)))))

(deftest full-adder-test
  (testing "full-adder"
    (is (= [false false] (full-adder false false false)))
    (is (= [true false] (full-adder false true false)))
    (is (= [true false] (full-adder true false false)))
    (is (= [false true] (full-adder true true false)))
    (is (= [true false] (full-adder false false true)))
    (is (= [false true] (full-adder false true true)))
    (is (= [false true] (full-adder true false true)))
    (is (= [true true] (full-adder true true true)))))

(deftest adder16-test
  (testing "adder16"
    (is (=
          [zero16 false]
          (adder16 zero16 zero16 false)))
    (is (=
          [one16 false]
          (adder16 zero16 zero16 true)))
    (is (=
          [one16 false]
          (adder16 zero16 one16 false)))
    (is (=
          [one16 false]
          (adder16 one16 zero16 false)))
    (is (=
          [(i2ba 2) false]
          (adder16 zero16 one16 true)))
    (is (=
          [(i2ba 2) false]
          (adder16 one16 zero16 true)))
    (is (=
          [(i2ba 3) false]
          (adder16 one16 one16 true)))
    (is (=
          [m-one16 false]
          (adder16 zero16 m-one16 false)))
    (is (=
          [m-one16 false]
          (adder16 m-one16 zero16 false)))
    (is (=
          [(i2ba -2) true]
          (adder16 m-one16 m-one16 false)))))

(deftest inc16-test
  (testing "inc16"
    (is (= one16 (inc16 zero16)))
    (is (= (i2ba 2) (inc16 one16)))
    (is (= max16 (inc16 (i2ba 32766))))))

(deftest alu-test
  (testing "alu 0"
    (is (= [zero16 true false] (alu zero16 zero16 true false true false true false false)))
    (is (= [zero16 true false] (alu zero16 m-one16 true false true false true false false)))
    (is (= [zero16 true false] (alu m-one16 zero16 true false true false true false [zero16 true false])))
    (is (= [zero16 true false] (alu m-one16 m-one16 true false true false true false false))))
  (testing "alu 1"
    (is (= [one16 false false] (alu zero16 zero16 true true true true true true false)))
    (is (= [one16 false false] (alu zero16 m-one16 true true true true true true false)))
    (is (= [one16 false false] (alu m-one16 zero16 true true true true true true false)))
    (is (= [one16 false false] (alu m-one16 m-one16 true true true true true true false))))
  (testing "alu -1"
    (is (= [m-one16 false true] (alu zero16 zero16 true true true false true false false)))
    (is (= [m-one16 false true] (alu zero16 m-one16 true true true false true false false)))
    (is (= [m-one16 false true] (alu m-one16 zero16 true true true false true false false)))
    (is (= [m-one16 false true] (alu m-one16 m-one16 true true true false true false false))))
  (testing "alu x"
    (is (= [false16 true false] (alu false16 false16 false false true true false false false)))
    (is (= [false16 true false] (alu false16 true16 false false true true false false false)))
    (is (= [true16 false true] (alu true16 false16 false false true true false false false)))
    (is (= [true16 false true] (alu true16 true16 false false true true false false false))))
  (testing "alu y"
    (is (= [false16 true false] (alu false16 false16 true true false false false false false)))
    (is (= [true16 false true] (alu false16 true16 true true false false false false false)))
    (is (= [false16 true false] (alu true16 false16 true true false false false false false)))
    (is (= [true16 false true] (alu true16 true16 true true false false false false false))))
  (testing "alu !x"
    (is (= [true16 false true] (alu false16 false16 false false true true false true false)))
    (is (= [true16 false true] (alu false16 true16 false false true true false true false)))
    (is (= [false16 true false] (alu true16 false16 false false true true false true false)))
    (is (= [false16 true false] (alu true16 true16 false false true true false true false))))
  (testing "alu !y"
    (is (= [true16 false true] (alu false16 false16 true true false false false true false)))
    (is (= [false16 true false] (alu false16 true16 true true false false false true false)))
    (is (= [true16 false true] (alu true16 false16 true true false false false true false)))
    (is (= [false16 true false] (alu true16 true16 true true false false false true false))))
  (testing "alu -x"
    (is (= [m-one16 false true] (alu one16 one16 false false true true true true false)))
    (is (= [one16 false false] (alu m-one16 one16 false false true true true true false))))
  (testing "alu -y"
    (is (= [m-one16 false true] (alu one16 one16 true true false false true true false)))
    (is (= [one16 false false] (alu one16 m-one16 true true false false true true false))))
  (testing "alu x+1"
    (is (= [zero16 true false] (alu m-one16 one16 false true true true true true false)))
    (is (= [m-one16 false true] (alu (i2ba -2) one16 false true true true true true false))))
  (testing "alu y+1"
    (is (= [zero16 true false] (alu one16 m-one16 true true false true true true false)))
    (is (= [m-one16 false true] (alu one16 (i2ba -2) true true false true true true false))))
  (testing "alu x-1"
    (is (= [zero16 true false] (alu one16 one16 false false true true true false false)))
    (is (= [m-one16 false true] (alu zero16 one16 false false true true true false false))))
  (testing "alu y-1"
    (is (= [zero16 true false] (alu one16 one16 true true false false true false false)))
    (is (= [m-one16 false true] (alu one16 zero16 true true false false true false false))))
  (testing "alu x+y"
    (is (= [zero16 true false] (alu zero16 zero16 false false false false true false false)))
    (is (= [one16 false false] (alu zero16 one16 false false false false true false false)))
    (is (= [one16 false false] (alu one16 zero16 false false false false true false false)))
    (is (= [(i2ba 2) false false] (alu one16 one16 false false false false true false false)))
    (is (= (alu one16 m-one16 false false false false true false false)))
    (is (= [zero16 true false] (alu m-one16 one16 false false false false true false false) [zero16 true false]))
    (is (= [(i2ba -2) false true] (alu m-one16 m-one16 false false false false true false false))))
  (testing "alu x-y"
    (is (= [zero16 true false] (alu zero16 zero16 false true false false true true false)))
    (is (= [m-one16 false true] (alu zero16 one16 false true false false true true false)))
    (is (= [one16 false false] (alu one16 zero16 false true false false true true false)))
    (is (= [zero16 true false] (alu one16 one16 false true false false true true false)))
    (is (= [(i2ba 2) false false] (alu one16 m-one16 false true false false true true false)))
    (is (= [(i2ba -2) false true] (alu m-one16 one16 false true false false true true false)))
    (is (= [zero16 true false] (alu m-one16 m-one16 false true false false true true false))))
  (testing "alu y-x"
    (is (= [zero16 true false] (alu zero16 zero16 false false false true true true false)))
    (is (= [one16 false false] (alu zero16 one16 false false false true true true false)))
    (is (= [m-one16 false true] (alu one16 zero16 false false false true true true false)))
    (is (= [zero16 true false] (alu one16 one16 false false false true true true false)))
    (is (= [(i2ba -2) false true] (alu one16 m-one16 false false false true true true false)))
    (is (= [(i2ba 2) false false] (alu m-one16 one16 false false false true true true false)))
    (is (= [zero16 true false] (alu m-one16 m-one16 false false false true true true false))))
  (testing "alu x&y"
    (is (= [false16 true false] (alu false16 false16 false false false false false false false)))
    (is (= [false16 true false] (alu false16 true16 false false false false false false false)))
    (is (= [false16 true false] (alu true16 false16 false false false false false false false)))
    (is (= [true16 false true] (alu true16 true16 false false false false false false false))))
  (testing "alu x|y"
    (is (= [false16 true false] (alu false16 false16 false true false true false true false)))
    (is (= [true16 false true] (alu false16 true16 false true false true false true false)))
    (is (= [true16 false true] (alu true16 false16 false true false true false true false)))
    (is (= [true16 false true] (alu true16 true16 false true false true false true false))))
  (testing "alu out>>1"
    (is (= [(i2ba 8) false false] (alu (i2ba 7) (i2ba 9) false false false false true false true))) ; (7 + 9) >> 1
    (is (= [(i2ba 64) false false] (alu (i2ba 128) (i2ba 64) false true false false true true false))))) ; (128 - 64) >> 1
