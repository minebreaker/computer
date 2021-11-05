(ns rip.deadcode.computer.component.memory-test
  (:require [clojure.test :refer :all]
            [rip.deadcode.computer.component.extensional :refer :all]
            [rip.deadcode.computer.component.memory :refer :all]))

(deftest register16-test
  (testing "default value"
    (is (= (let [r (make-register16)]
             (r false16 false))
          false16))
    (is (= (let [r (make-register16)]
             (r true16 false))
          false16)))
  (testing "remembering"
    (is (= (let [r (make-register16)]
             (r false16 true)
             (r false16 false))
          false16))
    (is (= (let [r (make-register16)]
             (r false16 true)
             (r true16 false))
          false16))
    (is (= (let [r (make-register16)]
             (r true16 true)
             (r false16 true))
          true16))
    (is (= (let [r (make-register16)]
             (r true16 true)
             (r true16 true))
          true16))))

(deftest make-ram8-test
  (testing "remembering"
    (is (= (let [r (make-ram8)]
             (r false16 [false false false] true)
             (r false16 [false false false] false))
          false16))
    (is (= (let [r (make-ram8)]
             (r false16 [false false false] true)
             (r true16 [false false false] false))
          false16))
    (is (= (let [r (make-ram8)]
             (r true16 [false false false] true)
             (r false16 [false false false] true))
          true16))
    (is (= (let [r (make-ram8)]
             (r true16 [false false false] true)
             (r true16 [false false false] true))
          true16)))
  (testing "address"
    (is (= (let [r (make-ram8)]
             (r true16 [false false false] true)
             (r false16 [true false false] true)
             (r false16 [false false false] false))
          true16))
    (is (= (let [r (make-ram8)]
             (r true16 [true false false] true)
             (r false16 [true false false] false))
          true16))))

(deftest make-pc-test
  (testing "load"
    (is (= (let [pc (make-pc)]
             (pc true16 false true false)
             (pc false16 false true false))
          true16))
    (is (= (let [pc (make-pc)]
             (pc true16 false true false)
             (pc one16 false true false)
             (pc true16 false true false))
          one16)))
  (testing "increment"
    (is (= (let [pc (make-pc)]
             (pc zero16 true false false))
          zero16))
    (is (= (let [pc (make-pc)]
             (pc zero16 true false false)
             (pc zero16 true false false))
          one16))
    (is (= (let [pc (make-pc)]
             (pc zero16 true false false)
             (pc zero16 true false false)
             (pc zero16 true false false))
          (i2ba 2))))
  (testing "reset"
    (is (= (let [pc (make-pc)]
             (pc true16 false true false)
             (pc true16 false false true)
             (pc true16 false true false))
          zero16)))
  (testing "unchanged"
    (is (= (let [pc (make-pc)]
             (pc true16 false true false)
             (pc false16 false false false)
             (pc false16 false false false))
          true16))))