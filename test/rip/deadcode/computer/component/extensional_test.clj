(ns rip.deadcode.computer.component.extensional-test
  (:require [clojure.test :refer :all]
            [rip.deadcode.computer.component.extensional :refer :all]))

(deftest i2ba-test
  (testing "works fine"
    (is (= (i2ba 0) zero16))
    (is (= (i2ba 1) one16))
    (is (= (i2ba 2) [false true false false false false false false false false false false false false false false]))
    (is (= (i2ba 32767) max16))
    (is (= (i2ba -1) m-one16))
    (is (= (i2ba -2) [false true true true true true true true true true true true true true true true]))
    (is (= (i2ba -3) [true false true true true true true true true true true true true true true true]))
    (is (= (i2ba -32768) min16))))

(deftest ba2i-test
  (testing "works fine"
    (is (= 0 (ba2i zero16)))
    (is (= 1 (ba2i one16)))
    (is (= -1 (ba2i m-one16)))
    (is (= 2 (ba2i two16)))
    (is (= 3 (ba2i three16)))
    (is (= 32767 (ba2i max16)))
    (is (= -32768 (ba2i min16)))))

(deftest expand16-test
  (testing "expand16"
    (is (= (expand16 false) false16))
    (is (= (expand16 true) true16))))
