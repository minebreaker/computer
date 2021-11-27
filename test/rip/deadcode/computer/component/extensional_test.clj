(ns rip.deadcode.computer.component.extensional-test
  (:require [clojure.test :refer :all]
            [rip.deadcode.computer.component.extensional :refer :all]))

(deftest i2ba-test
  (testing "works fine"
    (is (= zero16 (i2ba 0)))
    (is (= one16 (i2ba 1)))
    (is (= [false true false false false false false false false false false false false false false false] (i2ba 2)))
    (is (= max16 (i2ba 32767)))
    (is (= m-one16 (i2ba -1)))
    (is (= [false true true true true true true true true true true true true true true true] (i2ba -2)))
    (is (= [true false true true true true true true true true true true true true true true] (i2ba -3)))
    (is (= min16 (i2ba -32768)))))

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
    (is (= false16 (expand16 false)))
    (is (= true16 (expand16 true)))))
