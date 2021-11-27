(ns rip.deadcode.computer.component.memory-test
  (:require [clojure.test :refer :all]
            [rip.deadcode.computer.component.extensional :refer :all]
            [rip.deadcode.computer.component.memory :refer :all]))

(deftest register16-test
  (testing "default value"
    (is (= false16
          (let [r (make-register16)]
            (r false16 false))))
    (is (= false16
          (let [r (make-register16)]
            (r true16 false)))))
  (testing "remembering"
    (is (= false16
          (let [r (make-register16)]
            (r false16 true)
            (r false16 false))))
    (is (= false16
          (let [r (make-register16)]
            (r false16 true)
            (r true16 false))))
    (is (= true16
          (let [r (make-register16)]
            (r true16 true)
            (r false16 true))))
    (is (= true16
          (let [r (make-register16)]
            (r true16 true)
            (r true16 true))))))

(deftest make-ram8-test
  (testing "remembering"
    (is (= false16
          (let [r (make-ram8)]
            (r false16 [false false false] true)
            (r false16 [false false false] false))))
    (is (= false16
          (let [r (make-ram8)]
            (r false16 [false false false] true)
            (r true16 [false false false] false))))
    (is (= true16
          (let [r (make-ram8)]
            (r true16 [false false false] true)
            (r false16 [false false false] true))))
    (is (= true16
          (let [r (make-ram8)]
            (r true16 [false false false] true)
            (r true16 [false false false] true)))))
  (testing "address"
    (is (= true16
          (let [r (make-ram8)]
            (r true16 [false false false] true)
            (r false16 [true false false] true)
            (r false16 [false false false] false))))
    (is (= true16
          (let [r (make-ram8)]
            (r true16 [true false false] true)
            (r false16 [true false false] false))))))

(deftest make-ram8-test2
  (testing "ram8"
    (let [r (make-ram8)
          _ (r (i2ba 0) [false false false] true)
          _ (r (i2ba 1) [true false false] true)

          o0 (r zero16 [false false false] false)
          o1 (r zero16 [true false false] false)
          ]
      (is (= (i2ba 0) o0))
      (is (= (i2ba 1) o1))
      )
    ))

(deftest make-ram64-test
  (testing "ram64"
    (let [r (make-ram64)
          _ (r (i2ba 1) [false false false false false false] true)
          _ (r (i2ba 2) [true false false false false false] true)
          _ (r (i2ba 3) [false false false true false false] true)
          _ (r (i2ba 4) [true false false true false false] true)

          o0 (r zero16 [false false false false false false] false)
          o1 (r zero16 [true false false false false false] false)
          o2 (r zero16 [false false false true false false] false)
          o3 (r zero16 [true false false true false false] false)
          ]
      (is (= o0 (i2ba 1)))
      (is (= o1 (i2ba 2)))
      (is (= o2 (i2ba 3)))
      (is (= o3 (i2ba 4)))
      )
    ))

(deftest make-ram16k-test
  (testing "ram16k"
    (let [r (make-ram16k)
          _ (r (i2ba 1) (i2ba 0) true)
          _ (r (i2ba 2) (i2ba 1) true)
          _ (r (i2ba 3) (i2ba 2) true)
          _ (r (i2ba 4) (i2ba 3) true)
          _ (r (i2ba 5) (i2ba 4) true)
          _ (r (i2ba 6) (i2ba 5) true)
          _ (r (i2ba 7) (i2ba 6) true)
          _ (r (i2ba 8) (i2ba 7) true)

          o0 (r zero16 (i2ba 0) false)
          o1 (r zero16 (i2ba 1) false)
          o2 (r zero16 (i2ba 2) false)
          o3 (r zero16 (i2ba 3) false)
          o4 (r zero16 (i2ba 4) false)
          o5 (r zero16 (i2ba 5) false)
          o6 (r zero16 (i2ba 6) false)
          o7 (r zero16 (i2ba 7) false)
          ]
      (is (= o0 (i2ba 1)))
      (is (= o1 (i2ba 2)))
      (is (= o2 (i2ba 3)))
      (is (= o3 (i2ba 4)))
      (is (= o4 (i2ba 5)))
      (is (= o5 (i2ba 6)))
      (is (= o6 (i2ba 7)))
      (is (= o7 (i2ba 8)))
      )
    ))

(deftest make-pc-test
  (testing "load"
    (is (= true16
          (let [pc (make-pc)]
            (pc true16 false true false)
            (pc false16 false true false))))
    (is (= one16
          (let [pc (make-pc)]
            (pc true16 false true false)
            (pc one16 false true false)
            (pc true16 false true false)))))
  (testing "increment"
    (is (= zero16
          (let [pc (make-pc)]
            (pc zero16 true false false))))
    (is (= one16
          (let [pc (make-pc)]
            (pc zero16 true false false)
            (pc zero16 true false false))))
    (is (= (i2ba 2)
          (let [pc (make-pc)]
            (pc zero16 true false false)
            (pc zero16 true false false)
            (pc zero16 true false false)))))
  (testing "reset"
    (is (= zero16
          (let [pc (make-pc)]
            (pc true16 false true false)
            (pc true16 false false true)
            (pc true16 false true false)))))
  (testing "unchanged"
    (is (= true16
          (let [pc (make-pc)]
            (pc true16 false true false)
            (pc false16 false false false)
            (pc false16 false false false))))))
