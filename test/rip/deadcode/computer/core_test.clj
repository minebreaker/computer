(ns rip.deadcode.computer.core-test
  (:require [clojure.test :refer :all]
            [clojure.string :refer [join]]
            [clojure.core.match :refer [match]]
            [rip.deadcode.computer.core :refer :all]))

(deftest main-test
  (testing "main"
    (is (=
          (str
            (->>
              (range 1 101)
              (map
                #(match [(rem %1 3) (rem %1 5)]
                   [0 0] "FizzBuzz"
                   [0 _] "Fizz"
                   [_ 0] "Buzz"
                   :else %1)
                )
              (join "\n"))
            "\n")
          (with-out-str (-main))))))
