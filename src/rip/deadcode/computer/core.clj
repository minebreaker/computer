(ns rip.deadcode.computer.core
  (:require [rip.deadcode.computer.component.primitive :refer [nand]]
            [rip.deadcode.computer.component.compiler :refer :all]
            [rip.deadcode.computer.component.computer :refer [computer]]
            [rip.deadcode.computer.component.hardware :refer [make-program-memory]]))

(defn -main []
  (computer
    (make-program-memory (compile (parse (lex "
      for (int i = 1; i <= 100; i++) {
        if (i % 15 == 0) {
          println(\"FizzBuzz\");
        } else if (i % 5 == 0) {
          println(\"Buzz\");
        } else if (i % 3 == 0) {
          println(\"Fizz\");
        } else {
          println(i);
        }
      }
    "))))))
