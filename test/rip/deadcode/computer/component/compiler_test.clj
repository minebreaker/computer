(ns rip.deadcode.computer.component.compiler-test
  (:require [clojure.test :refer :all]
            [rip.deadcode.computer.component.compiler :refer :all]
            [rip.deadcode.computer.component.debug :refer :all]
            [rip.deadcode.computer.component.computer :refer [computer]]
            [rip.deadcode.computer.component.hardware :refer [make-program-memory]]))


(deftest lex-test
  (testing "lex"
    (is (=
          '("if" "(" "x" "==" "2" ")" "{" "println" "(" "\"OK\"" ")" ";" "}" "else" "{" "println" "(" "\"NG\"" ")" ";" "}")
          (lex "if (x == 2) { println(\"OK\"); } else { println(\"NG\"); }")))))

(defn cleanup [map]
  (if (not (map? map))
    map
    (into {} (for [[k v] map] (if (not= k :rest) [k (cleanup v)])))))

(deftest parse-test
  (testing "parse"
    (is (=
          {:type "assign" :name "x" :value {:type "int" :value 2}}
          (cleanup (parse (lex "int x = 2;")))))
    (is (=
          {:type "if"
           :cond {:type "rem" :x {:type "ref" :name "x"} :y {:type "int" :value 2} :z {:type "int" :value 0}}
           :then {:type "call" :name "f1" :arg {:type "str" :value "A"}}
           :else {:type "if"
                  :cond {:type "rem" :x {:type "ref" :name "y"} :y {:type "int" :value 3} :z {:type "int" :value 0}}
                  :then {:type "call" :name "f2" :arg {:type "str" :value "B"}}
                  :else {:type "call" :name "f3" :arg {:type "int" :value 4}}}}
          (cleanup (parse (lex "if (x % 2 == 0) {
                                  f1(\"A\");
                                } else if (y % 3 == 0) {
                                  f2(\"B\");
                                } else {
                                  f3(4);
                                }")))))
    (is (=
          {:type "for"
           :init {:type "assign" :name "i" :value {:type "int" :value 0}}
           :cond {:type "le" :x {:type "ref" :name "i"} :y {:type "int" :value 10}}
           :loop {:type "inc" :name "i"}
           :body {:type "if"
                  :cond {:type "rem" :x {:type "ref" :name "i"} :y {:type "int" :value 2} :z {:type "int" :value 0}}
                  :then {:type "call" :name "println" :arg {:type "str" :value "even"}}
                  :else {:type "call" :name "println" :arg {:type "str" :value "odd"}}}}
          (cleanup (parse (lex "for (int i = 0; i <= 10; i++) {
                                  if (i % 2 == 0) {
                                    println(\"even\");
                                  } else {
                                    println(\"odd\");
                                  }
                                }
                               ")))))))

; Not a test
(deftest decoder
  (testing "decode"
    (let [op (compile (parse (lex "if (5 % 3 == 0) {\n                                                           println(\"NG\");\n                                                         } else {\n                                                           println(\"OK\");\n                                                         }")))]
      (println op)
      (decode-ops op))))

(deftest compile-test
  (testing "compile"
    (is (=
          "helloworld\n"
          (with-out-str
            (computer
              (make-program-memory (compile (parse (lex "println(\"helloworld\");"))))))))
    )
  (testing "if"
    (is (=
          "OK\n"
          (with-out-str
            (computer
              (make-program-memory (compile (parse (lex "if (1) {
                                                           println(\"OK\");
                                                         } else {
                                                           println(\"NG\");
                                                         }"))))))))
    (is (=
          "OK\n"
          (with-out-str
            (computer
              (make-program-memory (compile (parse (lex "if (0) {
                                                           println(\"NG\");
                                                         } else {
                                                           println(\"OK\");
                                                         }"))))))))
    (is (=
          "OK\n"
          (with-out-str
            (computer
              (make-program-memory (compile (parse (lex "if (4 % 2 == 0) {
                                                           println(\"OK\");
                                                         } else {
                                                           println(\"NG\");
                                                         }"))))))))
    (is (=
          "OK\n"
          (with-out-str
            (computer
              (make-program-memory (compile (parse (lex "if (5 % 3 == 0) {
                                                           println(\"NG\");
                                                         } else {
                                                           println(\"OK\");
                                                         }"))))))))
    (is (=
          "OK\n"
          (with-out-str
            (computer
              (make-program-memory (compile (parse (lex "if (0) {
                                                             println(\"NG\");
                                                           } else if (1) {
                                                             println(\"OK\");
                                                           } else {
                                                             println(\"NG\");
                                                           }"))))))))
    (is (=
          "OK\n"
          (with-out-str
            (computer
              (make-program-memory (compile (parse (lex "if (0) {
                                                           println(\"NG\");
                                                         } else if (0) {
                                                           println(\"NG\");
                                                         } else {
                                                           println(\"OK\");
                                                         }"))))))))
    (is (=
          "OK\n"
          (with-out-str
            (computer
              (make-program-memory (compile (parse (lex "if (0 <= 1) { println(\"OK\"); } else { println(\"NG\"); }"))))))))
    (is (=
          "OK\nOK\nOK\nOK\n"
          (with-out-str
            (computer
              (make-program-memory (compile (parse (lex "for (int i = 0; i <= 3; i++) {  println(\"OK\");  }"))))))))
    (is (=
          "9\n10\n11\n"
          (with-out-str
            (computer
              (make-program-memory (compile (parse (lex "for (int i = 9; i <= 11; i++) {  println(i);  }"))))))))
    )
  )
