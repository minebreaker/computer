(ns rip.deadcode.computer.component.compiler-test
  (:require [clojure.test :refer :all]
            [rip.deadcode.computer.component.compiler :refer :all]))


(deftest lext-test
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
          (cleanup (parse (lex "x = 2;")))))
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
                                }")))))))
