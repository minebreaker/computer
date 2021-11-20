(ns rip.deadcode.computer.component.compiler
  (:require [rip.deadcode.computer.component.opcode :refer :all]
            [clojure.string :refer [blank? split]]
            [clojure.core.match :refer [match]]))

; #include <stdio.h>;
; int main() {
;   for (i = 0; i <= 100; i++) {
;     if (i % 15 == 0) {
;       println("FizzBuzz");
;     } else if (i % 5 == 0) {
;       println("Buzz");
;     } else if (i % 3 == 0) {
;       println("Fizz");
;     } else {
;       println(i);
;     }
;   }
; }

(defn lex [^String code]
  (filter #(not (blank? %))
    (re-seq #"[\w\"]+|[\s\(\)\{\}\;]|\+\+|<=|==|=|%" code))
  )

(declare parse)

(defn parse-for [code]
  (let [
        res-init (match [code]
                   [(["for" "(" & res] :seq)] (parse res))
        res-cond (match [(:rest res-init)]
                   [([";" & res] :seq)] (parse res))
        res-next (parse (match [(:rest res-cond)]
                          [([";" & res] :seq)] res))
        res-stmt (match [(:rest res-next)]
                   [([")" "{" & res] :seq)] (parse res))
        res (match [(:rest res-stmt)]
              [(["}" & res] :seq)] res)
        ]
    {:type      "for"
     :init      res-init
     :cond-expr res-cond
     :res-next  res-next
     :rest      res}))

(defn parse-if [code]
  (let [
        res-cond (match [code]
                   [(["if" "(" & res] :seq)] (parse res))
        res-then (match [(:rest res-cond)]
                   [([")" "{" & res] :seq)] (parse res))
        res-then' (:rest res-then)
        [res-else res] (match [res-then']
                         [(["}" "else" "{" & res] :seq)] (let [res' (parse res)]
                                                           (match [(:rest res')]
                                                             [(["}" & res''] :seq)] [res' res'']))
                         [(["}" "else" "if" & _] :seq)] [(parse (drop 2 res-then')) nil])
        ]
    {:type "if"
     :cond res-cond
     :then res-then
     :else res-else
     :rest res}))

(defn parse-value [v]
  (cond
    (re-matches #"\d+" v) {:type "int" :value (Integer/parseInt v)}
    (and (.startsWith v "\"") (.endsWith v "\"")) {:type "str" :value (.substring v 1 (- (.length v) 1))}
    :else {:type "ref" :name v}))

(defn drop-semi [code]
  (if (= (first code) ";") (rest code) code))

(defn parse
  [code]
  (let [[t0 t1 t2 t3 t4] code]
    (cond
      (= t1 "=") {:type "assign" :name t0 :value (parse-value t2) :rest (drop-semi (drop 3 code))}
      (and (= t1 "%") (= t3 "==")) {:type "rem" :x (parse-value t0) :y (parse-value t2) :z (parse-value t4) :rest (drop-semi (drop 5 code))}
      (= t0 "for") (parse-for code)
      (= t0 "if") (parse-if code)
      (and (re-matches #"[\w]+" t0) (= t1 "(") (= t3 ")")) {:type "call" :name t0 :arg (parse-value t2) :rest (drop-semi (drop 4 code))}
      :else (parse (rest code)))))

(defn compile [^String code]
  )
