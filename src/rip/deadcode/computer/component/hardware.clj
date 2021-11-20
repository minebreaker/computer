(ns rip.deadcode.computer.component.hardware
  (:require [rip.deadcode.computer.component.extensional :refer :all]))

(defn i2s [i] "Converts given int code point to String" (new String (Character/toChars i)))

(defn console [in load]
  (if load (print (i2s (ba2i in)))))

(defn s2ba [s] "Converts a first letter of given string to bytes" (i2ba (int (nth s 0))))
(def console-idx (bit15 (i2ba 16384)))

(defn make-program-memory [opcodes]
  (fn [addr] (nth opcodes (ba2i addr))))
