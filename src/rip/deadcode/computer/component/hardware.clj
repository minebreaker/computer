(ns rip.deadcode.computer.component.hardware
  (:require [rip.deadcode.computer.component.extensional :refer :all]))

(defn console [in load]
  (if load (print (i2s (ba2i in)))))

(defn make-program-memory [opcodes]
  (fn [addr] (nth opcodes (ba2i addr))))
