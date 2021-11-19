(ns rip.deadcode.computer.component.extensional
  (:require [rip.deadcode.computer.component.primitive :refer [nand]]))

; Functions to cooperate with Hardware-world and Clojure world
; Also contains some utility functions for readability

(defn i2ba [i]
  (reduce
    #(assoc %1 %2 (not=
                    (bit-and i (bit-shift-left 1 %2))
                    0))
    []
    (range 0 16)))

(def false16 (vec (repeat 16 false)))
(def zero16 false16)
(def true16 (vec (repeat 16 true)))
(def m-one16 true16)
(def one16 (assoc zero16 0 true))
(def two16 (assoc zero16 1 true))
(def three16 (assoc two16 0 true))
(def one16 (assoc zero16 0 true))
(def max16 (assoc true16 15 false))
(def min16 (assoc false16 15 true))

(defn expand16 [in]
  "Expands 1bit input to 16bit"
  [in in in in in in in in in in in in in in in in])
