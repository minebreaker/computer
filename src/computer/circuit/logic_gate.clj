(ns computer.circuit.logic_gate)

(defn nand [x y]
  (not (and x y)))

(defn not [x]
  (nand x x))

(defn and [x y]
  (not (nand x y)))

(defn or [x y]
  (nand (nand x x) (nand y y)))

(defn xor [x y]
  (let [t (nand x y)]
    (nand (nand x t) (nand y t))))
