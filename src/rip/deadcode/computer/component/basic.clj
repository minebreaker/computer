(ns rip.deadcode.computer.component.basic
  (:require [rip.deadcode.computer.component.primitive :refer [nand]]))

(defn not [x]
  (nand x x))

(defn not16-16 [in]
  (let [[in0 in1 in2 in3 in4 in5 in6 in7 in8 in9 in10 in11 in12 in13 in14 in15] in]
    [(not in0) (not in1) (not in2) (not in3) (not in4) (not in5) (not in6) (not in7)
     (not in8) (not in9) (not in10) (not in11) (not in12) (not in13) (not in14) (not in15)]))

(defn and [x y]
  (not (nand x y)))

(defn and16-16 [x y]
  (let [[x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15] x
        [y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15] y]
    [(and x0 y0) (and x1 y1) (and x2 y2) (and x3 y3) (and x4 y4) (and x5 y5) (and x6 y6) (and x7 y7)
     (and x8 y8) (and x9 y9) (and x10 y10) (and x11 y11) (and x12 y12) (and x13 y13) (and x14 y14) (and x15 y15)]))

(defn or [x y]
  (nand (not x) (not y)))

(defn or16-16 [x y]
  (let [[x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15] x
        [y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15] y]
    [(or x0 y0) (or x1 y1) (or x2 y2) (or x3 y3) (or x4 y4) (or x5 y5) (or x6 y6) (or x7 y7)
     (or x8 y8) (or x9 y9) (or x10 y10) (or x11 y11) (or x12 y12) (or x13 y13) (or x14 y14) (or x15 y15)]))

(defn or16-1 [x]
  (let [[x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15] x]
    (or
      (or (or (or x0 x1) (or x2 x3)) (or (or x4 x5) (or x6 x7)))
      (or (or (or x8 x9) (or x10 x11)) (or (or x12 x13) (or x14 x15))))))

(defn xor [x y]
  (and (or x y) (nand x y)))

(defn xor16-16 [x y]
  (let [[x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15] x
        [y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15] y]
    [(xor x0 y0) (xor x1 y1) (xor x2 y2) (xor x3 y3) (xor x4 y4) (xor x5 y5) (xor x6 y6) (xor x7 y7)
     (xor x8 y8) (xor x9 y9) (xor x10 y10) (xor x11 y11) (xor x12 y12) (xor x13 y13) (xor x14 y14) (xor x15 y15)]))

(defn mux [x y select]
  (or
    (and x (not select))
    (and y select)))

(defn mux16-16 [x y select]
  (let [[x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15] x
        [y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15] y]
    [(mux x0 y0 select) (mux x1 y1 select) (mux x2 y2 select) (mux x3 y3 select)
     (mux x4 y4 select) (mux x5 y5 select) (mux x6 y6 select) (mux x7 y7 select)
     (mux x8 y8 select) (mux x9 y9 select) (mux x10 y10 select) (mux x11 y11 select)
     (mux x12 y12 select) (mux x13 y13 select) (mux x14 y14 select) (mux x15 y15 select)]))

(defn dmux [in select]
  [(and in (not select))
   (and in select)])
