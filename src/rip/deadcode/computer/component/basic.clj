(ns rip.deadcode.computer.component.basic
  (:require [rip.deadcode.computer.component.primitive :refer [nand]]))

(defn not [x]
  (nand x x))

; TODO: strict naming convention between "bit" and "way"

(defn not16-16 [in]
  (let [[in0 in1 in2 in3 in4 in5 in6 in7 in8 in9 in10 in11 in12 in13 in14 in15] in]
    [(not in0) (not in1) (not in2) (not in3) (not in4) (not in5) (not in6) (not in7)
     (not in8) (not in9) (not in10) (not in11) (not in12) (not in13) (not in14) (not in15)]))

(defn and [x y]
  (not (nand x y)))

(defn and3 [x y z] (and x (and y z)))

(defn and4 [a b c d] (and (and a b) (and c d)))

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

(defn or4-1 [x]
  (let [[x0 x1 x2 x3] x]
    (or (or x0 x1) (or x2 x3))))

(defn or8-1 [x]
  (let [[x0 x1 x2 x3 x4 x5 x6 x7] x]
    (or
      (or (or x0 x1) (or x2 x3))
      (or (or x4 x5) (or x6 x7)))))

(defn or16-1 [x]
  (let [[x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15] x]
    (or
      (or8-1 [x0 x1 x2 x3 x4 x5 x6 x7])
      (or8-1 [x8 x9 x10 x11 x12 x13 x14 x15]))))

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

(defn mux16 [x y select]
  (let [[x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15] x
        [y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15] y]
    [(mux x0 y0 select) (mux x1 y1 select) (mux x2 y2 select) (mux x3 y3 select)
     (mux x4 y4 select) (mux x5 y5 select) (mux x6 y6 select) (mux x7 y7 select)
     (mux x8 y8 select) (mux x9 y9 select) (mux x10 y10 select) (mux x11 y11 select)
     (mux x12 y12 select) (mux x13 y13 select) (mux x14 y14 select) (mux x15 y15 select)]))

(defn mux4 [a b c d select]
  (let [[s0 s1] select]
    (or4-1
      [(and3 a (not s0) (not s1))
       (and3 b s0 (not s1))
       (and3 c (not s0) s1)
       (and3 d s0 s1)])))

(defn mux8 [a b c d e f g h select]
  (let [[s0 s1 s2] select]
    (or8-1
      [(and4 a (not s0) (not s1) (not s2))
       (and4 b s0 (not s1) (not s2))
       (and4 c (not s0) s1 (not s2))
       (and4 d s0 s1 (not s2))
       (and4 e (not s0) (not s1) s2)
       (and4 f s0 (not s1) s2)
       (and4 g (not s0) s1 s2)
       (and4 h s0 s1 s2)])))

(defn mux16-4 [a b c d select]
  (let [[a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15] a
        [b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15] b
        [c0 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15] c
        [d0 d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 d11 d12 d13 d14 d15] d]
    [(mux4 a0 b0 c0 d0 select)
     (mux4 a1 b1 c1 d1 select)
     (mux4 a2 b2 c2 d2 select)
     (mux4 a3 b3 c3 d3 select)
     (mux4 a4 b4 c4 d4 select)
     (mux4 a5 b5 c5 d5 select)
     (mux4 a6 b6 c6 d6 select)
     (mux4 a7 b7 c7 d7 select)
     (mux4 a8 b8 c8 d8 select)
     (mux4 a9 b9 c9 d9 select)
     (mux4 a10 b10 c10 d10 select)
     (mux4 a11 b11 c11 d11 select)
     (mux4 a12 b12 c12 d12 select)
     (mux4 a13 b13 c13 d13 select)
     (mux4 a14 b14 c14 d14 select)
     (mux4 a15 b15 c15 d15 select)]))

(defn mux16-8 [a b c d e f g h select]
  (let [[a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15] a
        [b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15] b
        [c0 c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15] c
        [d0 d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 d11 d12 d13 d14 d15] d
        [e0 e1 e2 e3 e4 e5 e6 e7 e8 e9 e10 e11 e12 e13 e14 e15] e
        [f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15] f
        [g0 g1 g2 g3 g4 g5 g6 g7 g8 g9 g10 g11 g12 g13 g14 g15] g
        [h0 h1 h2 h3 h4 h5 h6 h7 h8 h9 h10 h11 h12 h13 h14 h15] h]
    [(mux8 a0 b0 c0 d0 e0 f0 g0 h0 select)
     (mux8 a1 b1 c1 d1 e1 f1 g1 h1 select)
     (mux8 a2 b2 c2 d2 e2 f2 g2 h2 select)
     (mux8 a3 b3 c3 d3 e3 f3 g3 h3 select)
     (mux8 a4 b4 c4 d4 e4 f4 g4 h4 select)
     (mux8 a5 b5 c5 d5 e5 f5 g5 h5 select)
     (mux8 a6 b6 c6 d6 e6 f6 g6 h6 select)
     (mux8 a7 b7 c7 d7 e7 f7 g7 h7 select)
     (mux8 a8 b8 c8 d8 e8 f8 g8 h8 select)
     (mux8 a9 b9 c9 d9 e9 f9 g9 h9 select)
     (mux8 a10 b10 c10 d10 e10 f10 g10 h10 select)
     (mux8 a11 b11 c11 d11 e11 f11 g11 h11 select)
     (mux8 a12 b12 c12 d12 e12 f12 g12 h12 select)
     (mux8 a13 b13 c13 d13 e13 f13 g13 h13 select)
     (mux8 a14 b14 c14 d14 e14 f14 g14 h14 select)
     (mux8 a15 b15 c15 d15 e15 f15 g15 h15 select)]))

(defn dmux [in select]
  [(and in (not select))
   (and in select)])

(defn dmux4 [in select]
  (let [[s0 s1] select]
    [(and3 in (not s0) (not s1))
     (and3 in s0 (not s1))
     (and3 in (not s0) s1)
     (and3 in s0 s1)]))

(defn dmux8 [in select]
  (let [[s0 s1 s2] select
        and4 (fn [i0 i1 i2 i3] (and (and i0 i1) (and i2 i3)))]
    [(and4 in (not s0) (not s1) (not s2))
     (and4 in s0 (not s1) (not s2))
     (and4 in (not s0) s1 (not s2))
     (and4 in s0 s1 (not s2))
     (and4 in (not s0) (not s1) s2)
     (and4 in s0 (not s1) s2)
     (and4 in (not s0) s1 s2)
     (and4 in s0 s1 s2)]))
