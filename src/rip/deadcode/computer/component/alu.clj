(ns rip.deadcode.computer.component.alu
  (:require [rip.deadcode.computer.component.primitive :refer :all]
            [rip.deadcode.computer.component.basic :refer :all]
            [rip.deadcode.computer.component.extensional :refer :all]))

(defn half-adder [x y]
  "@returns [sum carry]"
  [(or
     (and x (not y))
     (and (not x) y))
   (and x y)])

(defn full-adder [x y cin]
  "@returns [sum carry]"
  (let [[s0 c0] (half-adder x y)
        [s1 c1] (half-adder s0 cin)]
    [s1 (or c0 c1)]))

(defn adder16 [x y cin]
  (let [[x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15] x
        [y0 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15] y
        [s0 c0] (full-adder x0 y0 cin)
        [s1 c1] (full-adder x1 y1 c0)
        [s2 c2] (full-adder x2 y2 c1)
        [s3 c3] (full-adder x3 y3 c2)
        [s4 c4] (full-adder x4 y4 c3)
        [s5 c5] (full-adder x5 y5 c4)
        [s6 c6] (full-adder x6 y6 c5)
        [s7 c7] (full-adder x7 y7 c6)
        [s8 c8] (full-adder x8 y8 c7)
        [s9 c9] (full-adder x9 y9 c8)
        [s10 c10] (full-adder x10 y10 c9)
        [s11 c11] (full-adder x11 y11 c10)
        [s12 c12] (full-adder x12 y12 c11)
        [s13 c13] (full-adder x13 y13 c12)
        [s14 c14] (full-adder x14 y14 c13)
        [s15 c15] (full-adder x15 y15 c14)
        ]
    [[s0 s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12 s13 s14 s15] c15]))

(defn inc16 [in]
  (let [[result] (adder16 in [true false false false false false false false false false false false false false false false] false)]
    result))

(defn alu [x y zx nx zy ny f no rs]
  "
  ALU taken from *The Elements of Computing Systems*
  x 16bit input 1
  y 16bit input 2
  zx force x to be 0
  nx negate x
  zy force y to be 0
  ny negate y
  f false: add; true: and
  no negate output
  rs right-shift output
  @returns [out zr ng] where
    out 16bit output
    zr true when out = 0
    ng true when out < 0
  "
  (let [
        x-zero (and16bit x (expand16 (not zx)))
        y-zero (and16bit y (expand16 (not zy)))
        x-negate (xor16bit x-zero (expand16 nx))
        y-negate (xor16bit y-zero (expand16 ny))

        [radd] (adder16 x-negate y-negate false)
        rand (and16bit x-negate y-negate)
        out-added (mux16bit rand radd f)
        out-negated (xor16bit out-added (expand16 no))
        out (let [[_ v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15] out-negated
                  shifted [v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 false]]
              (mux16bit out-negated shifted rs))
        zr (not (or16 out))
        ng (nth out 15)]                                    ; it's ok to use nth as long as the index is a constant

    ;(println "========")
    ;(println x-negate)
    ;(println y-negate)
    ;(println out-temp)
    ;(println out)

    [out zr ng]))
