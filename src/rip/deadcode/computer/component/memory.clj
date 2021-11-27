(ns rip.deadcode.computer.component.memory
  (:require [rip.deadcode.computer.component.primitive :refer :all]
            [rip.deadcode.computer.component.basic :refer :all]
            [rip.deadcode.computer.component.extensional :refer :all]
            [rip.deadcode.computer.component.alu :refer :all]))

(defn make-register16 []
  (let [r0 (make-register) r1 (make-register) r2 (make-register) r3 (make-register)
        r4 (make-register) r5 (make-register) r6 (make-register) r7 (make-register)
        r8 (make-register) r9 (make-register) r10 (make-register) r11 (make-register)
        r12 (make-register) r13 (make-register) r14 (make-register) r15 (make-register)]
    (fn [in load]
      (let [[in0 in1 in2 in3 in4 in5 in6 in7 in8 in9 in10 in11 in12 in13 in14 in15] in]
        [(r0 in0 load) (r1 in1 load) (r2 in2 load) (r3 in3 load) (r4 in4 load) (r5 in5 load) (r6 in6 load) (r7 in7 load)
         (r8 in8 load) (r9 in9 load) (r10 in10 load) (r11 in11 load) (r12 in12 load) (r13 in13 load) (r14 in14 load) (r15 in15 load)]))))

(defn make-ram8 []
  (let [r0 (make-register16) r1 (make-register16) r2 (make-register16) r3 (make-register16)
        r4 (make-register16) r5 (make-register16) r6 (make-register16) r7 (make-register16)]
    (fn [in addr load]
      (let [[l0 l1 l2 l3 l4 l5 l6 l7] (dmux8 load addr)
            ro0 (r0 in l0)
            ro1 (r1 in l1)
            ro2 (r2 in l2)
            ro3 (r3 in l3)
            ro4 (r4 in l4)
            ro5 (r5 in l5)
            ro6 (r6 in l6)
            ro7 (r7 in l7)]
        (mux16bit-8 ro0 ro1 ro2 ro3 ro4 ro5 ro6 ro7 addr)))))

(defn make-ram64 []
  (let [r0 (make-ram8) r1 (make-ram8) r2 (make-ram8) r3 (make-ram8)
        r4 (make-ram8) r5 (make-ram8) r6 (make-ram8) r7 (make-ram8)]
    (fn [in addr load]
      (let [[a0 a1 a2 a3 a4 a5] addr
            addr-this [a0 a1 a2]
            addr-8 [a3 a4 a5]
            [l0 l1 l2 l3 l4 l5 l6 l7] (dmux8 load addr-this)
            ro0 (r0 in addr-8 l0)
            ro1 (r1 in addr-8 l1)
            ro2 (r2 in addr-8 l2)
            ro3 (r3 in addr-8 l3)
            ro4 (r4 in addr-8 l4)
            ro5 (r5 in addr-8 l5)
            ro6 (r6 in addr-8 l6)
            ro7 (r7 in addr-8 l7)]
        (mux16bit-8 ro0 ro1 ro2 ro3 ro4 ro5 ro6 ro7 addr)))))

(defn make-ram512 []
  (let [r0 (make-ram64) r1 (make-ram64) r2 (make-ram64) r3 (make-ram64)
        r4 (make-ram64) r5 (make-ram64) r6 (make-ram64) r7 (make-ram64)]
    (fn [in addr load]
      (let [[a0 a1 a2 a3 a4 a5 a6 a7 a8] addr
            addr-this [a0 a1 a2]
            addr-64 [a3 a4 a5 a6 a7 a8]
            [l0 l1 l2 l3 l4 l5 l6 l7] (dmux8 load addr-this)
            ro0 (r0 in addr-64 l0)
            ro1 (r1 in addr-64 l1)
            ro2 (r2 in addr-64 l2)
            ro3 (r3 in addr-64 l3)
            ro4 (r4 in addr-64 l4)
            ro5 (r5 in addr-64 l5)
            ro6 (r6 in addr-64 l6)
            ro7 (r7 in addr-64 l7)]
        (mux16bit-8 ro0 ro1 ro2 ro3 ro4 ro5 ro6 ro7 addr)))))

(defn make-ram4k []
  (let [r0 (make-ram512) r1 (make-ram512) r2 (make-ram512) r3 (make-ram512)
        r4 (make-ram512) r5 (make-ram512) r6 (make-ram512) r7 (make-ram512)]
    (fn [in addr load]
      (let [[a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11] addr
            addr-this [a0 a1 a2]
            addr-512 [a3 a4 a5 a6 a7 a8 a9 a10 a11]
            [l0 l1 l2 l3 l4 l5 l6 l7] (dmux8 load addr-this)
            ro0 (r0 in addr-512 l0)
            ro1 (r1 in addr-512 l1)
            ro2 (r2 in addr-512 l2)
            ro3 (r3 in addr-512 l3)
            ro4 (r4 in addr-512 l4)
            ro5 (r5 in addr-512 l5)
            ro6 (r6 in addr-512 l6)
            ro7 (r7 in addr-512 l7)]
        (mux16bit-8 ro0 ro1 ro2 ro3 ro4 ro5 ro6 ro7 addr)))))

(defn make-ram16k []
  (let [r0 (make-ram4k) r1 (make-ram4k) r2 (make-ram4k) r3 (make-ram4k)]
    (fn [in addr load]
      (let [[a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13] addr
            addr-this [a0 a1]
            addr-16k [a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13]
            [l0 l1 l2 l3] (dmux4 load addr-this)
            ro0 (r0 in addr-16k l0)
            ro1 (r1 in addr-16k l1)
            ro2 (r2 in addr-16k l2)
            ro3 (r3 in addr-16k l3)]
        (mux16bit-4 ro0 ro1 ro2 ro3 addr)))))

(defn make-pc []
  "program counter"
  (let [r (make-register16)]
    (fn [in inc load reset]
      (r
        (mux16bit-4
          zero16
          zero16
          in
          (inc16bit (r false16 false))
          [(or inc (not load)) (not reset)])
        (or (or load inc) reset)))))
