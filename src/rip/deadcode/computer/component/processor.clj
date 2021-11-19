(ns rip.deadcode.computer.component.processor
  (:require [rip.deadcode.computer.component.primitive :refer :all]
            [rip.deadcode.computer.component.basic :refer :all]
            [rip.deadcode.computer.component.alu :refer :all]
            [rip.deadcode.computer.component.memory :refer :all]
            [rip.deadcode.computer.component.extensional :refer :all]))


(def dont-care false16)                                     ; just a convention

(defn -cpu [a-register
            d-register
            pc
            instruction inM reset]
  "
  CPU
  @returns [outM writeM addressM pc] where
    outM     16-bit output
    writeM   write outM to the memory?
    addressM target address
    pc       next program counter
  "
  (let [
        [instruction-type v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15] instruction ; A-instruction
        [_ _ _
         a c1 c2 c3 c4 c5 c6
         d1                                                 ; Save to A?
         d2                                                 ; Save to D?
         d3                                                 ; Save to M?
         j1
         j2
         j3
         ] instruction                                      ; C-instruction

        a-current (a-register dont-care false)
        d-current (d-register dont-care false)

        _ (println (str "I " instruction))
        _ (println (str "A " a-current))
        _ (println (str "D " d-current))

        [outM zr ng] (alu
                       d-current
                       (mux16 a-current inM a)              ; if comp-a == true inM else A-register
                       c1 c2 c3 c4 c5 c6)

        _ (println (str "O " outM))

        _ (a-register (mux16 [v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 false] outM instruction-type) (or (not instruction-type) d1))
        _ (d-register outM d2)

        load (and
               instruction-type                             ; not an a-instruction
               (or
                 (and3 j1 j2 j3)                            ; 1 1 1; just jump
                 (or3
                   (and j1 ng)                              ; j1 is true and output < 0
                   (and j2 zr)                              ; j2 is true and output is 0
                   (and j3 (not (or ng zr)))                ; j3 is true and output > 0
                   )))
        pc-out (pc a-current (not load) load reset)

        _ (println (str "ng " ng))
        _ (println (str "zr " zr))
        _ (println (str "L " load))
        _ (println (str "PC " pc-out))
        _ (println)
        ]
    [outM d3 a-current pc-out]
    ))

(defn make-cpu []
  (let [
        a-register (make-register16)
        d-register (make-register16)
        pc (make-pc)]
    (fn [instruction inM reset]
      (-cpu a-register d-register pc instruction inM reset))))
