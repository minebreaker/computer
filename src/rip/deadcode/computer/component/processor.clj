(ns rip.deadcode.computer.component.processor
  (:require [rip.deadcode.computer.component.primitive :refer :all]
            [rip.deadcode.computer.component.basic :refer :all]
            [rip.deadcode.computer.component.alu :refer :all]
            [rip.deadcode.computer.component.memory :refer :all]
            [rip.deadcode.computer.component.extensional :refer :all]
            [rip.deadcode.computer.component.debug :refer [decode]]))


(defn -cpu [a-register
            d-register
            pc
            instruction inM reset
            disable-loading]
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
        [_
         exit                                               ; Exit command
         e                                                  ; Bit shift right?
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

        [outM zr ng] (alu
                       d-current
                       (mux16 a-current inM a)              ; if comp-a == true inM else A-register
                       c1 c2 c3 c4 c5 c6
                       e)


        _ (a-register
            (mux16 [v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 false] outM instruction-type)
            (and
              (or (not instruction-type) d1)
              (not disable-loading)))
        _ (d-register outM (and d2 (not disable-loading)))

        pc-load (and3
                  instruction-type                          ; not an a-instruction
                  (or
                    (and3 j1 j2 j3)                         ; 1 1 1; just jump
                    (or3
                      (and j1 ng)                           ; j1 is true and output < 0
                      (and j2 zr)                           ; j2 is true and output is 0
                      (and j3 (not (or ng zr)))             ; j3 is true and output > 0
                      ))
                  (not disable-loading))                    ; not disabled
        pc-inc (not (or pc-load disable-loading))           ; neither load nor disabled
        pc-out (pc a-current pc-inc pc-load reset)
        ]

    ;(println (str "Load disabled " disable-loading))
    ;(println (decode instruction))
    ;(println (str "I " instruction))
    ;(println (str "A " a-current " " (ba2i a-current)))
    ;(println (str "D " d-current " " (ba2i d-current)))
    ;(println (str "O " outM " " (ba2i outM)))
    ;(println (str "ng " ng))
    ;(println (str "zr " zr))
    ;(println (str "PC INC " pc-inc))
    ;(println (str "PC LOAD " pc-load))
    ;(println (str "PC " pc-out " " (ba2i pc-out)))
    ;(println (str "L " pc-load))
    ;(println)

    [outM d3 a-current pc-out]
    ))

(defn make-cpu []
  (let [
        a-register (make-register16)
        d-register (make-register16)
        pc (make-pc)
        cpu (fn [instruction inM reset disable-loading]
              (-cpu a-register d-register pc instruction inM reset disable-loading))
        ]
    (fn
      ([instruction inM reset] (cpu instruction inM reset false))
      ([instruction inM reset disable-loading] (cpu instruction inM reset disable-loading)))
    ))
