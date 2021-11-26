(ns rip.deadcode.computer.component.computer
  (:require [rip.deadcode.computer.component.primitive :refer :all]
            [rip.deadcode.computer.component.basic :refer :all]
            [rip.deadcode.computer.component.memory :refer :all]
            [rip.deadcode.computer.component.processor :refer :all]
            [rip.deadcode.computer.component.hardware :refer :all]
            [rip.deadcode.computer.component.extensional :refer :all]))


(defn computer [program-memory]
  (let [
        cpu (make-cpu)
        ram (make-ram16k)
        memory (fn [in addr load]
                 (let [[a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14] addr
                       addr-memory [a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13]
                       [l0 l1] (dmux load a14)
                       ro (ram in addr-memory l0)
                       _ (console in l1)
                       ]
                   (mux16-4 ro false16 false16 false16 [a13 false])
                   )
                 )
        ]
    (loop []
      (let [
            [_ _ current-address pc] (cpu false16 false16 false true)
            instruction (program-memory pc)
            inM (memory false16 current-address false)
            [outM writeM addressM _ exit] (cpu instruction inM false)
            ]
        (memory outM addressM writeM)

        ;(println (str "Mem "
        ;           (ba2i (memory false16 zero16 false)) " "
        ;           (ba2i (memory false16 one16 false)) " "
        ;           (ba2i (memory false16 (i2ba 2) false)) " "
        ;           (ba2i (memory false16 (i2ba 3) false)) " "
        ;           (ba2i (memory false16 (i2ba 4) false))))

        (if (false? exit) (recur))
        )
      )
    )
  )
