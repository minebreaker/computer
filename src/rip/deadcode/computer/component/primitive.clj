(ns rip.deadcode.computer.component.primitive)

; Contains primitive two components: nand & flipflop

(defn nand [x y]
  (not (and x y)))

(defn make-flipflop []
  (let [r (ref false)]
    (fn [in]
      (let [current @r]
        (dosync
          (ref-set r in)
          current)))))
