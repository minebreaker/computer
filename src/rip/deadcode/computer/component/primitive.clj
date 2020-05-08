(ns rip.deadcode.computer.component.primitive)

; Contains primitive two components: nand & flipflop

(defn nand [x y]
  (not (and x y)))

;(defn make-flipflop []
;  (let [r (ref false)]
;    (fn [in]
;      (let [current @r]
;        (dosync
;          (ref-set r in)
;          current)))))

(defn make-register []
  (let [r (ref false)]
    (fn [in load]
      (let [current @r]
        (if load
          (dosync
            (ref-set r in)
            current)
          current)))))
