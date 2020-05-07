(ns rip.deadcode.computer.core
  (:require [rip.deadcode.computer.component.primitive :refer [nand]]))

(defn -main []
  (println (nand false false)))
