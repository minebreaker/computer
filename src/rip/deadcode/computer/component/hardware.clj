(ns rip.deadcode.computer.component.hardware
  (:require [rip.deadcode.computer.component.extensional :refer :all]))

(defn console [in]
  (print (new String (Character/toChars (ba2i in)))))
