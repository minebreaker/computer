(ns rip.deadcode.computer.component.opcode
  (:require [rip.deadcode.computer.component.primitive :refer :all]
            [rip.deadcode.computer.component.basic :refer :all]
            [rip.deadcode.computer.component.extensional :refer :all]))

(def dont-care false16)                                     ; just a convention

; comp mnemonics
(def op-0 [true false true false true false])
(def op-1 [true true true true true true])
(def op--1 [true true true false true false])
(def op-d [false false true true false false])
(def op-a [true true false false false false])
(def op-!d [false false true true false true])
(def op-!a [true true false false false true])
(def op--d [false false true true true true])
(def op--a [true true false false true true])
(def op-d+1 [false true true true true true])
(def op-a+1 [true true false true true true])
(def op-d-1 [false false true true true false])
(def op-a-1 [true true false false true false])
(def op-d+a [false false false false true false])
(def op-d-a [false true false false true true])
(def op-a-d [false false false true true true])
(def op-a&d [false false false false false false])
(def op-d|a [false true false true false true])

; dest mnemonics
(def op-dest-null [false false false])
(def op-dest-m [false false true])
(def op-dest-d [false true false])
(def op-dest-md [false true true])
(def op-dest-a [true false false])
(def op-dest-am [true false true])
(def op-dest-ad [true true false])
(def op-dest-amd [true true true])

; jump mnemonics
(def op-no-jump [false false false])
(def op-jgt [false false true])
(def op-jeq [false true false])
(def op-jge [false true true])
(def op-jlt [true false false])
(def op-jne [true false true])
(def op-jle [true true false])
(def op-jmp [true true true])
