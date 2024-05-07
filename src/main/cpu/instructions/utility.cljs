(ns cpu.instructions.utility
  (:require [cpu.registers.core :as reg]))



(defn read-byte [memory index]
  (nth memory index)) ; nth needs a better name honestly

(defn read-next-byte [memory]
  (reg/inc-program-counter!) ;should this step be performed?
  ; if loading an immediate value? for instance: ADD A,3 
  ; Feels logical. Otherwise, the immediate value would be read.
  ; And the program might misinterpret it as an instruction.
  ; I could be wrong.
  (println "Incrementing pc")
  ; Maybe don't do this in utility file
  (read-byte memory (reg/get-reg :pc)))

(defn is-zero [num]
  (= num 0))
(defn is-not-zero [num]
  (not= num 0))

(defn determine-zero [num]
  (if (is-zero num)
    :set
    :clear))

(defn determine-carry [target value]
  (if (reg/addition-is-half-carry? target value)
    :set
    :clear))

(defn determine-half-carry [target value]
  (if (reg/addition-is-carry? target value)
    :set
    :clear))

(defn NotYetImplemented [data]
  (throw (new js/Error "Instruction Not Yet Implemented!")))
