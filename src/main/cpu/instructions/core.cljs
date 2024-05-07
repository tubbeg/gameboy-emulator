(ns cpu.instructions.core
  (:require [cpu.registers.core :as reg]))




(defn NotYetImplemented []
  (throw (new js/Error "Instruction Not Yet Implemented!")))


(defn add-from-source-reg
  "Flags are Z 0 H C"
  [source-reg])


(defn add [data]
  (let [op (:op data)
        flags (:flags data)
        target (:target data)
        source (:source data)])
  (case target-reg
    :a )
  (NotYetImplemented))


(defn nop []
  (println "Add delay here for cycles? Synchronization?")
  (println "Something to think about"))

(def instructions
  {:add add
   :nop nop
   :ldi NotYetImplemented
   :default (fn [] :ErrorNotFound!)})