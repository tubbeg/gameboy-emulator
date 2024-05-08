(ns cpu.instructions.incdec.core
  (:require [cpu.instructions.utility :as ut]
            [cpu.registers.core :as reg]))


(defn parity-error []
  (throw (new js/Error (str "ERROR! Tried to inc/dec with 2 arguments!"))))


(defn inc-instr [decoder-entry memory]
  (let [op inc
        data decoder-entry
        operation (:op data)
        flags (:flags data)
        target-reg (:target data)
        target-data (reg/get-reg target-reg)]
    (ut/calc-and-set-flags! flags op target-data)
    (->> (op target-data)
         (reg/set-reg! target-reg))))

(defn dec-instr [decoder-entry memory]
  (let [op dec
        data decoder-entry
        operation (:op data)
        flags (:flags data)
        target-reg (:target data)
        target-data (reg/get-reg target-reg)]
    (ut/calc-and-set-flags! flags op target-data)
    (->> (op target-data)
         (reg/set-reg! target-reg))))
