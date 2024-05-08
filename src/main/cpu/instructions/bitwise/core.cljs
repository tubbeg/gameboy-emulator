(ns cpu.instructions.bitwise.core
  (:require [cpu.instructions.utility :as ut]
            [cpu.registers.core :as reg]))



(defn and-instr [decoder-entry memory]
  (let [op bit-and
        data decoder-entry
        operation (:op data)
        flags (:flags data)
        target-reg (:target data)
        source-reg (:source data)
        source-data (ut/get-source-data! source-reg memory)
        target-data (reg/get-reg target-reg)]
    (ut/calc-and-set-flags! flags op target-data source-data)
    (->> (op target-data source-data)
         (reg/set-reg! target-reg))))

(defn xor-instr [decoder-entry memory]
  (let [op bit-xor
        data decoder-entry
        operation (:op data)
        flags (:flags data)
        target-reg (:target data)
        source-reg (:source data)
        source-data (ut/get-source-data! source-reg memory)
        target-data (reg/get-reg target-reg)]
    (ut/calc-and-set-flags! flags op target-data source-data)
    (->> (op target-data source-data)
         (reg/set-reg! target-reg))))

(defn or-instr [decoder-entry memory]
  (let [op bit-or
        data decoder-entry
        operation (:op data)
        flags (:flags data)
        target-reg (:target data)
        source-reg (:source data)
        source-data (ut/get-source-data! source-reg memory)
        target-data (reg/get-reg target-reg)]
    (ut/calc-and-set-flags! flags op target-data source-data)
    (->> (op target-data source-data)
         (reg/set-reg! target-reg))))