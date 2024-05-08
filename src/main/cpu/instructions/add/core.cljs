(ns cpu.instructions.add.core
  (:require [cpu.instructions.utility :as ut]
            [cpu.registers.core :as reg]))


(def op +)

(defn adc [decoder-entry memory]
  (let [data decoder-entry
        operation (:op data)
        flags (:flags data)
        target-reg (:target data)
        source-reg (:source data)
        source-data (ut/get-source-data! source-reg memory)
        target-data (reg/get-reg target-reg)]
    (ut/calc-and-set-flags! flags op target-data source-data)
    (->> (op target-data source-data (reg/get-carry-bit)) ;note the carry bit
         (reg/set-reg! target-reg))))

(defn add [decoder-entry memory]
  (let [data decoder-entry
        operation (:op data)
        flags (:flags data)
        target-reg (:target data)
        source-reg (:source data)
        ; there's a big question of what happens with carry
        ; and half carry when adding 16 bit data
        ; when should overflow flag be set
        ; UPDATE: made some changes that might work. Needs testing
        source-data (ut/get-source-data! source-reg memory)
        target-data (reg/get-reg target-reg)]
    (ut/calc-and-set-flags! flags op target-data source-data)
    (->> (op target-data source-data)
         (reg/set-reg! target-reg))))