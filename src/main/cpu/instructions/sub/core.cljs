(ns cpu.instructions.sub.core
  (:require [cpu.instructions.utility :as ut]
            [cpu.registers.core :as reg]))


(def op -)

(defn sbc [decoder-entry memory]
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

(defn sub [decoder-entry memory]
  (let [data decoder-entry
        operation (:op data)
        flags (:flags data)
        target-reg (:target data)
        source-reg (:source data)
        source-data (ut/get-source-data! source-reg memory)
        target-data (reg/get-reg target-reg)]
    (ut/calc-and-set-flags! flags op target-data source-data)
    (->> (op target-data source-data)
         (reg/set-reg! target-reg))))