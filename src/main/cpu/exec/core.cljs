(ns cpu.exec.core
  (:require [cpu.registers.core :as reg]
            [cpu.instructions.utility :as ut]
            [cpu.decoder.core :as codes]
            [cpu.memory.bus.core :as bus]
            [cpu.instructions.core :as instr]))


(defn map-byte-to-entry [memory registers]
  (->> (reg/PC registers)
       (bus/read-byte memory)
       (codes/get-hex-instruction)))


(def regs reg/registers)

(defn main-cpu-loop [memory registers]
  (loop [m memory
         r registers
         halt-flag-is-set false]
    (if halt-flag-is-set
       (do (println "CPU Main Loop Completed.")
               :done)
      (let [entry (map-byte-to-entry m r)
            instr (instr/get-instruction entry)
            res (instr entry m r)
            updated-memory (:memory res)
            updated-registers (:registers res)
            halt-flag (:halt res)]
        (recur updated-memory updated-registers halt-flag)))))