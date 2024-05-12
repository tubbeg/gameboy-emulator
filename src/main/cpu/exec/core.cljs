(ns cpu.exec.core
  (:require [cpu.registers.core :as reg]
            [cpu.instructions.utility :as ut]
            [cpu.decoder.core :as codes]
            [cpu.memory.bus.core :as bus]
            [cpu.instructions.core :as instr]
            [cpu.registers.flags.core :as flags]))



(def regs reg/registers)
(def flags flags/flags)

(defn main-cpu-loop [memory registers]
  (loop [m memory 
         r registers 
         halt-flag-is-set false
      ] 
    (let [entry (instr/map-byte-to-entry m r)
          res (instr/execute-instruction entry memory registers)
          updated-memory (:memory res)
          updated-registers (:registers res)
          halt-flag (:halt res)]
      (if halt-flag-is-set
        (do (println "CPU Main Loop Completed.")
            :done)
        (recur updated-memory updated-registers halt-flag)))))