(ns cpu.instructions.core
  (:require [cpu.registers.core :as reg]
            [cpu.instructions.utility :as ut]
            [cpu.instructions.add.core :as add]
            [cpu.memory.bus.core :as bus]
            [cpu.instructions.misc.core :as misc]
            [cpu.decoder.core :as dec]))


(comment
(defn map-byte-to-entry [memory registers]
  (->> (reg/program-counter registers)
       (bus/read-byte memory)
      ; (codes/get-hex-instruction)
       )))



  (defn run-instruction [entry memory registers]
    (case (:op entry)
      :ADD (add/add-instruction entry memory registers)
      (ut/missing-instr-error entry)))

(defn execute-instruction [entry memory registers]
  (if (ut/is-invalid-entry entry)
    (ut/error-msg "Execute instruction" entry)
    (run-instruction entry memory registers)))

(def e (-> (byte 0xE8)
           (dec/get-hex-instruction)))

(def regs (as-> reg/registers $ 
            (reg/update-register (byte 0xFF) $ reg/accumulator)
            (reg/update-register (byte 0xFF) $ reg/E)))
  
(def mem (as-> (bus/create-memory (byte 0xFFFF)) m
           (bus/set-data-in-memory 0 m 30)
           (bus/set-data-in-memory 1 m 30)))
  
(def res (execute-instruction e mem regs))
(println (:registers res))