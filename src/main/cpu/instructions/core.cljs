(ns cpu.instructions.core
  (:require [cpu.registers.core :as reg]
            [cpu.instructions.utility :as ut]
            [cpu.instructions.add.core :as add]
            [cpu.memory.bus.core :as bus]
            [cpu.instructions.misc.core :as misc]
            [cpu.decoder.core :as dec]))



(dec/get-hex-instruction (byte 0xB1))



;(reg/set-reg! :A 0)
;(reg/set-reg! :C (byte 0xF0))
;(reg/set-reg! :HL 15)
(dec/get-hex-instruction (byte 0xB1))
;(-> (dec/get-hex-instruction (byte 0xB1))
;    (ut/cpu-instr dummyMemory))

;(reg/print-reg-status)
(byte 0x01E0)



(def instructions
  {:ADD add/add-instruction 
   :HALT misc/halt
   :STOP ut/NotYetImplemented
   :NOP ut/NotYetImplemented
   :LD ut/NotYetImplemented
   :default (fn [& args] (ut/missing-instr-error (first args)))})


(defn get-instruction [entry]
  (let [op (:op entry)]
    (case op
      nil (ut/error-msg entry)
      (op instructions))))


(def e (-> (byte 0x83)
           (dec/get-hex-instruction)))


(def regs (as-> reg/registers $ 
            (reg/update-register (byte 0xFE) $ reg/accumulator)
            (reg/update-register (byte 0x0F $ reg/))))

(def mem (bus/create-memory (byte 0xFFFF)))

(def i (get-instruction e))

(println (:registers (i e mem regs)))