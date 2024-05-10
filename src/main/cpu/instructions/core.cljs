(ns cpu.instructions.core
  (:require [cpu.registers.core :as reg]
            [cpu.instructions.utility :as ut]
            [cpu.decoder.core :as dec]))


(defn stop [data]
  (println "Detected stop instruction"))

(defn halt [data]
  (println "Set abort flag here!"))

(defn nop [data]
  (println "Add delay here for cycles? Synchronization?")
  (println "Something to think about"))




(dec/get-hex-instruction (byte 0xB1))

(def dummyMemory
  (->
   (for [i (range 0 (byte 0xFFFF))]
     (if (not= i (byte 0xF000))
       (byte 0x00)
       1337))
   (into [])))

;(reg/set-reg! :A 0)
;(reg/set-reg! :C (byte 0xF0))
;(reg/set-reg! :HL 15)
(dec/get-hex-instruction (byte 0xB1))
;(-> (dec/get-hex-instruction (byte 0xB1))
;    (ut/cpu-instr dummyMemory))

;(reg/print-reg-status)
(byte 0x01E0)

(comment
  (def example-data-add
    {:op :add
     :target :a
     :source :b
     :flags [:z :clear-n :c :h]
     :byte-length 1
     :duration 4})

  (def dummyMemory
    (->
     (for [i (range 0 (byte 0xFFF))]
       (byte 0x00))
     (into [])))

  (reg/set-reg! :b 5)
  (reg/set-reg! :a 0)
  (add/add example-data-add dummyMemory))


(def instructions
  {:ADD ut/cpu-instr
   :ADC ut/cpu-instr
   :SBC ut/cpu-instr
   :SUB ut/cpu-instr
   :INC ut/cpu-instr
   :DEC ut/cpu-instr
   :AND ut/cpu-instr
   :OR ut/cpu-instr
   :XOR ut/cpu-instr
   :HALT ut/NotYetImplemented

   :STOP ut/NotYetImplemented
   :NOP ut/NotYetImplemented
   :LD ut/NotYetImplemented
   :default (fn [& args] (ut/missing-instr-error (first args)))})


(defn get-instruction [entry]
  (let [op (:op entry)]
    (case op
      nil (ut/error-msg entry)
      (op instructions))))