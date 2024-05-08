(ns cpu.instructions.core
  (:require [cpu.registers.core :as reg]
            [cpu.instructions.add.core :as add]
            [cpu.instructions.bitwise.core :as bit]
            [cpu.instructions.incdec.core :as inc]
            [cpu.instructions.sub.core :as sub]
            [cpu.instructions.utility :as ut]))


(defn stop [data]
  (println "Detected stop instruction"))

(defn halt [data]
  (println "Set abort flag here!"))

(defn nop [data]
  (println "Add delay here for cycles? Synchronization?")
  (println "Something to think about"))

(comment (def example-data-add
           {:op :add :target :a :source :n8 :flags [:z :clear-n :c :h]
            :byte-length 1 :duration 4})

         (def dummyMemory
           (->
            (for [i (range 0 (byte 0xFFFF))]
              (if (not= i (byte 0xF0))
                (byte 0x00)
                1337))
            (into [])))

         (reg/set-reg! :pc (- (byte 0xF0) 1))
         (reg/set-reg! :a 0)
         (add/add example-data-add dummyMemory)
         (reg/get-reg :pc))



(def add-default-flags
  {:zero :calc :sub :clear :carry :calc :half-carry :calc})
(def example-data-add
  {:op :add :target :a :source :hl-pointer
   :flags add-default-flags :byte-length 1
   :duration 4})

(def dummyMemory
  (->
   (for [i (range 0 (byte 0xFFFF))]
     (if (not= i (byte 0xF000))
       (byte 0x00)
       1337))
   (into [])))


(reg/set-reg! :a 0)
(add/add example-data-add dummyMemory)


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
  {:ADD add/add
   :ADC add/adc
   :SBC sub/sbc
   :SUB sub/sbc
   :INC inc/inc-instr
   :DEC inc/dec-instr
   :AND bit/and-instr
   :OR bit/or-instr
   :XOR bit/xor-instr
   :HALT ut/NotYetImplemented
   
   :STOP ut/NotYetImplemented
   :NOP ut/NotYetImplemented
   :LD ut/NotYetImplemented
   :default (fn [& args] (ut/missing-instr-error (first args)))})