(ns cpu.instructions.core
  (:require [cpu.registers.core :as reg]
            [cpu.instructions.add.core :as add]
            [cpu.instructions.utility :as ut]))



(defn stop [data]
  (println "Detected stop instruction"))

(defn nop [data]
  (println "Add delay here for cycles? Synchronization?")
  (println "Something to think about"))

(comment
  (def example-data-add
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



(comment
  (def example-data-add
    {:op :add :target :a :source :hl-pointer
     :flags [:z :clear-n :c :h] :byte-length 1
     :duration 4})

  (def dummyMemory
    (->
     (for [i (range 0 (byte 0xFFFF))]
       (if (not= i 61440)
         (byte 0x00)
         1337))
     (into [])))

  (reg/set-reg! :h (byte 0xF0))
  (reg/set-reg! :a 0)
  (add/add example-data-add dummyMemory))

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
  {:add add/add
   :stop stop
   :nop nop
   :ldi ut/NotYetImplemented
   :default (fn [] :ErrorNotFound!)})