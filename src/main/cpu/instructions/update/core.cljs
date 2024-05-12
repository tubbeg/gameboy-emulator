(ns cpu.instructions.update.core
  (:require [cpu.registers.core :as reg]
            [cpu.memory.bus.core :as bus]
            [cpu.instructions.fetch.core :as fetch]
            [cpu.instructions.utility :as ut]))


(defn err-update [msg]
  (println msg)
  :ERROR-UPDATE!)

(defn update-memory [data memory adress]
 (bus/set-data-in-memory adress memory data))

(defn update-16 [data regs reg]
  (let [[high low] (reg/translate-virtual-to-regs reg)]
    (reg/set-16-bit-reg data regs high low)))

(defn update-reg [data res regs]
   (let [reg (:reg res)]
    (cond
      (reg/is-16bit-register reg) (update-16 data regs reg)
      (reg/is-8bit-register reg) (reg/update-register data
                                                      regs
                                                      reg)
      :else (err-update "INVALID REGISTER"))))

(defn pointer? [resource]
  (true? (:pointer resource)))

(defn reg-data? [resource]
  (and (not (pointer? resource))
       (case (:reg resource)
         :A true
         :B true
         :C true
         :D true
         :E true
         :F true
         :H true
         :L true
         :SP true
         :PC true
         :AF true
         :HL true
         :BC true
         :DE true
         false)))

(defn update-resource
  "res is RESource.
   :target and :source are resources.
   Output: [registers memory]"
  [data res registers memory adress]
  (cond
    (reg-data? res) [(update-reg data res registers) memory]
    (pointer? res) [registers (update-memory data memory adress)]
    :else (err-update "INVALID RESOURCE")))