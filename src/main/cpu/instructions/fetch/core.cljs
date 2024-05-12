(ns cpu.instructions.fetch.core
  (:require [cpu.registers.core :as reg]
            [cpu.memory.bus.core :as bus]
            [cpu.instructions.utility :as ut]))


(defn fetch-register-data [reg regs]
  (cond
    (reg/is-16bit-register reg) (reg/get-virtual16b-reg reg regs)
    (reg/is-8bit-register reg) (reg regs)
    :else (ut/error-msg "UNKOWN REGISTER TYPE. FETCH DATA.")))

(defn fetch-immedi8-no-pc-inc
  "Does not actually increment PC. Only reads next adress"
  [regs memory]
  (as-> regs $
    (reg/inc-program-counter $)
    (reg/program-counter $)
    (bus/read-byte memory $)))

(defn fetch-next-immedi8-no-pc-inc
  "Does not actually increment PC.
   Only reads (next) next adress"
  [regs memory]
  (as-> regs $
    (reg/inc-program-counter $)
    (reg/inc-program-counter $)
    (reg/program-counter $)
    (bus/read-byte memory $)))

(defn make-16 [high low]
  (reg/create-virtual-16-bit-reg high low))

(defn fetch-immedi16-no-pc-inc
  [regs memory]
  (make-16 (fetch-immedi8-no-pc-inc regs memory)
           (fetch-next-immedi8-no-pc-inc regs memory)))

(defn fetch-immedi16-inc
  "Output [16bit-number registers]"
  [regs memory]
  [(fetch-immedi16-no-pc-inc regs memory)
   (-> regs
       (reg/inc-program-counter)
       (reg/inc-program-counter))
   :none])

(defn fetch-immedi8-inc
  "Returns [byte regs]"
  [regs memory]
  [(fetch-immedi8-no-pc-inc regs memory)
   (reg/inc-program-counter regs)
   :none])

(defn fetch-immediate-data
  "Can read 1 or 2 bytes. Increments PC accordingly.
   Output: [data registers]"
  [b16? memory regs]
  (case b16?
    true (fetch-immedi16-inc regs memory)
    (fetch-immedi8-inc regs memory)))

(defn add-byte-if-8bit [data reg]
  (if (reg/is-8bit-register reg)
    (+ (byte 0xFF00) data)
    data))

(defn get-byte-from-adress-at-reg
  "Returns [data regs]"
  [reg memory regs]
  [(as-> reg $
     (fetch-register-data $ regs)
     (add-byte-if-8bit $ reg)
     (bus/read-byte memory $))
   regs])

(defn fetch-pointer [])



(defn pointer? [resource]
  (true? (:pointer resource)))

(defn reg-data? [resource]
  (println "Checking reg data")
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

(defn immedi8? [resource]
  (case (:reg resource)
    :n8 true
    :a8 true ;adress
    :e8 true
    false))

(defn immedi16? [resource]
  (case (:reg resource)
    :a16 true
    :n16 true
    false))

(defn not-yet-implemented? [resource]
  (println "Checking resource: " resource )
  (case (:reg resource)
    :NZ true
    :Z true
    :NC true
    :is-byte true
    false))

(def reg-keys [:BC
               :n16 :A :B
               :n8 :a16 :SP
               :HL :C :DE :D
               :e8 :E :NZ :H
               :Z :L :NC
               :is-byte :a8
               :AF])

(defn remove-pointer [res]
  (assoc res :pointer false))

(defn inc-dec-pointer
  "Only used in a few instructions with pointers.
   The HL register is typically incremented/decremented
   after reading from memory, which is what this function
   does."
  [regs reg inc? dec?]
  (cond
    inc? (reg/increment-register regs reg)
    dec? (reg/decrement-register regs reg)
    :else regs))

(defn get-pt-byte-and-regs
  "It's important that it returns a vector so that the
   fetch-resource function also returns the correct
   registers when terminating. If it only accepted the
   result from the memory bus then the registers would
   be lost. Note that none parameter should always be :none"
  [[adress regs none] memory reg inc? dec?]
  [(bus/read-byte memory adress)
   (inc-dec-pointer regs reg inc? dec?)
   adress])

(defn err-fetch [msg]
  (println msg)
  :ERROR-FETCH!)

(comment
  "Example resource data"
  {:reg :A, :inc false, ; inc happens afterwords to the source
   ; which is usually 16b adress from register
   :dec false, :pointer false,
   :byte :none})

(defn fetch-resource
  "res is RESource
   :target and :source are resources.
   Output: [data registers adress]
   data : 1 or 2 byte data
   registers : defined in regs package
   adress : either a 16bit adress or :none
   "
  [res memory regs]
  (let [reg (:reg res)]
    (cond
      (not-yet-implemented? res) (err-fetch "NotYetImplemented!")
      (pointer? res) (-> res
                         (remove-pointer)
                         (fetch-resource memory regs)
                         (get-pt-byte-and-regs memory reg
                                               (:inc res)
                                               (:dec res)))
      (immedi16? res) (fetch-immediate-data true memory regs)
      (immedi8? res) (fetch-immediate-data false memory regs)
      (reg-data? res) [(fetch-register-data reg regs) regs :none]
      :else (err-fetch "INVALID RESOURCE"))))