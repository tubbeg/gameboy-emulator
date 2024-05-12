(ns cpu.instructions.utility
  (:require [cpu.registers.core :as reg]
            [cpu.memory.bus.core :as bus]))




(defn error-msg [& args]
  (println "CPU ERROR!" args)
  :ERROR!)

(defn create-mem-reg-record [regs halt memory]
  {:memory memory :registers regs :halt halt})

(defn entry-source-is-pointer [entry]
  (-> entry
      (:source)
      (:pointer)))

(defn entry-target-is-pointer [entry]
  (-> entry
      (:target)
      (:pointer)))

(defn get-target-reg [entry]
  (-> entry
      (:target)
      (:reg)))

(defn get-source-reg [entry]
  (-> entry
      (:source)
      (:reg)))

(defn source-is-n8 [entry]
  (-> entry
      (get-source-reg)
      (= reg/n8)))


(defn create-data-record
  "Returns {:data ... :registers ...}"
  [data registers]
  {:data data :registers registers})

(defn read-and-increment-pc
  "Returns {:data ... :registers ...}"
  [memory registers]
  (let [regs-update (reg/inc-program-counter registers)
        adress (reg/program-counter regs-update)
        _byte  (bus/read-byte memory adress)]
    (create-data-record _byte regs-update)))

(defn read-byte-HL-pointer
  "Returns {:data ... :registers ...}"
  [memory registers]
  (as-> registers $
    (reg/HL $)
    (bus/read-byte memory $)
    (create-data-record $ registers)))

(defn source-is-8bit-reg [entry]
  (-> entry
      (get-source-reg)
      (reg/is-8bit-register)))

(defn source-is-16bit-reg [entry]
  (-> entry
      (get-source-reg)
      (reg/is-16bit-register)))

(defn target-is-8bit-reg [entry]
  (-> entry
      (get-target-reg)
      (reg/is-8bit-register)))

(defn target-is-16bit-reg [entry]
  (-> entry
      (get-target-reg)
      (reg/is-16bit-register)))

(defn target-is-HL-reg [entry]
  (and (= (get-target-reg entry) reg/HL)
       (not (entry-target-is-pointer entry))))

(defn target-is-HL-pointer [entry]
  (and (= (get-target-reg entry) reg/HL)
       (entry-target-is-pointer entry)))

(defn source-is-HL-pointer [entry]
  (and (= (get-source-reg entry) reg/HL)
       (entry-source-is-pointer entry)))

(defn target-is-SP [entry]
   (-> entry
       (:target)
       (:reg)
       (= reg/stack-pointer)))


(defn missing-instr-error [& args]
  (throw (new js/Error (str "Instruction " args " Not Found!"))))


(defn NotYetImplemented [& args]
  (println args)
  (throw (new js/Error "Instruction Not Yet Implemented!")))


(defn is-invalid-entry [entry]
  (cond
    (= entry nil) true
    (= (:op entry) nil) true
    (= (:op entry) :none) true
    :else false))

(defn get-src-data
  "Returns {:data ... :registers ...}"
  [entry memory registers src-type]
  (case src-type
    :reg8  (-> entry
               (get-source-reg)
               (registers)
               (create-data-record registers))
    :n8 (read-and-increment-pc memory registers)
    :e8 (read-and-increment-pc memory registers)
    :HL-pointer (read-byte-HL-pointer memory registers)
    :reg16 (-> entry
               (get-source-reg)
               (reg/get-virtual16b-reg registers)
               (create-data-record registers))
    (do
      (error-msg "Unkown switch: " src-type "Entry: " entry)
      :ERROR-UNKOWN-SWITCH!)))


(defn get-targ-data
  "Returns {:data ... :registers ...}"
  [entry memory registers sw]
  (case sw
    :reg8  (-> entry
               (get-target-reg)
               (registers)
               (create-data-record registers))
    :reg16 (-> entry
               (get-target-reg)
               (reg/get-virtual16b-reg registers)
               (create-data-record registers))
    :HL-pointer (read-byte-HL-pointer memory registers)
    (do
      (error-msg "Unkown switch: " sw "Entry: " entry)
      :ERROR-UNKOWN-SWITCH!)))

(defn update-target-reg [data flags memory target-reg regs]
  (-> data
      (reg/update-register regs target-reg)
      (reg/set-clear-flags flags)
      ; HALT flag will always be false for all ops
      ; except halt-op
      (create-mem-reg-record false memory)))

(defn inc-hl-if-true [regs inc?]
  (if inc?
    (reg/increment-register-HL regs)
    regs))

(defn dec-hl-if-true [regs dec?]
  (if dec?
    (reg/increment-register-HL regs)
    regs))

(defn update-target-memory
  [data adress flags memory regs inc? dec?]
  (let [mem (bus/set-data-in-memory adress memory data)]
    (-> regs
        (dec-hl-if-true dec?)
        (inc-hl-if-true inc?)
        (reg/set-clear-flags flags)
        (create-mem-reg-record false mem))))

(defn update-target
  "The vast majority of ops will update registers.
   Returns mem-reg record:
   {:memory memory :registers regs :halt halt}"
  [data entry flags memory regs inc? dec? sw]
  (let [target-reg (get-target-reg entry)]
    (case sw
      :memory
      (update-target-memory data
                            (target-reg regs)
                            flags
                            memory
                            regs
                            inc?
                            dec?)
      :reg (update-target-reg data
                              flags
                              memory
                              target-reg
                              regs)
      (do (error-msg "Unkown switch: " sw)
          :ERROR-UNKOWN-SWITCH!))))