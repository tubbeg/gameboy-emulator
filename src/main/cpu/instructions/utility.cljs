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

(defn read-and-increment-pc
  "Returns {:byte ... :registers ...}"
  [memory registers]
  (let [pc (reg/inc-program-counter registers)
        _byte  (bus/read-byte memory pc)
        regs (reg/update-register pc
                                  registers
                                  reg/program-counter)]
    {:byte _byte :registers regs}))


(defn read-byte-HL-pointer [memory registers]
  (->> (reg/HL registers)
       (bus/read-byte memory)))

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

(defn missing-instr-error [msg]
  (throw (new js/Error (str "Instruction " msg " Not Found!"))))


(defn is-zero [num]
  (= num 0))
(defn is-not-zero [num]
  (not= num 0))

(defn determine-size [target]
  (case target
    reg/HL :b16
    reg/AF :b16
    reg/BC :b16
    reg/DE :b16
    reg/stack-counter :b16
    reg/program-counter :b16
    :byte))

(defn determine-zero
  ([op target flag]
   (letfn [(clear-set [op target]
             (if (is-zero (op target))
               :set
               :clear))]
     (case flag
       :calc (clear-set op target)
       flag)))
  ([op target flag value]
   (letfn [(clear-set [op target value]
             (if (is-zero (op target value))
               :set
               :clear))]
     (case flag
       :calc (clear-set op target value)
       flag))))

(defn determine-carry
  ([op target flag]
   (letfn [(car [op target size]
             (if (reg/operation-is-carry? op target size)
               :set
               :clear))]
     (case flag
       :calc (car op target (determine-size target))
       flag)))
  ([op target flag value]
   (letfn [(car [op target value size]
             (if (reg/operation-is-carry? op target size value)
               :set
               :clear))]
     (case flag
       :calc (car op target value (determine-size target))
       flag))))

(defn determine-half-carry
  ([op target flag]
   (letfn [(hcar [op target size]
             (if (reg/operation-is-half-carry? op target size)
               :set
               :clear))]
     (case flag
       :calc (hcar op target (determine-size target))
       flag)))
  ([op target flag value]
   (letfn [(hcar [op target value size]
             (if (reg/operation-is-half-carry? op target  size value)
               :set
               :clear))]
     (case flag
       :calc (hcar op target value (determine-size target))
       flag))))

(defn NotYetImplemented [& args]
  (println args)
  (throw (new js/Error "Instruction Not Yet Implemented!")))


(defn is-invalid-entry [entry]
  (cond
    (= entry nil) true
    (= (:op entry) nil) true
    :else false))
