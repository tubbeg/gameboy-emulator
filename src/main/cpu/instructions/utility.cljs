(ns cpu.instructions.utility
  (:require [cpu.registers.core :as reg]
            [cpu.memory-bus.core :as bus]))

(defn error-msg [& args]
  (println "MISSING CPU INSTRUCTION!" args)
  :ERROR!)

(defn missing-instr-error [msg]
  (throw (new js/Error (str "Instruction " msg " Not Found!"))))

(defn read-next-byte-with-pc-inc
  "Increments the PC register and reads
   the byte pointed by the register."
  [memory registers]
  (let [regs (reg/inc-program-counter registers)
        pc (reg/program-counter registers)
        _byte (bus/read-byte memory pc)]
    {:registers regs :byte _byte}))


(defn read-hl-pointed-byte
  "Reads byte pointed by HL-register"
  [memory registers]
  (bus/read-byte memory (reg/HL registers)))

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



(comment
  "Flags can have following values:
   :clear - set to zero
   :set - set to 1
   :none - do nothing
   :calc - determine value through calculation
   ")

(defn calc-and-set-flags
  ([flags op target registers]
   (let [zero (determine-zero op target (:zero flags))
         carry (determine-carry op target (:carry flags))
         sub (:sub flags) ; sub will always be predetermined
         half-carry (determine-half-carry op target (:carry flags))
         updated-regs (reg/set-clear-flags zero
                                           carry
                                           half-carry
                                           sub
                                           registers)]
     updated-regs))
  ([flags op target registers source]
   (let [zero (determine-zero op target (:zero flags) source)
         carry (determine-carry op target (:carry flags) source)
         sub (:sub flags) ; sub will always be predetermined
         half-carry (determine-half-carry op target
                                          (:carry flags)
                                          source)
         updated-regs (reg/set-clear-flags zero
                                           carry
                                           half-carry
                                           sub
                                           registers)]
     updated-regs)))


(defn read-data-from-pointer
  "Returns {:registers ... :byte ...}"
  [registers reg memory inc dec]
  (case reg
    :a8 (do (println "Not Yet Implemented!") :ERROR!)
    :a16 (do (println "Not Yet Implemented!") :ERROR!)
    (let [data (->> (reg registers)
                    (bus/read-byte memory))]
      (if inc
        {:registers (reg/increment-register-HL registers)
         :byte data}
        (if dec
          {:registers (reg/decrement-register-HL registers)
           :byte data}
          {:registers registers
           :byte data})))))

(defn read-default-regs [reg registers]
  (case reg
    reg/accumulator (reg/accumulator registers)
    reg/flag-register (reg/flag-register registers)
    reg/B (reg/B registers)
    reg/C (reg/C registers)
    reg/D (reg/D registers)
    reg/E (reg/E registers)
    reg/H (reg/H registers)
    reg/L (reg/L registers)
    reg/program-counter (reg/program-counter registers)
    reg/stack-counter (reg/stack-counter registers)
    reg/AF (reg/get-reg-AF registers)
    reg/BC (reg/get-reg-BC registers)
    reg/DE (reg/get-reg-DE registers)
    reg/HL (reg/get-reg-HL registers)
    (do (println "Not Implemented! " reg) :ERROR!)))

(defn get-data-from-entry
  [registers reg memory pointer _byte _inc _dec]
  (if pointer
    (read-data-from-pointer registers reg memory _inc _dec)
    (case reg
      :n8 (read-next-byte-with-pc-inc memory registers)
      :is-byte {:registers registers
                :byte (bus/read-byte memory _byte)}
      {:registers registers
       :byte (read-default-regs reg registers)})))

(defn get-data-from-operand [registers operand memory]
  (case operand
    :none (do (println "Operand is none! Cannot get data: ")
              (println operand)
              :ERROR!)
    (let [targ (:reg operand)
          m memory
          p (:pointer operand)
          b (:byte targ)
          i (:inc targ)
          d (:dec targ)]
      (get-data-from-entry registers targ m p b i d))))

(defn op-with-carry [op carry-bit]
  (fn [a b] (op a b carry-bit)))

(defn halt []
  (fn []
    (println "Detected halt. Stopping emulator...")
    (reg/set-halt-flag!)))

(defn translate-keyop-to-func [op-key carry-bit]
  (case op-key
    :NOP (fn [] (do (println "Do something here")
                    error-msg))
    :HALT (fn []
            (do
              (println "HALT Detected! Aborting...")
              (reg/set-halt-flag!)))
    :ADD +
    :SUB -
    :ADC (op-with-carry + carry-bit)
    :SBC (op-with-carry - carry-bit)
    :AND bit-and
    :OR (fn [a b] (do
                    (println "Easy way to log data")
                    (println a b)
                    (bit-or a b)))
    :XOR bit-xor
    :INC inc
    :DEC dec
    error-msg))

(defn log-data [data target source]
  (println "Result: " data target source)
  data)

(defn set-data
  "Returns memory"
  ([registers op targ-dat is-adress adress targ-reg mem]
   (let [result (op targ-dat)]
     (if is-adress
       (bus/set-data-in-memory adress mem result)
       (do (reg/set-reg! targ-reg result) mem))))
  ([registers op targ-dat is-adress adress targ-reg mem src-data]
   (let [result (op targ-dat src-data)]
     (if is-adress
       (bus/set-data-in-memory adress mem result)
       (do (reg/set-reg! targ-reg result) mem)))))

(defn set-flags-and-execute-op
  ([target-reg registers flags byte op targ-data pointer memory]
   (-> (calc-and-set-flags flags op targ-data registers)
       (set-data op targ-data pointer byte target-reg memory)))
  ([target-reg registers byte flags op targ-data pointer memory src-data]
   (-> (calc-and-set-flags flags op targ-data registers src-data)
       (set-data op targ-data pointer byte target-reg memory src-data))))



(defn is-invalid-entry [entry]
  (cond
    (= entry nil) true
    (= (:op entry) nil) true
    :else false))

(defn has-no-operands [entry]
  (if (= (:target entry) nil)
    (error-msg entry)
    (= (:target entry) :none)))

(defn has-one-operand [entry]
  (if (= (:source entry) nil)
    (error-msg entry)
    (= (:target entry) :none)))

(defn handle-no-operands-instr 
  "Returns {:memory ... :registers ... :halt false}"
  [entry memory registers]
  (-> (error-msg entry)))

(defn handle-one-operand
  "Returns {:memory ... :registers ... :halt false}"
  [entry memory registers]
  (-> (error-msg entry)))

(defn handle-two-operands
  "Returns {:memory ... :registers ... :halt false}"
  [entry memory registers])

; generic function for some instructions
; doesn't work for every op
(defn cpu-instr
  "Returns {:memory ... :registers ... :halt false}"
  [entry memory registers]
  (cond
    (is-invalid-entry entry) (error-msg entry registers)
    (has-no-operands entry) (handle-no-operands-instr entry
                                                      memory
                                                      registers)
    (has-one-operand entry) (handle-one-operand entry
                                                memory
                                                registers)
    :else (handle-two-operands entry memory registers)))