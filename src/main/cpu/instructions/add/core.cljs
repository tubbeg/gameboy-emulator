(ns cpu.instructions.add.core
  (:require [cpu.registers.core :as reg]
            [cpu.instructions.utility :as ut]
            [cpu.memory.bus.core :as bus]
            [cpu.registers.flags.core :as flags]))


(comment
  "Example data"
  {:op :ADD, :target {:reg :A, :inc false,
                      :dec false, :pointer false,
                      :byte :none},
   :source {:reg :HL, :inc false,
            :dec false, :pointer true,
            :byte :none},
   :flags  {:zero :calc, :sub :clear,
            :half-carry :calc, :carry :calc},
   :byte-length 1, :duration 8})

(comment
  "Types of add:
   add A reg8
   add A n8
   add A HL-pointer
   add HL reg16
   add SP e8
   ")

(defn add-reg8
  "Returns {:memory ... :registers ... :halt false}"
  [entry memory registers] 
  (-> (ut/get-target-reg entry)
      (registers)
      (+ (-> entry
             (ut/get-source-reg)
             (registers)))
      (reg/update-register registers reg/accumulator)
      (flags/set-flags-acc entry)
      (ut/create-mem-reg-record false memory)))

(defn add-n8
  "n8 is indicated by the next byte of PC.
   Returns {:memory ... :registers ... :halt false}"
  [entry memory registers]
  (let [record (ut/read-and-increment-pc memory registers)
        regs (:registers record)
        _byte (:byte record)]
    (-> entry
        (ut/get-target-reg)
        (registers)
        (+ _byte)
        (reg/update-register regs reg/accumulator)
        (flags/set-flags-acc entry)
        (ut/create-mem-reg-record false memory))))

(defn add-HL-pointer-source  [entry memory registers]
  (-> (ut/read-byte-HL-pointer memory registers)
      (+ (reg/accumulator registers))
      (reg/update-register registers reg/accumulator)
      (flags/set-flags-acc entry)
      (ut/create-mem-reg-record false memory)))

(defn add-reg16-to-HL  [entry memory registers]
  (-> (ut/get-source-reg entry)
      (reg/get-virtual16b-reg registers)
      (+ (reg/get-reg-HL registers))
      (flags/set-flags-16bit-HL entry)
      (reg/set-reg-HL registers)
      (ut/create-mem-reg-record false memory)))

(defn add-to-SP
  "n8 is indicated by the next byte of PC.
   Returns {:memory ... :registers ... :halt false}"
  [entry memory registers]
  (let [record (ut/read-and-increment-pc memory registers)
        regs (:registers record)
        _byte (:byte record)]
    (-> entry
        (reg/get-reg-HL)
        (+ _byte)
        (reg/set-reg-HL regs)
        (flags/set-flags-16bit-HL entry)
        (ut/create-mem-reg-record false memory))))


(defn add-instruction
  "Returns {:memory ... :registers ... :halt false}"
  [entry memory registers]
  (let [halt false]
    (cond
      (ut/is-invalid-entry entry) (ut/error-msg "INVALID ENTRY" entry)
      (ut/source-is-8bit-reg entry) (add-reg8 entry memory registers)
      (ut/source-is-n8 entry) (add-n8  entry memory registers)
      (ut/source-is-HL-pointer entry) (add-HL-pointer-source
                                       entry
                                       memory
                                       registers)
      (ut/target-is-HL-reg entry) (add-reg16-to-HL
                                   entry memory registers)
      (ut/target-is-SP entry) (add-to-SP entry memory registers)
      :else (ut/error-msg "ADD" entry))))


