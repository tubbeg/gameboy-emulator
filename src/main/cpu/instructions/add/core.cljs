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

(defn +l [type & args]
  (loop [i args
         sum 0]
    (case (count i)
      0 sum
      (let [f (first i)
            rem (rest i)
            max (case type
                  :b16 (byte 0xFFFF)
                  :byte (byte 0xFF)
                  (do (ut/error-msg "Unkown switch: " type)
                      0))]
        (cond
          (> (+ f sum) max) max
          :else (recur rem (+ f sum)))))))

(defn create-flags-8bit-add [targ src]
  (let [z (zero? (+ targ src))
        c (flags/is-carry-8bit-2operands? targ src +)
        h (flags/is-half-carry-8bit-2operands? targ src +)]
    (flags/create-flag-record-sub-add-adc-8 z h c)))

(defn create-flags-16bit-add [targ src]
  (let [z (zero? (+ targ src))
        c (flags/is-carry-16bit-2operands? targ src +)
        h (flags/is-half-carry-16bit-2operands? targ src +)]
    (flags/create-flag-record-sub-add-adc-8 z h c)))

(defn fetch-add-update
  "Returns {:memory ... :registers ... :halt false}"
  [entry memory registers src-type targ-type flags-fn]
  (let [t-rec (ut/get-targ-data entry memory registers targ-type)
        s-rec (ut/get-src-data entry
                               memory
                               (:registers t-rec)
                               src-type)
        flags (flags-fn (:data t-rec) (:data s-rec))]
    (-> (+l :byte (:data t-rec) (:data s-rec))
        (ut/update-target entry
                          flags
                          memory
                          registers
                          false
                          false
                          :reg))))

(defn add-reg8 [entry memory registers]
  (fetch-add-update entry
                    memory
                    registers
                    :reg8
                    :reg8
                    create-flags-8bit-add))

(defn add-n8 [entry memory registers]
  (fetch-add-update entry
                     memory
                     registers
                     :n8
                     :reg8
                     create-flags-8bit-add))

(defn add-HL-pointer-source
  [entry memory registers]
  (fetch-add-update entry
                    memory
                    registers
                    :HL-pointer
                    :reg8
                    create-flags-8bit-add))

(defn add-reg16-to-HL 
  "Returns {:memory ... :registers ... :halt false}"
  [entry memory registers]
  (fetch-add-update entry
                    memory
                    registers
                    :reg16
                    :reg16
                    create-flags-16bit-add))

; signed value
; it's just the 2-comp representation
; signed -1 <-> unsigned 255 
; so addition should be the same regardless (hopefully)
; and it doesn't matter that it's 8 bit source
; since all numbers are technically 32-bit
(defn add-to-SP
  "Returns {:memory ... :registers ... :halt false}"
  [entry memory registers]
  (fetch-add-update entry
                    memory
                    registers
                    :e8
                    :reg8 ; NOTE! SP is defined among
                    ; all of the other 8 bit registers
                    ; which is why we use :reg8
                    create-flags-16bit-add))



(defn add-instruction
  "Returns {:memory ... :registers ... :halt false}"
  [entry memory registers] 
  (cond
    (ut/source-is-8bit-reg entry) (add-reg8 entry memory registers)
    (ut/source-is-n8 entry) (add-n8  entry memory registers)
    (ut/source-is-HL-pointer entry) (add-HL-pointer-source
                                     entry
                                     memory
                                     registers)
    (ut/target-is-HL-reg entry) (add-reg16-to-HL
                                 entry memory registers)
    (ut/target-is-SP entry) (add-to-SP entry memory registers)
    :else (do
            (ut/error-msg "ADD" entry))))






