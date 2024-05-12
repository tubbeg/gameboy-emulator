(ns cpu.instructions.add2.core
  (:require [cpu.registers.core :as reg]
            [cpu.instructions.utility :as ut]
            [cpu.memory.bus.core :as bus]
            [cpu.instructions.fetch.core :as f]
            [cpu.instructions.update.core :as up]
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

(defn create-flags-8bit-add [targ src]
  (let [z (zero? (+ targ src))
        c (flags/is-carry-8bit-2operands? targ src +)
        h (flags/is-half-carry-8bit-2operands? targ src +)]
    (flags/create-flag-record-sbc-sub-add-adc-8 z h c)))

(defn create-flags-16bit-add [targ src]
  (let [z (zero? (+ targ src))
        c (flags/is-carry-16bit-2operands? targ src +)
        h (flags/is-half-carry-16bit-2operands? targ src +)]
    (flags/create-flag-record-sbc-sub-add-adc-8 z h c)))

(defn validate-max [sum]
  (cond
    (> sum (byte 0xFF)) (byte 0xFF)
    :else sum))

(defn add-instruction
  "Returns {:memory ... :registers ... :halt false}"
  [entry memory registers]
  (let [targ-res (:target entry)
        src-res (:source entry)
        [src regs _] (f/fetch-resource src-res memory registers)
        [targ regs2 adress] (f/fetch-resource targ-res memory regs)]
    (-> (+ targ src)
        (validate-max)
        (up/update-resource targ-res regs2 memory adress))))





