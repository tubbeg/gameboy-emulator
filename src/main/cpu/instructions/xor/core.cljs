(ns cpu.instructions.xor.core
  (:require [cpu.registers.core :as reg]
            [cpu.instructions.utility :as ut]
            [cpu.memory.bus.core :as bus]
            [cpu.registers.flags.core :as flags]))


(defn xor-l [a b]
  (let [res (bit-xor a b)
        max (byte 0xFF)]
    (cond
      (> res max) max
      :else res)))

(defn create-flags-8bit-xor [targ src]
  (let [z (zero? (bit-xor src targ))]
    (flags/create-flag-record-or-xor-8 z)))

(defn fetch-xor-update
  "Returns {:memory ... :registers ... :halt false}"
  [entry memory registers src-type targ-type flags-fn]
  (let [t-rec (ut/get-targ-data entry memory registers targ-type)
        s-rec (ut/get-src-data entry
                               memory
                               (:registers t-rec)
                               src-type)
        flags (flags-fn (:data t-rec) (:data s-rec))]
    (-> (xor-l (:data t-rec) (:data s-rec))
        (ut/update-target entry
                          flags
                          memory
                          registers
                          false
                          false
                          :reg))))

(defn xor-reg8 [entry memory registers]
  (fetch-xor-update entry
                    memory
                    registers
                    :reg8
                    :reg8
                    create-flags-8bit-xor))

(defn xor-n8 [entry memory registers]
  (fetch-xor-update entry
                     memory
                     registers
                     :n8
                     :reg8
                     create-flags-8bit-xor))

(defn xor-HL-pointer-source
  [entry memory registers]
  (fetch-xor-update entry
                    memory
                    registers
                    :HL-pointer
                    :reg8
                    create-flags-8bit-xor))


(defn xor-instruction
  "Returns {:memory ... :registers ... :halt false}"
  [entry memory registers] 
  (cond
    (ut/source-is-8bit-reg entry) (xor-reg8 entry memory registers)
    (ut/source-is-n8 entry) (xor-n8  entry memory registers)
    (ut/source-is-HL-pointer entry) (xor-HL-pointer-source
                                     entry
                                     memory
                                     registers)
    :else (do
            (ut/error-msg "XOR" entry))))






