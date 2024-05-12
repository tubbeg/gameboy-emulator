(ns cpu.registers.flags.core
  (:require [cpu.registers.core :as reg]))

(def none :none)

(defn create-flag-record [z hc c s]
  {:zero z :half-carry hc :carry c :sub s})

(defn if-true-set [bool]
  (case bool
    true :set
    :clear))

(defn create-none-flags []
  (create-flag-record none none none none))

(defn is-zero [num]
  (= num 0))

(defn is-half-carry-8bit-2operands? [byte1 byte2 op-fn]
  (-> byte1
      (reg/get-lower-nibble)
      (op-fn (reg/get-lower-nibble byte2))
      (> (byte 0x0F))))

(defn is-carry-8bit-2operands? [byte1 byte2 op-fn]
  (-> byte1
      (op-fn byte2)
      (> (byte 0xFF))))

(defn is-half-carry-16bit-2operands? [b16-1 b16-2 op-fn]
  (let [byte1low (reg/get-low-byte b16-1)
        byte2low (reg/get-low-byte b16-2)
        byte1high (reg/get-high-byte b16-1)
        byte2high (reg/get-high-byte b16-2)]
    (or (is-half-carry-8bit-2operands? byte1low byte2low op-fn)
        (is-half-carry-8bit-2operands? byte1high byte2high op-fn))))

(defn is-carry-16bit-2operands? [b16-1 b16-2 op-fn]
  (let [byte1low (reg/get-low-byte b16-1)
        byte2low (reg/get-low-byte b16-2)
        byte1high (reg/get-high-byte b16-1)
        byte2high (reg/get-high-byte b16-2)]
    (or (is-carry-8bit-2operands? byte1low byte2low op-fn)
        (is-carry-8bit-2operands? byte1high byte2high op-fn))))

(defn create-flag-record-sub-add-adc-8 [z hc c]
  (let [zero (if-true-set z)
        half (if-true-set hc)
        carry (if-true-set c)
        sub :clear]
    (create-flag-record zero half carry sub)))

(defn create-flag-record-add-16 [hc c]
  (let [zero :none
        half (if-true-set hc)
        carry (if-true-set c)
        sub :clear]
    (create-flag-record zero half carry sub)))

(defn create-flag-record-add-SP [hc c]
  (let [zero :clear
        sub :clear
        half (if-true-set hc)
        carry (if-true-set c)]
    (create-flag-record zero half carry sub)))

;edge case
(defn create-flag-record-sbc-8 [source-reg z hc c]
  (let [zero (if-true-set z)
        half (if-true-set hc)
        carry (if-true-set c)]
    {:zero zero
     :half-carry half
     :carry (if (= source-reg reg/accumulator) :none carry)
     :sub :clear}))


; OR - XOR ---------------------------------

(defn create-flag-record-or-xor-8 [z]
  (let [zero (if-true-set z)
        half :clear
        carry :clear
        sub :clear]
    (create-flag-record zero half carry sub)))


; AND --------------------------------------

(defn create-flag-record-and-8 [z]
  (let [zero (if-true-set z)
        half :set
        carry :clear
        sub :clear]
    (create-flag-record zero half carry sub)))

; INC DEC -----------------------------------

(defn is-half-carry-8bit-1operand? [_byte op-fn]
  (-> _byte
      (op-fn)
      (> (byte 0x0F))))

(defn is-carry-8bit-1operand? [_byte op-fn]
  (-> _byte
      (op-fn)
      (> (byte 0xFF))))

(defn create-flag-record-inc-8 [z h]
  (let [zero (if-true-set z)
        half (if-true-set h)
        carry :clear
        sub :clear]
    (create-flag-record zero half carry sub)))

(defn create-flag-record-dec-8 [z h]
  (let [zero (if-true-set z)
        half (if-true-set h)
        carry :clear
        sub :set]
    (create-flag-record zero half carry sub)))

(defn create-flag-record-dec-inc-16 []
  (create-none-flags))


; -----------------------------------------




; -----------------------------------------

