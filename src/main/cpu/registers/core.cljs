(ns cpu.registers.core)


(comment
  " Sharp SM83 8-bit processor

   Similar instruction set to the Z80 processor,
   but not identical.
   
   Goals for functional CPU:
   
   1. Add all registers and utility functions

   2. Add all instructions

   3. Add decoder

   4. CPU main loop

   5. Fully functional test using assembly
   ")

(def def-byte (byte 0x00))
(def def-reg-PC-start-location (byte 0x0100)) ; after boot ROM
(def def-reg-SP-start-location (byte 0xFFFE)) ; this might be wrong


; Some instructions reads and writes using 16-bit
; Essentially, there are 16-bit virtual registers
; For example: register-hl

(def init-byte (byte 0x00))

; flag register
  ; the last 4 bits are always zero
  ; 0bxxxx0000
  ; bit 7 zero flag ; dec 8
  ; bit 6 subtraction ; dec 4
  ; bit 5 half carry ; dec 2
  ; bit 4 carry ; dec 1
  ; 0bzshc0000


(comment "
          Removed all mutable state! No more atoms.

          Should reduce the amount of bugs
          ")

(def flag-register :F)
(def accumulator :A)
(def program-counter :PC)
(def stack-pointer :SP)
(def AF :AF)
(def DE :DE)
(def BC :BC)
(def HL :HL)
(def B :B)
(def C :C)
(def D :D)
(def E :E)
(def H :H)
(def L :L)
(def n8 :n8)
(def e8 :e8)


(def virtual-registers-list
  (list
   AF
   DE
   BC
   HL))

(def registers
  {accumulator init-byte
   B init-byte
   C init-byte
   D init-byte
   E init-byte
   flag-register init-byte
   H init-byte
   L init-byte
   program-counter init-byte
   stack-pointer init-byte})


(defn is-register [reg]
  (-> reg
      (registers)
      (not= nil)))

(defn is-8bit-register [reg]
  (println "Checking reg" reg)
  (cond
    (= reg accumulator) true
    (= reg B) true
    (= reg C) true
    (= reg D) true
    (= reg E) true
    (= reg flag-register) true
    (= reg H) true
    (= reg L) true
    :else false))

(defn is-16bit-register [reg]
  (println "Checking reg" reg)
  (cond
    (= reg AF) true
    (= reg BC) true
    (= reg DE) true
    (= reg HL) true
    (= reg program-counter) true
    (= reg stack-pointer) true
    :else false))

(def zero-bit (byte 0x80))
(def carry-bit (byte 0x10))
(def half-carry-bit (byte 0x20))
(def sub-bit (byte 0x40))


(defn get-sub-bit [registers]
  (->
   (flag-register registers)
   (bit-and sub-bit)
   (bit-shift-right 4)))

(defn get-carry-bit [registers]
  (->
   (flag-register registers)
   (bit-and carry-bit)
   (bit-shift-right 4)))

(defn get-half-carry-bit [registers]
  (->
   (flag-register registers)
   (bit-and half-carry-bit)
   (bit-shift-right 4)))

(defn get-zero-bit [registers]
  (->
   (flag-register registers)
   (bit-and zero-bit)
   (bit-shift-right 4)))

(defn update-register
  "Example of return data: {:A (byte 0xFF) :B (byte 0xFE) ...}"
  [value registers switch]
  (-> registers
      (assoc switch value)))

(defn set-zero-flag [registers]
  (-> zero-bit
      (bit-or (flag-register registers))
      (update-register registers flag-register)))

(defn set-carry-flag [registers]
  (-> carry-bit
      (bit-or (flag-register registers))
      (update-register registers flag-register)))

(defn set-half-carry-flag [registers]
  (-> half-carry-bit
      (bit-or (flag-register registers))
      (update-register registers flag-register)))

(defn set-sub-flag [registers]
  (-> sub-bit
      (bit-or (flag-register registers))
      (update-register registers flag-register)))

(defn clear-zero-flag [registers]
  (-> zero-bit
      (bit-not)
      (bit-and (flag-register registers))
      (update-register registers flag-register)))

(defn clear-carry-flag [registers]
  (-> carry-bit
      (bit-not)
      (bit-and (flag-register registers))
      (update-register registers flag-register)))

(defn clear-half-carry-flag [registers]
  (-> half-carry-bit
      (bit-not)
      (bit-and (flag-register registers))
      (update-register registers flag-register)))

(defn clear-sub-flag [registers]
  (-> sub-bit
      (bit-not)
      (bit-and (flag-register registers))
      (update-register registers flag-register)))

(defn increment-register
  "Example usage: (increment-register registers :B)"
  [registers register]
  (-> (register registers)
      (inc)
      (update-register registers register)))

(defn decrement-register
  "Example usage: (decrement-register registers :B)"
  [registers register]
  (-> (register registers)
      (dec)
      (update-register registers register)))




; I have not figured out what happens when
; adding 16 bit numbers, when does it overflow?
; According to :
; https://stackoverflow.com/questions/57958631/game-boy-half-carry-flag-and-16-bit-instructions-especially-opcode-0xe8
; the ALU is 8 bit, so this needs to be updated
; to accommodate overflow to (> 0xFFFF) (carry) and overflow to 0x1FFF (half carry) 
; UPDATE: made some changes that are going to hopefully work

(defn get-high-byte [b16]
  (bit-and (byte 0xFF00) b16))

(defn get-low-byte [b16]
  (bit-and (byte 0x00FF) b16))

(defn get-higher-nibble [_byte]
  (-> (byte 0xF0)
      (bit-and _byte)
      (bit-shift-right 4)))

(defn get-lower-nibble [_byte]
  (-> (byte 0x0F)
      (bit-and _byte)))

(defn byte-operation-is-carry?
  "This only works for bytes
   NOTE! Incorrect use of inc with multiple arguments
   can be catastrophic!"
  ([op target]
   (let [mask-target (bit-and (byte 0x00FF) target)]
     (-> (op mask-target)
         (bit-shift-right  8)
         (>  0))))
  ([op target value]
   (let [mask-target (bit-and (byte 0x00FF) target)
         mask-value (bit-and (byte 0x00FF) value)]
     (-> (op mask-target mask-value)
         (bit-shift-right  8)
         (>  0)))))

(defn byte-operation-is-half-carry?
  "This only works for bytes
   NOTE! Incorrect use of inc with multiple arguments
   can be catastrophic!"
  ([op target]
   (let [t-low (get-lower-nibble target)
         res (op t-low)]
     (-> res
         (bit-shift-right 4)
         (> 0))))
  ([op target value]
   (let [t-low (get-lower-nibble target)
         v-low (get-lower-nibble value)
         res (op t-low v-low)]
     (-> res
         (bit-shift-right 4)
         (> 0)))))


(defn b16-operation-is-carry?
  ([op target]
   (let [high-byte-target (get-high-byte target)
         low-byte-target (get-low-byte target)]
     (or
      (byte-operation-is-carry? op high-byte-target)
      (byte-operation-is-carry? op low-byte-target))))
  ([op target value]
   (let [high-byte-target (get-high-byte target)
         high-byte-value (get-high-byte value)
         low-byte-target (get-low-byte target)
         low-byte-value (get-low-byte value)]
     (or
      (byte-operation-is-carry? op high-byte-target high-byte-value)
      (byte-operation-is-carry? op low-byte-target low-byte-value)))))


(defn b16-operation-is-half-carry?
  ([op target]
   (let [high-byte-target (get-high-byte target)
         low-byte-target (get-low-byte target)]
     (or
      (byte-operation-is-half-carry? op high-byte-target)
      (byte-operation-is-half-carry? op low-byte-target))))
  ([op target value]
   (let [high-byte-target (get-high-byte target)
         high-byte-value (get-high-byte value)
         low-byte-target (get-low-byte target)
         low-byte-value (get-low-byte value)]
     (or
      (byte-operation-is-half-carry? op high-byte-target high-byte-value)
      (byte-operation-is-half-carry? op low-byte-target low-byte-value)))))

(defn operation-is-carry?
  ([op target size]
   (case size
     :byte (byte-operation-is-carry? op target)
     :b16 (b16-operation-is-carry? op target)))
  ([op target size value]
   (case size
     :byte (byte-operation-is-carry? op target value)
     :b16 (b16-operation-is-carry? op target value))))

(defn operation-is-half-carry?
  ([op target size]
   (case size
     :byte (byte-operation-is-half-carry? op target)
     :b16 (b16-operation-is-half-carry? op target)))
  ([op target size value]
   (case size
     :byte (byte-operation-is-half-carry? op target value)
     :b16 (b16-operation-is-half-carry? op target value))))

(defn create-virtual-16-bit-reg [r-high r-low]
  (-> (bit-shift-left r-high 8)
      (bit-or r-low)))

(defn get-high-16-bit [r-16]
  (->
   (create-virtual-16-bit-reg (byte 0xFF) (byte 0x00))
   (bit-and r-16)
   (bit-shift-right 8)
   (byte)))

(defn get-low-16-bit [r-16]
  (->
   (create-virtual-16-bit-reg (byte 0x00) (byte 0xFF))
   (bit-and r-16)
   (byte)))

(defn set-16-bit-reg! [b16value registers reg-high reg-low]
  (let [r-high (get-high-16-bit b16value)
        r-low (get-low-16-bit b16value)
        update-high (-> (r-high)
                        (update-register registers reg-high))
        update-low (-> (r-low)
                       (update-register update-high reg-low))]
    update-low))

(defn increment-register-HL [registers]
  (-> (HL registers)
      (inc)
      (set-16-bit-reg! registers H L)))

(defn decrement-register-HL [registers]
  (-> (HL registers)
      (dec)
      (set-16-bit-reg! registers H L)))

(defn get-reg-AF [registers]
  (create-virtual-16-bit-reg
   (-> registers
       (accumulator))
   (-> registers
       (flag-register))))

(defn get-reg-BC [registers]
  (create-virtual-16-bit-reg
   (-> registers
       (B))
   (-> registers
       (C))))

(defn get-reg-DE [registers]
  (create-virtual-16-bit-reg
   (-> registers
       (D))
   (-> registers
       (E))))

(defn get-reg-HL [registers]
  (create-virtual-16-bit-reg
   (-> registers
       (H))
   (-> registers
       (L))))

(defn set-reg-HL 
  "Example of return data: {:A (byte 0xFF) :B (byte 0xFE) ...}"
  [bit16-val registers]
  (let [high (get-high-16-bit bit16-val)
        low (get-low-16-bit bit16-val)]
    (as->
     (update-register high registers H) regs
      (update-register low regs L))))

(defn get-virtual16b-reg [reg registers]
  (case reg
    AF (get-reg-AF registers)
    BC (get-reg-BC registers)
    DE (get-reg-DE registers)
    HL (get-reg-HL registers)))

(defn inc-program-counter [regs]
  (increment-register regs program-counter))

(defn set-byte-if-set-clear
  "NOTE! bit argument is actually a byte.
   Example: (byte 0x80) -> zero bit"
  [_byte bit set]
  (case set
    :calc _byte
    :clear (-> bit
               (bit-not)
               (bit-and _byte))
    :none _byte
    :set (-> bit
             (bit-or _byte))))

(defn set-clear-flags
  "Set flags to :none if no changes are made"
  [zero carry half-carry sub registers]
  (-> registers
      (flag-register)
      (set-byte-if-set-clear carry-bit carry)
      (set-byte-if-set-clear sub-bit sub)
      (set-byte-if-set-clear zero-bit zero)
      (set-byte-if-set-clear carry-bit half-carry)
      (update-register registers flag-register)))

(defn translateNibbleToHexString [nibble]
  (case nibble
    10 "A"
    11 "B"
    12 "C"
    13 "D"
    14 "E"
    15 "F"
    (str nibble)))

(defn byteToHexString [_byte]
  (let [h (get-higher-nibble _byte)
        l (get-lower-nibble _byte)
        s (str (translateNibbleToHexString h)
               (translateNibbleToHexString l))]
    s))

(defn b16ToHexString [b16]
  (let [h (-> b16
              (bit-and (byte 0xFF00))
              (bit-shift-right 8))
        l (-> (byte 0x00FF)
              (bit-and b16))
        s (str (byteToHexString h) (byteToHexString l))]
    s))



(defn print-reg-status
  "Helper function. For debugging"
  [registers]
  (let [a (accumulator registers)
        b (B registers)
        c (C registers)
        d (D registers)
        e (E registers)
        f (flag-register registers)
        h (H registers)
        l (L registers)
        af (get-reg-AF registers)
        bc (get-reg-BC registers)
        hl (get-reg-HL registers)
        de (get-reg-DE registers)
        pc (program-counter registers)
        sp (stack-pointer registers)
        half-carry (get-half-carry-bit registers)
        carry (get-carry-bit registers)
        zero (get-zero-bit registers)
        sub (get-sub-bit registers)
        s "0x"]
    (println "Register A: " s (byteToHexString a))
    (println "Register B: " s (byteToHexString b))
    (println "Register C: " s (byteToHexString c))
    (println "Register D: " s (byteToHexString d))
    (println "Register E: " s (byteToHexString e))
    (println "Register F: " s (byteToHexString f))
    (println "Register H: " s (byteToHexString h))
    (println "Register L: " s (byteToHexString l))
    (println "Register AF: " s (b16ToHexString af))
    (println "Register HL: " s (b16ToHexString hl))
    (println "Register BC: " s (b16ToHexString bc))
    (println "Register DE: " s (b16ToHexString de))
    (println "Register SP: " s (b16ToHexString sp))
    (println "Register PC: " s (b16ToHexString pc))
    (println "Half-Carry: " (> half-carry 0))
    (println "Carry: " (> carry 0))
    (println "Zero: " (> zero 0))
    (println "Sub: " (> sub 0))))

