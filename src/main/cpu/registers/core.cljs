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


(def reg-a ;accumulator
  (atom {:reg def-byte}))
(def reg-b
  (atom {:reg def-byte}))
(def reg-c
  (atom {:reg def-byte}))
(def reg-d
  (atom {:reg def-byte}))
(def reg-e
  (atom {:reg def-byte}))
(def reg-f ; flag register
  ; the last 4 bits are always zero
  ; 0bxxxx0000
  ; bit 7 zero flag ; dec 8
  ; bit 6 subtraction ; dec 4
  ; bit 5 half carry ; dec 2
  ; bit 4 carry ; dec 1
  ; 0bzshc0000
  (atom {:reg (byte 0x00)}))
(def reg-h
  (atom {:reg def-byte}))
(def reg-l
  (atom {:reg def-byte}))

(def reg-PC ;16-bit
  (atom {:reg def-reg-PC-start-location}))
(def reg-SP ;16-bit
  (atom {:reg def-reg-SP-start-location}))

@reg-PC
(defn swap-byte! [atom-reg new-value]
  (swap! atom-reg assoc :reg new-value))

(defn set-zero-flag! [atom-reg-f]
  (let [reg-value (-> (byte 0x80)
                      (bit-or (:reg @atom-reg-f)))]
    (swap-byte! atom-reg-f reg-value)))

(defn set-half-carry-flag! [atom-reg-f]
  (let [reg-value (-> (byte 0x20)
                      (bit-or (:reg @atom-reg-f)))]
    (swap-byte! atom-reg-f reg-value)))

(defn set-carry-flag! [atom-reg-f]
  (let [reg-value (-> (byte 0x10)
                      (bit-or (:reg @atom-reg-f)))]
    (swap-byte! atom-reg-f reg-value)))

(defn set-subtraction-flag! [atom-reg-f]
  (let [reg-value (-> (byte 0x40)
                      (bit-or (:reg @atom-reg-f)))]
    (swap-byte! atom-reg-f reg-value)))

(defn clear-zero-flag! [atom-reg-f]
  (let [reg-value (-> (byte 0x80)
                      (bit-not)
                      (bit-and (:reg @atom-reg-f)))]
    (swap-byte! atom-reg-f reg-value)))

(defn clear-half-carry-flag! [atom-reg-f]
  (let [reg-value (-> (byte 0x20)
                       (bit-not)
                       (bit-and (:reg @atom-reg-f)))]
     (swap-byte! atom-reg-f reg-value)))

(defn clear-carry-flag! [atom-reg-f]
  (let [reg-value (-> (byte 0x10)
                       (bit-not)
                       (bit-and (:reg @atom-reg-f)))]
     (swap-byte! atom-reg-f reg-value)))

(defn clear-subtraction-flag! [atom-reg-f]
  (let [reg-value (-> (byte 0x40)
                       (bit-not)
                       (bit-and (:reg @atom-reg-f)))]
     (swap-byte! atom-reg-f reg-value)))

(defn clear-flags! [atom-reg-f]
  (swap-byte! atom-reg-f (byte 0x00)))

(defn increment-register! [atom-reg]
  (->> (inc (:reg @atom-reg))
       (swap-byte! atom-reg)))

(defn decrement-register! [atom-reg]
  (->> (dec (:reg @atom-reg))
       (swap-byte! atom-reg)))

(defn addition-is-carry? [target value]
  (-> (+ target value)
      (> (byte 0xFF))))

(defn addition-is-half-carry? [target value]
  (let [clear-high-reg (bit-and (byte 0x0F) target)
        clear-high-val (bit-and (byte 0x0F) value)]
    (->
     (+ clear-high-reg clear-high-val)
     (> (byte 0x0F)))))

(comment  (->
           (byte 0xC5)
           (addition-is-half-carry? (byte 0xFB))))


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

(defn set-16-bit-reg! [b16value atom-reg-high atom-reg-low]
  (let [r-high (get-high-16-bit b16value)
        r-low (get-low-16-bit b16value)]
    (swap-byte! atom-reg-high  r-high)
    (swap-byte! atom-reg-low  r-low)))

(defn get-reg-af []
  (create-virtual-16-bit-reg (:reg @reg-a) (:reg @reg-f)))
(defn get-reg-hl []
  (create-virtual-16-bit-reg (:reg @reg-h) (:reg @reg-l)))
(defn get-reg-bc []
  (create-virtual-16-bit-reg (:reg @reg-b) (:reg @reg-c)))
(defn get-reg-de []
  (create-virtual-16-bit-reg (:reg @reg-d) (:reg @reg-e)))

(defn set-reg-af! [b16value]
  (set-16-bit-reg! b16value reg-a reg-f))
(defn set-reg-hl! [b16value]
  (set-16-bit-reg! b16value reg-h reg-l))
(defn set-reg-bc! [b16value]
  (set-16-bit-reg! b16value reg-b reg-c))
(defn set-reg-de! [b16value]
  (set-16-bit-reg! b16value reg-d reg-e))

(defn inc-program-counter! []
  (increment-register! reg-PC))

(defn get-reg [switch]
  (case switch
    :a (:reg @reg-a)
    :b (:reg @reg-b)
    :c (:reg @reg-c)
    :d (:reg @reg-d)
    :e (:reg @reg-e)
    :f (:reg @reg-f)
    :h (:reg @reg-h)
    :l (:reg @reg-l)
    :af (get-reg-af)
    :hl (get-reg-hl)
    :bc (get-reg-bc)
    :de (get-reg-de)
    :sp (:reg @reg-SP)
    :pc (:reg @reg-PC)
    (keyword (str "ErrorNotFound!" switch))))

(defn set-reg! [switch value]
  (case switch
    :a (swap-byte! reg-a value)
    :b (swap-byte! reg-b value)
    :c (swap-byte! reg-c value)
    :d (swap-byte! reg-d value)
    :e (swap-byte! reg-e value)
    :f (swap-byte! reg-f value)
    :h (swap-byte! reg-h value)
    :l (swap-byte! reg-l value)
    :af (set-reg-af! value) 
    :hl (set-reg-hl! value) 
    :bc (set-reg-bc! value) 
    :de (set-reg-de! value)
    :sp (swap-byte! reg-SP value)
    :pc (swap-byte! reg-PC value)
    (keyword (str "ErrorNotFound!" switch))))


(defn set-clear-flags!
  "Set flags to :none if no changes are made"
  [zero carry half-carry sub]
  (when (= zero :set)
    (set-zero-flag! reg-f))
  (when (= zero :clear)
    (clear-zero-flag! reg-f))
  (when (= carry :set)
    (set-carry-flag! reg-f))
  (when (= carry :clear)
    (clear-carry-flag! reg-f))
  (when (= half-carry :set)
    (set-half-carry-flag! reg-f))
  (when (= half-carry :clear)
    (clear-half-carry-flag! reg-f))
  (when (= sub :set)
    (set-subtraction-flag! reg-f))
  (when (= sub :clear)
    (clear-subtraction-flag! reg-f)))

(defn printAllRegistersAndFlags
  "Helper function. For debugging"
  []
  (let [a (get-reg :a)
        b (get-reg :b)
        c (get-reg :c)
        d (get-reg :d)
        e (get-reg :e)
        ]))