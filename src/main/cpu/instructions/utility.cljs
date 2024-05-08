(ns cpu.instructions.utility
  (:require [cpu.registers.core :as reg]))


(defn missing-instr-error [msg]
  (throw (new js/Error (str "Instruction " msg " Not Found!"))))

; i fixed the name of nth
(defn get-value-at-index [coll index]
  (nth coll index))

(defn read-byte [memory index]
  (get-value-at-index memory index))

(defn read-current-byte-pc [memory]
  (read-byte memory (reg/get-reg :pc)))

(defn read-next-byte-no-pc-inc [memory]
  (read-byte memory (+ (reg/get-reg :pc) 1)))

(defn read-next-byte-with-pc-inc! [memory]
  (reg/inc-program-counter!)
  (read-byte memory (reg/get-reg :pc)))

(defn is-zero [num]
  (= num 0))
(defn is-not-zero [num]
  (not= num 0))

(defn determine-size [target]
  (case target
    :hl :b16
    :af :b16
    :bc :b16
    :de :b16
    :sp :b16
    :pc :b16
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
  ( [op target flag value]
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

(defn NotYetImplemented [data]
  (throw (new js/Error "Instruction Not Yet Implemented!")))

(defn get-source-data! [source memory]
  (case source
    :n8 (read-next-byte-with-pc-inc! memory)
    :hl-pointer (read-byte memory (reg/get-reg :hl))
    (reg/get-reg source)))

(comment
  "Flags can have following values:
   :clear - set to zero
   :set - set to 1
   :none - do nothing
   :calc - determine value through calculation
   ")

(defn calc-and-set-flags!
  ([flags op target]
   (let [zero (determine-zero op target (:zero flags))
         carry (determine-carry op target (:carry flags))
         sub (:sub flags) ; sub will always be predetermined
         half-carry (determine-half-carry op target (:carry flags))]
     (reg/set-clear-flags! zero carry half-carry sub)))
  ([flags op target source]
   (let [zero (determine-zero op target  (:zero flags) source)
         carry (determine-carry op target  (:carry flags) source)
         sub (:sub flags) ; sub will always be predetermined
         half-carry (determine-half-carry op target  (:carry flags) source)]
     (reg/set-clear-flags! zero carry half-carry sub))))