(ns cpu.instructions.add.core
  (:require [cpu.instructions.utility :as ut]
            [cpu.registers.core :as reg]))

(defn add-from-source-reg
  "Flags are Z 0 H C. 
   Function covers 0x80 -> 0x87 + 0xC6.
   Set value to :none if source is register"
  [value memory source]
  (letfn [(getSource [source]
            (case source
              :n8 value
              :hl-pointer (ut/read-byte memory (reg/get-reg :hl))
              (reg/get-reg source)))]
    (let [a (reg/get-reg :a)
          src (getSource source)
          res  (+ a src)
          sub :clear
          zero (ut/determine-zero res)
          half-carry (ut/determine-half-carry a src)
          carry (ut/determine-carry a src)]
      (reg/set-clear-flags! zero carry half-carry sub)
      (reg/set-reg! :a res))))

(defn add-to-sp
  "Flags are 0 0 H C. 
   Function covers 0xE8"
  [signed-int]
  (let [sp (reg/get-reg :sp)
        res  (+ sp signed-int)
        zero :clear
        sub :clear
        half-carry (ut/determine-half-carry sp signed-int)
        carry (ut/determine-carry sp signed-int)]
    (reg/set-clear-flags! zero carry half-carry sub)
    (reg/set-reg! :sp res)))

(defn add-to-hl
  "Flags are - 0 H C.
   Function covers 0x09 -> 0x39
   16 bit"
  [virtual-16-bit-reg]
  (let [target (reg/get-reg :hl)
        value (reg/get-reg virtual-16-bit-reg)
        zero :none
        sub :clear
        half-carry (ut/determine-half-carry target value)
        carry (ut/determine-carry target value)]
    (reg/set-clear-flags! zero carry half-carry sub)))


(defn add [data memory]
  (let [op (:op data)
        flags (:flags data) ; not required since all flags are known in advance
        target (:target data)
        source (:source data)]
    (if (not= op :add)
      (println "Error! Incorrect instruction!")
      (case target
        :a (-> (ut/read-next-byte memory) ; might need to fix this
               (add-from-source-reg memory source))
        :hl (add-to-hl source)
        :sp (-> (ut/read-next-byte memory) ; might need to fix this  
                (add-to-sp))
        (keyword (str "Error! Not Found!" target))))))
