(ns cpu.decoder.core
  (:require [cpu.registers.core :as reg]
            [cpu.decoder.opcodes :as codes]))

(comment
  "Flags can have following values:
   :clear - set to zero
   :set - set to 1
   :none - do nothing
   :calc - determine value through calculation
   "
  )


(comment
  
  "IMPORTANT!!!
   
   ALU instructions (ADD, ADC, SUB, SBC, AND, XOR, OR, and CP) can be written with the left-hand side A omitted.
Thus for example ADD A, B has the alternative mnemonic ADD B, and CP A, $F has the alternative mnemonic CP $F. 
   "
  )


(defn convert-to-hex-key [num]
  (let [h (reg/get-higher-nibble num)
        l (reg/get-lower-nibble num)
        name (str "hex" h l)]
    (keyword name)))

(defn get-instruction [num]
  (let [res (-> (convert-to-hex-key num)
                (codes/opcodes))]
    (if (= res nil)
      :ErrorNotFound!
      res)))

(get-instruction (byte 0x86))