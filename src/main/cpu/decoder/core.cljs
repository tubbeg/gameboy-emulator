(ns cpu.decoder.core
  (:require [cpu.registers.core :as reg]
            [cpu.decoder.opcodes :as codes]))

(comment
  "Flags can have following values:
   :clear - set to zero
   :set - set to 1
   :none - do nothing
   :calc - determine value through calculation
   ")

(comment

  "IMPORTANT!!!
   
   ALU instructions (ADD, ADC, SUB, SBC, AND, XOR, OR, and CP) can be written with the left-hand side A omitted.
Thus for example ADD A, B has the alternative mnemonic ADD B, and CP A, $F has the alternative mnemonic CP $F. 
   
   
  QUESTION
   
   Does this affect the binary data? Or is it going to look the
   same? How does the CPU know otherwise if it says ADD A,B or ADD B?

   The assembler should (logically) assemble to the same result
   regardless of how it is written

   I think I need to test this in an actual assembler
   ")

(defn convert-to-key [num type]
  (let [h (-> num
              (reg/get-higher-nibble)
              (reg/translateNibbleToHexString))
        l (-> num
              (reg/get-lower-nibble)
              (reg/translateNibbleToHexString))
        name (str type h l)]
    (keyword name)))

(defn get-instruction [opcodes num type]
  (let [key (convert-to-key num type)
        res (key opcodes)]
    (if (= res nil)
      :ErrorNotFound!
      res)))

(defn get-hex-instruction [byte]
  (get-instruction codes/opcodes byte "hex"))

(defn get-prefix-instruction [byte]
  (get-instruction codes/opcodes byte "prefix"))

(byte 0x28)

(defn print-every-entry [opcodes]
  (let [keys (keys opcodes)]
    (doseq [k keys]
      (println "Key: " k)
      (println "Entry: " (k opcodes)))))

(get-hex-instruction (byte 0x86))
(byte 0x38)
;(get-prefix-instruction (byte 0xFF))