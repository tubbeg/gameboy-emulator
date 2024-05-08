(ns emulator.core
  (:require [clojure.math :as m]
            [cpu.registers.core :refer [reg-PC]]))

(comment
  "Disregard audio for now-
   
   Just make a (simple) game work.
   
   Start with the CPU. Make sure that it works.
   It should be similar to the Z80.
   Test it using assembly.

   1. Functional CPU

   2. Loading Cartridge into Memory

   3. Functional GPU

   4. Bonus
   
   ")

(def begin-read-rom (byte 0xFF)) ;wrong
(def begin-work-ram (byte 0xC000))
(def end-work-ram (byte 0xCFFF))
;(def begin-ram-adr (byte 0x0000))

(defn init-fn []
  (println "hello emulator"))

(def clock-frequency
  (as-> (m/pow 10 6) mhz
    (float 4.19)
    (* mhz)))

(def memory-init
  (->
   (for [i (range 0 (byte 0xFFFF))]
     (byte 0x00))
   (into [])))

(def memory
  (atom {:mem memory-init}))



(println clock-frequency)

(defn set-to-next-instruction-pointer! [])

(defn decode-reg-PC [program-counter])

(defn fetch-data! [memory-access-mode])

(defn execute-instruction! [])

(def abort-flag (atom {:abort false}))

(defn cpu-main-loop []
  (when (not ((:abort @abort-flag)))
    (do
      (set-to-next-instruction-pointer!)
      (fetch-data! @reg-PC)
      (execute-instruction!)))
  :done)



(comment
  "Info on the Gameboy:
   
   Sharp LR35902 System-On-Chip
   
   Sharp SM83 8-bit processor

   Similar instruction set to the Z80 processor,
   but not identical.

   4.19 MHz clock

   Work RAM 4KB - GameBoy (DMG) 
   Work RAM 32KB - GameBoy (CGB) 


   ")