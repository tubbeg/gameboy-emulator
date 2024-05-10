(ns cpu.registers.flags.core)


(defn is-carry? [result-calc]
  )

(defn is-half-carry? [result-calc])



(defn set-flags-acc
  "Sets flags based on calculation in 8 bit accumulator.
   Returns {:A (byte 0x01) :B (byte 0x0FF) ...}"
  [registers  entry ]
  (println "NOT YET IMPLEMENTED!!")
  registers)

(defn set-flags-16bit-HL
  "Returns {:A (byte 0x01) :B (byte 0x0FF) ...}"
  [registers  entry ]
  (println "NOT YET IMPLEMENTED!!")
   registers)

(defn set-flags-16bit-SP
  "Returns {:A (byte 0x01) :B (byte 0x0FF) ...}"
  [registers  entry]
  (println "NOT YET IMPLEMENTED!!")
   registers)