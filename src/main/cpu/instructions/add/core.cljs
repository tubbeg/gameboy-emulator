(ns cpu.instructions.add.core)




(comment
  "Types of add:
   add A reg8
   add A n8
   add A HL-pointer
   add HL reg16
   "
  )



(defn is-reg8 [])
(defn is-n8 [])
(defn is-HL-pointer [])
(defn is-reg16 [])

(defn add-instruction 
  "Returns {:memory ... :registers ... :halt false}"
  [entry memory registers]
  (let [halt false]
    ))