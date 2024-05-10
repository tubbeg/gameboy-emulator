(ns cpu.instructions.misc.core)


(defn halt 
  "Returns {:memory ... :registers ... :halt true}"
  [entry memory registers]
  {:memory memory :registers registers :halt true})