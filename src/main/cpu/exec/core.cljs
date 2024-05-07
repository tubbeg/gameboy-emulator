(ns cpu.exec.core
  (:require [cpu.registers.core :refer [reg-PC
                                        increment-register!]]))





(defn inc-program-counter! []
  (increment-register! reg-PC))

(defn read-current-byte [memory atom-reg-pc]
  (nth memory @atom-reg-pc)) ; nth needs a better name honestly

(defn read-and-increment! [memory atom-reg-pc]
  (let [_byte (read-current-byte memory atom-reg-pc)]
    (inc-program-counter!)
    _byte))



