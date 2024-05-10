(ns cpu.memory.bus.core)

; i fixed the name of nth
(defn get-value-at-index [coll index]
  (nth coll index))


(defn error-msg [& args]
  (println "ERROR! NOT FOUND MEMORY" args)
  :ERROR-MEMORY-BUS)

(defn error-check [coll]
  (case coll
    nil (error-msg coll)
    (if (= (first coll) nil)
      (error-msg nil)
      (first coll))))

(defn read-byte [memory adress] ;index should be 16-bit
  (-> (filter #(= (:adress %) adress) memory)
      (error-check)))

(defn create-memory [size]
  (for [i (range size)]
    {:adress i :data 0}))

(defn set-data-in-memory [adress memory data]
  (map #(case (:adress %)
          adress {:adress adress :data data}
          %) memory))