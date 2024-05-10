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

(def adress :adress)


(defn read-byte [memory _adress] ;index should be 16-bit
  (-> (filter #(= (adress %) _adress) memory)
      (error-check)))

(defn create-data [data _adress]
  {adress _adress :data data})

(defn create-memory [size]
  (for [i (range size)]
    (create-data 0 i)))

(defn set-data-in-memory [_adress memory data]
  (map #(case (adress %)
          _adress (create-data _adress data)
          %) memory))