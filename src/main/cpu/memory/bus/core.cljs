(ns cpu.memory.bus.core)

; i fixed the name of nth
(defn get-value-at-index [coll index]
  (nth coll index))


(defn error-msg [& args]
  (println "ERROR! NOT FOUND MEMORY" args)
  :ERROR-MEMORY-BUS)


(defn create-adress [num]
  (->> num
      (str "adress")
      (keyword)))

(defn error-memory [& args]
  (println "ERROR IN MEMORY: " args)
  :ERROR-MEMORY)

(defn read-byte [memory _adress] 
  (let [max-size (:size memory)
        def (:init memory)
        storage (:data memory)
        data (->  (-> _adress
                      (create-adress)
                      (storage)))]
    (cond
      (= storage nil) (error-memory :MISSING-DATA-NIL)
      (> _adress max-size) (error-memory :MAX-SIZE)
      (= nil data) def
      :else data)))

(defn create-memory [size]
  {:size size
   :init 0
   :data {}})

(defn set-data-in-memory
  "Returns memory"
  [_adress memory data]
  (let [max-size (:size memory)
        storage (:data memory)
        set-addr (create-adress _adress) 
        updated-storage (assoc storage set-addr data)
        updated-memory (assoc memory :data updated-storage)] 
    (cond
      (= storage nil) (error-memory :MISSING-DATA-NIL)
      (> _adress max-size) (error-memory :MAX-SIZE) 
      :else updated-memory)))
