(ns cpu.decoder.core)


(def hex-instructions
  {:hex00 {:op :nop :flags [] :byte-length 1 :duration 4}
   ;----- ADD 8x ------
   :hex80 {:op :add :target :a :source :b :flags [:z :clear-n :c :h] :byte-length 1 :duration 4}
   :hex81 {:op :add :target :a :source :c :flags [:z :clear-n :c :h] :byte-length 1 :duration 4}
   :hex82 {:op :add :target :a :source :d :flags [:z :clear-n :c :h] :byte-length 1 :duration 4}
   :hex83 {:op :add :target :a :source :e :flags [:z :clear-n :c :h] :byte-length 1 :duration 4}
   :hex84 {:op :add :target :a :source :h :flags [:z :clear-n :c :h] :byte-length 1 :duration 4}
   :hex85 {:op :add :target :a :source :l :flags [:z :clear-n :c :h] :byte-length 1 :duration 4}
   :hex86 {:op :add :target :a :source :hl-pointer :flags [:z :clear-n :c :h] :byte-length 1 :duration 4}
   :hex87 {:op :add :target :a :source :a :flags [:z :clear-n :c :h] :byte-length 1 :duration 4}
   ;----- ADD continue ------
   :hexC6 {:op :add :target :a :source :n8 :flags [:z :clear-n :c :h] :byte-length 1 :duration 4}
   })

(defn get-lower-nibble [num]
  (bit-and (byte 0x0F) num))

(defn get-higher-nibble [num]
  (bit-shift-right num 4))

(defn convert-to-hex-key [num]
  (let [h (get-higher-nibble num)
        l (get-lower-nibble num)
        name (str "hex" h l)]
    (keyword name)))

(defn get-instruction [num]
  (let [res (-> (convert-to-hex-key num)
                (hex-instructions))]
    (if (= res nil)
      :ErrorNotFound!
      res)))

(get-instruction (byte 0x00))