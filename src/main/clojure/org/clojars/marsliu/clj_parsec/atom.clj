(ns org.clojars.marsliu.clj-parsec.atom)

(defn one [data]
  (if (nil? data) (throw (IllegalStateException. "eof")))
  (if (not (seq data))
    (throw (IllegalArgumentException. "need a sequence")))
  (if (not (empty? data))
    [(first data) (rest data)]
    (throw (IllegalStateException. "eof"))))

(defn eof [data]
  (if (nil? data) [nil '()])
  (if (not (empty? data))
      (throw (IllegalStateException.
              (format "Expect end of flow but %s." data)))
      [nil '()]))

(defn by-pred [pred]
  (fn [data]
    (let [head (one data)]
      (if (pred head) [head (rest data)]
          (->> (format "Predicate %s failed for %s." pred, head)
               IllegalStateException. throw)))))

(defn eq [value]
  (fn [data]
    (let [head (one data)]
      (if (= value head) [head (rest data)]
          (->> (format "Expect %s equal to %s." head value)
               IllegalStateException. throw)))))

(defn neq [value]
  (fn [data]
    (let [head (one data)]
      (if (= value head)
        (->> (format "Expect %s not equal to %s." head value)
             IllegalStateException. throw)
        [head (rest data)]))))

(defn one-of [values]
  (fn [data]
    (let [[item residue] (one data)]
      (loop [values values]
        (if values
          (let [value (first values)]
            (if (= value item)
              [item residue]
              (recur (next values))))
          (->> (format "Except one of %s but %s" values item)
               IllegalStateException. throw))))))

(defn none-of [values]
  (fn [data]
    (let [[item residue] (one data)]
      (loop [values values]
        (if values
          (let [value (first values)]
            (if (= value item)
              (->> (format "Except none of %s but %s" values item)
                   IllegalStateException. throw)
              (recur (next values))))
          [item residue])))))

(defn return [value]
  (fn [data] [value data]))

(defn fail [^String message]
  (fn [data] (throw (IllegalStateException. message))))
