(ns org.clojar.marsliu.clj-parsec.text
  (:use org.clojar.marsliu.clj-parsec.parsec)
  (:use org.clojar.marsliu.clj-parsec.atom))

(defn char
  "(ch c) parses a single character c. Returns the parsed character (i.e. c).
  semiColon = char ’;’"
  [^Character c]
  (fn [data]
    (let [[head residue] (one data)]
      (if (= c head) [head residue]
          (->> (format "Expect char %s but get %s." c head)
               IllegalStateException. throw)))))

(defn string
  "(string s) parses a sequence of characters given by s. Returns the parsed 
string (i.e. s)."
  [^String s]
  (fn [data]
    (loop [chars (vec s) state data]
      (if (empty? chars)
        [s state]
        (let [c (first chars), [_ residue] ((char c) state)]
          (recur (rest chars) residue))))))

(defn digit
  "digit parser Parses a digit. Returns the parsed character."
  [data]
  (let [[result residue] (one data)]
    (if (Character/isDigit result)
      [result residue]
      (->> (format "Except a digit char but get %s" result)
           IllegalStateException. throw))))


(defn any-char
  [^String chars]
  (fn [data]
    (let [[result residue] (one data)]
      (loop [item (first chars) items (next chars)]
        (if items
          (if (= item result)
            [result residue]
            (recur (first items) (next items)))
          (->> (format "Except char in %s but get %s" chars result)
               IllegalStateException. throw))))))
