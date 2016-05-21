(ns org.clojars.marsliu.clj-parsec.text
  (:use [org.clojars.marsliu.clj-parsec.atom :refer [one]]
        [org.clojars.marsliu.clj-parsec.combinator :refer [skip many1]]
        [clojure.string :refer [join]]))

(defn char
  "(char c) parses a single character c. Returns the parsed character (i.e. c).
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
  "This parser succeeds for any character. Returns the parsed character."
  [^String chars]
  (fn [data]
    (let [[result residue] (one data)]
      (loop [item (first chars) items (next chars)]
        (if item
          (if (= item result)
            [result residue]
            (recur (first items) (next items)))
          (->> (format "Except char in %s but get %s" chars result)
               IllegalStateException. throw))))))

(defn uint
  "This parser parses a string of digits sequence. The result should be parse a integer."
  [data]
  (let [[result residue] ((many1 digit) data)]
    [(join result) residue]))

(defn space
  "Parses a white space character. Returns the parsed character."
  [data]
  (let [[result residue] (one data)]
    (if (Character/isWhitespace result) [result residue]
        (->> (format "Except spaces but get %s " result)
             IllegalStateException. throw))))

(defn newline
  "Parses a newline character (’\n’). Returns a newline character."
  [data]
  ((char \newline) data))


(defn spaces
  "Skips zero or more white space characters. See also skip."
  [data]
  ((skip space) data))
