(ns org.clojars.marsliu.clj-parsec.combinator
  (:use [org.clojars.marsliu.clj-parsec.parsec :refer [>> bind then jump]]
        [org.clojars.marsliu.clj-parsec.atom :refer [one return]]))

(defn try-pipe 
  "try-pipe create a function, accept a results seq and a data,
try to parse the data use the parser. Then append result to results 
and return [results residue success?]. If failed, just return 
inputs and false. It is helper for seq combinators as many and 
  many1."
  [parser]
  (fn [results data]
    (try
      (let [[result residue] (parser data)]
        [(cons result results) residue true])
      (catch Exception e
        [results data false]))))

(defn many 
  "(many p) applies the parser p zero or more times. Returns a list 
  of the returned values of p."
  [parser]
  (fn [data]
    (loop [results '(), data data, continue? true]
      (if continue?
        (let [[res residue success?]
              ((try-pipe parser) results data)]
          (recur res residue success?))
        [(reverse results) data]))))

(defn many1 
  "(many1 p) applies the parser p one or more times. Returns a list 
  of the returned values of p."
  [parser]
  (fn [data]
    (let [[result residue] (parser data)]
      (loop [results (list result),
             data residue,
             continue? true]
        (if continue?
          (let [[res residue success?]
                ((try-pipe parser) results data)]
            (recur res residue success?))
          [(reverse results) data])))))

(defn find-first
  "(find-first p) try p on data, return the result and residue, or skip one and try next,
until eof."
  [parser]
  (fn [data]
    (loop [[[result] residue success?] ((try-pipe parser) [] data)]
      (if success?
        [result residue]
        (let [[_ res] (one residue)]
          (recur ((try-pipe parser) [] res)))))))

(defn try-then 
  "try-then create a function, accept data and try to parse the 
data use the parser. Return [residue true] if parsed. If 
failed, just return [inputs false]. It is helper for some 
  combinator not need save result that like skip and skip1."
  [parser]
  (fn [data]
    (try
      (let [[result residue] (parser data)]
        [residue true])
      (catch Exception e
        [data false]))))

(defn skip
  "skip create a parser, just use parser consume the data and 
without results. It return [nil residue]."
  [parser]
  (fn [data]
    (let [p (try-then parser)]
      (loop [data data]
        (let [[residue success?] (p data)]
          (if success? (recur residue)
              [nil residue]))))))

(defn skip1
  "skip create a parser, just use parser consume the data and 
without results. The parser must success once or more.It 
return [nil residue]."
  [parser]
  (fn [data]
    (parser data)
    ((skip parser) data)))

(defn sepBy
  "(sepBy p sep) parses zero or more occurrences of p, separated 
by sep. Returns a list of values returned by p."
  [p sep]
  (fn [data]
    (try
      (let [[result state] (p data)
            [results residue] ((many (then sep p)) state)]
        [(cons result results) residue])
      (catch Exception e
        [[] data]))))

(defn sepBy1
  "(sepBy1 p sep) parses one or more occurrences of p, separated by 
sep. Returns a list of values returned by p."
  [p sep]
  (fn [data]
    (let [[result state] (p data)
          [results residue] ((many (then sep p)) state)]
      [(cons result results) residue])))

(defn choice
  "(choice & ps) tries to apply the parsers in the list ps in order, 
until one of them succeeds. Returns the value of the succeeding 
parser."
  [parser & parsers]
  (fn [data]
    (if (empty? parsers)
      (parser data)
      (try
        (parser data)
        (catch Exception e
          ((apply choice parsers) data))))))

(defn option
  "(option parser value) tries to apply the parser to data and return [result residue].
And return [value data] if parser failed."
  [parser value]
  (fn [data]
    (try
      (parser data)
      (catch Exception e
        [value data]))))


(defn success?
  "(success? parser) just  try the parser and return true/false and the residue."
  [parser]
  (fn [data]
    (try
      (let [[_ residue] (parser data)]
        [true residue])
      (catch Exception e
        [false data]))))

(defn many-till
  "manyTill p end applies parser p zero or more times until parser end succeeds. Returns 
  the list of values returned by p. "
  [parser end]
  (fn [data]
    (loop [res '() state data]
      (let [[ok? residue] ((success? end) state)]
        (if ok?
          [(reverse res) residue]
          (let [[result tail] (parser state)]
            (recur (cons result res) tail)))))))

(defn between
  "(between open close p)  parses open, followed by p and close. 
Returns the value returned by p. If only (between open close), 
just return (then open (many-till p close))"
  ([open close p]
   (fn [data]
     ((then open (jump p close)) data)))
  ([open close]
   (fn [data]
     (then open (many-till one close)))))
