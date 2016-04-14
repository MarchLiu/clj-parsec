(ns org.clojar.marsliu.clj-parsec.combinator
  (:use org.clojar.marsliu.clj-parsec.parsec)
  (:use org.clojar.marsliu.clj-parsec.atom))

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

(defn many [parser]
  "(many p) applies the parser p zero or more times. Returns a list 
of the returned values of p."
  (fn [data]
    (loop [results '(), data data, continue? true]
      (if continue?
        (let [[res residue success?]
              ((try-pipe parser) results data)]
          (recur res residue success?))
        [(reverse results) data]))))

(defn many1 [parser]
  "(many1 p) applies the parser p one or more times. Returns a list 
of the returned values of p."
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

(defn try-then 
  "try-then create a function, accept data and try to parse the 
data use the parser. Return [residue true] if parsed. If 
failed, just return [inputs false]. It is helper for some 
  combinator not need save result that like skip and skip1."
  [parser]
  (fn [results data]
    (try
      (let [[result residue] (parser data)]
        [(cons result results) residue true])
      (catch Exception e
        [results data false]))))

(defn skip
  "skip create a parser, just use parser consume the data and 
without results. It return [nil residue]."
  [parser]
  (fn [data]
    (let [p (try-then parser)]
      (loop [data data]
        (let [[_ residue success?] (try-then data)]
          (if success? (recur residue)
              [nil residue]))))))

(defn skip1
  "skip create a parser, just use parser consume the data and 
without results. The parser must success once or more.It 
return [nil residue]."
  [parser]
  (fn [data]
    (parser data)
    (let [p (try-then parser)]
      (loop [data data]
        (let [[_ residue success?] (try-then data)]
          (if success? (recur residue)
              [nil residue]))))))

(defn bind
  "if parser success, pass the result into binder and get a new parser,
pass residue data into it and return [result residue]."
  [parser binder]
  (fn [data]
    (let [[result state] (parser data)
          action (binder result)]
      (action state))))

(defn then
  "if parser x success, parse the residue party into y and return 
[result residue]."
  [x y]
  (fn [data]
    (let [[_ state] (x data)]
      (y state))))

(defn over
  "if x parser success then call y, if y success, return x's result 
and y's residue, else throw error."
  [x y]
  (fn [data]
    (let [[result state] (x data)
          [_ residue] (y data)]
      [result residue])))

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
          ((choice (first parsers) (rest parsers)) data))))))

(defn between
  "(between open close p)  parses open, followed by p and close. 
Returns the value returned by p."
  [open close p]
  (fn [data]
    (then open (over p close))))
