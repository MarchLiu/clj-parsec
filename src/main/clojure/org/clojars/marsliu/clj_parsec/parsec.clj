(ns org.clojars.marsliu.clj-parsec.parsec
  (:use [org.clojars.marsliu.clj-parsec.atom :refer [return]]))

(defmacro >>
  "Threads the state through the parsers. Inserts state as the
  parameter in the first parser. If there are more parsers,
  inserts the first residue as the second state in second parser, etc.
  If a parser throw exception, throw it and exit, else return last result
  and residue state.
    "
  [data & forms]
  (loop [arg data, forms forms]
    (let [form (first forms), next-forms (next forms)
          tail (with-meta (list form arg) (meta form))]
      (if (nil? next-forms) tail
          (recur (list second tail) next-forms)))))

(defmacro >>=
  "Threads the state through the parsers. Inserts state as the
  parameter in the first parser. If there are more parsers,
  inserts the first residue as the second state in second parser, etc.
  If a parser throw exception, throw it and exit, else return all bind result
  as a dictionay and residue data.
  if it is form as [:key-name parsec], save the result into 
  result dictionay.
    "
  [data & forms]
  (loop [data (list (list return {}) data), forms forms]
    (if forms
      (let [form (first forms)
            bind? (and (vector? form) (keyword? (first form)))
            func (if bind? (second form) form)
            tail (if bind?
                   (let [key (first form)]
                     `(let [[results# data#] ~data,
                            [result# residue#] (~func data#)]
                        (list (assoc results# ~key result#)
                              residue#)))
                   `(let [[results# data#] ~data,
                          [result# residue#] (~func data#)]
                      (list results# residue#)))]
        (recur (with-meta tail (meta func)) (next forms)))
      data)))

(defn parse
  "A combinator just like =>>. It just parse everyone, and
  pass the result dictionary and residue data to rest form. 
  if it is form as [:key-name parsec], save the result into 
  result dictionay."
  [& forms]
  (fn [data]
    (let [step (fn [state form]
                 (let [bind? (and (vector? form)
                                  (keyword? (first form)))
                       [results data] state,
                       func (if bind? (second form) form)
                       [result residue] (func data)]
                   (if bind?
                     (list (assoc results (first form) result)
                           residue)
                     (list results residue))))]
      (reduce step [{} data] forms))))

(defn chain
  "A combinator pipe. It just parse everyone, and
  pass the result and residue data to rest form. 
  It save the result into result vector."
  [& forms]
  (fn [data]
    (let [step (fn [state form]
                 (let [[results data] state,
                       [result residue] (form data)]
                   [(cons result results) residue]))]
      (reduce step [[] data] forms))))


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

(defn jump
  "if x parser success then call y, if y success, return x's result 
and y's residue, else throw error."
  [x y]
  (fn [data]
    (let [[result state] (x data)
          [_ residue] (y state)]
      [result residue])))

