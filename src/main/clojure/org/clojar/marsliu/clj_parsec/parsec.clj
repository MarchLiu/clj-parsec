(ns org.clojar.marsliu.clj-parsec.parsec
  (:use org.clojar.marsliu.clj-parsec.atom))

(defmacro >>
  "Threads the state through the parsers. Inserts state as the
  parameter in the first parser. If there are more parsers,
  inserts the first residue as the second state in second parser, etc.
  If a parser throw exception, throw it and exit, else return last result
  and residue state.
    "
  [data & forms]
  (loop [data data, forms forms]
    (let [form (first forms), next-forms (next forms)
          tail (list form data)]
      (if (nil? next-forms) tail
          (recur (list second tail) next-forms)))))

(defmacro =>>
  "Threads the state through the parsers. Inserts state as the
  parameter in the first parser. If there are more parsers,
  inserts the first residue as the second state in second parser, etc.
  If a parser throw exception, throw it and exit, else return all bind result
  as a dictionay and residue data.
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
        (recur tail (next forms)))
      data)))

(defn parse
  "A combinator just like =>>. It just parse everyone, and
    pass the residue data to rest form. if it has a meta as {:result :key-name},
    save the result to "
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
