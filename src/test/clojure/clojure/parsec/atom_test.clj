(ns clojure.parsec.atom-test
  (:use clojure.test)
  (:use clojure.parsec.atom))

(deftest test-one
  "test one parser"
  (let [data "It is a sample string"
        helper (fn [state]
                      (let [[result residue] (one state)]
                        (is (= result (first state))
                            "Expect one just as car.")
                        (if (not (empty? residue))
                          (recur residue))))]
    (helper data)))

(deftest test-eof0
  "test eof at empty"
  (let [[data _] (eof '())]
    (is (nil? data)
        (format "Expect eof success on a empty collection but %s"
                data))))
