(ns org.clojars.marsliu.clj-parsec.text-test
  (:require [clojure.string :only join :as s])
  (:use clojure.test)
  (:require [org.clojars.marsliu.clj-parsec.text :as t]))

(deftest test-ch-0
  "test ch parser 0"
  (let [data "It is a sample string"
        helper (fn [state]
                 (let [ch (first state)
                       [result residue] ((t/char ch) state)]
                   (is (= result (first state))
                       (format "Expect char %s but get %s." ch result))
                   (if (not (empty? residue))
                     (recur residue))))]
    (helper data)))

(deftest test-str-0
  "test str parser 0"
  (let [data "It is a sample string"]
    (let [[word residue] ((t/string "It") data)]
      (is (= word "It"))
      (is (= (s/join residue) " is a sample string")))
    (let [[word residue] ((t/string "It ") data)]
      (is (= word "It "))
      (is (= (s/join residue) "is a sample string")))))

(deftest test-any-char-0
  "test any-char parser 0"
  (let [data "It is a sample string", [ch residue] ((t/any-char "It") data)]
    (is (= ch \I))))

(deftest test-space
  "test space parser"
  (let [data " There is a space at start."
        [ch residue] (t/space data)]
    (is (= ch \space)
        (= residue (rest data)))))

(deftest test-spaces
  "test spaces parser"
  (let [data "     There are spaces at start."
        [chars residue] (t/spaces data)]
    (is (= (s/join residue) "There are spaces at start."))))