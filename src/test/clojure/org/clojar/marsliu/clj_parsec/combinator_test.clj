(ns org.clojar.marsliu.clj-parsec.combinator-test
  (:use clojure.test)
  (:use org.clojar.marsliu.clj-parsec.parsec)
  (:use org.clojar.marsliu.clj-parsec.atom)
  (:use org.clojar.marsliu.clj-parsec.combinator))

(deftest test-many-0
  "test many combinator 0"
  (are [data result residue] (= ((many one) data) [result residue])
    "abc" '(\a \b \c) '()
    "abcd" '(\a \b \c \d) '()
    "a" '(\a) '()
    "" '() ""))

(deftest test-many1-0
  "test many1 combinator 0"
  (are [data result residue] (= ((many1 one) data) [result residue])
    "abc" '(\a \b \c) '()
    "abcd" '(\a \b \c \d) '()
    "a" '(\a) '()))
