(ns org.clojars.marsliu.clj-parsec.combinator-test
  (:use clojure.test
        [org.clojars.marsliu.clj-parsec.parsec :refer :all]
        [org.clojars.marsliu.clj-parsec.atom :refer [one]]
        [org.clojars.marsliu.clj-parsec.combinator :refer :all]
        [org.clojars.marsliu.clj-parsec.text :refer [char]]))

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

(deftest test-many-till-0
  "test many-till combinator"
  (are [data result residue] (= ((many-till one (char \.)) data) [result residue])
    "abc." '(\a \b \c) '()
    "abcd.efg" '(\a \b \c \d) '(\e \f \g)
    ".a" '() '(\a)))
