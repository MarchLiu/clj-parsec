(ns org.clojars.marsliu.clj-parsec.parsec-test
  (:use clojure.test)
  (:use org.clojars.marsliu.clj-parsec.parsec)
  (:use org.clojars.marsliu.clj-parsec.atom))

(deftest test>>0
  "test >> micro 0"
  (let [[result residue] (>> "abc" one one one eof)]
    (are [x y] (= x y)
      result nil
      residue '())))

(deftest test>>1
  "test >> micro 1"
  (let [[result residue] (>> "abc" one one one)]
    (are [x y] (= x y)
      result \c
      residue '())))

(deftest test>>2
  "test >> micro 2"
  (let [[result residue] (>> "abc" one one)]
    (are [x y] (= x y)
      result \b
      residue '(\c))))

(deftest test>>3
  "test >> micro 3"
  (let [[result residue] (>> "abc" one)]
    (are [x y] (= x y)
      result \a
      residue '(\b \c))))

(deftest test>>=0
  "test >>= micro 0"
  (let [[results residue] (>>= "abc" one one one eof)]
    (are [x y] (= x y)
      results {}
      residue '())))

(deftest test>>=1
  "test >>= micro 1"
  (let [[results residue] (>>= "abc" one one one)]
    (are [x y] (= x y)
      results {}
      residue '())))

(deftest test>>=2
  "test >>= micro 2"
  (let [[results residue] (>>= "abc" [:a one] one)]
    (are [x y] (= x y)
      results {:a \a}
      residue '(\c))))

(deftest test>>=3
  "test >>= micro 3"
  (let [[results residue] (>>= "abc" [:a one] [:b one] one eof)]
    (are [x y] (= x y)
      results {:a \a :b \b}
      residue '())))

(deftest test-parse-0
  "test parse combinator 0"
  (are [data parser results residue]
    (= ((apply parse parser) data) [results residue])
    "abc" [one one] {} '(\c)
    "abc" [one one one] {} '()
    "abcd" [[:a one] one [:c one]] {:a \a :c \c} '(\d) ))
