(ns script
  (:require [clojure.spec.alpha :as spec]
            [clojure.spec.test.alpha :as test]
            [clojure.string :as str]
            [ubergraph.core :as uber]))

(comment
  (defn my-index-of
    [source search]
    (str/index-of source search))

  (my-index-of "foobar" "b")
  (apply my-index-of ["foobar" "b"])

  (spec/def ::index-of-args
    (spec/cat :source string? :search string?))

  (spec/valid? ::index-of-args ["foobar" "b"])
  (spec/valid? ::index-of-args ["foobar" 2])

  (spec/conform ::index-of-args ["foo" "f"])
  (spec/unform ::index-of-args {:source "foo" :search "f"})
  (spec/explain-str ::index-of-args ["foo" 3])

  (spec/explain-str (spec/every ::index-of-args) [["good" "a"]
                                                  ["ok" "b"]
                                                  ["bad" 42]])

  (spec/check-asserts true)
  (spec/assert ::index-of-args ["foo" 2])




  (def x ""))