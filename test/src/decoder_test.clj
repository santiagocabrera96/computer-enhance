(ns decoder-test
  (:require [clojure.test :refer :all]
            [decoder :refer [-main]])
  (:use [clojure.java.shell :only [sh]]))


(def test-files
  ["listing_0037_single_register_mov"
   "listing_0038_many_register_mov"
   "listing_0039_more_movs"
   "listing_0040_challenge_movs"
   "listing_0041_add_sub_cmp_jnz"
   "listing_0042_completionist_decode"])

(deftest decoder-test
  (doseq [filename test-files]
    (->> (sh "clj" "-m" "decoder" filename)
     :out
     (spit (str "test/resources/" filename ".asm")))
    (sh "nasm" (str "test/resources/" filename ".asm"))
    (let [expected (slurp (str "resources/" filename))
          actual   (slurp (str "test/resources/" filename))]
      (is (clojure.string/starts-with?
           expected
           actual)
          (str "The file " filename " does not produce the expected binary"))
      (is (= expected actual)
          (str "The result in " filename " is missing instructions")))))
