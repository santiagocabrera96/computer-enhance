(ns simulator-test
  (:require [clojure.test :refer [is deftest]]
            [clojure.java.shell :refer [sh]]
            [clojure.string :as string]))

(def test-files
  ["listing_0043_immediate_movs"
   "listing_0044_register_movs"
   "listing_0045_challenge_register_movs"])

(deftest simulator-test
  (doseq [filename test-files]
    (->> (sh "clj" "-m" "simulator" filename)
         :out
         (spit (str "test/resources/" filename ".txt")))
    (let [expected (rest (string/split-lines (slurp (str "resources/" filename ".txt"))))
          actual   (rest (string/split-lines (slurp (str "test/resources/" filename ".txt"))))]
      (is (= expected actual)
          (str "The result in " filename " is missing instructions")))))

