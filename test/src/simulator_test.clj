(ns simulator-test
  (:require [clojure.test :refer [is deftest]]
            [clojure.java.shell :refer [sh]]
            [clojure.string :as string]))

(def test-files
  [["listing_0043_immediate_movs"]
   ["listing_0044_register_movs"]
   ["listing_0045_challenge_register_movs"]
   ["listing_0046_add_sub_cmp"]
   ["listing_0047_challenge_flags"]
   ["listing_0048_ip_register" "print-ip"]
   ["listing_0049_conditional_jumps" "print-ip"]
   ["listing_0050_challenge_jumps" "print-ip"]
   ["listing_0051_memory_mov" "print-ip"]
   ["listing_0052_memory_add_loop" "print-ip"]
   ["listing_0053_add_loop_challenge" "print-ip"]
   ["listing_0054_draw_rectangle" "print-ip" "dump"]
   ["listing_0055_challenge_rectangle" "print-ip" "dump"]])

(deftest simulator-test
  (doseq [[filename print-ip? dump?] test-files]
    (println "Simulating" filename)
    (time (->> (sh "clj" "-m" "simulator" filename (str print-ip?) (str dump?))
               :out
               (spit (str "test/resources/" filename ".txt"))))
    (let [expected (map #(-> % (string/split #";") last (string/trim)) (rest (filter not-empty (string/split-lines (slurp (str "resources/" filename ".txt"))))))
          actual   (map #(-> % (string/split #";") last (string/trim)) (rest (filter not-empty (string/split-lines (slurp (str "test/resources/" filename ".txt"))))))]
      (is (= expected actual)
          (apply str "The result in " filename " is wrong\n"
                 (map #(str (when (not= %1 %2) "DIFF") %1 "|" %2 "\n") expected actual))))))

