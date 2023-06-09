(ns decoder-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.java.shell :refer [sh]]
            [clojure.string :as string]
            [decoder]))


(def test-files
  ["listing_0037_single_register_mov"
   "listing_0038_many_register_mov"
   "listing_0039_more_movs"
   "listing_0040_challenge_movs"
   "listing_0041_add_sub_cmp_jnz"
   "listing_0042_completionist_decode"
   "listing_0043_immediate_movs"
   "listing_0044_register_movs"
   "listing_0045_challenge_register_movs"
   "listing_0046_add_sub_cmp"
   "listing_0047_challenge_flags"
   "listing_0048_ip_register"
   "listing_0049_conditional_jumps"
   "listing_0050_challenge_jumps"
   "listing_0051_memory_mov"
   "listing_0052_memory_add_loop"
   "listing_0053_add_loop_challenge"
   "listing_0054_draw_rectangle"
   "listing_0055_challenge_rectangle"])

(deftest decoder-test
  (doseq [filename test-files]
    (println "Decoding" filename)
    (time
     (->> (with-out-str (decoder/-main (str "resources/" filename)))
          (spit (str "test/resources/" filename ".asm"))))
    (sh "nasm" (str "test/resources/" filename ".asm"))
    (let [expected (slurp (str "resources/" filename))
          actual   (slurp (str "test/resources/" filename))]
      (is (string/starts-with? expected actual)
          (str "The file " filename " does not produce the expected binary"))
      (is (= expected actual)
          (str "The result in " filename " is missing instructions")))))
