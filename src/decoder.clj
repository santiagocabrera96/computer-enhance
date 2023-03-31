(ns decoder
  (:require [clojure.java.io :as io]
            [clojure.math :refer [pow]])
  (:import [java.io ByteArrayOutputStream]))

(slurp (io/resource "listing_0037_single_register_mov"))

(defn mask [n]
  (dec (bit-shift-left 1 n)))

(defn print-byte
  ([b] (print-byte b 8))
  ([b n]
   (Integer/toBinaryString (bit-and b (mask n)))))

(defn binary [filename]
  (with-open [in  (clojure.java.io/input-stream (io/resource filename))
              out (ByteArrayOutputStream.)]
    (clojure.java.io/copy in out)
    (vec (.toByteArray out))))

(def operations {2r100010 'mov})
(defn operation [b]
  (-> b
      (unsigned-bit-shift-right 2)
      (bit-and 2r111111)
      operations))

(defn direction-to-reg? [b] (bit-test b 1))
(defn word? [b] (bit-test b 0))

(def modes
  {2r00 :no-displacement
   2r01 :8-bit-displacement
   2r10 :16-bit-displacement
   2r11 :register-mode})
(defn mode [b]
  (-> b
      (bit-and 0xff)
      (unsigned-bit-shift-right 6)
      modes))

(def regs
  {2r000 ['al 'ax]
   2r001 ['cl 'cx]
   2r010 ['dl 'dx]
   2r011 ['bl 'bx]
   2r100 ['ah 'sp]
   2r101 ['ch 'bp]
   2r110 ['dh 'si]
   2r111 ['bh 'di]})

(defn reg [b w]
  (let [idx (if w 1 0)]
    (-> b
        (unsigned-bit-shift-right 3)
        (bit-and 2r111)
        regs
        (nth idx)
        )))

(defn r-m [b memory-mode word?]
  (let [rm          (bit-and b 2r111)
        idx         (if word? 1 0)]
    (-> rm
        regs
        (nth idx))))

(defn decode-file [filename]
  (println "bits 16")
  (loop [by (binary filename)]
    (let [byte1 (first by)
          byte2 (second by)
          op    (operation byte1)
          d     (direction-to-reg? byte1)
          w     (word? byte1)
          mod   (mode byte2)
          reg   (reg byte2 w)
          r-m   (r-m byte2 mod w)]
      (if d
        (println (str op " " reg ", " r-m))
        (println (str op " " r-m ", " reg)))
      (when-not (empty? (rest (rest by)))
        (recur (rest (rest by)))))))

(decode-file "listing_0038_many_register_mov")
