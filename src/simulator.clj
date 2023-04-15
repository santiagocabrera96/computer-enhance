(ns simulator
  (:require [decoder :refer [decode-instruction
                             load-memory-from-file
                             print-instruction]]
            [clojure.data :refer [diff]]))

(def lo-registers
  {'al 'ax
   'bl 'bx
   'cl 'cx
   'dl 'dx})
(def hi-registers
  {'ah 'ax
   'bh 'bx
   'ch 'cx
   'dh 'dx})

(def initial-state
  {'ax 0
   'bx 0
   'cx 0
   'dx 0
   'sp 0
   'bp 0
   'si 0
   'di 0
   'es 0
   'cs 0
   'ss 0
   'ds 0})

(defn get-register [state register]
  (cond (contains? state register) (get state register)
        (lo-registers register) (bit-and (get state (lo-registers register)) 0xff)
        (hi-registers register) (bit-shift-right (get state (hi-registers register)) 8)))

(defn set-register [state register value]
  (cond (contains? state register) (assoc state register value)
        (lo-registers register)
        (assoc state (lo-registers register) (bit-or (bit-and (get state (lo-registers register)) 0xff00)
                                                     value))
        (hi-registers register)
        (assoc state (hi-registers register) (bit-or (bit-and (get state (hi-registers register)) 0xff)
                                                     (bit-shift-left value 8)))))


(defn simulate-instruction [state {:keys [op operand1 operand2]}]
  (case op
    'mov (cond (and (symbol? operand1)
                    (int? operand2)) (set-register state operand1 operand2)
               (and (symbol? operand1)
                    (symbol? operand2))
               (set-register state operand1 (get-register state operand2)))))

(defn print-hex-word [b]
  (str "0x" (Integer/toHexString b)))

(defn print-full-hex-word [b]
  (loop [s (Integer/toHexString b)]
    (if (= 4 (count s))
      (str "0x" s)
      (recur (str "0" s)))))

(defn print-state [state]
  (println "Final registers:")
  (doseq [k ['ax 'bx 'cx 'dx 'sp 'bp 'si 'di 'es 'cs 'ss 'ds]]
    (when-not (zero? (state k))
      (println (str "      " k ": " (print-full-hex-word (state k)) " " "(" (state k) ")")))))

(defn -main [filename]
  (let [bytes-to-read (load-memory-from-file filename)]
    (println (str "--- " filename " execution ---"))
    (loop [ip    0
           state initial-state]
      (if (< ip (count bytes-to-read))
        (let [{:keys [ip decoded]} (decode-instruction bytes-to-read ip)]
          (print-instruction decoded false)
          (let [new-state (simulate-instruction state decoded)
                [delta1 delta2] (diff new-state state)]
            (print " ; ")
            (doseq [k (keys delta1)]
              (print (str k ":" (print-hex-word (delta2 k)) "->" (print-hex-word (delta1 k)) " ")))
            (println)
            (recur ip new-state)))
        (do (println)
            (print-state state))))))


(comment
 (-main "listing_0045_challenge_register_movs")

 (def filename "listing_0045_challenge_register_movs")
 (def bytes-to-read (load-memory-from-file filename))
 (do
   (def ip 0)
   (def state initial-state))

 (let [{:keys [ip decoded]} (decode-instruction bytes-to-read ip)]
   (def ip ip)
   (print-instruction decoded false)
   (let [new-state (simulate-instruction state decoded)
         [delta1 delta2] (diff new-state state)]
     (def state new-state)
     (print " ; ")
     (doseq [k (keys delta1)]
       (print (str k ":" (print-hex-word (delta2 k)) "->" (print-hex-word (delta1 k)) " ")))
     (println)
     )
   (print-state state)

   state
   )
 )
