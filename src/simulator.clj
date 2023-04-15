(ns simulator
  (:require [decoder :refer [decode-instruction
                             load-memory-from-file
                             print-instruction
                             mask]]
            [clojure.data :refer [diff]]
            [clojure.string :as string]))

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
   'ds 0
   'flags {'C false
           'Z false
           'S false
           'O false
           'P false
           'A false}})

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

(defn number-of-ones [number]
  (count (string/replace (Integer/toBinaryString number)
                         #"0" "")))

(defn has-auxiliary-carry? [operation operand1 operand2]
  (let [nibble-operand1 (bit-and operand1 0xf)
        nibble-operand2 (bit-and operand2 0xf)]
    (bit-test (operation nibble-operand1 nibble-operand2)
              4)))

(defn run-operation [operator operand1 operand2]
  (let [operand1 (bit-and operand1 (mask 16))
        operand2 (bit-and operand2 (mask 16))
        result (operator operand1 operand2)
        result-16-bit (bit-and result (mask 16))
        ;_ (do (println operator operand1 operand2))
        flags  {'C (bit-test result 16)
                'Z (zero? result-16-bit)
                'S (bit-test result-16-bit 15)
                'O (condp = operator
                     + (bit-test (bit-and (bit-not (bit-xor operand1 operand2))
                                          (bit-xor operand2 result))
                                 15)
                     - (bit-test (bit-and (bit-xor operand1 operand2)
                                          (bit-not (bit-xor operand2 result)))
                                 15))
                'P (zero? (mod (number-of-ones (bit-and result (mask 8))) 2))
                'A (has-auxiliary-carry? operator operand1 operand2)}]
    [result-16-bit flags]))

(defn set-flags [state flags new-flags]
  (loop [flags flags
         state state]
    (if (not-empty flags)
      (let [new-state
            (condp = (first flags)
              'C (assoc-in state ['flags 'C] (new-flags 'C))
              'Z (assoc-in state ['flags 'Z] (new-flags 'Z))
              'S (assoc-in state ['flags 'S] (new-flags 'S))
              'O (assoc-in state ['flags 'O] (new-flags 'O))
              'P (assoc-in state ['flags 'P] (new-flags 'P))
              'A (assoc-in state ['flags 'A] (new-flags 'A))
              state)]
        (recur (rest flags)
               new-state))
      state)))

(defn simulate-instruction [state {:keys [op operand1 operand2 flags]}]
  (let [operand2 (cond (symbol? operand2) (get-register state operand2)
                       (int? operand2) operand2)]
    (condp = op
      'mov (when (and (symbol? operand1)
                      (int? operand2))
             (set-register state operand1 operand2))
      'add (cond (and (symbol? operand1)
                      (int? operand2))
                 (let [operator         +
                       destination      operand1
                       operand1         (get-register state operand1)
                       [result new-flags] (run-operation operator operand1 operand2)]
                   (set-flags (set-register state destination result)
                              flags
                              new-flags)))
      'sub (cond (and (symbol? operand1)
                      (int? operand2))
                 (let [operator         -
                       destination      operand1
                       operand1         (get-register state operand1)
                       [result new-flags] (run-operation operator operand1 operand2)]
                   (set-flags (set-register state destination result)
                              flags
                              new-flags)))
      'cmp (cond (and (symbol? operand1)
                      (int? operand2))
                 (let [operator    -
                       operand1    (get-register state operand1)
                       [_ new-flags] (run-operation operator operand1 operand2)]
                   (set-flags state
                              flags
                              new-flags))))))

(defn print-hex-word [b]
  (str "0x" (Integer/toHexString b)))

(defn print-full-hex-word [b]
  (loop [s (Integer/toHexString b)]
    (if (= 4 (count s))
      (str "0x" s)
      (recur (str "0" s)))))

(defn flags->str [flags-map]
  (apply str (filter flags-map ['C 'P 'A 'Z 'S 'O])))

(defn print-state [state]
  (println "Final registers:")
  (doseq [k ['ax 'bx 'cx 'dx 'sp 'bp 'si 'di 'es 'cs 'ss 'ds]]
    (when-not (zero? (state k))
      (println (str "      " k ": " (print-full-hex-word (state k)) " " "(" (state k) ")"))))
  (let [flags-str (flags->str (get state 'flags))]
    (when (not-empty flags-str)
      (println "   flags:" flags-str))))

(defn -main [filename]
  (let [bytes-to-read (load-memory-from-file filename)]
    (println (str "--- " filename " execution ---"))
    (loop [ip    0
           state initial-state]
      (if (< ip (count bytes-to-read))
        (let [{:keys [ip decoded]} (decode-instruction bytes-to-read ip)]
          (print-instruction decoded false)
          (let [new-state (simulate-instruction state decoded)
                [delta1] (diff new-state state)]
            (print " ; ")
            (doseq [k (keys delta1)]
              (if (= 'flags k)
                (print (str "flags:" (flags->str (state 'flags)) "->" (flags->str (new-state 'flags)) " "))
                (print (str k ":" (print-hex-word (state k)) "->" (print-hex-word (new-state k)) " "))))
            (println)
            (recur ip new-state)))
        (do (println)
            (print-state state))))))
