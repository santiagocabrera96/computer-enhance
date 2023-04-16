(ns simulator
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [decoder :refer [decode-instruction
                             load-memory-from-file
                             print-instruction
                             mask
                             signed-8-bit]])
  (:import [java.io File]))

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

(defn initial-state [bytes-to-read]
  {'ax     0
   'bx     0
   'cx     0
   'dx     0
   'sp     0
   'bp     0
   'si     0
   'di     0
   'es     0
   'cs     0
   'ss     0
   'ds     0
   'ip     0
   'flags  {'C false
            'Z false
            'S false
            'O false
            'P false
            'A false}
   'memory (byte-array (bit-shift-left 1 16) bytes-to-read)})

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
  (let [operand1      (bit-and operand1 (mask 16))
        operand2      (bit-and operand2 (mask 16))
        result        (operator operand1 operand2)
        result-16-bit (bit-and result (mask 16))
        ;_ (do (println operator operand1 operand2))
        flags         {'C (bit-test result 16)
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

(defn set-memory [state address value w]
  (aset (state 'memory) address (byte (signed-8-bit (bit-and value 0xff))))
  (if (zero? w)
    state
    (do (aset (state 'memory) (inc address) (byte (signed-8-bit (bit-shift-right (bit-and value 0xff00) 8))))
        state)))

(defn simulate-instruction [state {:keys [op w operand1 operand2 flags]}]
  (let [operand1 (if (vector? operand1)
                   (let [address (reduce + (map #(if (symbol? %)
                                                   (get-register state %)
                                                   %) operand1))]
                     [address])
                   operand1)
        operand2 (cond (symbol? operand2) (get-register state operand2)
                       (int? operand2) operand2
                       (vector? operand2) (let [address (reduce + (map #(if (symbol? %)
                                                                          (get-register state %)
                                                                          %) operand2))]
                                            (if (zero? w)
                                              (get-in state ['memory address])
                                              (bit-or (get-in state ['memory address])
                                                      (bit-shift-left (get-in state ['memory (+ address 1)]) 8)))))]
    (condp = op
      'mov (cond (and (symbol? operand1)
                      (int? operand2)) (set-register state operand1 operand2)
                 (and (vector? operand1)
                      (int? operand2)) (set-memory state (first operand1) operand2 w))
      'add (cond (and (symbol? operand1)
                      (int? operand2))
                 (let [operator    +
                       destination operand1
                       operand1    (get-register state operand1)
                       [result new-flags] (run-operation operator operand1 operand2)]
                   (set-flags (set-register state destination result)
                              flags
                              new-flags)))
      'sub (cond (and (symbol? operand1)
                      (int? operand2))
                 (let [operator    -
                       destination operand1
                       operand1    (get-register state operand1)
                       [result new-flags] (run-operation operator operand1 operand2)]
                   (set-flags (set-register state destination result)
                              flags
                              new-flags)))
      'cmp (cond (and (symbol? operand1)
                      (int? operand2))
                 (let [operator -
                       operand1 (get-register state operand1)
                       [_ new-flags] (run-operation operator operand1 operand2)]
                   (set-flags state
                              flags
                              new-flags)))
      'jne (if (not (get-in state ['flags 'Z]))
             (set-register state 'ip (+ (get-register state 'ip)
                                        operand1))
             state)
      'je (if (get-in state ['flags 'Z])
            (set-register state 'ip (+ (get-register state 'ip)
                                       operand1))
            state)
      'jp (if (get-in state ['flags 'P])
            (set-register state 'ip (+ (get-register state 'ip)
                                       operand1))
            state)
      'jb (if (get-in state ['flags 'S])
            (set-register state 'ip (+ (get-register state 'ip)
                                       operand1))
            state)
      'loopnz (let [state (set-register state 'cx (dec (get-in state ['cx])))]
                (if (not (zero? (get-register state 'cx)))
                  (set-register state 'ip (+ (get-register state 'ip)
                                             operand1))
                  state))
      'loop (let [state (set-register state 'cx (dec (get-in state ['cx])))]
              (if (not (zero? (get-register state 'cx)))
                (set-register state 'ip (+ (get-register state 'ip)
                                           operand1))
                state)))))

(defn print-hex-word [b]
  (str "0x" (Integer/toHexString b)))

(defn print-full-hex-word [b]
  (loop [s (Integer/toHexString b)]
    (if (= 4 (count s))
      (str "0x" s)
      (recur (str "0" s)))))

(defn flags->str [flags-map]
  (apply str (filter flags-map ['C 'P 'A 'Z 'S 'O])))

(def register-keys ['ax 'bx 'cx 'dx 'sp 'bp 'si 'di 'es 'cs 'ss 'ds 'ip 'flags])

(defn print-state [state print-ip?]
  (println "Final registers:")
  (doseq [k register-keys
          :when (not= k 'flags)]
    (when (or (not= k 'ip)
              print-ip?)
      (when-not (zero? (state k))
        (println (str "      " k ": " (print-full-hex-word (state k)) " " "(" (state k) ")")))))
  (let [flags-str (flags->str (get state 'flags))]
    (when (not-empty flags-str)
      (println "   flags:" flags-str))))

(defn -main
  ([filename] (-main filename "" ""))
  ([filename print-ip] (-main filename print-ip ""))
  ([filename print-ip? dump?]
   (let [print-ip?     (= "print-ip" (str print-ip?))
         bytes-to-read (load-memory-from-file filename)
         dump?         (= dump? "dump")
         filename      (last (string/split filename #"/"))]
     (println (str "--- " filename " execution ---"))
     (loop [state (initial-state bytes-to-read)]
       (if (< (state 'ip) (count bytes-to-read))
         (let [{:keys [ip decoded]} (decode-instruction bytes-to-read (state 'ip))]
           (print-instruction decoded false)
           (let [new-state (assoc state 'ip ip)
                 new-state (simulate-instruction new-state decoded)]
             (print " ; ")
             (doseq [k register-keys
                     :when (and (not= 'memory k)
                                (not= (k state) (k new-state))
                                (or (not= k 'ip)
                                    print-ip?))]
               (if (= 'flags k)
                 (print (str "flags:" (flags->str (state 'flags)) "->" (flags->str (new-state 'flags)) " "))
                 (print (str k ":" (print-hex-word (state k)) "->" (print-hex-word (new-state k)) " "))))
             (println)
             (recur new-state)))
         (do (println)
             (print-state state print-ip?)
             (when dump?
               (io/copy (state 'memory) (File. "result.data")))))))))
