(ns decoder
  (:require [clojure.java.io :as io]
            [clojure.string :as string])
  (:import [java.io ByteArrayOutputStream]))

(defmacro def-locals []
  (let [bindings (filter #(and (simple-symbol? %)
                               (not (string/includes? (str %) "--"))
                               (not (re-matches #"(output|input)-(schema|checker)\d\d\d+.*" (str %)))) (keys &env))]
    `(do ~@(for [sym bindings]
             `(def ~sym ~sym)))))

(defn mask [n]
  (dec (bit-shift-left 1 n)))

(defn load-memory-from-file [filename]
  (with-open [in  (clojure.java.io/input-stream (io/resource filename))
              out (ByteArrayOutputStream.)]
    (clojure.java.io/copy in out)
    (mapv #(bit-and % (mask 8)) (.toByteArray out))))

(defn byte->string
  ([b] (byte->string b 8))
  ([b n]
   (let [s (Integer/toBinaryString (bit-and b (mask n)))]
     (str (apply str (repeat (- n (count s)) "0")) s))))


(defn add-bytes [lo hi]
  (+ lo (bit-shift-left hi 8)))

(defn signed-8-bit [byte]
  (if (bit-test byte (dec 8))
    (- byte (bit-shift-left 1 8))
    byte))

(defn signed-16-bit [two-byte]
  (if (bit-test two-byte (dec 16))
    (- two-byte (bit-shift-left 1 16))
    two-byte))

(defn valid-number? [val bits]
  (and (not (nil? val)) (<= 0 bits (mask 8))))

(def MOD
  [:mod 2 (fn [mod & _]
            (assert (valid-number? mod 2))
            mod)])

(def REG-ENCODING
  [['al 'ax]
   ['cl 'cx]
   ['dl 'dx]
   ['bl 'bx]
   ['ah 'sp]
   ['ch 'bp]
   ['dh 'si]
   ['bh 'di]])

(def REG
  [:reg 3 (fn [reg {:keys [w]}]
            (assert (valid-number? reg 3))
            (assert (valid-number? w 1))
            ((REG-ENCODING reg) w))])

(def R|M-ENCODING
  [['[bx si] '[bx si D8] '[bx si D16] (REG-ENCODING 0)]
   ['[bx di] '[bx di D8] '[bx di D16] (REG-ENCODING 1)]
   ['[bp si] '[bp si D8] '[bp si D16] (REG-ENCODING 2)]
   ['[bp di] '[bp di D8] '[bp di D16] (REG-ENCODING 3)]
   ['[si] '[si D8] '[si D16] (REG-ENCODING 4)]
   ['[di] '[di D8] '[di D16] (REG-ENCODING 5)]
   ['[DIRECT-ADDRESS] '[bp D8] '[bp D16] (REG-ENCODING 6)]
   ['[bx] '[bx D8] '[bx D16] (REG-ENCODING 7)]])

(def R|M
  [:r|m 3 (fn [r|m {:keys [mod w]}]
            (assert (valid-number? r|m 3))
            (assert (valid-number? mod 2))
            (let [encoding ((R|M-ENCODING r|m) mod)]
              (if (= mod 2r11)
                (do
                  (assert (valid-number? w 1))
                  (encoding w))
                encoding)))])

(def SR-ENCODING
  ['es
   'cs
   'ss
   'ds])

(def SR-segment
  [:segment 2 (fn [sr & _]
                (assert (valid-number? sr 2))
                (SR-ENCODING sr))])

(def SR
  [:reg 2 (fn [sr & _]
            (assert (valid-number? sr 2))
            (SR-ENCODING sr))])

(def D [:d 1 (fn [bit & _]
               (assert (valid-number? bit 1))
               bit)])

(def S [:s 1 (fn [bit & _]
               (assert (valid-number? bit 1))
               bit)])

(def W [:w 1 (fn [bit & _]
               (assert (valid-number? bit 1))
               bit)])

(def V [:v 1 (fn [bit & _]
               (assert (valid-number? bit 1))
               bit)])

(def Z [:z 1 (fn [bit & _]
               (assert (valid-number? bit 1))
               bit)])

(defn literal [bits pattern]
  (assert (valid-number? pattern bits))
  [:literal bits (fn [n & _]
                   (assert (valid-number? n bits))
                   (when (= pattern n)
                     pattern))])

(def DATA-8
  [[:data 8 (fn [data & _]
              (assert (valid-number? data 8))
              data)]])

(def DATA
  [[:data 8 (fn [data & _]
              (assert (valid-number? data 8))
              data)]])

(defn DATA-if-SW [{:keys [data s w] :or {s 0}}]
  (when (and (= s 0) (= 1 w))
    [[:data 8 (fn [data-hi & _]
                (assert (valid-number? data-hi 8))
                (add-bytes (bit-and (mask 8) data)
                           data-hi))]]))

(defn D-implicit [d]
  [:d 0 (constantly d)])

(defn DISP-LO [{:keys [r|m mod]}]
  (cond (or (= 2r10 mod)
            (= '[DIRECT-ADDRESS] r|m))
        [[:disp-lo 8 (fn [disp-lo & _]
                       (assert (valid-number? disp-lo 8))
                       disp-lo)]]

        (= 2r01 mod)
        [[:r|m 8 (fn [disp & {:keys [r|m]}]
                   (valid-number? disp 8)
                   (conj (pop r|m) (signed-8-bit disp)))]]))

(defn DISP-HI [{:keys [disp-lo r|m mod]}]
  (when (or (= 2r10 mod)
            (= '[DIRECT-ADDRESS] r|m))
    [[:r|m 8 (fn [disp-hi {:keys [r|m]}]
               (assert (valid-number? disp-hi 8))
               (conj (pop r|m) (signed-16-bit (add-bytes disp-lo disp-hi))))]]))

(defn AX-implicit
  ([] (AX-implicit :reg))
  ([k]
   [k 0 (fn [_ {:keys [w]}] ((REG-ENCODING 0) w))]))

(def DX-implicit
  [:r|m 0 (fn [& _] 'dx)])

(def W-implicit
  [:w 0 (fn [& _] 1)])

(def ADDR-LO
  [[:addr-lo 8 (fn [addr-lo & _]
                 (assert (valid-number? addr-lo 8))
                 addr-lo)]])

(def ADDR-HI
  [[:r|m 8 (fn [addr-hi & {:keys [addr-lo]}]
             (assert (valid-number? addr-hi 8))
              [(add-bytes addr-lo addr-hi)])]])

(def IP-INC8
  [[:ip-inc8 8 (fn [ip-inc8 & _]
                 (assert (valid-number? ip-inc8 8))
                 (signed-8-bit ip-inc8))]])

(def DATA-V-implicit
  [:data 0 (fn [_ {:keys [v]}]
             (if (zero? v)
               1
               'cl))])

(defn INCLUDE-SIZE? [include-size?]
  [:include-size? 0 (fn [& _]
                      include-size?)])

(def IP-LO
  [[:ip-inc 8 (fn [ip-lo & _]
               ip-lo)]])

(def IP-HI
  [[:ip-inc 8 (fn [ip-hi {:keys [ip-inc]}]
            (signed-16-bit (add-bytes ip-inc ip-hi)))]])

(def CS-LO
  [[:cs 8 (fn [cs-lo & _]
               cs-lo)]])

(def CS-HI
  [[:cs 8 (fn [cs-hi {:keys [cs]}]
            (add-bytes cs cs-hi))]])

(defn OP [op]
  [:op 0 (constantly op)])

(def REP-literal 2r1111001)

(defn REP-symbol [sym]
  [:rep 0 (fn [_ {:keys [w]}]
            (assert (not (nil? w)) "REP instructions must have W set")
            (symbol
             (str sym
                  (if (zero? w)
                    'b 'w))))])

(def INSTRUCTIONS
  [[[(OP 'mov) (literal 6 2r100010) D W] [MOD REG R|M] DISP-LO DISP-HI]
   [[(OP 'mov) (literal 7 2r1100011) W] [MOD (literal 3 2r000) R|M] DISP-LO DISP-HI DATA DATA-if-SW]
   [[(OP 'mov) (literal 4 2r1011) (D-implicit 1) W REG] DATA DATA-if-SW]
   [[(OP 'mov) (literal 7 2r1010000) W (D-implicit 1) (AX-implicit)] ADDR-LO ADDR-HI]
   [[(OP 'mov) (literal 7 2r1010001) W (D-implicit 0) (AX-implicit)] ADDR-LO ADDR-HI]
   [[(OP 'mov) (literal 8 2r10001110) (D-implicit 1) W-implicit] [MOD (literal 1 2r0) SR R|M] DISP-LO DISP-HI]
   [[(OP 'mov) (literal 8 2r10001100) (D-implicit 0) W-implicit] [MOD (literal 1 2r0) SR R|M] DISP-LO DISP-HI]
   [[(OP 'push) (literal 8 2r11111111) W-implicit] [MOD (literal 3 2r110) R|M] DISP-LO DISP-HI]
   [[(OP 'push) (literal 5 2r01010) (D-implicit 1) W-implicit REG]]
   [[(OP 'push) (literal 3 2r000) (D-implicit 1) W-implicit SR (literal 3 2r110)]]
   [[(OP 'pop) (literal 8 2r10001111) W-implicit] [MOD (literal 3 2r000) R|M] DISP-LO DISP-HI]
   [[(OP 'pop) (literal 5 2r01011) (D-implicit 1) W-implicit REG]]
   [[(OP 'pop) (literal 3 2r000) (D-implicit 1) W-implicit SR (literal 3 2r111)]]
   [[(OP 'xchg) (literal 7 2r1000011) (D-implicit 1) W] [MOD REG R|M] DISP-LO DISP-HI]
   [[(OP 'xchg) (literal 5 2r10010) (D-implicit 1) W-implicit REG (AX-implicit :r|m)]]
   [[(OP 'in) (literal 7 2r1110010) (D-implicit 1) W (AX-implicit)] DATA-8]
   [[(OP 'in) (literal 7 2r1110110) (D-implicit 1) W (AX-implicit) DX-implicit]]
   [[(OP 'out) (literal 7 2r1110011) (D-implicit 0) W (AX-implicit)] DATA-8]
   [[(OP 'out) (literal 7 2r1110111) (D-implicit 0) W (AX-implicit) DX-implicit]]
   [[(OP 'xlat) (literal 8 2r11010111)]]
   [[(OP 'lea) (literal 8 2r10001101) (D-implicit 1) W-implicit] [MOD REG R|M] DISP-LO DISP-HI]
   [[(OP 'lds) (literal 8 2r11000101) (D-implicit 1) W-implicit] [MOD REG R|M] DISP-LO DISP-HI]
   [[(OP 'les) (literal 8 2r11000100) (D-implicit 1) W-implicit] [MOD REG R|M] DISP-LO DISP-HI]
   [[(OP 'lahf) (literal 8 2r10011111)]]
   [[(OP 'sahf) (literal 8 2r10011110)]]
   [[(OP 'pushf) (literal 8 2r10011100)]]
   [[(OP 'popf) (literal 8 2r10011101)]]
   [[(OP 'add) (literal 6 2r000000) D W] [MOD REG R|M] DISP-LO DISP-HI]
   [[(OP 'add) (literal 6 2r100000) S W] [MOD (literal 3 2r000) R|M] DISP-LO DISP-HI DATA DATA-if-SW]
   [[(OP 'add) (literal 7 2r0000010) W (AX-implicit) (D-implicit 1)] DATA DATA-if-SW]
   [[(OP 'adc) (literal 6 2r000100) D W] [MOD REG R|M] DISP-LO DISP-HI]
   [[(OP 'adc) (literal 6 2r100000) S W] [MOD (literal 3 2r010) R|M] DISP-LO DISP-HI DATA DATA-if-SW]
   [[(OP 'adc) (literal 7 2r0001010) W (AX-implicit) (D-implicit 1)] DATA DATA-if-SW]
   [[(OP 'inc) (literal 7 2r1111111) W] [MOD (literal 3 2r000) R|M] DISP-LO DISP-HI]
   [[(OP 'inc) (literal 5 2r01000) (D-implicit 1) W-implicit REG]]
   [[(OP 'aaa) (literal 8 2r00110111)]]
   [[(OP 'daa) (literal 8 2r00100111)]]
   [[(OP 'sub) (literal 6 2r001010) D W] [MOD REG R|M] DISP-LO DISP-HI]
   [[(OP 'sub) (literal 6 2r100000) S W] [MOD (literal 3 2r101) R|M] DISP-LO DISP-HI DATA DATA-if-SW]
   [[(OP 'sub) (literal 7 2r0010110) W (AX-implicit) (D-implicit 1)] DATA DATA-if-SW]
   [[(OP 'sbb) (literal 6 2r000110) D W] [MOD REG R|M] DISP-LO DISP-HI]
   [[(OP 'sbb) (literal 6 2r100000) S W] [MOD (literal 3 2r011) R|M] DISP-LO DISP-HI DATA DATA-if-SW]
   [[(OP 'sbb) (literal 7 2r0001110) W (AX-implicit) (D-implicit 1)] DATA DATA-if-SW]
   [[(OP 'dec) (literal 7 2r1111111) W] [MOD (literal 3 2r001) R|M] DISP-LO DISP-HI]
   [[(OP 'dec) (literal 5 2r01001) (D-implicit 1) W-implicit REG]]
   [[(OP 'neg) (literal 7 2r1111011) W] [MOD (literal 3 2r011) R|M] DISP-LO DISP-HI]
   [[(OP 'cmp) (literal 6 2r001110) D W] [MOD REG R|M] DISP-LO DISP-HI]
   [[(OP 'cmp) (literal 6 2r100000) S W] [MOD (literal 3 2r111) R|M] DISP-LO DISP-HI DATA DATA-if-SW]
   [[(OP 'cmp) (literal 7 2r0011110) W (AX-implicit) (D-implicit 1)] DATA DATA-if-SW]
   [[(OP 'aas) (literal 8 2r00111111)]]
   [[(OP 'das) (literal 8 2r00101111)]]
   [[(OP 'mul) (literal 7 2r1111011) W] [MOD (literal 3 2r100) R|M] DISP-LO DISP-HI]
   [[(OP 'imul) (literal 7 2r1111011) W] [MOD (literal 3 2r101) R|M] DISP-LO DISP-HI]
   [[(OP 'aam) (literal 8 2r11010100)] [(literal 8 2r00001010)]]
   [[(OP 'div) (literal 7 2r1111011) W] [MOD (literal 3 2r110) R|M] DISP-LO DISP-HI]
   [[(OP 'idiv) (literal 7 2r1111011) W] [MOD (literal 3 2r111) R|M] DISP-LO DISP-HI]
   [[(OP 'aad) (literal 8 2r11010101)] [(literal 8 2r00001010)]]
   [[(OP 'cbw) (literal 8 2r10011000)]]
   [[(OP 'cwd) (literal 8 2r10011001)]]
   [[(OP 'not) (literal 7 2r1111011) W] [MOD (literal 3 2r010) R|M] DISP-LO DISP-HI]
   [[(OP 'shl) (literal 6 2r110100) (INCLUDE-SIZE? true) V DATA-V-implicit W] [MOD (literal 3 2r100) R|M] DISP-LO DISP-HI]
   [[(OP 'shr) (literal 6 2r110100) (INCLUDE-SIZE? true) V DATA-V-implicit W] [MOD (literal 3 2r101) R|M] DISP-LO DISP-HI]
   [[(OP 'rol) (literal 6 2r110100) (INCLUDE-SIZE? true) V DATA-V-implicit W] [MOD (literal 3 2r000) R|M] DISP-LO DISP-HI]
   [[(OP 'ror) (literal 6 2r110100) (INCLUDE-SIZE? true) V DATA-V-implicit W] [MOD (literal 3 2r001) R|M] DISP-LO DISP-HI]
   [[(OP 'sar) (literal 6 2r110100) (INCLUDE-SIZE? true) V DATA-V-implicit W] [MOD (literal 3 2r111) R|M] DISP-LO DISP-HI]
   [[(OP 'rcl) (literal 6 2r110100) (INCLUDE-SIZE? true) V DATA-V-implicit W] [MOD (literal 3 2r010) R|M] DISP-LO DISP-HI]
   [[(OP 'rcr) (literal 6 2r110100) (INCLUDE-SIZE? true) V DATA-V-implicit W] [MOD (literal 3 2r011) R|M] DISP-LO DISP-HI]
   [[(OP 'and) (literal 6 2r001000) D W] [MOD REG R|M] DISP-LO DISP-HI]
   [[(OP 'and) (literal 7 2r1000000) W] [MOD (literal 3 2r100) R|M] DISP-LO DISP-HI DATA DATA-if-SW]
   [[(OP 'and) (literal 7 2r0010010) W (AX-implicit) (D-implicit 1)] DATA DATA-if-SW]
   [[(OP 'test) (literal 6 2r100001) D W] [MOD REG R|M] DISP-LO DISP-HI]
   [[(OP 'test) (literal 7 2r1111011) W] [MOD (literal 3 2r000) R|M] DISP-LO DISP-HI DATA DATA-if-SW]
   [[(OP 'test) (literal 7 2r1010100) W (AX-implicit) (D-implicit 1)] DATA DATA-if-SW]
   [[(OP 'or) (literal 6 2r000010) D W] [MOD REG R|M] DISP-LO DISP-HI]
   [[(OP 'or) (literal 7 2r1000000) W] [MOD (literal 3 2r001) R|M] DISP-LO DISP-HI DATA DATA-if-SW]
   [[(OP 'or) (literal 7 2r0000110) W (AX-implicit) (D-implicit 1)] DATA DATA-if-SW]
   [[(OP 'xor) (literal 6 2r001100) D W] [MOD REG R|M] DISP-LO DISP-HI]
   [[(OP 'xor) (literal 7 2r0011010) W (AX-implicit) (D-implicit 1)] DATA DATA-if-SW]
   [[(OP 'xor) (literal 7 2r1000000) W] [MOD (literal 3 2r110) R|M] DISP-LO DISP-HI DATA DATA-if-SW]
   [[(OP 'rep) (literal 7 REP-literal) Z] [(literal 7 2r1010010) W (REP-symbol 'movs)]]
   [[(OP 'rep) (literal 7 REP-literal) Z] [(literal 7 2r1010011) W (REP-symbol 'cmps)]]
   [[(OP 'rep) (literal 7 REP-literal) Z] [(literal 7 2r1010111) W (REP-symbol 'scas)]]
   [[(OP 'rep) (literal 7 REP-literal) Z] [(literal 7 2r1010110) W (REP-symbol 'lods)]]
   [[(OP 'rep) (literal 7 REP-literal) Z] [(literal 7 2r1010101) W (REP-symbol 'stos)]]
   [[(OP 'call) (literal 8 2r11101000)] IP-LO IP-HI]
   [[(OP 'call) (literal 8 2r11111111) (INCLUDE-SIZE? false) W-implicit] [MOD (literal 3 2r010) R|M] DISP-LO DISP-HI]
   [[(OP 'call) (literal 8 2r10011010)] IP-LO IP-HI CS-LO CS-HI]
   [[(OP "call far") (literal 8 2r11111111) (INCLUDE-SIZE? false) W-implicit] [MOD (literal 3 2r011) R|M] DISP-LO DISP-HI]
   [[(OP 'jmp) (literal 8 2r11101001)] IP-LO IP-HI]
   [[(OP 'jmp) (literal 8 2r11111111) (INCLUDE-SIZE? false) W-implicit] [MOD (literal 3 2r100) R|M] DISP-LO DISP-HI]
   [[(OP 'jmp) (literal 8 2r11101010)] IP-LO IP-HI CS-LO CS-HI]
   [[(OP "jmp far") (literal 8 2r11111111) (INCLUDE-SIZE? false) W-implicit] [MOD (literal 3 2r101) R|M] DISP-LO DISP-HI]
   [[(OP 'ret) (literal 8 2r11000010) W-implicit] DATA DATA-if-SW]
   [[(OP 'ret) (literal 8 2r11000011)]]
   [[(OP 'retf) (literal 8 2r11001011)]]
   [[(OP 'retf) (literal 8 2r11001010) W-implicit] DATA DATA-if-SW]
   [[(OP 'je) (literal 8 2r01110100)] IP-INC8]
   [[(OP 'jl) (literal 8 2r01111100)] IP-INC8]
   [[(OP 'jle) (literal 8 2r01111110)] IP-INC8]
   [[(OP 'jb) (literal 8 2r01110010)] IP-INC8]
   [[(OP 'jbe) (literal 8 2r01110110)] IP-INC8]
   [[(OP 'jp) (literal 8 2r01111010)] IP-INC8]
   [[(OP 'jo) (literal 8 2r01110000)] IP-INC8]
   [[(OP 'js) (literal 8 2r01111000)] IP-INC8]
   [[(OP 'jne) (literal 8 2r01110101)] IP-INC8]
   [[(OP 'jnl) (literal 8 2r01111101)] IP-INC8]
   [[(OP 'jg) (literal 8 2r01111111)] IP-INC8]
   [[(OP 'jnb) (literal 8 2r01110011)] IP-INC8]
   [[(OP 'ja) (literal 8 2r01110111)] IP-INC8]
   [[(OP 'jnp) (literal 8 2r01111011)] IP-INC8]
   [[(OP 'jno) (literal 8 2r01110001)] IP-INC8]
   [[(OP 'jns) (literal 8 2r01111001)] IP-INC8]
   [[(OP 'loop) (literal 8 2r11100010)] IP-INC8]
   [[(OP 'loopz) (literal 8 2r11100001)] IP-INC8]
   [[(OP 'loopnz) (literal 8 2r11100000)] IP-INC8]
   [[(OP 'jcxz) (literal 8 2r11100011)] IP-INC8]
   [[(OP 'int) (literal 8 2r11001101)] DATA-8]
   [[(OP 'int3) (literal 8 2r11001100)]]
   [[(OP 'into) (literal 8 2r11001110)]]
   [[(OP 'iret) (literal 8 2r11001111)]]
   [[(OP 'clc) (literal 8 2r11111000)]]
   [[(OP 'cmc) (literal 8 2r11110101)]]
   [[(OP 'stc) (literal 8 2r11111001)]]
   [[(OP 'cld) (literal 8 2r11111100)]]
   [[(OP 'std) (literal 8 2r11111101)]]
   [[(OP 'cli) (literal 8 2r11111010)]]
   [[(OP 'sti) (literal 8 2r11111011)]]
   [[(OP 'hlt) (literal 8 2r11110100)]]
   [[(OP 'wait) (literal 8 2r10011011)]]
   [[(OP 'lock) (literal 8 2r11110000) [:prefix? 0 (constantly true)]]]
   [[(OP 'segment) (literal 3 2r001) SR-segment (literal 3 2r110)]]

   ]

  )

(defn byte-instruction-match? [byte-instruction byte]
  (loop [bits-read 0
         part-to-read byte-instruction]
    (if (empty? part-to-read)
      true
      (let [[t bits validating-fn] (first part-to-read)]
        (if (not= :literal t)
          (recur (+ bits-read bits)
                 (rest part-to-read))
          (if (validating-fn (bit-and (unsigned-bit-shift-right byte (- 8 (+ bits-read bits)))
                                      (mask bits)))
            (recur (+ bits-read bits)
                   (rest part-to-read))
            false))))))

(defn instruction-match? [[first-byte-instruction second-byte-instruction] first-byte second-byte]
  (if second-byte-instruction
    (and (byte-instruction-match? first-byte-instruction first-byte)
         (byte-instruction-match? second-byte-instruction second-byte))
    (byte-instruction-match? first-byte-instruction first-byte)))

(defn find-instruction [bytes-to-read ip]
  (let [first-byte (bytes-to-read ip)
        second-byte (bytes-to-read (inc ip))]
    (loop [instructions INSTRUCTIONS]
      (if (not-empty instructions)
        (if (instruction-match? (first instructions) first-byte second-byte)
          (first instructions)
          (recur (rest instructions)))
        (throw (ex-info "Instruction not found" {}))))))

(defn decode-byte [instruction byte-to-decode instruction-decoded]
  (loop [bits-decoded 0
         instruction instruction
         instruction-decoded instruction-decoded]
    (if (not-empty instruction)
      (let [[keyword-to-include
             bits-to-decode
             decoding-fn] (first instruction)]
        (recur
         (+ bits-to-decode bits-decoded)
         (vec (next instruction))
         (assoc instruction-decoded
           keyword-to-include
           (decoding-fn (bit-and (bit-shift-right byte-to-decode (- 8 (+ bits-decoded bits-to-decode)))
                                 (mask bits-to-decode))
                        instruction-decoded)))
        )
      (dissoc instruction-decoded :literal))))

(defn resume-instruction [{:keys [d reg r|m data ip-inc8 rep ip-inc cs] :as decoded-instruction}
                          ip]
  (let [[operand1 operand2] (cond
                              (and ip-inc cs) [(str cs ":" ip-inc) nil]
                              ip-inc [(+ ip ip-inc) nil]
                              rep [rep nil]
                              ip-inc8 [ip-inc8 nil]
                              reg
                              (if (zero? d)
                                [(or data r|m) reg]
                                [reg (or data r|m)])
                              r|m [r|m data]
                              :else [data r|m])]
    (assoc decoded-instruction
      :operand1 operand1
      :operand2 operand2)))

(defn decode-instruction [bytes-to-read ip]
  (loop [instruction-part (find-instruction bytes-to-read ip)
         acc              {}
         ip               ip]
    (cond
      (vector? (first instruction-part))
      (recur (next instruction-part) (decode-byte (first instruction-part) (bytes-to-read ip) acc)
             (inc ip))

      (fn? (first instruction-part))
      (if ((first instruction-part) acc)
        (recur (next instruction-part) (decode-byte ((first instruction-part) acc) (bytes-to-read ip) acc)
               (inc ip))
        (recur (next instruction-part) acc ip))

      (= 'segment (:op acc))
      (recur (find-instruction bytes-to-read ip)
             acc
             ip)

      :else
      {:ip      ip
       :decoded (dissoc (resume-instruction acc ip) nil)})))

(def jumps
  #{'je 'jl 'jle 'jb 'jbe 'jp 'jo 'js 'jne 'jnl 'jg 'jnb 'ja 'jnp 'jno 'jns 'loop 'loopz 'loopnz 'jcxz})
(defn print-instruction [{:keys [op w operand1 operand2 include-size? prefix? segment]} new-line?]
  (if (jumps op)
    (let [operand1 (+ 2 operand1)]
      (println op (str "$" (if (neg? operand1) operand1 (str "+" operand1)))))
    (let [include-size? (if (nil? include-size?)
                          (and (not (symbol? operand1))
                               (not (symbol? operand2)))
                          include-size?)
          include-+-seg-and-size (fn [operand]
                                   (if (vector? operand)
                                     (let [operand (clojure.string/replace (str operand) " " " + ")
                                           operand (if segment
                                                      (str segment ":" operand)
                                                      operand)]
                                       (str (when include-size? (if (zero? w) "byte " "word ")) operand))
                                     operand))
          operand1      (include-+-seg-and-size operand1)
          operand2      (include-+-seg-and-size operand2)]
      (if (nil? prefix?)
        (let [print-fn (if new-line? println print)]
          (cond (and operand1 operand2) (print-fn op (str operand1 ",") operand2)
                operand1 (print-fn op operand1)
                :else (print-fn op)))
        (print (str op " "))))))

(defn -main [filename]
  (let [bytes-to-read (load-memory-from-file filename)]
    (println ";" filename "disassembly:")
    (println "bits 16")
    (loop [ip 0]
      (when (< ip (count bytes-to-read))
        (let [{:keys [ip decoded]} (decode-instruction bytes-to-read ip)]
          (print-instruction decoded true)
          (recur ip))))
    )
  )
