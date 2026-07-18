(ns cbor.core-test
  (:require [clojure.string :as str]
            #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer [deftest is] :include-macros true])
            [cbor.core :as cbor]))

(defn- hx [b]
  (apply str (map (fn [x]
                    #?(:clj (format "%02x" (bit-and (int x) 0xff))
                       :cljs (let [h (.toString (bit-and x 0xff) 16)]
                               (if (= 1 (count h)) (str "0" h) h))))
                  (seq b))))
(defn- enc [x] (hx (cbor/encode x)))

(defn- bytes-of [ints]
  #?(:clj (byte-array (map unchecked-byte ints))
     :cljs (js/Uint8Array. (clj->js ints))))

;; ── RFC 8949 Appendix-A vectors ───────────────────────────────────────────────
(deftest rfc8949-vectors
  (is (= "00" (enc 0)))
  (is (= "0a" (enc 10)))
  (is (= "17" (enc 23)))
  (is (= "1818" (enc 24)))
  (is (= "1864" (enc 100)))
  (is (= "1903e8" (enc 1000)))
  (is (= "1a000f4240" (enc 1000000)))
  (is (= "20" (enc -1)))
  (is (= "3863" (enc -100)))
  (is (= "60" (enc "")))
  (is (= "6161" (enc "a")))
  (is (= "6449455446" (enc "IETF")))
  (is (= "80" (enc [])))
  (is (= "83010203" (enc [1 2 3])))
  (is (= "a0" (enc {})))
  (is (= "f4" (enc false)))
  (is (= "f5" (enc true)))
  (is (= "f6" (enc nil)))
  ;; map {"a":1,"b":[2,3]}  → a2 6161 01 6162 820203
  (is (= "a26161016162820203" (enc {"a" 1 "b" [2 3]}))))

;; ── canonical dag-cbor key ordering (shorter first, then bytewise) ────────────
(deftest canonical-key-order
  ;; canonical sort is input-order-independent: a < b < aa regardless of insertion
  (is (= (enc (array-map "aa" 3 "b" 2 "a" 1))
         (enc (array-map "a" 1 "aa" 3 "b" 2))))
  ;; exact bytes: a3 "a"(6161)01 "b"(6162)02 "aa"(626161)03
  (is (= "a361610161620262616103" (enc (array-map "aa" 3 "b" 2 "a" 1))))
  ;; UTF-8 BYTE length, not character count: "z" is one byte, "é" is two.
  (is (= "a2617a0162c3a902" (enc (array-map "é" 2 "z" 1)))))

;; ── encode-ordered preserves the given order (CACAO-style) ────────────────────
(deftest encode-ordered-keeps-order
  ;; pairs b,a emitted in order → header a2, "b"(6162) 01, "a"(6161) 02 — NOT sorted
  (is (= "a2616201616102" (hx (cbor/encode-ordered [["b" 1] ["a" 2]]))))
  (is (not= (hx (cbor/encode-ordered [["b" 1] ["a" 2]])) (enc {"b" 1 "a" 2}))
      "ordered ≠ canonical when input is unsorted")
  (is (= {"b" 1 "a" 2} (cbor/decode (cbor/encode-ordered [["b" 1] ["a" 2]])))))

;; ── round-trips ───────────────────────────────────────────────────────────────
(deftest roundtrips
  (doseq [x [0 1 23 24 255 256 65535 65536 1000000 -1 -100 -1000000
             "" "hello" "日本語"
             [] [1 2 3] ["a" ["b" ["c"]]]
             {} {"k" "v"} {"a" 1 "b" [2 3] "c" {"d" true "e" nil}}
             true false nil]]
    (is (= x (cbor/decode (cbor/encode x))) (str "roundtrip " (pr-str x)))))

(deftest byte-string-roundtrip
  (let [b (bytes-of [0 1 2 250 255])
        dec (cbor/decode (cbor/encode b))]
    ;; `vec`, not `seq` -- `(seq a-typed-array)` isn't reliably `sequential?`
    ;; (and so isn't `=`-comparable to another such seq) on every cljs
    ;; runtime; `vec` always produces a real, comparable PersistentVector.
    (is (= (vec b) (vec dec)))))

(deftest big-uint-and-negint
  (is (= "1affffffff" (enc 0xffffffff)))               ; uint32 max → 4-byte form
  (is (= "1b0000000100000000" (enc 0x100000000)))      ; one past uint32 → 8-byte form
  (is (= 4294967295 (cbor/decode (cbor/encode 4294967295))))
  (is (= 4294967296 (cbor/decode (cbor/encode 4294967296)))))

;; ── nested order-preserving maps (CACAO envelope shape) ───────────────────────
(deftest nested-ordered
  ;; {h:{t:"x"}, p:{b:1,a:2}, s:{t:"E"}} with EVERY level order-preserved
  (let [c (cbor/encode (cbor/ordered [["h" (cbor/ordered [["t" "x"]])]
                                      ["p" (cbor/ordered [["b" 1] ["a" 2]])]
                                      ["s" (cbor/ordered [["t" "E"]])]]))]
    ;; decodes back to the same data (as plain maps)
    (is (= {"h" {"t" "x"} "p" {"b" 1 "a" 2} "s" {"t" "E"}} (cbor/decode c)))
    ;; p's bytes keep b-before-a (61 62 = "b" first), not a-before-b
    (is (str/includes? (hx c) "616201616102"))))

;; ── tags (major type 6) ───────────────────────────────────────────────────────
(deftest tag-encode-vectors
  ;; RFC 8949 Appendix A: 0("2013-03-21T20:04:00Z") → c0 74 …
  (is (= "c074323031332d30332d32315432303a30343a30305a"
         (enc (cbor/tagged 0 "2013-03-21T20:04:00Z"))))
  ;; tag 42 wrapping a 3-byte byte string → d8 2a 43 010203
  (is (= "d82a43010203" (enc (cbor/tagged 42 (bytes-of [1 2 3])))))
  ;; tag number uses the same head widths as uints: tag 1000 → d9 03e8
  (is (= "d903e800" (enc (cbor/tagged 1000 0)))))

(deftest tag-roundtrip
  (let [t (cbor/decode (cbor/encode (cbor/tagged 42 (bytes-of [0 1 113]))))]
    (is (cbor/tagged? t))
    (is (= 42 (cbor/tag-number t)))
    (is (= [0 1 113] (map #(bit-and % 0xff) (seq (cbor/tag-value t))))))
  ;; tags nest inside maps/arrays and round-trip in place
  (let [m (cbor/decode (cbor/encode {"link" (cbor/tagged 42 (bytes-of [9]))
                                     "n" 7}))]
    (is (= 7 (get m "n")))
    (is (cbor/tagged? (get m "link")))
    (is (= 42 (cbor/tag-number (get m "link"))))))

;; ── truncated input fails closed rather than zero-padding ────────────────────
(deftest truncated-string-throws-instead-of-zero-padding
  ;; A byte-string/text-string header declaring more bytes than actually
  ;; remain must raise, same as a truncated array/map/tag already does via
  ;; decode-from's own read-byte! check -- not silently return a
  ;; shorter-than-claimed result padded with zero bytes.
  (let [full (cbor/encode "hello world this is a longer string")
        truncated #?(:clj (byte-array (take 5 (seq full)))
                     :cljs (.slice full 0 5))]
    (is (thrown? #?(:clj Exception :cljs js/Error) (cbor/decode truncated))))
  (let [full (cbor/encode (bytes-of (range 42)))
        truncated #?(:clj (byte-array (take 4 (seq full)))
                     :cljs (.slice full 0 4))]
    (is (thrown? #?(:clj Exception :cljs js/Error) (cbor/decode truncated)))))

(deftest tagged-equality
  (is (= (cbor/tagged 42 "x") (cbor/tagged 42 "x")))
  (is (not= (cbor/tagged 42 "x") (cbor/tagged 43 "x")))
  (is (not= (cbor/tagged 42 "x") (cbor/tagged 42 "y"))))
