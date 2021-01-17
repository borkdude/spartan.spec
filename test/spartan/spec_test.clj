(ns spartan.spec-test
  (:require
   [clojure.string :as str]
   [spartan.test :as t :refer [deftest is]]))

(require 'spartan.spec) ;; side effect

(require '[clojure.spec.alpha :as s])


(deftest pred-test
  (is (= 1 (s/conform int? 1)))
  (is (s/invalid? (s/conform string? 1)))
  (is (= "--opts" (s/conform #{"--opts"} "--opts")))
  (is (= "\"foo\" - failed: int?\n" (with-out-str (s/explain int? "foo"))))
  (is (= 'int? (s/describe int?))))

(deftest cat-test
  (is (= {:a 1, :b "foo"} (s/conform (s/cat :a int? :b string?) [1 "foo"])))
  (is (s/invalid? (s/conform (s/cat :a int? :b string?) [1 "foo" "bar"])))
  (is (= "1 - failed: string? in: [0] at: [:i]\n" (with-out-str (s/explain (s/cat :i string?) [1]))))
  (is (= '(cat :a int? :b string?) (s/describe (s/cat :a int? :b string?)))))

(deftest alt-test
  (is (= [:a 1] (s/conform (s/alt :a int? :b string?) [1])))
  (is (s/invalid? (s/conform (s/alt :a int? :b string?) 1)))
  (is (= "1 - failed: (or (nil? %) (sequential? %))\n"
         (with-out-str (s/explain (s/alt :a int? :b string?) 1))))
  (is (= '(alt :a int? :b string?) (s/describe (s/alt :a int? :b string?)))))

(deftest and-test
  (is (= 6 (s/conform (s/and number? #(> % 5)) 6)))
  (is (s/invalid? (s/conform (s/and number? #(> % 5)) 5)))
  (is (= "5 - failed: (> % 5)\n"
         (with-out-str (s/explain (s/and number? #(> % 5)) 5))))
  (is (= '(and number? (> % 5)) (s/describe (s/and number? #(> % 5))))))

(deftest or-test
  (is (= [:a 1] (s/conform (s/or :a int? :b string?) 1)))
  (is (s/invalid? (s/conform (s/or :a int? :b string?) {:a 1})))
  (is (= "{:a 1} - failed: int? at: [:a]\n{:a 1} - failed: string? at: [:b]\n"
         (with-out-str (s/explain (s/or :a int? :b string?) {:a 1}))))
  (is (= '(or :a int? :b string?) (s/describe (s/or :a int? :b string?)))))

(deftest *-test
  (is (= {:i [1 2 3]} (s/conform (s/cat :i (s/* number?)) [1 2 3])))
  (is (s/invalid? (s/conform (s/cat :i (s/* number?)) [1 2 3 "foo"])))
  (is (s/invalid? (s/conform (s/cat :i (s/* number?)) 1)))
  (is (= "\"foo\" - failed: number? in: [3] at: [:i]\n"
         (with-out-str (s/explain (s/cat :i (s/* number?)) [1 2 3 "foo"]))))
  (is (= '(* number?) (s/describe (s/* number?)))))

(deftest ?-test
  (is (= {:i 1 :j 2} (s/conform (s/cat :i number? :j (s/? number?)) [1 2])))
  (is (= {:i 1} (s/conform (s/cat :i number? :j (s/? number?)) [1])))
  (is (= '(? number?) (s/describe (s/? number?)))))

(deftest +-test
  (is (= {:j [1]} (s/conform (s/cat :j (s/+ number?)) [1])))
  (is (s/invalid? (s/conform (s/cat :j (s/+ number?)) [])))
  (is (= "Success!\n"
         (with-out-str (s/explain (s/cat :j (s/+ number?)) [1]))))
  (is (= "() - failed: Insufficient input at: [:j]\n"
         (with-out-str (s/explain (s/cat :j (s/+ number?)) []))))
  (is (= '(+ number?) (s/describe (s/+ number?)))))

(deftest def-test
  (is (true? (do (s/def ::int int?) (s/valid? ::int 1))))
  (is (= "\"foo\" - failed: int? spec: :spartan.spec-test/int\n" (with-out-str (s/explain ::int "foo")))))

(deftest keys-test
  (s/def ::a (s/keys :req-un [::b ::c]))
  (s/def ::b (s/cat :i int? :j int?))
  (is (= {:c 2, :b {:i 1, :j 2}} (s/conform ::a {:b [1 2] :c 2})))
  (is (= (str/trim "
1 - failed: (or (nil? %) (sequential? %)) in: [:b] at: [:b] spec: :spartan.spec-test/b
{:b 1} - failed: (contains? % :c) spec: :spartan.spec-test/a")
         (str/trim (with-out-str (s/explain ::a {:b 1})))))
  (is (= '(keys :req-un [:spartan.spec-test/b :spartan.spec-test/c])
         (s/describe (s/keys :req-un [::b ::c])))))

(deftest merge-test
  (s/def ::a (s/keys :req-un [::a1 ::a2]))
  (s/def ::b (s/keys :req-un [::b1 ::b2]))
  (s/def ::ab (s/merge ::a ::b))
  (is (= (str/trim "
{:a1 1, :b1 2} - failed: (contains? % :a2) spec: :spartan.spec-test/a
{:a1 1, :b1 2} - failed: (contains? % :b2) spec: :spartan.spec-test/b
")
         (str/trim (with-out-str (s/explain ::ab {:a1 1 :b1 2})))))
  (is (= '(merge :spartan.spec-test/a :spartan.spec-test/b)
         (s/describe (s/merge ::a ::b)))))

(deftest keys*-test
  (s/def ::a* (s/keys* :req-un [::a*1 ::a*2]))
  (is (= {:a*1 1, :a*2 2} (s/conform ::a* [:a*1 1 :a*2 2])))
  (is (s/describe (s/keys* :req-un [::a*1 ::a*2]))))

(deftest nilable-test
  (is (s/valid? (s/nilable int?) nil))
  (is (= {:i 1} (s/conform (s/cat :i (s/nilable int?)) [1])))
  (is (= {:i nil} (s/conform (s/cat :i (s/nilable int?)) [nil])))
  (is (= (str/trim "
:foo - failed: int? in: [0] at: [:i :clojure.spec.alpha/pred]
:foo - failed: nil? in: [0] at: [:i :clojure.spec.alpha/nil]\n")
         (str/trim (with-out-str (s/explain (s/cat :i (s/nilable int?)) [:foo])))))
  (is (= '(nilable int?) (s/describe (s/nilable int?)))))

(deftest every-test
  (is (= '[1 2 3] (s/conform (s/every int?) [1 2 3])))
  (is (s/invalid? (s/conform (s/every int?) [1 2 "a"])))
  (is (= "\"a\" - failed: int? in: [2]\n" (with-out-str (s/explain (s/every int?) [1 2 "a"]))))
  (is (= '(every int?) (s/describe (s/every int?)))))

(deftest every-kv-test
  (is (= '{:foo "foo" :bar "bar"}
         (s/conform (s/every-kv keyword? string?) {:foo "foo" :bar "bar"})))
  (is (s/invalid? (s/conform (s/every-kv keyword? string?) {:foo 1 :bar "bar"})))
  (is (= "1 - failed: string? in: [:foo 1] at: [1]\n"
         (with-out-str (s/explain (s/every-kv keyword? string?) {:foo 1 :bar "bar"}))))
  (is (= '(every-kv keyword? string?) (s/describe (s/every-kv keyword? string?)))))

(deftest coll-of-test
  (is (= '[1 2 3] (s/conform (s/coll-of int?) [1 2 3])))
  (is (s/invalid? (s/conform (s/coll-of int?) [1 2 "a"])))
  (is (= "\"a\" - failed: int? in: [2]\n" (with-out-str (s/explain (s/coll-of int?) [1 2 "a"]))))
  (is (= '(coll-of int?) (s/describe (s/coll-of int?)))))

(deftest fn-literal-in-spec-test
  (s/def ::kws (s/and keyword? #(= (namespace %) "my.domain")))
  (is (s/valid? ::kws :my.domain/name))
  (is (= ":foo - failed: (= (namespace %) \"my.domain\") spec: :spartan.spec-test/kws\n"
         (with-out-str (s/explain ::kws :foo))))
  (is (= '(and keyword? (= (namespace %) "my.domain")) (s/describe (s/and keyword? #(= (namespace %) "my.domain"))))))

(deftest assert-test
  (s/check-asserts true)
  (is (re-find #"(?s)assertion failed.*int\?"
               (try (s/assert int? "foo")
                    (catch clojure.lang.ExceptionInfo e
                      (ex-message e))))))

(deftest explain-str-test
  (s/def ::str string?)
  (is (= "1 - failed: string? spec: :spartan.spec-test/str\n" (s/explain-str ::str 1))))

(deftest int-in-test
  (s/def ::int-in (s/int-in 0 10))
  (is (s/valid? ::int-in 9))
  (is (= "11 - failed: (and int? (fn* [%1] (int-in-range? 0 10 %1))) spec: :spartan.spec-test/int-in\n" (s/explain-str ::int-in 11))))
