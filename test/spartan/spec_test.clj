(ns spartan.spec-test
  (:require ;; [clojure.spec.alpha :as s]
   [spartan.spec :as s]
   [spartan.test :as t :refer [deftest is]]
   [clojure.string :as str]))

(deftest pred-test
  (is (= 1 (s/conform int? 1)))
  (is (s/invalid? (s/conform string? 1)))
  (is (= "--opts" (s/conform #{"--opts"} "--opts"))))

(deftest cat-test
  (is (= {:a 1, :b "foo"} (s/conform (s/cat :a int? :b string?) [1 "foo"])))
  (is (s/invalid? (s/conform (s/cat :a int? :b string?) [1 "foo" "bar"]))))

(deftest alt-test
  (is (= [:a 1] (s/conform (s/alt :a int? :b string?) [1])))
  (is (s/invalid? (s/conform (s/alt :a int? :b string?) 1))))

(deftest and-test
  (is (= 6 (s/conform (s/and number? #(> % 5)) 6)))
  (is (s/invalid? (s/conform (s/and number? #(> % 5)) 5))))

(deftest or-test
  (is (= [:a 1] (s/conform (s/or :a int? :b string?) 1)))
  (is (s/invalid? (s/conform (s/or :a int? :b string?) {:a 1}))))

(deftest *-test
  (is (= {:i [1 2 3]} (s/conform (s/cat :i (s/* number?)) [1 2 3])))
  (is (s/invalid? (s/conform (s/cat :i (s/* number?)) [1 2 3 "foo"])))
  (is (s/invalid? (s/conform (s/cat :i (s/* number?)) 1))))

(deftest ?-test
  (is (= {:i 1 :j 2} (s/conform (s/cat :i number? :j (s/? number?)) [1 2])))
  (is (= {:i 1} (s/conform (s/cat :i number? :j (s/? number?)) [1]))))

(deftest +-test
  (is (= {:j [1]} (s/conform (s/cat :j (s/+ number?)) [1])))
  (is (s/invalid? (s/conform (s/cat :j (s/+ number?)) []))))

(deftest def-test
  (is (true? (do (s/def ::int int?) (s/valid? ::int 1)))))

(deftest keys-test
  (s/def ::a (s/keys :req-un [::b ::c]))
  (s/def ::b (s/cat :i int? :j int?))
  (is (= {:c 2, :b {:i 1, :j 2}} (s/conform ::a {:b [1 2] :c 2}))))

(deftest nilable-test
  (is (s/valid? (s/nilable int?) nil))
  (is (= {:i 1} (s/conform (s/cat :i (s/nilable int?)) [1])))
  (is (= {:i nil} (s/conform (s/cat :i (s/nilable int?)) [nil]))))

(deftest every-test
  (is (= '[1 2 3] (s/conform (s/every int?) [1 2 3])))
  (is (s/invalid? (s/conform (s/every int?) [1 2 "a"]))))

(deftest every-kv-test
  (is (= '{:foo "foo" :bar "bar"}
         (s/conform (s/every-kv keyword? string?) {:foo "foo" :bar "bar"})))
  (is (s/invalid? (s/conform (s/every-kv keyword? string?) {:foo 1 :bar "bar"}))))

(deftest coll-of-test
  (is (= '[1 2 3] (s/conform (s/coll-of int?) [1 2 3])))
  (is (s/invalid? (s/conform (s/coll-of int?) [1 2 "a"]))))

(deftest fn-literal-in-spec-test
  (s/def ::kws (s/and keyword? #(= (namespace %) "my.domain")))
  (is (s/valid? ::kws :my.domain/name)))

(deftest explain-test
  (prn (s/explain int? "foo")))

(deftest assert-test
  (s/check-asserts true)
  (is (str/includes? (try (s/assert int? "foo")
                          (catch clojure.lang.ExceptionInfo e
                            (ex-message e)))
                     "assertion failed")))
