(ns spartan.spec-guide)

(require 'spartan.spec) ;; side effect

(require '[clojure.spec.alpha :as s])

(s/conform even? 1000)
(s/valid? even? 10)

(s/def ::date inst?)
(s/def ::suit #{:club :diamond :heart :spade})

(s/valid? ::date (java.util.Date.))
(s/conform ::suit :club)

;; (doc ::date)

(s/def ::big-even (s/and int? even? #(> % 1000)))
(s/valid? ::big-even :foo) ;; false
(s/valid? ::big-even 10) ;; false
(s/valid? ::big-even 100000) ;; true

(s/def ::name-or-id (s/or :name string?
                          :id   int?))
(s/valid? ::name-or-id "abc") ;; true
(s/valid? ::name-or-id 100) ;; true
(s/valid? ::name-or-id :foo) ;; false

(s/conform ::name-or-id "abc")
;;=> [:name "abc"]
(s/conform ::name-or-id 100)
;;=> [:id 100]

(s/valid? string? nil)
;;=> false
(s/valid? (s/nilable string?) nil)
;;=> true

(s/explain ::suit 42)
;; 42 - failed: #{:spade :heart :diamond :club} spec: :user/suit
(s/explain ::big-even 5)
;; 5 - failed: even? spec: :user/big-even
(s/explain ::name-or-id :foo)
;; :foo - failed: string? at: [:name] spec: :user/name-or-id
;; :foo - failed: int? at: [:id] spec: :user/name-or-id

(s/explain-data ::name-or-id :foo)

(ns my.domain (:require [spartan.spec :as s]))
(def email-regex #"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,63}$")
(s/def ::email-type (s/and string? #(re-matches email-regex %)))

(s/def ::acctid int?)
(s/def ::first-name string?)
(s/def ::last-name string?)
(s/def ::email ::email-type)

(s/def ::person (s/keys :req [::first-name ::last-name ::email]
                        :opt [::phone]))

(s/valid? ::person
          {::first-name "Bugs"
           ::last-name "Bunny"
           ::email "bugs@example.com"})
;;=> true

;; Fails required key check
(s/explain ::person
           {::first-name "Bugs"})
;; #:my.domain{:first-name "Bugs"} - failed: (contains? % :my.domain/last-name)
;;   spec: :my.domain/person
;; #:my.domain{:first-name "Bugs"} - failed: (contains? % :my.domain/email)
;;   spec: :my.domain/person

;; Fails attribute conformance
(s/explain ::person
           {::first-name "Bugs"
            ::last-name "Bunny"
            ::email "n/a"})
;; "n/a" - failed: (re-matches email-regex %) in: [:my.domain/email]
;;   at: [:my.domain/email] spec: :my.domain/email-type

(s/def :unq/person
  (s/keys :req-un [::first-name ::last-name ::email]
          :opt-un [::phone]))

(s/conform :unq/person
           {:first-name "Bugs"
            :last-name "Bunny"
            :email "bugs@example.com"})
;;=> {:first-name "Bugs", :last-name "Bunny", :email "bugs@example.com"}

(s/explain :unq/person
           {:first-name "Bugs"
            :last-name "Bunny"
            :email "n/a"})
;; "n/a" - failed: (re-matches email-regex %) in: [:email] at: [:email]
;;   spec: :my.domain/email-type

(s/explain :unq/person
           {:first-name "Bugs"})
;; {:first-name "Bugs"} - failed: (contains? % :last-name) spec: :unq/person
;; {:first-name "Bugs"} - failed: (contains? % :email) spec: :unq/person

(s/def ::port number?)
(s/def ::host string?)
(s/def ::id keyword?)
(s/def ::server
  (s/keys* :req [::id ::host] :opt [::port]))
(prn ">" (s/conform ::server [::id :s1 ::host "example.com" ::port 5555]))
;;=> {:my.domain/id :s1, :my.domain/host "example.com", :my.domain/port 5555}

(s/def :animal/kind string?)
(s/def :animal/says string?)
(s/def :animal/common (s/keys :req [:animal/kind :animal/says]))
(s/def :dog/tail? boolean?)
(s/def :dog/breed string?)
(s/def :animal/dog (constantly true)
  ;; TODO:
  #_(s/merge :animal/common
           (s/keys :req [:dog/tail? :dog/breed])))
(s/valid? :animal/dog
          {:animal/kind "dog"
           :animal/says "woof"
           :dog/tail? true
           :dog/breed "retriever"})

(s/conform (s/coll-of keyword?) [:a :b :c])
;;=> [:a :b :c]
(s/conform (s/coll-of number?) #{5 10 2})
;;=> #{2 5 10}

(s/def ::vnum3 (s/coll-of number? :kind vector? :count 3 :distinct true :into #{}))
(s/conform ::vnum3 [1 2 3])
;;=> #{1 2 3}
(s/explain ::vnum3 #{1 2 3})   ;; not a vector
;; #{1 3 2} - failed: vector? spec: :user/vnum3
(s/explain ::vnum3 [1 1 1])    ;; not distinct
;; [1 1 1] - failed: distinct? spec: :user/vnum3
(s/explain ::vnum3 [1 2 :a])   ;; not a number
;; :a - failed: number? in: [2] spec: :user/vnum3

(s/def ::point (s/tuple double? double? double?))
(s/conform ::point [1.5 2.5 -0.5])
;; => [1.5 2.5 -0.5]

(s/def ::scores (s/map-of string? int?))
(s/conform ::scores {"Sally" 1000, "Joe" 500})
;;=> {"Sally" 1000, "Joe" 500}

(s/def ::ingredient (s/cat :quantity number? :unit keyword?))
(s/conform ::ingredient [2 :teaspoon])
;;=> {:quantity 2, :unit :teaspoon}

;; pass string for unit instead of keyword
(s/explain ::ingredient [11 "peaches"])
;; "peaches" - failed: keyword? in: [1] at: [:unit] spec: :user/ingredient

;; leave out the unit
(s/explain ::ingredient [2])
;; () - failed: Insufficient input at: [:unit] spec: :user/ingredient

(s/def ::seq-of-keywords (s/* keyword?))
(s/conform ::seq-of-keywords [:a :b :c])
;;=> [:a :b :c]
(s/explain ::seq-of-keywords [10 20])
;; 10 - failed: keyword? in: [0] spec: :user/seq-of-keywords

(s/def ::odds-then-maybe-even (s/cat :odds (s/+ odd?)
                                     :even (s/? even?)))
(s/conform ::odds-then-maybe-even [1 3 5 100])
;;=> {:odds [1 3 5], :even 100}
(s/conform ::odds-then-maybe-even [1])
;;=> {:odds [1]}
(s/explain ::odds-then-maybe-even [100])
;; 100 - failed: odd? in: [0] at: [:odds] spec: :user/odds-then-maybe-even

;; opts are alternating keywords and booleans
(s/def ::opts (s/* (s/cat :opt keyword? :val boolean?)))
(s/conform ::opts [:silent? false :verbose true])
;;=> [{:opt :silent?, :val false} {:opt :verbose, :val true}]

(s/def ::config (s/*
                 (s/cat :prop string?
                        :val  (s/alt :s string? :b boolean?))))
(s/conform ::config ["-server" "foo" "-verbose" true "-user" "joe"])
;;=> [{:prop "-server", :val [:s "foo"]}
;;    {:prop "-verbose", :val [:b true]}
;;    {:prop "-user", :val [:s "joe"]}]

;; TODO
;; (s/describe ::seq-of-keywords)
;;=> (* keyword?)
;; (s/describe ::odds-then-maybe-even)
;;=> (cat :odds (+ odd?) :even (? even?))
;; (s/describe ::opts)
;;=> (* (cat :opt keyword? :val boolean?))

(s/def ::even-strings (s/& (s/* string?) #(even? (count %))))
(s/valid? ::even-strings ["a"])  ;; false
(s/valid? ::even-strings ["a" "b"])  ;; true
(s/valid? ::even-strings ["a" "b" "c"])  ;; false
(s/valid? ::even-strings ["a" "b" "c" "d"])  ;; true

(s/def ::nested
  (s/cat :names-kw #{:names}
         :names (s/spec (s/* string?))
         :nums-kw #{:nums}
         :nums (s/spec (s/* number?))))
(s/conform ::nested [:names ["a" "b"] :nums [1 2 3]])
;;=> {:names-kw :names, :names ["a" "b"], :nums-kw :nums, :nums [1 2 3]}

(s/def ::unnested
  (s/cat :names-kw #{:names}
         :names (s/* string?)
         :nums-kw #{:nums}
         :nums (s/* number?)))
(s/conform ::unnested [:names "a" "b" :nums 1 2 3])
;;=> {:names-kw :names, :names ["a" "b"], :nums-kw :nums, :nums [1 2 3]}

;; TODO: pre and post
#_(defn person-name
  [person]
  {:pre [(s/valid? ::person person)]
   :post [(s/valid? string? %)]}
  (str (::first-name person) " " (::last-name person)))

;; (person-name 42)
;;=> java.lang.AssertionError: Assert failed: (s/valid? :my.domain/person person)

;; (person-name {::first-name "Bugs" ::last-name "Bunny" ::email "bugs@example.com"})
;; Bugs Bunny


(defn person-name2
  [person]
  (let [p (s/assert ::person person)]
    (str (::first-name p) " " (::last-name p))))

(s/check-asserts true)
(person-name2 42)
;; Execution error - invalid arguments to my.domain/person-name at (REPL:3).
;; 100 - failed: map?
