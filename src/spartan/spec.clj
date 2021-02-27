;;   This source contains code from
;;   https://github.com/clojure/spec.alpha/blob/master/src/main/clojure/clojure/spec/alpha.clj
;;   which is licensed as follows:

;;   Copyright (c) Rich Hickey. All rights reserved.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns spartan.spec)

(ns clojure.spec.gen.alpha)

(ns clojure.spec.alpha ;; DANGER!
  (:refer-clojure :exclude [+ * and assert or cat def keys merge])
  (:require [clojure.walk :as walk]
            [clojure.main :refer [demunge]]))

;; 22
(alias 'c 'clojure.core)

;; 36
(def ^:dynamic *coll-check-limit*
  "The number of elements validated in a collection spec'ed with 'every'"
  101)

(def ^:dynamic *coll-error-limit*
  "The number of errors reported by explain in a collection spec'ed with 'every'"
  20)

;; 52
(defonce ^:private registry-ref (atom {}))

;; 54
(defn- deep-resolve [reg k]
  (loop [spec k]
    (if (ident? spec)
      (recur (get reg spec))
      spec)))

;; 60
(defn- reg-resolve
  "returns the spec/regex at end of alias chain starting with k, nil if not found, k if k not ident"
  [k]
  (if (ident? k)
    (let [reg @registry-ref
          spec (get reg k)]
      (if-not (ident? spec)
        spec
        (deep-resolve reg spec)))
    k))

;; 71
(defn- reg-resolve!
  "returns the spec/regex at end of alias chain starting with k, throws if not found, k if k not ident"
  [k]
  (if (ident? k)
    (c/or (reg-resolve k)
          (throw (Exception. (str "Unable to resolve spec: " k))))
    k))

;; 79
(defn spec?
  "returns x if x is a spec object, else logical false"
  [x]
  (when (identical? ::spec (:type x))
    x))

;; 85
(defn regex?
  [x]
  (c/and (::op x) x))

;; 90
(defn- with-name [spec name]
  (cond
    (ident? spec) spec
    (regex? spec) (assoc spec ::name name)
    (instance? clojure.lang.IObj spec)
    (with-meta spec (assoc (meta spec) ::name name))
    :else spec))

;; 98
(defn- spec-name [spec]
  (cond
    (ident? spec) spec
    (regex? spec) (::name spec)
    (instance? clojure.lang.IObj spec)
    (-> (meta spec) ::name)))

;; 107
(declare spec-impl)
;; 108
(declare regex-spec-impl)

;; 110
(defn- maybe-spec
  "spec-or-k must be a spec, regex or resolvable kw/sym, else returns nil."
  [spec-or-k]
  (let [s (c/or (c/and (ident? spec-or-k) (reg-resolve spec-or-k))
                (spec? spec-or-k)
                (regex? spec-or-k)
                nil)]
    (if (regex? s)
      (with-name (regex-spec-impl s nil) (spec-name s))
      s)))

;; 121
(defn- the-spec
  "spec-or-k must be a spec, regex or kw/sym, else returns nil. Throws if unresolvable kw/sym"
  [spec-or-k]
  (c/or
   (maybe-spec spec-or-k)
   (when (ident? spec-or-k)
     (throw (Exception. (str "Unable to resolve spec: " spec-or-k))))))

;; 131
(defn- fn-sym [^Object f]
  (let [[_ f-ns f-n] (re-matches #"(.*)\$(.*?)(__[0-9]+)?" (.. f getClass getName))]
    ;; check for anonymous function
    (when (not= "fn" f-n)
      (symbol (demunge f-ns) (demunge f-n) #_(clojure.lang.Compiler/demunge f-ns) #_(clojure.lang.Compiler/demunge f-n)))))

(defn specize*
  ([x] (specize* x nil))
  ([x form]
   (cond (keyword? x) (specize* (reg-resolve! x))
         (symbol? x) (specize* (reg-resolve! x))
         (set? x) (spec-impl form x nil nil)
         (regex? x) (spec-impl form x nil nil)
         :else (if (c/and (not (map? x)) (ifn? x))
                 (if-let [s (fn-sym x)]
                   (spec-impl s x nil nil)
                   (spec-impl ::unknown x nil nil))
                 (spec-impl ::unknown x nil nil)))))

;; 158
(defn- specize
  ([s]
   (c/or (spec? s) (specize* s)))
  ([s form]
   (c/or (spec? s) (specize* s form))))

;; 162
(defn invalid?
  "tests the validity of a conform return value"
  [ret]
  (identical? ::invalid ret))

(declare re-conform)

(defn conform* [spec x]
  (cond (regex? spec)
        (if (c/or (nil? x) (sequential? x))
          (re-conform spec (seq x))
          ::invalid)
        (:cform spec)
        ((:cform spec) spec x)
        :else
        (throw (ex-info "No conform function implemented yet."
                        {:spec spec
                         :x x}))))

;; 167
(defn conform
  [spec x]
  (conform* (specize spec) x))

;; 173
(declare op-unform)

(defn unform* [spec x]
  (cond (regex? spec)
        (if (c/or (nil? x) (sequential? x))
          (op-unform spec (seq x))
          ::invalid)
        (:unform spec)
        ((:unform spec) spec x)
        :else
        (throw (ex-info "No unform function implemented yet."
                 {:spec spec
                  :x x}))))

(defn unform
  "Given a spec and a value created by or compliant with a call to
  'conform' with the same spec, returns a value with all conform
  destructuring undone."
  [spec x]
  (unform* (specize spec) x))

(defn describe* [spec]
  (if-let [d (:describe spec)]
    (d spec)
    (throw (ex-info "No describe function implemented yet."
                    {:spec spec}))))

;; 180
(defn form
  "returns the spec as data"
  [spec]
  ;;TODO - incorporate gens
  (describe* (specize spec)))

;; 186
(defn abbrev [form]
  (cond
    (seq? form)
    (walk/postwalk (fn [form]
                     (cond
                       (c/and (symbol? form) (namespace form))
                       (-> form name symbol)

                       (c/and (seq? form) (= 'fn (first form)) (= '[%] (second form)))
                       (last form)
                       :else form))
                   form)

    (c/and (symbol? form) (namespace form))
    (-> form name symbol)
    :else form))

;; 205:
(defn describe
  "returns an abbreviated description of the spec as data"
  [spec]
  (abbrev (form spec)))

;; 210:
;; with-gen: TODO
(defmacro with-gen [& _args]
  (binding [*out* *err*]
    (prn "WARNING: spartan.spec doesn't have with-gen yet" (assoc (meta &form) :file *file*))))

(defn explain* [spec path via in x]
  (let [{explain-f :explain} spec]
    (if explain-f
      (explain-f spec path via in x)
      (throw (ex-info "No explain function implemented yet."
                      {:spec spec
                       :path path
                       :via via
                       :in :in
                       :x x})))))

;; 218:
(defn explain-data* [spec path via in x]
  (let [probs (explain* (specize spec) path via in x)]
    (when-not (empty? probs)
      {::problems probs
       ::spec spec
       ::value x})))

;; 225:
(defn explain-data
  "Given a spec and a value x which ought to conform, returns nil if x
  conforms, else a map with at least the key ::problems whose value is
  a collection of problem-maps, where problem-map has at least :path :pred and :val
  keys describing the predicate and the value that failed at that
  path."
  [spec x]
  (explain-data* spec [] (if-let [name (spec-name spec)] [name] []) [] x))

;; 234:
(defn explain-printer
  "Default printer for explain-data. nil indicates a successful validation."
  [ed]
  (if ed
    (let [problems (->> (::problems ed)
                        (sort-by #(- (count (:in %))))
                        (sort-by #(- (count (:path %)))))]
      (doseq [{:keys [path pred val reason via in] :as prob} problems]
        (pr val)
        (print " - failed: ")
        (if reason (print reason) (pr (abbrev pred)))
        (when-not (empty? in)
          (print (str " in: " (pr-str in))))
        (when-not (empty? path)
          (print (str " at: " (pr-str path))))
        (when-not (empty? via)
          (print (str " spec: " (pr-str (last via)))))
        (doseq [[k v] prob]
          (when-not (#{:path :pred :val :reason :via :in} k)
            (print "\n\t" (pr-str k) " ")
            (pr v)))
        (newline)))
    (println "Success!")))

;; 259:
(def ^:dynamic *explain-out* explain-printer)

;; 261:
(defn explain-out
  "Prints explanation data (per 'explain-data') to *out* using the printer in *explain-out*,
   by default explain-printer."
  [ed]
  (*explain-out* ed))

;; 267:
(defn explain
  "Given a spec and a value that fails to conform, prints an explanation to *out*."
  [spec x]
  (explain-out (explain-data spec x)))

;; 272:
(defn explain-str
  "Given a spec and a value that fails to conform, returns an explanation as a string."
  ^String [spec x]
  (with-out-str (explain spec x)))

;; 277
(declare valid?)

;; 279
;; gensub-: TODO

;; 292
;; gen: TODO

;; 305
(defn- ->sym
  "Returns a symbol from a symbol or var"
  [x]
  (cond (var? x)
        (let [m (meta x)]
          (symbol (str (ns-name (:ns m))) (str (:name m))))
        (fn? x) (fn-sym x)
        :else x))
;; 314
(defn- unfn [expr]
  (if (c/and (seq? expr)
             (symbol? (first expr))
             (= "fn*" (name (first expr))))
    (let [[[s] & form] (rest expr)]
      (conj (walk/postwalk-replace {s '%} form) '[%] 'fn))
    expr))

;; 322
(defn- res [form]
  (cond
    (keyword? form) form
    (symbol? form) (c/or (-> form resolve ->sym) form)
    (sequential? form) (walk/postwalk #(if (symbol? %) (res %) %) (unfn form))
    :else form))

;; 329
(defn def-impl
  "Do not call this directly, use 'def'"
  [k form spec]
  (c/assert (c/and (ident? k) (namespace k)) "k must be namespaced keyword or resolvable symbol")
  (if (nil? spec)
    (swap! registry-ref dissoc k)
    (let [spec (if (c/or (spec? spec) (regex? spec) (get @registry-ref spec))
                 spec
                 (spec-impl form spec nil nil))]
      (swap! registry-ref assoc k (with-name spec k))))
  k)

;; 341
(defn- ns-qualify
    "Qualify symbol s by resolving it or using the current *ns*."
    [s]
    (if-let [ns-sym (some-> s namespace symbol)]
      (c/or (some-> (get (ns-aliases *ns*) ns-sym) str (symbol (name s)))
                       s)
      (symbol (str (ns-name *ns*)) (str s))))

;; 349
(defmacro def
  "Given a namespace-qualified keyword or resolvable symbol k, and a
  spec, spec-name, predicate or regex-op makes an entry in the
  registry mapping k to the spec. Use nil to remove an entry in
  the registry for k."
  [k spec-form]
  (let [k (if (symbol? k) (ns-qualify k) k)]
    `(def-impl '~k '~(res spec-form) ~spec-form)))

;; 358
(defn registry
  "returns the registry map, prefer 'get-spec' to lookup a spec by name"
  []
  @registry-ref)

;; 363
(defn get-spec
  "Returns spec registered for keyword/symbol/var k, or nil."
  [k]
  (get (registry) (if (keyword? k) k (->sym k))))

;; 368
(defmacro spec
  [form & {:keys [gen]}]
  (when form
    `(spec-impl '~(res form) ~form ~gen nil)))

;; 387
(defmacro multi-spec [mm retag]
  `(multi-spec-impl ~mm ~retag))

;; 416
(defmacro keys
  [& {:keys [req req-un opt opt-un gen]}]
  (let [unk #(-> % name keyword)
        req-keys (filterv keyword? (flatten req))
        req-un-specs (filterv keyword? (flatten req-un))
        _ (c/assert (every? #(c/and (keyword? %) (namespace %)) (concat req-keys req-un-specs opt opt-un))
                               "all keys must be namespace-qualified keywords")
        req-specs (into req-keys req-un-specs)
        req-keys (into req-keys (map unk req-un-specs))
        opt-keys (into (vec opt) (map unk opt-un))
        opt-specs (into (vec opt) opt-un)
        gx (gensym)
        parse-req (fn [rk f]
                    (map (fn [x]
                           (if (keyword? x)
                             `(contains? ~gx ~(f x))
                             (walk/postwalk
                              (fn [y] (if (keyword? y) `(contains? ~gx ~(f y)) y))
                              x)))
                         rk))
        pred-exprs [`(map? ~gx)]
        pred-exprs (into pred-exprs (parse-req req identity))
        pred-exprs (into pred-exprs (parse-req req-un unk))
        keys-pred `(fn* [~gx] (c/and ~@pred-exprs))
        pred-exprs (mapv (fn [e] `(fn* [~gx] ~e)) pred-exprs)
        pred-forms (walk/postwalk res pred-exprs)]
    `(map-spec-impl {:req '~req :opt '~opt :req-un '~req-un :opt-un '~opt-un
                     :req-keys '~req-keys :req-specs '~req-specs
                     :opt-keys '~opt-keys :opt-specs '~opt-specs
                     :pred-forms '~pred-forms
                     :pred-exprs ~pred-exprs
                     :keys-pred ~keys-pred
                     :gfn ~gen})))

;; 478
(defmacro or
  "Takes key+pred pairs, e.g.
  (s/or :even even? :small #(< % 42))
  Returns a destructuring spec that returns a map entry containing the
  key of the first matching pred and the corresponding value. Thus the
  'key' and 'val' functions can be used to refer generically to the
  components of the tagged return."
  [& key-pred-forms]
  (let [pairs (partition 2 key-pred-forms)
        keys (mapv first pairs)
        pred-forms (mapv second pairs)
        pf (mapv res pred-forms)]
    (c/assert (c/and (even? (count key-pred-forms)) (every? keyword? keys)) "spec/or expects k1 p1 k2 p2..., where ks are keywords")
    `(or-spec-impl ~keys '~pf ~pred-forms)))

;; 495
(defmacro and
  "Takes predicate/spec-forms, e.g.
  (s/and even? #(< % 42))
  Returns a spec that returns the conformed value. Successive
  conformed values propagate through rest of predicates."
  [& pred-forms]
  `(and-spec-impl '~(mapv res pred-forms) ~(vec pred-forms)))

;; 505
(defmacro merge
  "Takes map-validating specs (e.g. 'keys' specs) and
  returns a spec that returns a conformed map satisfying all of the
  specs.  Unlike 'and', merge can generate maps satisfying the
  union of the predicates."
  [& pred-forms]
  `(merge-spec-impl '~(mapv res pred-forms) ~(vec pred-forms) nil))

;; 513
(defn- res-kind
  [opts]
  (let [{kind :kind :as mopts} opts]
    (->>
     (if kind
       (assoc mopts :kind `~(res kind))
       mopts)
     (mapcat identity))))

;; 522
(defmacro every
  [pred & {:keys [into kind count max-count min-count distinct gen-max gen] :as opts}]
  (let [desc (::describe opts)
        nopts (-> opts
                  (dissoc :gen ::describe)
                  (assoc ::kind-form `'~(res (:kind opts))
                         ::describe (c/or desc `'(every ~(res pred) ~@(res-kind opts)))))
        gx (gensym)
        cpreds (cond-> [(list (c/or kind `coll?) gx)]
                 count (conj `(= ~count (bounded-count ~count ~gx)))

                 (c/or min-count max-count)
                 (conj `(<= (c/or ~min-count 0)
                            (bounded-count (if ~max-count (inc ~max-count) ~min-count) ~gx)
                            (c/or ~max-count Integer/MAX_VALUE)))

                 distinct
                 (conj `(c/or (empty? ~gx) (apply distinct? ~gx))))]
    `(every-impl '~pred ~pred ~(assoc nopts ::cpred `(fn* [~gx] (c/and ~@cpreds))) ~gen)))

;; 570
(defmacro every-kv
  "like 'every' but takes separate key and val preds and works on associative collections.
  Same options as 'every', :into defaults to {}
  See also - map-of"

  [kpred vpred & opts]
  (let [desc `(every-kv ~(res kpred) ~(res vpred) ~@(res-kind opts))]
    `(every (tuple ~kpred ~vpred) ::kfn (fn [i# v#] (nth v# 0)) :into {} ::describe '~desc ~@opts)))

;; 581
(defmacro coll-of
  "Returns a spec for a collection of items satisfying pred. Unlike
  'every', coll-of will exhaustively conform every value.
  Same options as 'every'. conform will produce a collection
  corresponding to :into if supplied, else will match the input collection,
  avoiding rebuilding when possible.
  See also - every, map-of"
  [pred & opts]
  (let [desc `(coll-of ~(res pred) ~@(res-kind opts))]
    `(every ~pred ::conform-all true ::describe '~desc ~@opts)))

;; 594
(defmacro map-of
  "Returns a spec for a map whose keys satisfy kpred and vals satisfy
  vpred. Unlike 'every-kv', map-of will exhaustively conform every
  value.
  Same options as 'every', :kind defaults to map?, with the addition of:
  :conform-keys - conform keys as well as values (default false)
  See also - every-kv"
  [kpred vpred & opts]
  (let [desc `(map-of ~(res kpred) ~(res vpred) ~@(res-kind opts))]
    `(every-kv ~kpred ~vpred ::conform-all true :kind map? ::describe '~desc ~@opts)))

;; 609
(defmacro *
  "Returns a regex op that matches zero or more values matching
  pred. Produces a vector of matches iff there is at least one match"
  [pred-form]
  `(rep-impl '~(res pred-form) ~pred-form))

;; 615
(defmacro +
  "Returns a regex op that matches one or more values matching
  pred. Produces a vector of matches"
  [pred-form]
  `(rep+impl '~(res pred-form) ~pred-form))

;; 621
(defmacro ?
  "Returns a regex op that matches zero or one value matching
  pred. Produces a single value (not a collection) if matched."
  [pred-form]
  `(maybe-impl ~pred-form '~(res pred-form)))

;; 627
(defmacro alt
  "Takes key+pred pairs, e.g.
  (s/alt :even even? :small #(< % 42))
  Returns a regex op that returns a map entry containing the key of the
  first matching pred and the corresponding value. Thus the
  'key' and 'val' functions can be used to refer generically to the
  components of the tagged return"
  [& key-pred-forms]
  (let [pairs (partition 2 key-pred-forms)
        keys (mapv first pairs)
        pred-forms (mapv second pairs)
        pf (mapv res pred-forms)]
    (c/assert (c/and (even? (count key-pred-forms)) (every? keyword? keys)) "alt expects k1 p1 k2 p2..., where ks are keywords")
    `(alt-impl ~keys ~pred-forms '~pf)))

;; 644
(defmacro cat
  "Takes key+pred pairs, e.g.
  (s/cat :e even? :o odd?)
  Returns a regex op that matches (all) values in sequence, returning a map
  containing the keys of each pred and the corresponding value."
  [& key-pred-forms]
  ;; (prn key-pred-forms) ;; ok
  (let [pairs (partition 2 key-pred-forms)
        keys (mapv first pairs)
        pred-forms (mapv second pairs)
        pf (mapv res pred-forms)]
    (c/assert (c/and (even? (count key-pred-forms)) (every? keyword? keys)) "cat expects k1 p1 k2 p2..., where ks are keywords")
    ;; (prn "pf" pf) ;; too soon
    `(cat-impl ~keys ~pred-forms '~pf)))

;; 660
(defmacro &
  "takes a regex op re, and predicates. Returns a regex-op that consumes
  input as per re but subjects the resulting value to the
  conjunction of the predicates, and any conforming they might perform."
  [re & preds]
  (let [pv (vec preds)]
    `(amp-impl ~re '~(res re) ~pv '~(mapv res pv))))

;; 668
(defmacro conformer
  "takes a predicate function with the semantics of conform i.e. it should return either a
  (possibly converted) value or :clojure.spec.alpha/invalid, and returns a
  spec that uses it as a predicate/conformer. Optionally takes a
  second fn that does unform of result of first"
  ([f] `(spec-impl '(conformer ~(res f)) ~f nil true))
  ([f unf] `(spec-impl '(conformer ~(res f) ~(res unf)) ~f nil true ~unf)))

;; 676
(defmacro fspec
  "takes :args :ret and (optional) :fn kwargs whose values are preds
  and returns a spec whose conform/explain take a fn and validates it
  using generative testing. The conformed value is always the fn itself.
  See 'fdef' for a single operation that creates an fspec and
  registers it, as well as a full description of :args, :ret and :fn
  fspecs can generate functions that validate the arguments and
  fabricate a return value compliant with the :ret spec, ignoring
  the :fn spec if present.
  Optionally takes :gen generator-fn, which must be a fn of no args
  that returns a test.check generator."
  [& {:keys [args ret fn gen] :or {ret `any?}}]
  `(fspec-impl (spec ~args) '~(res args)
               (spec ~ret) '~(res ret)
               (spec ~fn) '~(res fn) ~gen))

;; 696
(defmacro tuple
  "takes one or more preds and returns a spec for a tuple, a vector
  where each element conforms to the corresponding pred. Each element
  will be referred to in paths using its ordinal."
  [& preds]
  (c/assert (seq preds))
  `(tuple-impl '~(mapv res preds) ~(vec preds)))

;; 704:
;; macroexpand-check: TODO

;; 716:
(defmacro fdef
  [fn-sym & specs]
  `(clojure.spec.alpha/def ~fn-sym (clojure.spec.alpha/fspec ~@specs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; impl ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 759
(defn- dt
  ([pred x form] (dt pred x form nil))
  ([pred x form cpred?]
   (if pred
     (if-let [spec (the-spec pred)]
       (conform spec x)
       (if (ifn? pred)
         (if cpred?
           (pred x)
           (if (pred x) x ::invalid))
         (throw (Exception. (str (pr-str form) " is not a fn, expected predicate fn")))))
     x)))

;; 772
(defn valid?
  "Helper function that returns true when x is valid for spec."
  ([spec x]
   (let [spec (specize spec)]
     (not (invalid? (conform* spec x)))))
  ([spec x form]
   (let [spec (specize spec form)]
     (not (invalid? (conform* spec x))))))

;; 781
(defn- pvalid?
  "internal helper function that returns true when x is valid for spec."
  ([pred x]
   (not (invalid? (dt pred x ::unknown))))
  ([pred x form]
   (not (invalid? (dt pred x form)))))

;; 788
(defn- explain-1 [form pred path via in v]
  ;;(prn {:form form :pred pred :path path :in in :v v})
  (let [pred (maybe-spec pred)]
    (if (spec? pred)
      (explain* pred path (if-let [name (spec-name pred)] (conj via name) via) in v)
      [{:path path :pred form :val v :via via :in in}])))

;; 842
(defn map-spec-impl
  "Do not call this directly, use 'spec' with a map argument"
  [{:keys [req-un opt-un keys-pred pred-exprs opt-keys req-specs req req-keys opt-specs pred-forms opt gfn]
    :as argm}]
  (let [k->s (zipmap (concat req-keys opt-keys) (concat req-specs opt-specs))
        keys->specnames #(c/or (k->s %) %)
        id (java.util.UUID/randomUUID)]
    {:type ::spec
     :id id
     :cform  (fn [_ m]
               (if (keys-pred m)
                 (let [reg (registry)]
                   (loop [ret m, [[k v] & ks :as keys] m]
                     (if keys
                       (let [sname (keys->specnames k)]
                         (if-let [s (get reg sname)]
                           (let [cv (conform s v)]
                             (if (invalid? cv)
                               ::invalid
                               (recur (if (identical? cv v) ret (assoc ret k cv))
                                      ks)))
                           (recur ret ks)))
                       ret)))
                 ::invalid))
     :unform  (fn [_ m]
                (let [reg (registry)]
                  (loop [ret m, [k & ks :as keys] (c/keys m)]
                    (if keys
                      (if (contains? reg (keys->specnames k))
                        (let [cv (get m k)
                              v (unform (keys->specnames k) cv)]
                          (recur (if (identical? cv v) ret (assoc ret k v))
                            ks))
                        (recur ret ks))
                      ret))))
     :explain (fn [_ path via in x]
               (if-not (map? x)
                 [{:path path :pred `map? :val x :via via :in in}]
                 (let [reg (registry)]
                   (apply concat
                          (when-let [probs (->> (map (fn [pred form] (when-not (pred x) form))
                                                     pred-exprs pred-forms)
                                                (keep identity)
                                                seq)]
                            (map
                             #(identity {:path path :pred % :val x :via via :in in})
                             probs))
                          (map (fn [[k v]]
                                 (when-not (c/or (not (contains? reg (keys->specnames k)))
                                                            (pvalid? (keys->specnames k) v k))
                                   (explain-1 (keys->specnames k) (keys->specnames k) (conj path k) via (conj in k) v)))
                               (seq x))))))
     :describe (fn [_] (cons `keys
                             (cond-> []
                               req (conj :req req)
                               opt (conj :opt opt)
                               req-un (conj :req-un req-un)
                               opt-un (conj :opt-un opt-un))))}))

;; 915
(defn ^:skip-wiki spec-impl
  "Do not call this directly, use 'spec'"
  ([form pred gfn cpred?] (spec-impl form pred gfn cpred? nil))
  ([form pred gfn cpred? unc]
     (cond
      (spec? pred) pred ;; (cond-> pred gfn (with-gen gfn))
      (regex? pred) (regex-spec-impl pred gfn)
      (ident? pred) (the-spec pred) #_(cond-> (the-spec pred) gfn (with-gen gfn))
      :else
      {:type ::spec
       :form form
       :cform (fn [_ x] (let [ret (pred x)]
                          (if cpred?
                            ret
                            (if ret x ::invalid))))
       :unform (fn [_ x]
                 (if cpred?
                   (if unc
                     (unc x)
                     (throw (Exception. "no unform fn for conformer")))
                   x))
       :explain (fn [_ path via in x]
                  (when (invalid? (dt pred x form cpred?))
                    [{:path path :pred form :val x :via via :in in}]))
       :describe (fn [_] form)})))
;;; 948
(defn multi-spec-impl [mm retag]
  (let [id (gensym)
        predx #(let [mm mm]
                 (mm %))]
    {:type   ::spec
     :cform  (fn [_ x]
               (if-let [pred (predx x)]
                 (dt pred x nil #_form)
                 ::invalid))
     :unform (fn [_ x]
               (if-let [pred (predx x)]
                         (unform pred x)
                         (throw (Exception. (str "No method of: " form " for dispatch value: " #_ (dval x))))))}))
;; 998
(defn ^:skip-wiki tuple-impl
  "Do not call this directly, use 'tuple'"
  ([forms preds] (tuple-impl forms preds nil))
  ([forms preds gfn]
     (let [specs (delay (mapv specize preds forms))
           cnt (count preds)]
       {:type ::spec
        :cform (fn [_ x]
                 (let [specs @specs]
                   (if-not (c/and (vector? x)
                                             (= (count x) cnt))
                     ::invalid
                     (loop [ret x, i 0]
                       (if (= i cnt)
                         ret
                         (let [v (x i)
                               cv (conform* (specs i) v)]
                           (if (invalid? cv)
                             ::invalid
                             (recur (if (identical? cv v) ret (assoc ret i cv))
                                    (inc i)))))))))
        :unform (fn [_ x]
                  (c/assert (c/and (vector? x)
                                   (= (count x) (count preds))))
                  (loop [ret x, i 0]
                    (if (= i (count x))
                      ret
                      (let [cv (x i)
                            v (unform (preds i) cv)]
                        (recur (if (identical? cv v) ret (assoc ret i v))
                          (inc i))))))
        :explain (fn [_ path via in x]
                   (cond
                     (not (vector? x))
                     [{:path path :pred `vector? :val x :via via :in in}]

                     (not= (count x) (count preds))
                     [{:path path :pred `(= (count ~'%) ~(count preds)) :val x :via via :in in}]

                     :else
                     (apply concat
                            (map (fn [i form pred]
                                   (let [v (x i)]
                                     (when-not (pvalid? pred v)
                                       (explain-1 form pred (conj path i) via (conj in i) v))))
                                 (range (count preds)) forms preds))))})))

;; 1060
(defn- tagged-ret [tag ret]
  (clojure.lang.MapEntry. tag ret))

;; 1063
(defn or-spec-impl
  [keys forms preds]
  (let [id (java.util.UUID/randomUUID)
        kps (zipmap keys preds)
        specs (delay (mapv specize preds forms))
        cform (case (count preds)
                2 (fn [x]
                    (let [ret  (let [specs @specs
                                    ret (conform* (specs 0) x)]
                                (if (invalid? ret)
                                  (let [ret (conform* (specs 1) x)]
                                    (if (invalid? ret)
                                      ::invalid
                                      (tagged-ret (keys 1) ret)))
                                  (tagged-ret (keys 0) ret)))]
                      ret))
                3 (fn [x]
                    (let [specs @specs
                          ret (conform* (specs 0) x)]
                      (if (invalid? ret)
                        (let [ret (conform* (specs 1) x)]
                          (if (invalid? ret)
                            (let [ret (conform* (specs 2) x)]
                              (if (invalid? ret)
                                ::invalid
                                (tagged-ret (keys 2) ret)))
                            (tagged-ret (keys 1) ret)))
                        (tagged-ret (keys 0) ret))))
                (fn [x]
                  (let [specs @specs]
                    (loop [i 0]
                      (if (< i (count specs))
                        (let [spec (specs i)]
                          (let [ret (conform* spec x)]
                            (if (invalid? ret)
                              (recur (inc i))
                              (tagged-ret (keys i) ret))))
                        ::invalid)))))]
    {:type ::spec
     :id id
     :cform (fn [_ x] (cform x))
     :unform (fn [_ [k x]] (unform (kps k) x))
     :explain (fn [this path via in x]
                (when-not (pvalid? this x)
                  (apply concat
                         (map (fn [k form pred]
                                (when-not (pvalid? pred x)
                                  (explain-1 form pred (conj path k) via in x)))
                              keys forms preds))))
     :describe (fn [_] `(or ~@(mapcat vector keys forms)))}))

;; 1130
(defn- and-preds [x preds forms]
  (loop [ret x
         [pred & preds] preds
         [form & forms] forms]
    (if pred
      (let [nret (dt pred ret form)]
        (if (invalid? nret)
          ::invalid
          ;;propagate conformed values
          (recur nret preds forms)))
      ret)))

;; 1142
(defn- explain-pred-list
  [forms preds path via in x]
  (loop [ret x
         [form & forms] forms
         [pred & preds] preds]
    (when pred
      (let [nret (dt pred ret form)]
        (if (invalid? nret)
          (explain-1 form pred path via in ret)
          (recur nret forms preds))))))

;; 1153
(defn ^:skip-wiki and-spec-impl
  "Do not call this directly, use 'and'"
  [forms preds]
  (let [specs (delay (mapv specize preds forms))
        cform
        (case (count preds)
              2 (fn [x]
                  (let [specs @specs
                        ret (conform* (specs 0) x)]
                    (if (invalid? ret)
                      ::invalid
                      (conform* (specs 1) ret))))
              3 (fn [x]
                  (let [specs @specs
                        ret (conform* (specs 0) x)]
                    (if (invalid? ret)
                      ::invalid
                      (let [ret (conform* (specs 1) ret)]
                        (if (invalid? ret)
                          ::invalid
                          (conform* (specs 2) ret))))))
              (fn [x]
                (let [specs @specs]
                  (loop [ret x i 0]
                    (if (< i (count specs))
                      (let [nret (conform* (specs i) ret)]
                        (if (invalid? nret)
                          ::invalid
                          ;;propagate conformed values
                          (recur nret (inc i))))
                      ret)))))]
    {:type ::spec
     :cform (fn [_ x] (cform x))
     :unform (fn [_ x] (reduce #(unform %2 %1) x (reverse preds)))
     :explain (fn [_ path via in x] (explain-pred-list forms preds path via in x))
     :describe (fn [_] `(and ~@forms))}))

;; 1197
(defn ^:skip-wiki merge-spec-impl
  "Do not call this directly, use 'merge'"
  [forms preds gfn]
  {:type ::spec
   :cform (fn [_ x] (let [ms (map #(dt %1 x %2) preds forms)]
                      (if (some invalid? ms)
                        ::invalid
                        (apply c/merge ms))))
   :unform (fn [_ x] (apply c/merge (map #(unform % x) (reverse preds))))
   :explain (fn [_ path via in x]
              (apply concat
                     (map #(explain-1 %1 %2 path via in x)
                          forms preds)))
   :describe (fn [_] `(merge ~@forms))})

;; 1255
(defn- coll-prob [x kfn kform distinct count min-count max-count
                  path via in]
  (let [pred (c/or kfn coll?)
        kform (c/or kform `coll?)]
    (cond
      (not (pvalid? pred x))
      (explain-1 kform pred path via in x)

      (c/and count (not= count (bounded-count count x)))
      [{:path path :pred `(= ~count (c/count ~'%)) :val x :via via :in in}]

      (c/and (c/or min-count max-count)
             (not (<= (c/or min-count 0)
                      (bounded-count (if max-count (inc max-count) min-count) x)
                      (c/or max-count Integer/MAX_VALUE))))
      [{:path path :pred `(<= ~(c/or min-count 0) (c/count ~'%) ~(c/or max-count 'Integer/MAX_VALUE)) :val x :via via :in in}]
      (c/and distinct (seq x) (not (apply distinct? x)))
      [{:path path :pred 'distinct? :val x :via via :in in}])))

;; 1245
(def ^:private empty-coll {`vector? [], `set? #{}, `list? (), `map? {}})

;; 1247
(defn every-impl
  "Do not call this directly, use 'every', 'every-kv', 'coll-of' or 'map-of'"
  ([form pred opts] (every-impl form pred opts nil))
  ([form pred {conform-into :into
               describe-form ::describe
               :keys [kind ::kind-form count max-count min-count distinct gen-max ::kfn ::cpred
                      conform-keys ::conform-all]
               :or {gen-max 20}
               :as opts}
    gfn]
     (let [;; gen-into (if conform-into (empty conform-into) (get empty-coll kind-form))
           spec (delay (specize pred))
           check? #(valid? @spec %)
           kfn (c/or kfn (fn [i v] i))
           addcv (fn [ret i v cv] (conj ret cv))
           cfns (fn [x]
                  ;;returns a tuple of [init add complete] fns
                  (cond
                   (c/and (vector? x) (c/or (not conform-into) (vector? conform-into)))
                   [identity
                    (fn [ret i v cv]
                      (if (identical? v cv)
                        ret
                        (assoc ret i cv)))
                    identity]

                   (c/and (map? x) (c/or (c/and kind (not conform-into)) (map? conform-into)))
                   [(if conform-keys empty identity)
                    (fn [ret i v cv]
                      (if (c/and (identical? v cv) (not conform-keys))
                        ret
                        (assoc ret (nth (if conform-keys cv v) 0) (nth cv 1))))
                    identity]

                   (c/or (list? conform-into) (seq? conform-into) (c/and (not conform-into) (c/or (list? x) (seq? x))))
                   [(constantly ()) addcv reverse]

                   :else [#(empty (c/or conform-into %)) addcv identity]))]
       {:type ::spec
        :cform (fn [_ x]
                 (let [spec @spec]
                   (cond
                     (not (cpred x)) ::invalid

                     conform-all
                     (let [[init add complete] (cfns x)]
                       (loop [ret (init x), i 0, [v & vs :as vseq] (seq x)]
                         (if vseq
                           (let [cv (conform* spec v)]
                             (if (invalid? cv)
                               ::invalid
                               (recur (add ret i v cv) (inc i) vs)))
                           (complete ret))))
                     :else
                     (if (indexed? x)
                       (let [step (max 1 (long (/ (c/count x) *coll-check-limit*)))]
                         (loop [i 0]
                           (if (>= i (c/count x))
                             x
                             (if (valid? spec (nth x i))
                               (recur (c/+ i step))
                               ::invalid))))
                       (let [limit *coll-check-limit*]
                         (loop [i 0 [v & vs :as vseq] (seq x)]
                           (cond
                             (c/or (nil? vseq) (= i limit)) x
                             (valid? spec v) (recur (inc i) vs)
                             :else ::invalid)))))))
        :unform (fn [_ x]
                  (if conform-all
                    (let [spec @spec
                          [init add complete] (cfns x)]
                      (loop [ret (init x), i 0, [v & vs :as vseq] (seq x)]
                        (if (>= i (c/count x))
                          (complete ret)
                          (recur (add ret i v (unform* spec v)) (inc i) vs))))
                    x))
        :explain (fn [_ path via in x]
                   (c/or (coll-prob x kind kind-form distinct count min-count max-count
                                               path via in)
                                    (apply concat
                                           ((if conform-all identity (partial take *coll-error-limit*))
                                            (keep identity
                                                  (map (fn [i v]
                                                         (let [k (kfn i v)]
                                                           (when-not (check? v)
                                                             (let [prob (explain-1 form pred path via (conj in k) v)]
                                                               prob))))
                                                       (range) x))))))
        :describe (fn [_] (c/or describe-form `(every ~(res form) ~@(mapcat identity opts))))})))


;; 1382
(defn- accept [x] {::op ::accept :ret x})

;; 1384
(defn- accept? [{:keys [::op]}]
  (= ::accept op))

;; 1387
(defn- pcat* [{[p1 & pr :as ps] :ps,  [k1 & kr :as ks] :ks, [f1 & fr :as forms] :forms, ret :ret, rep+ :rep+}]
  (when (every? identity ps)
    (if (accept? p1)
      (let [rp (:ret p1)
            ret (conj ret (if ks {k1 rp} rp))]
        (if pr
          (pcat* {:ps pr :ks kr :forms fr :ret ret})
          (accept ret)))
      {::op ::pcat, :ps ps, :ret ret, :ks ks, :forms forms :rep+ rep+})))

;; 1379
(defn- pcat [& ps] (pcat* {:ps ps :ret []}))

;; 1399
(defn cat-impl
  "Do not call this directly, use 'cat'"
  [ks ps forms]
  (pcat* {:ks ks, :ps ps, :forms forms, :ret {}}))

;; 1404
(defn- rep* [p1 p2 ret splice form]
  (when p1
    (let [r {::op ::rep, :p2 p2, :splice splice, :forms form :id (java.util.UUID/randomUUID)}]
      (if (accept? p1)
        (assoc r :p1 p2 :ret (conj ret (:ret p1)))
        (assoc r :p1 p1, :ret ret)))))

;; 1411
(defn rep-impl
  [form p] (rep* p p [] false form))

;; 1415
(defn rep+impl
  [form p]
  (pcat* {:ps [p (rep* p p [] true form)] :forms `[~form (* ~form)] :ret [] :rep+ form}))

;; 1420
(defn amp-impl
  [re re-form preds pred-forms]
  {::op ::amp :p1 re :amp re-form :ps preds :forms pred-forms})

;; 1425
(defn- filter-alt [ps ks forms f]
  (if (c/or ks forms)
    (let [pks (->> (map vector ps
                        (c/or (seq ks) (repeat nil))
                        (c/or (seq forms) (repeat nil)))
                   (filter #(-> % first f)))]
      [(seq (map first pks)) (when ks (seq (map second pks))) (when forms (seq (map #(nth % 2) pks)))])
    [(seq (filter f ps)) ks forms]))

;; 1434
(defn- alt* [ps ks forms]
  (let [[[p1 & pr :as ps] [k1 :as ks] forms] (filter-alt ps ks forms identity)]
    (when ps
      (let [ret {::op ::alt, :ps ps, :ks ks :forms forms}]
        (if (nil? pr)
          (if k1
            (if (accept? p1)
              (accept (tagged-ret k1 (:ret p1)))
              ret)
            p1)
          ret)))))

;; 1446
(defn- alts [& ps] (alt* ps nil nil))
;; 1447
(defn- alt2 [p1 p2] (if (c/and p1 p2) (alts p1 p2) (c/or p1 p2)))

;; 1449
(defn alt-impl
  "Do not call this directly, use 'alt'"
  [ks ps forms] (assoc (alt* ps ks forms) :id (java.util.UUID/randomUUID)))

;; 1453
(defn maybe-impl
  [p form] (assoc (alt* [p (accept ::nil)] nil [form ::nil]) :maybe form))

;; 1457
(defn- noret? [p1 pret]
  (c/or (= pret ::nil)
                   (c/and (#{::rep ::pcat} (::op (reg-resolve! p1))) ;;hrm, shouldn't know these
                                     (empty? pret))
                   nil))

;; 1463
(declare preturn)

;; 1465
(defn- accept-nil? [p]
  (let [{:keys [::op ps p1 p2 forms] :as p} (reg-resolve! p)]
    (case op
      ::accept true
      nil nil
      ::amp (c/and (accept-nil? p1)
                              (let [ret (-> (preturn p1) (and-preds ps (next forms)))]
                                (not (invalid? ret))))
      ::rep (c/or (identical? p1 p2) (accept-nil? p1))
      ::pcat (every? accept-nil? ps)
      ::alt (c/some accept-nil? ps))))

;; 1477
(declare add-ret)

(defn- and-preds [x preds forms]
  (loop [ret x
         [pred & preds] preds
         [form & forms] forms]
    (if pred
      (let [nret (dt pred ret form)]
        (if (invalid? nret)
          ::invalid
          ;;propagate conformed values
          (recur nret preds forms)))
      ret)))

;; 1479
(defn- preturn [p]
  (let [{[p0 & pr :as ps] :ps, [k :as ks] :ks, :keys [::op p1 ret forms] :as p} (reg-resolve! p)]
    (case op
      ::accept ret
      nil nil
      ::amp (let [pret (preturn p1)]
              (if (noret? p1 pret)
                ::nil
                (and-preds pret ps forms)))
      ::rep (add-ret p1 ret k)
      ::pcat (add-ret p0 ret k)
      ::alt (let [[[p0] [k0]] (filter-alt ps ks forms accept-nil?)
                  r (if (nil? p0) ::nil (preturn p0))]
              (if k0 (tagged-ret k0 r) r)))))

; 1487
(defn- op-unform [p x]
  ;;(prn {:p p :x x})
  (let [{[p0 & pr :as ps] :ps, [k :as ks] :ks, :keys [::op p1 ret forms rep+ maybe] :as p} (reg-resolve! p)
        kps (zipmap ks ps)]
    (case op
      ::accept [ret]
      nil [(unform p x)]
      ::amp (let [px (reduce #(unform %2 %1) x (reverse ps))]
              (op-unform p1 px))
      ::rep (mapcat #(op-unform p1 %) x)
      ::pcat (if rep+
               (mapcat #(op-unform p0 %) x)
               (mapcat (fn [k]
                         (when (contains? x k)
                           (op-unform (kps k) (get x k))))
                 ks))
      ::alt (if maybe
              [(unform p0 x)]
              (let [[k v] x]
                (op-unform (kps k) v))))))

;; 1515
(defn- add-ret [p r k]
  (let [{:keys [::op ps splice] :as p} (reg-resolve! p)
        prop #(let [ret (preturn p)]
                (if (empty? ret) r ((if splice into conj) r (if k {k ret} ret))))]
    (case op
        nil r
        (::alt ::accept ::amp)
        (let [ret (preturn p)]
          ;;(prn {:ret ret})
          (if (= ret ::nil) r (conj r (if k {k ret} ret))))

        (::rep ::pcat) (prop))))

;; 1528
(defn- deriv
  [p x]
  (let [{[p0 & pr :as ps] :ps, [k0 & kr :as ks] :ks, :keys [::op p1 p2 ret splice forms amp] :as p} (reg-resolve! p)]
    (when p
      (case op
        ::accept nil
        nil (let [ret (dt p x p)]
              (when-not (invalid? ret) (accept ret)))
        ::amp (when-let [p1 (deriv p1 x)]
                (if (= ::accept (::op p1))
                  (let [ret (-> (preturn p1) (and-preds ps (next forms)))]
                    (when-not (invalid? ret)
                      (accept ret)))
                  (amp-impl p1 amp ps forms)))
        ::pcat (alt2 (pcat* {:ps (cons (deriv p0 x) pr), :ks ks, :forms forms, :ret ret})
                     (when (accept-nil? p0) (deriv (pcat* {:ps pr, :ks kr, :forms (next forms), :ret (add-ret p0 ret k0)}) x)))
        ::alt (alt* (map #(deriv % x) ps) ks forms)
        ::rep (alt2 (rep* (deriv p1 x) p2 ret splice forms)
                    (when (accept-nil? p1) (deriv (rep* p2 p2 (add-ret p1 ret nil) splice forms) x)))))))

;; 1548
(defn- op-describe [p]
  (let [{:keys [::op ps ks forms splice p1 rep+ maybe amp] :as p} (reg-resolve! p)]
    ;;(prn {:op op :ks ks :forms forms :p p})
    (when p
      (case op
        ::accept nil
        nil p
        ::amp (list* 'clojure.spec.alpha/& amp forms)
        ::pcat (if rep+
                 (list `+ rep+)
                 (cons `cat (mapcat vector (c/or (seq ks) (repeat :_)) forms)))
        ::alt (if maybe
                (list `? maybe)
                (cons `alt (mapcat vector ks forms)))
        ::rep (list (if splice `+ `*) forms)))))

;; 1564
(defn- op-explain [form p path via in input]
  ;;(prn {:form form :p p :path path :input input})
  (let [[x :as input] input
        {:keys [::op ps ks forms splice p1 p2] :as p} (reg-resolve! p)
        via (if-let [name (spec-name p)] (conj via name) via)
        insufficient (fn [path form]
                       [{:path path
                         :reason "Insufficient input"
                         :pred form
                         :val ()
                         :via via
                         :in in}])]
    (when p
      (case op
            ::accept nil
            nil (if (empty? input)
                  (insufficient path form)
                  (explain-1 form p path via in x))
            ::amp (if (empty? input)
                    (if (accept-nil? p1)
                      (explain-pred-list forms ps path via in (preturn p1))
                      (insufficient path (:amp p)))
                    (if-let [p1 (deriv p1 x)]
                      (explain-pred-list forms ps path via in (preturn p1))
                      (op-explain (:amp p) p1 path via in input)))
            ::pcat (let [pkfs (map vector
                                   ps
                                   (c/or (seq ks) (repeat nil))
                                   (c/or (seq forms) (repeat nil)))
                         [pred k form] (if (= 1 (count pkfs))
                                         (first pkfs)
                                         (first (remove (fn [[p]] (accept-nil? p)) pkfs)))
                         path (if k (conj path k) path)
                         form (c/or form (op-describe pred))]
                     (if (c/and (empty? input) (not pred))
                       (insufficient path form)
                       (op-explain form pred path via in input)))
            ::alt (if (empty? input)
                    (insufficient path (op-describe p))
                    (apply concat
                           (map (fn [k form pred]
                                  (op-explain (c/or form (op-describe pred))
                                              pred
                                              (if k (conj path k) path)
                                              via
                                              in
                                              input))
                                (c/or (seq ks) (repeat nil))
                                (c/or (seq forms) (repeat nil))
                                ps)))
            ::rep (op-explain (if (identical? p1 p2)
                                forms
                                (op-describe p1))
                              p1 path via in input)))))

;; 1660
(defn- re-conform [p [x & xs :as data]]
  ;;(prn {:p p :x x :xs xs})
  (if (empty? data)
    (if (accept-nil? p)
      (let [ret (preturn p)]
        (if (= ret ::nil)
          nil
          ret))
      ::invalid)
    (if-let [dp (deriv p x)]
      (recur dp xs)
      ::invalid)))

;; 1673
(defn- re-explain [path via in re input]
  (loop [p re [x & xs :as data] input i 0]
    ;;(prn {:p p :x x :xs xs :re re}) (prn)
    (if (empty? data)
      (if (accept-nil? p)
        nil ;;success
        (op-explain (op-describe p) p path via in nil))
      (if-let [dp (deriv p x)]
        (recur dp xs (inc i))
        (if (accept? p)
          (if (= (::op p) ::pcat)
            (op-explain (op-describe p) p path via (conj in i) (seq data))
            [{:path path
              :reason "Extra input"
              :pred (op-describe re)
              :val data
              :via via
              :in (conj in i)}])
          (c/or (op-explain (op-describe p) p path via (conj in i) (seq data))
                [{:path path
                  :reason "Extra input"
                  :pred (op-describe p)
                  :val data
                  :via via
                  :in (conj in i)}]))))))

;; 1699
(defn regex-spec-impl
  "Do not call this directly, use 'spec' with a regex op argument"
  [re gfn]
  (-> re
      (dissoc ::op)
      (assoc  :type ::spec
              :cform (fn [_ x]
                       (if (c/or (nil? x) (sequential? x))
                         (re-conform re (seq x))
                         ::invalid))
              :unform (fn [_ x] (op-unform re x))
              :explain (fn [_ path via in x]
                         (if (c/or (nil? x) (sequential? x))
                           (re-explain path via in re (seq x))
                           [{:path path :pred (res `#(c/or (nil? %) (sequential? %))) :val x :via via :in in}]))
              :describe (fn [_] (op-describe re)))))

;; 1737
(defn- validate-fn
  "returns f if valid, else smallest"
  [f specs iters]
  #_(let [g (gen (:args specs))
        prop (gen/for-all* [g] #(call-valid? f specs %))]
    (let [ret (gen/quick-check iters prop)]
      (if-let [[smallest] (-> ret :shrunk :smallest)]
        smallest
        f))))

;; 1747
(defn ^:skip-wiki fspec-impl
  "Do not call this directly, use 'fspec'"
  [argspec aform retspec rform fnspec fform gfn]
  (let [specs {:args argspec :ret retspec :fn fnspec}]
    (assoc specs
           :type ::spec
           :cform (fn [this _f]
                    (throw (Exception. (str "Not implemented in spartan.spec.Can't conform fspec without args spec: " (pr-str (describe this)))))
                    #_(if argspec
                      (if (ifn? f)
                        (if (identical? f (validate-fn f specs *fspec-iterations*)) f ::invalid)
                        ::invalid)
                      (throw (Exception. (str "Can't conform fspec without args spec: " (pr-str (describe this)))))))
           :unform (fn [_ f] f)
           :explain (fn [_ path via in f]
                      (if (ifn? f)
                        (let [args (validate-fn f specs 100)]
                          (if (identical? f args) ;;hrm, we might not be able to reproduce
                            nil
                            (let [ret (try (apply f args) (catch Throwable t t))]
                              (if (instance? Throwable ret)
                                ;;TODO add exception data
                                [{:path path :pred '(apply fn) :val args :reason (.getMessage ^Throwable ret) :via via :in in}]

                                (let [cret (dt retspec ret rform)]
                                  (if (invalid? cret)
                                    (explain-1 rform retspec (conj path :ret) via in ret)
                                    (when fnspec
                                      (let [cargs (conform argspec args)]
                                        (explain-1 fform fnspec (conj path :fn) via in {:args cargs :ret cret})))))))))
                        [{:path path :pred 'ifn? :val f :via via :in in}]))
           :describe (fn [_]
                       `(fspec :args ~aform :ret ~rform :fn ~fform)))))

;; 1794
(clojure.spec.alpha/def ::kvs->map (conformer #(zipmap (map ::k %) (map ::v %)) #(map (fn [[k v]] {::k k ::v v}) %)))

;; 1796
(defmacro keys*
  "takes the same arguments as spec/keys and returns a regex op that matches sequences of key/values,
  converts them into a map, and conforms that map with a corresponding
  spec/keys call:
  user=> (s/conform (s/keys :req-un [::a ::c]) {:a 1 :c 2})
  {:a 1, :c 2}
  user=> (s/conform (s/keys* :req-un [::a ::c]) [:a 1 :c 2])
  {:a 1, :c 2}
  the resulting regex op can be composed into a larger regex:
  user=> (s/conform (s/cat :i1 integer? :m (s/keys* :req-un [::a ::c]) :i2 integer?) [42 :a 1 :c 2 :d 4 99])
  {:i1 42, :m {:a 1, :c 2, :d 4}, :i2 99}"
  [& kspecs]
  `(let [mspec# (clojure.spec.alpha/keys ~@kspecs)]
     ;; NOTE: deleted with/gen
     (clojure.spec.alpha/& (clojure.spec.alpha/* (clojure.spec.alpha/cat ::k keyword? ::v any?)) ::kvs->map mspec#)))

; 1808
(defn ^:skip-wiki nonconforming
  "takes a spec and returns a spec that has the same properties except
  'conform' returns the original (not the conformed) value. Note, will specize regex ops."
  [spec]
  (let [spec (delay (specize spec))]
    {:type ::spec
     :cform (fn [_ x]
              (let [ret (conform* @spec x)]
                (if (invalid? ret)
                  ::invalid
                  x)))
     :unform (fn [_ x] x)
     :explain (fn [_ path via in x] (explain* @spec path via in x))
     :describe (fn [_] `(nonconforming ~(describe* @spec)))}))

;; 1836
(defn nilable-impl
  "Do not call this directly, use 'nilable'"
  [form pred]
  (let [spec (delay (specize pred form))]
    {:type ::spec
     :cform (fn [_ x] (if (nil? x) nil (conform* @spec x)))
     :unform (fn [_ x] (if (nil? x) nil (unform* @spec x)))
     :explain (fn [_ path via in x]
                (when-not (c/or (pvalid? @spec x) (nil? x))
                  (conj
                   (explain-1 form pred (conj path ::pred) via in x)
                   {:path (conj path ::nil) :pred 'nil? :val x :via via :in in})))
     :describe (fn [_] `(nilable ~(res form)))}))

;; 1862
(defmacro nilable
  "returns a spec that accepts nil and values satisfying pred"
  [pred]
  (let [pf (res pred)]
    `(nilable-impl '~pf ~pred)))

;; 1910
(defn int-in-range?
  "Return true if start <= val, val < end and val is a fixed
  precision integer."
  [start end val]
  (c/and (int? val) (<= start val) (< val end)))

;; 1916
(defmacro int-in
  "Returns a spec that validates fixed precision integers in the
  range from start (inclusive) to end (exclusive)."
  [start end]
  `(spec (and int? #(int-in-range? ~start ~end %))))

;; 1923
(defmacro double-in
  "Specs a 64-bit floating point number. Options:
    :infinite? - whether +/- infinity allowed (default true)
    :NaN?      - whether NaN allowed (default true)
    :min       - minimum value (inclusive, default none)
    :max       - maximum value (inclusive, default none)"
  [& {:keys [infinite? NaN? min max]
    :or {infinite? true NaN? true}
    :as m}]
  `(spec (and c/double?
              ~@(when-not infinite? '[#(not (Double/isInfinite %))])
              ~@(when-not NaN? '[#(not (Double/isNaN %))])
              ~@(when max `[#(<= % ~max)])
              ~@(when min `[#(<= ~min %)]))))

;; 1941
#_(defonce
  ^{:dynamic true
    :doc "If true, compiler will enable spec asserts, which are then
subject to runtime control via check-asserts? If false, compiler
will eliminate all spec assert overhead. See 'assert'.
Initially set to boolean value of clojure.spec.compile-asserts
system property. Defaults to true."}
  *compile-asserts*
  (not= "false" (System/getProperty "clojure.spec.compile-asserts")))

(defonce ^:dynamic *check-spec-asserts* false)

;; 1952
(defn check-asserts?
  "Returns the value set by check-asserts."
  []
  *check-spec-asserts*)

;; 1957
(defn check-asserts
  "Enable or disable spec asserts that have been compiled
with '*compile-asserts*' true.  See 'assert'.
Initially set to boolean value of clojure.spec.check-asserts
system property. Defaults to false."
  [flag]
  (alter-var-root #'*check-spec-asserts* (constantly flag)))

;; 1966
(defn assert*
  "Do not call this directly, use 'assert'."
  [spec x]
  (if (valid? spec x)
    x
    (let [ed (c/merge (assoc (explain-data* spec [] [] [] x)
                                        ::failure :assertion-failed))]
      (throw (ex-info
              (str "Spec assertion failed\n" (with-out-str (explain-out ed)))
              ed)))))

;; 1977
(defmacro assert
  "spec-checking assert expression. Returns x if x is valid? according
to spec, else throws an ex-info with explain-data plus ::failure of
:assertion-failed.
Can be disabled at either compile time or runtime:
If *compile-asserts* is false at compile time, compiles to x. Defaults
to value of 'clojure.spec.compile-asserts' system property, or true if
not set.
If (check-asserts?) is false at runtime, always returns x. Defaults to
value of 'clojure.spec.check-asserts' system property, or false if not
set. You can toggle check-asserts? with (check-asserts bool)."
  [spec x]
  `(if *check-spec-asserts*
     (assert* ~spec ~x)
     ~x))
