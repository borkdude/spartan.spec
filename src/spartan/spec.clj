(ns spartan.spec
  (:refer-clojure :exclude [+ * and assert or cat def keys merge ifn?])
  (:require [clojure.walk :as walk]))

;; TODO: make into ifn? once sci has ifn?
(def ifn? fn?)

(defn invalid?
  "tests the validity of a conform return value"
  [ret]
  (identical? ::invalid ret))

(defn- accept [x] {::op ::accept :ret x})

(defn- accept? [{:keys [::op]}]
  (= ::accept op))

(defn- pcat* [{[p1 & pr :as ps] :ps,  [k1 & kr :as ks] :ks, [f1 & fr :as forms] :forms, ret :ret, rep+ :rep+}]
  (when (every? identity ps)
    (if (accept? p1)
      (let [rp (:ret p1)
            ret (conj ret (if ks {k1 rp} rp))]
        (if pr
          (pcat* {:ps pr :ks kr :forms fr :ret ret})
          (accept ret)))
      {::op ::pcat, :ps ps, :ret ret, :ks ks, :forms forms :rep+ rep+})))

(defn- pcat [& ps] (pcat* {:ps ps :ret []}))

(defn- ->sym
  "Returns a symbol from a symbol or var"
  [x]
  (if (var? x)
    (let [^clojure.lang.Var v x]
      (symbol (str (.name (.ns v)))
              (str (.sym v))))
    x))

(defn- unfn [expr]
  (if (clojure.core/and (seq? expr)
                        (symbol? (first expr))
                        (= "fn*" (name (first expr))))
    (let [[[s] & form] (rest expr)]
      (conj (walk/postwalk-replace {s '%} form) '[%] 'fn))
    expr))

(defn- res [form]
  (cond
    (keyword? form) form
    (symbol? form) (clojure.core/or (-> form resolve ->sym) form)
    (sequential? form) (walk/postwalk #(if (symbol? %) (res %) %) (unfn form))
    :else form))

(defn cat-impl
  "Do not call this directly, use 'cat'"
  [ks ps forms]
  (pcat* {:ks ks, :ps ps, :forms forms, :ret {}}))

(defmacro cat
  "Takes key+pred pairs, e.g.
  (s/cat :e even? :o odd?)
  Returns a regex op that matches (all) values in sequence, returning a map
  containing the keys of each pred and the corresponding value."
  [& key-pred-forms]
  (let [pairs (partition 2 key-pred-forms)
        keys (mapv first pairs)
        pred-forms (mapv second pairs)
        pf (mapv res pred-forms)]
    (clojure.core/assert (clojure.core/and (even? (count key-pred-forms)) (every? keyword? keys)) "cat expects k1 p1 k2 p2..., where ks are keywords")
    `(cat-impl ~keys ~pred-forms '~pf)))

(defmacro ?
  "Returns a regex op that matches zero or one value matching
  pred. Produces a single value (not a collection) if matched."
  [pred-form]
  `(maybe-impl ~pred-form '~(res pred-form)))

;; TODO: make into defonce once sci has defonce
(def ^:private registry-ref (atom {}))

(defn- deep-resolve [reg k]
  (loop [spec k]
    (if (ident? spec)
      (recur (get reg spec))
      spec)))

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

(defn- reg-resolve!
  "returns the spec/regex at end of alias chain starting with k, throws if not found, k if k not ident"
  [k]
  (if (ident? k)
    (clojure.core/or (reg-resolve k)
                     (throw (Exception. (str "Unable to resolve spec: " k))))
    k))

(defn spec?
  "returns x if x is a spec object, else logical false"
  [x]
  (= ::spec (:type x)))

(defn regex?
  [x]
  (clojure.core/and (::op x) x))

(defn- with-name [spec name]
  (cond
    (ident? spec) spec
    (regex? spec) (assoc spec ::name name)
    ;; TODO: add clojure.lang.IObj to babashka
    #_(instance? clojure.lang.IObj spec)
    #_(with-meta spec (assoc (meta spec) ::name name))))

(defn- spec-name [spec]
  (cond
    (ident? spec) spec
    (regex? spec) (::name spec)
    ;; TODO: add clojure.lang.IObj to babashka
    #_(instance? clojure.lang.IObj spec)
    #_(-> (meta spec) ::name)))

(defn- maybe-spec
  "spec-or-k must be a spec, regex or resolvable kw/sym, else returns nil."
  [spec-or-k]
  (let [s (clojure.core/or (clojure.core/and (ident? spec-or-k) (reg-resolve spec-or-k))
                           (spec? spec-or-k)
                           (regex? spec-or-k)
                           nil)]
    (if (regex? s)
      (with-name s (spec-name s))
      s)))

(defn- the-spec
  "spec-or-k must be a spec, regex or kw/sym, else returns nil. Throws if unresolvable kw/sym"
  [spec-or-k]
  (clojure.core/or
   (maybe-spec spec-or-k)
   (when (ident? spec-or-k)
     (throw (Exception. (str "Unable to resolve spec: " spec-or-k))))))

;; regex

(declare accept-nil? conform)

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

(defn- noret? [p1 pret]
  (clojure.core/or (= pret ::nil)
                   (clojure.core/and (#{::rep ::pcat} (::op (reg-resolve! p1))) ;;hrm, shouldn't know these
                                     (empty? pret))
                   nil))

;; (declare preturn)
(def preturn) ;; TODO, something is wrong with declare
(def accept-nil?) ;; TODO: something is wrong with declare

(defn- add-ret [p r k]
  (let [{:keys [::op ps splice] :as p} (reg-resolve! p)
        prop #(let [ret (preturn p)]
                (if (empty? ret) r ((if splice into conj) r (if k {k ret} ret))))]
    ;; TODO: revert when sci has support for lists in case
    #_(case op
      nil r
      (::alt ::accept ::amp)
      (let [ret (preturn p)]
        ;;(prn {:ret ret})
        (if (= ret ::nil) r (conj r (if k {k ret} ret))))

      (::rep ::pcat) (prop))
    (cond ;; op
      (nil? op) r
      (contains? #{::alt ::accept ::amp} op)
      (let [ret (preturn p)]
        ;;(prn {:ret ret})
        (if (= ret ::nil) r (conj r (if k {k ret} ret))))
      (contains? #{::rep ::pcat} op) (prop)
      :else (throw (Exception. (str "No matching case: " op))))))

(defn- tagged-ret [tag ret]
  ;; TODO: add clojure.lang.MapEntry to bb
  #_(clojure.lang.MapEntry. tag ret)
  [tag ret])

(defn- filter-alt [ps ks forms f]
  (if (clojure.core/or ks forms)
    (let [pks (->> (map vector ps
                        (clojure.core/or (seq ks) (repeat nil))
                        (clojure.core/or (seq forms) (repeat nil)))
                   (filter #(-> % first f)))]
      [(seq (map first pks)) (when ks (seq (map second pks))) (when forms (seq (map #(nth % 2) pks)))])
    [(seq (filter f ps)) ks forms]))

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

(defn- accept-nil? [p]
  (let [{:keys [::op ps p1 p2 forms] :as p} (reg-resolve! p)]
    (case op
      ::accept true
      nil nil
      ::amp (clojure.core/and (accept-nil? p1)
                              (let [ret (-> (preturn p1) (and-preds ps (next forms)))]
                                (not (invalid? ret))))
      ::rep (clojure.core/or (identical? p1 p2) (accept-nil? p1))
      ::pcat (every? accept-nil? ps)
      ::alt (clojure.core/some accept-nil? ps))))

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

(defn- alts [& ps] (alt* ps nil nil))
(defn- alt2 [p1 p2] (if (clojure.core/and p1 p2) (alts p1 p2) (clojure.core/or p1 p2)))

(defn maybe-impl
  [p form] (assoc (alt* [p (accept ::nil)] nil [form ::nil]) :maybe form))

(defn- deriv
  [p x]
  (let [{[p0 & pr :as ps] :ps, [k0 & kr :as ks] :ks, :keys [::op p1 p2 ret splice forms amp] :as p} (reg-resolve! p)]
    (when p
      (case op
        ::accept nil
        nil (let [ret (dt p x p)]
              (when-not (invalid? ret) (accept ret)))
        ::amp ::TODO #_(when-let [p1 (deriv p1 x)]
                         (if (= ::accept (::op p1))
                           (let [ret (-> (preturn p1) (and-preds ps (next forms)))]
                             (when-not (invalid? ret)
                               (accept ret)))
                           (amp-impl p1 amp ps forms)))
        ::pcat (alt2 (pcat* {:ps (cons (deriv p0 x) pr), :ks ks, :forms forms, :ret ret})
                     (when (accept-nil? p0) (deriv (pcat* {:ps pr, :ks kr, :forms (next forms), :ret (add-ret p0 ret k0)}) x)))
        ::alt (alt* (map #(deriv % x) ps) ks forms)
        ::rep ::TODO #_(alt2 (rep* (deriv p1 x) p2 ret splice forms)
                             (when (accept-nil? p1) (deriv (rep* p2 p2 (add-ret p1 ret nil) splice forms) x)))))))

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

;; regex

(defn spec-impl [form pred]
  {:type ::spec
   :form form
   :pred pred})

(defn conform* [spec x]
  (cond (regex? spec) (re-conform spec x)
        :else
        (let [pred (:pred spec)
              ret (pred x)]
          (if ret x ::invalid))))

(defn specize*
  ([x] (specize* x nil))
  ([x form]
   (cond (keyword? x) (reg-resolve! x)
         (symbol? x) (reg-resolve! x)
         (set? x) (spec-impl form x)
         (regex? x) (assoc x :type ::spec)
         :else (if (clojure.core/and (not (map? x)) (ifn? x))
                 (if-let [s false
                          ;; TODO
                          #_(fn-sym o)]
                   (spec-impl s x)
                   (spec-impl ::unknown x))
                 (spec-impl ::unknown x)))))

(defn- specize
  ([s] (clojure.core/or (spec? s) (specize* s)))
  ([s form] (clojure.core/or (spec? s) (specize* s form))))

(defn conform
  [spec x]
  (conform* (specize spec) x))

(defn valid?
  "Helper function that returns true when x is valid for spec."
  ([spec x]
   (let [spec (specize spec)]
     (not (invalid? (conform* spec x)))))
  ([spec x form]
   (let [spec (specize spec form)]
     (not (invalid? (conform* spec x))))))
