(ns expound
  (:require [babashka.deps :as deps]))

(deps/add-deps
 '{:deps {borkdude/spartan.spec {:git/url "https://github.com/borkdude/spartan.spec"
                                 :sha "bf4ace4a857c29cbcbb934f6a4035cfabe173ff1"}
          expound/expound {:mvn/version "0.8.9"}}})

(require 'spartan.spec)
(alias 's 'clojure.spec.alpha)

(require '[expound.alpha :as expound])

(s/def ::a (s/cat :i int? :j string?))

(expound/expound ::a [1 2])
