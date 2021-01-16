(ns expound
  (:require [babashka.deps :as deps]))

(deps/add-deps '{:deps {borkdude/spartan.spec {:git/url "https://github.com/borkdude/spartan.spec"
                                               :sha "bf4ace4a857c29cbcbb934f6a4035cfabe173ff1"}
                        expound/expound {:mvn/version "0.8.7"}}})

(require 'spartan.spec)
(require '[clojure.spec.alpha :as s])

(binding [*err* (java.io.StringWriter.)] (require '[expound.alpha :as expound]))

(expound/expound string? 1)

(s/def ::a string?)

(expound/expound ::a 1)
