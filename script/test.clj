#!/usr/bin/env bb

(ns test
  (:require [babashka.deps :as deps]
            [clojure.edn :as edn]
            [clojure.test :as t]))

(deps/add-deps (edn/read-string (slurp "deps.edn")) {:aliases [:test]})

(require 'spartan.spec-test)

(let [{:keys [:fail :error]} (t/run-tests 'spartan.spec-test)]
  (System/exit (+ fail error)))
