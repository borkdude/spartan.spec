(ns crucible
  (:require [babashka.deps :as deps]))

(deps/add-deps
 '{:deps {borkdude/spartan.spec {:git/url "https://github.com/borkdude/spartan.spec"
                                 :sha "bf4ace4a857c29cbcbb934f6a4035cfabe173ff1"}
          crucible/crucible {:mvn/version "0.46.0"}}})

(require 'spartan.spec) ;; creates clojure.spec.alpha

(require '[crucible.aws.ec2 :as ec2]
         '[crucible.core :refer [template parameter output xref encode join]])

(def simple (template "A simple sample template"
                      :my-vpc-cidr (parameter)
                      :my-vpc (ec2/vpc {::ec2/cidr-block (xref :my-vpc-cidr)})
                      :vpc (output (join "/" ["foo" (xref :my-vpc)]))))

(println (encode simple))
