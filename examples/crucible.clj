(ns crucible.examples-test
  (:require [clojure.pprint :as pprint]
            [crucible.core :refer [template parameter resource output xref encode join]]
            [crucible.aws.ec2 :as ec2]))

(def simple (template "A simple sample template"
                      :my-vpc-cidr (parameter)
                      :my-vpc (ec2/vpc {::ec2/cidr-block (xref :my-vpc-cidr)})
                      :vpc (output (join "/" ["foo" (xref :my-vpc)]))))

(clojure.pprint/pprint (encode simple))
