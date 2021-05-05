# spartan.spec

<!-- [![CircleCI](https://circleci.com/gh/borkdude/spartan.test/tree/master.svg?style=shield)](https://circleci.com/gh/borkdude/spartan.test/tree/master) -->
<!-- [![Clojars Project](https://img.shields.io/clojars/v/borkdude/spartan.test.svg)](https://clojars.org/borkdude/spartan.test) -->
[![project chat](https://img.shields.io/badge/slack-join_chat-brightgreen.svg)](https://app.slack.com/client/T03RZGPFR/CLX41ASCS)

A spartan implementation of clojure.spec.alpha compatible with
[babashka](https://github.com/borkdude/babashka) (>= 0.2.5) and Clojure.

## Rationale

Currently [babashka](https://github.com/babashka/babashka) doesn't have a
built-in implementation of `clojure.spec.alpha`. This library can be used
meanwhile. If the application of spec in scripting turns out to be useful,
babashka will probably bundle a built-in implementation of spec2 at some point
which will be more performant than this interpreted version.

## Differences with clojure.spec.alpha

- No generators (yet)
- No `fdef` (yet)

## Usage:

Usage in a `deps.edn` project:

``` clojure
borkdude/spartan.spec {:git/url "https://github.com/borkdude/spartan.spec"
                       :sha "12947185b4f8b8ff8ee3bc0f19c98dbde54d4c90"}
```

Requiring `spartan.spec` will create a namespace `clojure.spec.alpha` for compatibility.

## Example

This is an example that you can run with babashka:

``` clojure
(ns expound
  (:require [babashka.deps :as deps]))

(deps/add-deps
 '{:deps {borkdude/spartan.spec {:git/url "https://github.com/borkdude/spartan.spec"
                                 :sha "12947185b4f8b8ff8ee3bc0f19c98dbde54d4c90"}
          expound/expound {:mvn/version "0.8.9"}}})

;; Loading spartan.spec will create a namespace clojure.spec.alpha for compatibility:
(require 'spartan.spec)
(alias 's 'clojure.spec.alpha)

;; Expound expects some vars to be there, like `with-gen`. Spartan prints warnings that these are used, but doesn't implement them yet.
(require '[expound.alpha :as expound])

(s/def ::a (s/cat :i int? :j string?))

(expound/expound ::a [1 2])
```

Output:

``` shell
-- Spec failed --------------------

  [... 2]
       ^

should satisfy

  string?

-- Relevant specs -------

:expound/a:
  (clojure.spec.alpha/cat :i clojure.core/int? :j clojure.core/string?)

-------------------------
```

## Tests

Install [babashka](https://github.com/borkdude/babashka) and [deps.clj](https://github.com/borkdude/deps.clj/).
Then run `script/test`.

## License

Copyright © 2020 Michiel Borkent

Distributed under the EPL License. See LICENSE.
