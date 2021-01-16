# spartan.spec

<!-- [![CircleCI](https://circleci.com/gh/borkdude/spartan.test/tree/master.svg?style=shield)](https://circleci.com/gh/borkdude/spartan.test/tree/master) -->
<!-- [![Clojars Project](https://img.shields.io/clojars/v/borkdude/spartan.test.svg)](https://clojars.org/borkdude/spartan.test) -->
[![project chat](https://img.shields.io/badge/slack-join_chat-brightgreen.svg)](https://app.slack.com/client/T03RZGPFR/CLX41ASCS)

A spartan implementation of clojure.spec.alpha compatible with
[babashka](https://github.com/borkdude/babashka) (>= 0.2.5) and Clojure.

## Rationale

Currently babashka doesn't have a built-in implementation of
`clojure.spec.alpha`. This library can be used meanwhile. If the application of
spec in scripting turns out to be useful, babashka will probably bundle a
built-in implementation of spec2 at some point which will be more performant than this interpreted version.

## Differences with clojure.spec.alpha

- No generators (yet)
- No `fdef` (yet)

## Usage:

Usage in a `deps.edn` project:

``` clojure
{:deps {borkdude/spartan.spec {:git/url "https://github.com/borkdude/spartan.spec"
                               :sha "e5c9f40ebcc64b27b3e3e83ad2a285ccc0997097"}}}
```

Requiring `spartan.spec` will create a namespace `clojure.spec.alpha` for compatibility.

## Example

``` clojure
(ns expound
  (:require [babashka.deps :as deps]))

(deps/add-deps '{:deps {borkdude/spartan.spec {:git/url "https://github.com/borkdude/spartan.spec"
                                               :sha "e5c9f40ebcc64b27b3e3e83ad2a285ccc0997097"}
                        expound/expound {:mvn/version "0.8.7"}}})

(require 'spartan.spec) ;; DANGER: loading spartan.spec will create a namespace clojure.spec.alpha for compatibility
(require '[clojure.spec.alpha :as s])

;; Expound expects some vars to be there, like `with-gen`. Spartan prints warnings that these are used, but doesn't implement them yet.
(binding [*err* (java.io.StringWriter.)]
  (require '[expound.alpha :as expound]))

(expound/expound string? 1)

(s/def ::a string?)

(expound/expound ::a 1)
```

## Tests

Install [babashka](https://github.com/borkdude/babashka) and [deps.clj](https://github.com/borkdude/deps.clj/).
Then run `script/test`.

## License

Copyright © 2020 Michiel Borkent

Distributed under the EPL License. See LICENSE.
