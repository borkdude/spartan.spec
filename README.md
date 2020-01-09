# spartan.spec

<!-- [![CircleCI](https://circleci.com/gh/borkdude/spartan.test/tree/master.svg?style=shield)](https://circleci.com/gh/borkdude/spartan.test/tree/master) -->
<!-- [![Clojars Project](https://img.shields.io/clojars/v/borkdude/spartan.test.svg)](https://clojars.org/borkdude/spartan.test) -->
[![project chat](https://img.shields.io/badge/slack-join_chat-brightgreen.svg)](https://app.slack.com/client/T03RZGPFR/CLX41ASCS)

A spartan implementation of clojure.spec.alpha compatible with
[babashka](https://github.com/borkdude/babashka) (>= 0.0.61) and Clojure.

## Rationale

Currently babashka doesn't have a built-in implementation of
`clojure.spec.alpha`. This library can be used meanwhile. If the application of
spec in scripting turns out to be useful, babashka will probably bundle a
built-in version spec2 at some point.

## Differences with clojure.spec.alpha

- No generators (yet)
- No `fdef` (yet)

## Usage:

Usage in a `deps.edn` project:

``` clojure
{:deps {borkdude/spartan.spec {:git/url "https://github.com/borkdude/spartan.spec"
                               :sha "4762f629ac2d146da2296923bb8b40fb42c69b30"}}}
```

``` shell
$ bb -cp "$(clojure -Spath)" -e "(require '[spartan.spec :as s]) (s/valid? int? :foo)"
false
```

## Tests

Install [babashka](https://github.com/borkdude/babashka) and [deps.clj](https://github.com/borkdude/deps.clj/).
Then run `script/test`.

## License

Copyright © 2020 Michiel Borkent

Distributed under the EPL License. See LICENSE.
