(ns run-tests
  "The suite under ClojureScript.

  `package.json` has a `test:cljs` script, and it runs `shadow-cljs compile
  test` -- a JVM build. That is a fine thing to have and the murakumo fleet
  cannot run it, so in practice this repo's ClojureScript half was checked
  only by whoever remembered to type it, and its GitHub Actions have been off
  since ADR-2607300900. This entry is the one a gate can invoke.

  It matters here more than in most places. `cbor/core` is one of the few
  namespaces in this workspace that already knows the hazard the fleet spent
  2026-08-17 finding in four other repos, and says so in its own comments:
  ClojureScript's bitwise operators coerce to Int32/Uint32 first, so the
  encoder uses division and `mod` rather than shifts, exact to 2^53. That
  discipline is only worth something if something re-checks it -- the
  equivalent code in `dev-protobuf` and `io-multiformats` carried the same
  reasoning in a comment while the implementation had quietly stopped
  matching it.

      npx nbb --classpath src:test run-tests.cljs"
  (:require [cljs.test :as t]
            [cbor.core-test]))

(defmethod t/report [::t/default :end-run-tests] [m]
  (when-not (t/successful? m)
    (js/process.exit 1)))

(t/run-tests 'cbor.core-test)
