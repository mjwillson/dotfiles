{:user
 {
  :plugins [[lein-midje "2.0.4"]]
  :dependencies [[clojure-complete "0.2.2" :exclusions [org.clojure/clojure]]
                 [clj-stacktrace "0.2.5"]]
  :repl-options
  {:init (do
           (println "Running ~/profiles.clj :user :repl-options :init")

           ;; Having to comment these out for now as they trigger
           ;; https://github.com/trptcolin/reply/issues/99 :-(
           ;; (set! *print-length* 10)
           ;; (set! *print-level* 5)

           (use 'clojure.stacktrace)
           ;; Substituting this for the above, as I would like to do,
           ;; for some reason causes ac-nrepl to hang caching JVM
           ;; class names: https://github.com/purcell/ac-nrepl/issues/25
           ;; (require '[clj-stacktrace.repl :refer [pst]])

           (use 'clojure.pprint)
           (use 'clojure.reflect))
   :timeout 90000}
  :injections [(let [orig (ns-resolve (doto 'clojure.stacktrace require)
                                      'print-cause-trace)
                     new (ns-resolve (doto 'clj-stacktrace.repl require)
                                     'pst)]
                 (alter-var-root orig (constantly @new)))]
  }
 }
