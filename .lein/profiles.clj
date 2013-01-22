{:user
 {
  :plugins [[lein-midje "2.0.4"]]
  :dependencies [[clojure-complete "0.2.2" :exclusions [org.clojure/clojure]]
                 [clj-stacktrace "0.2.5"]]
  :repl-options
  {:init (do
           (println "Running ~/profiles.clj :user :repl-options :init")
           (set! *print-length* 10)
           (set! *print-level* 5)
           (use 'clojure.stacktrace)
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
