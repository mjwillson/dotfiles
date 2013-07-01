{:user
 {
  :plugins [[lein-midje "3.0.0"]
            [lein-exec "0.3.0"]]
  :dependencies [[clojure-complete "0.2.3" :exclusions [org.clojure/clojure]]]
  :repl-options
  {:init (do
           (println "Running ~/profiles.clj :user :repl-options :init")

           ;; Having to comment these out for now as they trigger
           ;; https://github.com/trptcolin/reply/issues/99 :-(
           (set! *print-length* 10)
           (set! *print-level* 5)

           (use 'clojure.pprint)
           (use 'clojure.reflect)
           (use 'clojure.repl))
   :timeout 90000}}}
