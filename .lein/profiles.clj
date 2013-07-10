{:user
 {:plugins [[lein-midje "3.0.1"]
            [lein-exec "0.3.0"]]
  :dependencies [[clojure-complete "0.2.3" :exclusions [org.clojure/clojure]]]
  :repl-options
  {:init (do
           (println "Running ~/profiles.clj :user :repl-options :init")

           (set! *print-length* 20)
           (set! *print-level* 5)

           ;; make repl bindings available directly in 'user
           ;; namespace, but also alias them in a '. namespace so you
           ;; can do e.g. (./source foo) from any other namespace.
           (doseq [[ns _ names] clojure.main/repl-requires]
             (use ns)
             (binding [*ns* (create-ns '.)]
               (doseq [n names]
                 (let [s (symbol (name ns) (name n))]
                   (eval `(alter-meta!
                           (def ~n @(var ~s))
                           merge (meta (var ~s)))))))))
   :timeout 90000}}}
