{:user
  {
    :plugins [
              [lein-midje "2.0.0-SNAPSHOT"]
              [com.stuartsierra/lazytest "1.2.3"]
     ]
    :dependencies [
      [clojure-complete "0.2.2" :exclusions [org.clojure/clojure]]
      [clj-stacktrace "0.2.5"]
      ]
   :repositories {
                  "stuart" "http://stuartsierra.com/maven2"
                  }
    :repl-options {
      :init (do
         (println "Running ~/profiles.clj :user :repl-options :init")
         (set! *print-length* 10)
         (set! *print-level* 5)
         (use 'clojure.stacktrace)
         (use 'clojure.pprint)
         (require 'clojure.reflect)
         (defn java-methods [x]
           (sort (map :name
                (filter #(contains? (:flags %) :public)
                        (:members (clojure.reflect/reflect x))))))
      )
      :timeout 90000
    }
    :debug true ;; see https://groups.google.com/forum/?fromgroups=#!topic/leiningen/4LBgKOaLBXw
    :injections [(let [orig (ns-resolve (doto 'clojure.stacktrace require)
                                        'print-cause-trace)
                       new (ns-resolve (doto 'clj-stacktrace.repl require)
                                       'pst)]
                   (alter-var-root orig (constantly @new)))]}}
