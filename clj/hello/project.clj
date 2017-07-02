(defproject hello "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [
    [org.clojure/clojure "1.8.0"]
    ; [postgresql "9.3-1102.jdbc41"]
    [sqlitejdbc "0.5.6"]
    [java-jdbc/dsl "0.1.3"]
    [korma "0.4.3"]
    [ragtime "0.7.1"]
    [lobos "1.0.0-beta3"]
    [fx-clj "0.2.0-alpha1"]
    [honeysql "0.9.0"]
    [buddy "1.3.0"]
    [domaintypes/core "1.0.1"]
    [slamhound "1.5.5"]
    [prismatic/schema "1.1.6"]
    [defun "0.3.0-RC1"]
    [com.stuartsierra/component "0.3.2"]
    [mount "0.1.11"]
  ]
  :main ^:skip-aot hello.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}}

  ;; Plugins are code that runs in Leiningen itself and usually
  ;; provide new tasks or hooks.
  :plugins [
    [lein-cprint "1.2.0"]
    [lein-cljfmt "0.5.6"]
    [lein-kibit "0.1.5"]
    [jonase/eastwood "0.2.4"]
    [lein-bikeshed "0.4.1"]
    [lein-ancient "0.6.10"]
    [lein-checkall "0.1.1"]
    [lein-plz "0.4.0-SNAPSHOT"]
    [venantius/ultra "0.5.1"]
    [lein-exec "0.3.6"]
    [domaintypes/lein-domaintypesdoc "1.0.1"]
    [lein-gorilla "0.4.0"]
    [clj-ml "0.0.3-SNAPSHOT"]
  ]

)
