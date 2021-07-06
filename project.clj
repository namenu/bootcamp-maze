;; Netlify - leiningen 배포에 필요
(defproject namenu/maze "0.1.0-SNAPSHOT"
  :dependencies [[thheller/shadow-cljs "2.14.5" :classifier "aot"]
                 [org.clojure/core.async "1.3.618"]
                 [reagent/reagent "1.0.0"]])
