(use 'clojure.java.shell)


(def src-dir "/home/sa/src")

(def ops {
  ".bzr" ["bzr pull" "bzr update"],
  ".hg"  ["hg pull" "hg update"],
  ".svn" ["svn update"],
  ".git" ["git pull"]})


(defn update-project [project cmds]
  (doseq [cmd cmds]
    (let [args (. cmd split " ")
          res (sh (first args) (second args) 
            :dir (. project getAbsolutePath))]
      (println (res :out)))))

(defn update-root [path]
  ;(println "root: " path)
  (doseq [project (.. (java.io.File. path) (listFiles))]
    (when (. project isDirectory)
      (println "proj: " (. project getAbsolutePath))
      
      (doseq [dir (. project listFiles)]
      (when (. dir isDirectory)
        ;(println "dir: " (. dir getAbsolutePath))
        (let [dirname (. dir getName)]
          (when (boolean (ops dirname))
            (update-project project (ops dirname)))))))))

(if (> 1 (count *command-line-args*))
  (doseq [path *command-line-args*]
    (update-root path)))

(update-root src-dir)




