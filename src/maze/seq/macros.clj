(ns maze.seq.macros)

(defmacro defmaze [name args & body]
  (let [output (first args)]
    `(deftype ^:private ~name ~args
       cljs.core/ISeqable
       (cljs.core/-seq [this#] this#)

       cljs.core/ISeq
       (cljs.core/-first [_#] ~output)
       (cljs.core/-rest [_#]
         ~@body))))
