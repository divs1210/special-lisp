(ns special-lisp.util)

(defn throw+ [& strs]
  (throw (Exception. (apply str (cons "Error: " strs)))))
