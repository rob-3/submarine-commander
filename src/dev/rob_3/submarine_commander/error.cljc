(ns dev.rob-3.submarine-commander.error)

(defn err? [x]
  (boolean
   (and (keyword? x)
        (= "dev.rob-3.submarine-commander.error" (namespace x)))))

(defmacro err-> [expr & forms]
  (if (empty? forms)
    expr
    (let [a-sym (gensym)]
      `(let [~a-sym ~expr]
         (if (not (:error ~a-sym))
           (err-> ~(let [form (first forms)]
                     (if (seq? form)
                       (cons (first form) (cons a-sym (rest form)))
                       (list form a-sym)))
                  ~@(rest forms))
           ~a-sym)))))
