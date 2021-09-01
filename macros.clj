

(defn criticize-code
  [criticism code]
  `(println ~criticism (quote ~code)))


(defmacro code-critic
  [{:keys [good bad]}]
  `(do ~@(map #(apply criticize-code %)
              [["Sweet lion of Zion, this is bad code:" bad]
               ["Great cow of Moscow, this is good code:" good]])))
(code-critic (1 + 2) (+ 3 4))









(while true
  (println (eval (read-string (read-line)))))

(defn testing
  [[arg1 func arg2 & remaining]]
  (if (empty? remaining)
    (list func arg1 arg2)
    (testing (cons
              (list func arg1 arg2)
              remaining))))

(defmacro infix
  [[arg1 func arg2 & remaining]]
  (if (empty? remaining)
    (list func arg1 arg2)
    (infix (cons
            (list func arg1 arg2)
            remaining))))

(defmacro infix-simple
  [[a1 f a2]]
  (list f a1 a2))

(defmacro and#
  ([] true)
  ([x] x)
  ([x & rest]
   `(let [item ~x]
      (if item (and# ~@rest) item))))

(defmacro print-and-return
  [expression]
  (list 'let ['result expression]
        `(println result)
        'result))
(macroexpand '(print-and-return "asdf"))

(defmacro print-and-return-fancy
  [expression]
  `(let [result ~expression]
     (println result)
     result))
(macroexpand '(print-and-return-fancy "asdf"))

(defmacro testsum
  [& args]
  `(apply + ~args))
(macroexpand '(testsum 1 2))



(macroexpand '(when bool expr1 expr2))
(macroexpand '(defn func-name [args] body))
(macroexpand '(if a b))
(macroexpand '(infix-simple (1 + 2)))
(macroexpand '(and 1 2 false 3))

(and# 1 2)

;; 
