(ns functional-programming.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

;; just like haskell!!!
((comp #(/ % 3) inc *) 2 9)

(def character
  {:name "Smooches McCutes"
   :attributes {:intelligence 10
                :strength 4
                :dexterity 5}})

(def c-int (comp :intelligence :attributes))
(c-int character)

(def spell-slots (comp int inc #(/ % 2) c-int))
(spell-slots character)

(defn my-comp
  [& funcs]
  (fn [& args]
    (let [ordered-funcs (reverse funcs)
          first-result  (apply (first ordered-funcs) args)
          rest-funcs (rest ordered-funcs)]
      (reduce
       (fn [acc func] (func acc))
       first-result
       rest-funcs))))
     
(defn my-loop-comp 
  [& funcs]
  (fn [& args]
    (let [ordered-funcs (reverse funcs)
          first-result (apply (first ordered-funcs) args)]
      (loop [cur-res first-result
             rest-funcs (rest ordered-funcs)]
        (if (empty? rest-funcs)
          cur-res
          (recur ((first rest-funcs) cur-res)
                 (rest rest-funcs)))))))

(time ((my-comp       #(/ % 4) #(* 2 %) inc) 1))
(time ((my-loop-comp  #(/ % 4) #(* 2 %) inc) 1))
(time ((my-comp      first rest rest rest) "abcdefg"))
(time ((my-loop-comp first rest rest rest) "abcdefg"))

(defn get [x] 
  (Thread/sleep 1000)
  x)

(get "hello")
(def memo-get (memoize get))

(memo-get "hello")
(memo-get "Hi")

