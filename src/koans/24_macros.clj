(ns koans.24-macros
  (:require [koan-engine.core :refer [meditations]]))

(defmacro hello [x]
  (str "Hello, " x))

(defmacro infix [form]
  (list (second form) (first form) (nth form 2)))

(defmacro infix-concise [form]
  `(~(second form) ; Note the syntax-quote (`) and unquote (~) characters!
    ~(first form)
    ~(nth form 2)))

(defmacro recursive-infix [form]
  (cond
    (not (seq? form)) form
    (= 1 (count form)) `(recursive-infix ~(first form))
    :else (let [operator (second form)
                left-arg (first form)
                right-arg (rest (rest form))]
            `(~operator
              (recursive-infix ~left-arg)
              (recursive-infix ~right-arg)))))

(meditations
 "Macros are like functions created at compile time"
 (= "Hello, Macros!" (hello "Macros!"))

 "I can haz infix?"
 (= 10 (infix (9 + 1)))

 "Remember, these are nothing but code transformations"
 (= '(+ 9 1) (macroexpand '(infix (9 + 1))))

 "You can do better than that - hand crafting FTW!"
 (= '(* 10 2) (macroexpand '(infix-concise (10 * 2))))

 "Things don't always work as you would like them to... "
 (= '(+ 10 (2 * 3)) (macroexpand '(infix-concise (10 + (2 * 3)))))

 "Really, you don't understand recursion until you understand recursion"
 (= 36 (recursive-infix (10 + (2 * 3) + (4 * 5)))))
