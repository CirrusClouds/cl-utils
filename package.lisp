;;;; package.lisp

(defpackage #:cl-utils
  (:use #:cl)
  (:nicknames :util)
  (:export 
   :flatten 
   :range 
   :->> 
   :defarity
   :group-by
   :enumerations
   :defmatch
   :defmany
   :match))
