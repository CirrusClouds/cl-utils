;;;; cl-utils.lisp

(in-package #:cl-utils)

(defun flatten (xs acc)
  (cond
    ((null xs)
     acc)
    ((atom xs)
     (cons xs acc))
    (t
     (flatten (car xs) (flatten (cdr xs) acc)))))


(defun range (first &optional (second nil) (step 1))
  (macrolet ((for (second word first)
               `(loop :for x :from ,second ,word ,first :by step
                      :collect x)))
    (cond ((and second (> second first)) (for first to second))
          (second (for first downto second))
          (t (for 0 to first)))))

;; (defmacro range (m n &optional (step 1))
;;   `(loop :for i :from ,@(if (> m n)
;;                             `(,m :downto ,n)
;;                             `(,m :to ,n))
;;          :by ,step
;;          :collect i))


(defmacro ->> (&rest r)
  "Tail-led pipelining/threading"
  (reduce (lambda (o i)
            `(,@i ,o)) r))

(defmacro -> (&rest r)
  "Elixir-style front-running pipelining/threading"
  (reduce (lambda (o i)
            `(,(car i) ,o ,@(cdr i))) r))


(defmacro defarity (sym &rest bodies)
  "Clojure-style arity overloading. Useage: (defarity function-name ((x y) (+ x y)) ((x) x) (t other-stuff))"
  (let ((args (gensym))
        (acc '()))
    `(defun ,sym (&rest ,args)
       (case (length ,args)
         ,@(loop :for body in (remove-if (lambda (body)
                                           (eq (car body) t)) bodies)
                 :collect 
                 (unless (eq (first body) t)
                   `(,(length (first body))
                     (apply (lambda ,(first body) ,@(rest body)) ,args))))
         
         ,@(loop for body in bodies
                 do
                    (if (eq (car body) t)
                        (setq acc (rest body))))

         ,(if (not (null acc))
              `(otherwise ,@acc)
              `(otherwise (error "No behaviour specified for this arity")))))))


(defun group-by (n xs)
  (cond
    ((eq (length xs) 0)
     nil)
    ((< (length xs) n)
     (error "Can't group list by ~A neatly, please check modulus" n))
    (t
     (cons 
      (loop :for i :from 0 :to (- n 1)
            :collect
            (nth i xs))
      (group-by n (subseq xs n))))))

(defun enumerations (n r)
  "Be careful, this can get pretty memory intensive. Calculates all enumerations of list n in list size r"
  (labels ((flattened-enum (n1 r1 acc)
             (cond ((eq r1 0)
                    acc)
                   (t 
                    (loop :for i :in n1
                          :collect
                          (flattened-enum n1 (- r1 1) (cons i acc)))))))
    (group-by r (flattened-enum n r nil))))


(defmacro defmany (sym args lambdafunc xs)
  "Determines behaviour based on which element it's on. Pretty useful actually! Currently you need to pass a lambda E.G., (defmany hello (x) (lambda (i) (+ 1 i x)) (list 1 2 4)"
  `(progn
     ,@(loop :for i :in (cdr xs)
             :for symb = (read-from-string (format nil "~a~a" sym i))
             :collect
             `(defun ,symb ,args (funcall ,lambdafunc ,i)))))   


(defmacro defmatch (sym &rest bodies)
  "Basic pattern matching. Who needs trivia? Destructuring pls add"
  (let ((args (gensym)))
    `(defun ,sym (&rest ,args)
       (cond
         ,@(loop :for body :in bodies
                 :collect
                 `((and (eq (length ,args) (length ',(second body)))
                        (reduce (lambda (a b) (if a b nil)) (mapcar (lambda (i j)
                                    (funcall i j))
                                  ',(first body)
                                  ,args)))
                   (apply (lambda ,(second body)
                            ,@(cddr body)) ,args)))
         (t
          (error "Undefined pattern matching specifications for ~a" ,args))))))


(defmacro match (args &rest bodies)
  "Regular matching as well without defun"
  `(cond ,@(loop :for body in bodies
                 :collect
                 `((and (eq (length (list ,@args)) (length ',(second body)))
                        (reduce (lambda (a b) (if a b nil)) (mapcar (lambda (i j)
                                                                 (funcall i j))
                                                               ',(first body)
                                                               ',args)))
                   (funcall (lambda ,(second body)
                              ,@(cddr body)) ,@args)))
         (t
          (error "Undefined pattern matching specifications for ~a" ',args))))

;; (defmacro defmatch (sym &rest bodies)
;;   (let* ((args (gensym)))
;;     (declare (ignorable args))
;;     `(defun ,sym (&rest ,args)
;;        (match (,args) ,@bodies))))

(defmacro deflist (sym &body body)
  (let ((args (gensym)))
    `(defun ,sym (&rest ,args)
       (destructuring-bind (,(car body))
           ,args
         ,@(cdr body)))))


(defmacro str-ccase (clause &rest bodies)
  (let* ((cl clause))
    (declare (ignorable cl))
    `(cond ,@(loop :for body in bodies
                   :collecting
                   `( 
                     (string= ,cl ,(first body))
                     ,(second body)))
           (t
            (error "No case specified for clause ~a" ,cl)))
    ))


(defmacro list-ccase (clause &rest bodies)
  (let* ((cl clause))
    (declare (ignorable cl))
    `(cond ,@(loop :for body in bodies
                   :collecting
                   `((handler-case (destructuring-bind ,(first body)
                                       ,cl
                                     (equal (list ,@(first body)) ,cl))
                       (error ()
                         nil))
                     ,(second body)))
           (t
            (error "No case specified for clause ~a" ,cl)))))                             


(defmacro match-ccase (clause &rest bodies)
  "CCase where types are inferred and destructuring binds are performed. Now if only I could do this with match properly and to any arity... Use just like ccase"
  (let ((cl clause)
        (x (gensym)))
    `(match (,cl)
       ((stringp) (,x) (str-ccase ,x
                                  ,@bodies))
       ((numberp) (,x) (ccase ,x
                         ,@bodies))
       ((listp) (,x) (list-ccase ,x
                                 ,@bodies)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun remove-odds (xs &optional (acc 0))
    (cond ((null xs) nil)
          ((equal acc 1)
           (cons (car xs) (remove-odds (cdr xs) 0)))
          ((equal acc 0)
           (remove-odds (cdr xs) 1)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun remove-evens (xs &optional (acc 1))
    (cond ((null xs) nil)
          ((equal acc 1)
           (cons (car xs) (remove-odds (cdr xs) 0)))
          ((equal acc 0)
           (remove-odds (cdr xs) 1)))))


(defun Utypep (arg type/s)
  "Allows for the definition of Type unions and checking contents of type-lists"
  (cond ((atom type/s)
         (typep arg type/s))
        ((equal (car type/s) 'Union)
         (loop :for type :in (cdr type/s)
               :if (Utypep arg type)
                 :return t))
        (t
         (if (Utypep arg 'cons)
             (every (lambda (x) x)
                    (loop :for type :in type/s
                          :for cell :in arg
                          :collect (Utypep cell type)))
             nil))))

(defun parse-type (arg)
  "Tells you the type of the arg in a way Utypep would understand"
  (if (atom arg)
      (type-of arg)
      (loop :for i :in arg
            :collect (parse-type i))))

(defmacro tefun (sym args res &body body)
  "Statically typed functions! One big issue with this is its interaction with hash tables + optional vals?
   A lambda will accept optional vals..."
  (let ((funcargs (gensym))
        (odds (remove-odds args))
        (evens (remove-evens args)))
    (if (equal (rem (length args) 2) 0)
        `(defun ,sym (&rest ,funcargs)
           (mapc (lambda (arg type)
                   (unless (Utypep arg type)
                     (error "~A: Types do not match for ~A. Is ~A, but requires ~A" ',sym arg (parse-type arg) type)))
                 ,funcargs
                 (list ,@evens))
           (let ((result
                   (apply (lambda ,odds ,@body) ,funcargs)))
             (if (Utypep result ,res)
                 result
                 (error "~A: Result is not of type ~A but instead ~A" ',sym ,res (parse-type result)))))
        (error "arg list is improper"))))


;; Proof that this works.
;; Todo: Make a condition for type error

(defun NN-p (num)
  (> num 0))

(deftype NN ()
  `(and integer
        (satisfies NN-p)))              ; Will even work with user-defined types like these natural numbers

(setf *h* (make-hash-table :test 'equal))
(tefun fibonacci ('integer n 'hash-table memo) '(Union bit integer)
  (if (gethash n memo)
      (gethash n memo)
      (if (or (= n 0) (= n 1))
          1
          (let ((result  (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))
            (setf (gethash n memo) result)
            result))))


(tefun test ('integer x) '(integer)
  (list x))

