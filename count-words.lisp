(load "packages")
(defvar *text*)
(setf *text* (read-words::read-words (car *args*)))

;;; plain vanilla bst tests
(format t "~&BST tests.")
(in-package my-bst)
(defvar *bst*)
(time (setf *bst* (list->bst cl-user::*text*)))
(format t "~&nodes = ~d" (count-nodes *bst*))
(format t "~&leaves = ~d" (count-leaves *bst*))
(format t "~&height = ~d" (height *bst*))
(defvar *sorted-values*)
(setf *sorted-values* (sort-values *bst* #'>))
(in-package cl-user)
(let ((out (open "bst-report.txt"
		 :direction :output
		 :if-exists :supersede)))
  (dolist (x my-bst::*sorted-values*)
    (format out "~&~S: ~S" (car x) (cdr x)))
  (close out))
(format t "~&word frequency report written to file bst-report.txt~%~%")

;;; avl balanced bst tests
(format t "~&AVL balanced BST tests.")
(in-package my-avl)
(defvar *bst*)
(time (setf *bst* (list->bst cl-user::*text*)))
(format t "~&nodes = ~d" (count-nodes *bst*))
(format t "~&leaves = ~d" (count-leaves *bst*))
(format t "~&height = ~d" (height *bst*))
(defvar *sorted-values*)
(setf *sorted-values* (sort-values *bst* #'>))
(in-package cl-user)
(let ((out (open "avl-report.txt"
		 :direction :output
		 :if-exists :supersede)))
  (dolist (x my-avl::*sorted-values*)
    (format out "~&~S: ~S" (car x) (cdr x)))
  (close out))
(format t "~&word frequency report written to file avl-report.txt")
