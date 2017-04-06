;;; f.scm
;;; << WHAT DOES THIS FILE CONTAIN >>
;;;
;;; Programmer: Mayer Goldberg, 2012

(define fact
  (let ((* *)
	(- -)
	(zero? zero?))
    (lambda (n)
      (if (zero? n)
	  1
	  (* n (fact (- n 1)))))))