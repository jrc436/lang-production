;; Associative learning test
(load "act-up.lisp")
(require "act-up" "act-up.lisp")
(use-package :act-up)



(define-chunk-type person pname city)
(define-chunk-type food)

(defvar *beer* nil)
(defvar *chris* nil)
(setq *chris* nil)

(defun test ()
  (reset-model)

  ;; Create chunks
  (let ((john (make-person :name 'john))
	(ion (make-person :name 'ion))
	(david (make-person :name 'david))
	(chris (make-person :name 'chris))
	(fries (make-food :name 'fries))
	(beans (make-food :name 'beans))
	(beer (make-food :name 'beer))
	(toast (make-food :name 'toast)))

    (dotimes (n 100) (learn-chunk beer))


we simply assume that the semantics are only associated with those syntactic forms and their alternate forms and not with that many more.
the real problem is that the semantics and syntax combinations are very frequent but at the samre time so are the general syntactic chunks


    ;; learn / associate people with foods
    (dotimes (n 30)
      (learn-chunk john (list fries beer))   ; reference to "beer" chunk not present in DM
      (learn-chunk david (list toast beer)))
    (dotimes (n 10)
      (learn-chunk ion (list beans))
      (learn-chunk chris (list beer)))  ; reference to different (!) beer chunk

    (print (list (explain-activation john (list beer))
		 (explain-activation david (list beer))
		 (explain-activation ion (list beer))
		 (explain-activation chris (list beer))))

    (print (list (explain-activation john (list beer))
		 (explain-activation david (list beer))
		 (explain-activation ion (list beer))
		 (explain-activation chris (list beer))))
    (setq *chris* chris)
    (setq *beer* beer)
    ))


;; (slot-value (make-person) 'act-up::attrs)

;; (learn-chunk (make-person) '(fries beer))