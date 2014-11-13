;; act-r 6 compatibility code
;; it's a big hack.


(defun chunk-spec-variable-p (chunk)
  chunk)

(load (format nil "~a/misc-utils.lisp" (directory-namestring *load-truename*)))

(defmacro define-module-fct (&rest args)
	  nil)

(defvar *actr-random-module* nil)

(defmacro get-module (m)
  (if (eq m 'random-module)
      '*actr-random-module*
      nil))

;; allow random module to work
(load (format nil "~a/random.lisp" (directory-namestring *load-truename*)))


(setq  *actr-random-module* (create-random-module nil))

