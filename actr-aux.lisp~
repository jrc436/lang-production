(declaim (optimize (speed 0) (space 0) (debug 3)))
 
(defparameter *talking* t)
(defun say (form &rest args)
  (if *talking*
       (format t "~a ~a: ~a~%" (meta-process-name *current-actUP-meta-process*) (actup-time) (apply 'format nil form args))))

(defun mean (&rest list)
  (/ (apply 'sum list) (length list)))

(defun sum (&rest list)
  (loop for x in list sum x))
 
(defun repeat-seq (n sequence)
  (if (> n 0)
      (append sequence (repeat-seq (1- n) sequence))
      nil))



(defun structure-alist (object)
  (let ((spec (cdr (second (read-from-string (concatenate 'string "( \\" (let ((*print-circle* t)) (prin1-to-string object)) " )"))))))
    (loop for argval on spec by #'cddr collect
	 (cons (first argval) (second argval)))))

(defun structure-value-count (object &rest values)
  (let ((spec (cdr (second (read-from-string (concatenate 'string "( \\" (let ((*print-circle* t)) (prin1-to-string object)) " )"))))))
    (loop for argval on spec by #'cddr sum
	 (if (member (second argval) values) 1 0))))




(defparameter my-random-state (make-random-state))



(defun choice (sequence)
  (and sequence
       (elt sequence (random (length sequence) my-random-state))))

(defun pick-some (num sequence)
  (if (and (> num 0) sequence)
      (let ((index (random (length sequence) my-random-state)))
	
	(multiple-value-bind (rest remainder) 
	    (pick-some (1- num) (append (subseq sequence 0 index) (subseq sequence (1+ index))))

	  (values (cons (elt sequence index) rest)
		  remainder)))
      (values nil sequence)))

(defun shuffle (sequence)
  (pick-some (length sequence) sequence))
(defun noisy-shuffle (list noise)
  "Shuffles list.  List is changed by side-effect.
Noise [0,1] gives amount of shuffling. 
Returns shuffled list."
    (loop for i from 1 to (* 0.5 noise (length list)) do
	 
	 (let ((p1 (random (length list) my-random-state))
	       (p2 (random (length list) my-random-state)))
	   (let ((elt (nth p1 list)))
	     (setf (nth p1 list) (nth p2 list)
		   (nth p2 list) elt))))
    list)

(defun random-pareto (&optional xm k)
  "Returns a random sample from a pareto distribution.
We're using inverse transform sampling to obtain the sample."
  (/ (or xm 1) (expt (random 1.0 my-random-state) (/ 1 (or k 1)))))


;; calculate necessary noise

(defun actr-noise-for-activation-delta (x p)
  "This is derived from the cumulative distribution function
for a logistic distribution, with mu=0 (as in act-r-noise),
solved for S.  We provide the likelihood level p and the chosen activation level x.

I.e., this function will return s, leading to a logistic distribution yielding
samples that are `p' likely to fall below x.

p > 0.5 is a hard requirement, because s>0.  This may be understood given that
the noise distribution is two-tailed so that 0.5 is the maximum we can achieve
on one side of the mean."
  (if (> p 0.5)
      (- (/ x (log (- (/ 1 p) 1))))
      (error "p>0.5 (s>0) violation in parameter for noise.")))


(defun actr-noise-for-time (time1 time2 p)
"Calculcate noise s parameter to achieve a probability distribution 
of ((1-p),p) for the retrieval of two chunks (a,b),
a learned at t-time1, and a learned at t-time2.

p>0.5, because the more recent chunk will always be more likely to be retrieved.

e.g., for 180sec and 0sec, activation decay (0,5) causes a decay of 2.596.

however, if chunks were presented at t-380 and t-200, the decay would be 2.1036."

(let* ((decay (- (car (sgp :bll))))
       (mult 12)
       (act-delta (- (log (+ (* mult (expt time1 decay)) (* mult (expt time2 decay)))))))
 (actr-noise-for-activation-delta act-delta p)))


; In 1200 secs, decay (0.5) causes 0.0288 reduction in base-level activation

(defun actr-noise-unit-test ()

(loop for x in '(0.1 0.4 22 66) do
     (print "The following figures should be reasonably small.")
     (loop for p in '(0.6 0.8 0.95) do
	  (let ((sum 0) (num 0) (s (actr-noise-for-activation-delta x p)))
	    (print s)
	    (loop for c from 1 to 1000 do
		 (if (<= (act-r-noise s) x)
		     (incf num))
		 (incf sum))
	    ;;we probably want to print sdev here rather than mean(p-P)
	    (print (format nil "x=~a p=~a  \hat{p}=~a  mean (p-\hat{p})=~a" x p (/ num sum) (- (/ num sum) p)))))))
		 
 

;; Fay's experiment lasted about 6 hours
;; with 1536 trials, and 4 games played in parallel
;; this results in about 56sec per trial (not taking into account breaks and UI input, etc.)
;; thus, a 16-item game should take about 15 minutes.
;; Consequently, it would be difficult to explain switching behavior as a result of noise:
;; CL-USER> (actr-noise-for-time 1800 900 0.75)
;; :BLL 0.5 (default NIL) : Base Level Learning
;; 2.6091075
;; CL-USER> (actr-noise-for-time 1800 900 0.85)
;; :BLL 0.5 (default NIL) : Base Level Learning
;; 1.6524818

;; strengthening of individual links, i.e. association learning rather than learning a domain language?
;; this would probably not sit well with the rapid acquisition of the domain language, given that
;; the initial associations (hospital-ambulance) have been established in the long term, thus will only slowly change.

;; would multiple presentations of the domain chunks make a difference?
;; yes, a big one.
;; 12 presentations, and we get a 0.25/0.75 retrieval distribution at very reasonable noise levels of 0.34



;; other stuff


;;; insert-by-weight will add new child states to an ordered list of 
;;; states-to-try.  
(defun insert-by-weight (children sorted-list)
  (cond ((null children) sorted-list)
        (t (insert (car children) 
           (insert-by-weight (cdr children) sorted-list)))))

(defun insert (item sorted-list)
  (cond ((null sorted-list) (list item))
        ((> (car item) (car (car sorted-list)))
         (cons item sorted-list))
        (t (cons (car sorted-list) (insert item (cdr sorted-list))))))




;; aggregation


(defparameter *aggregate-sum* nil)
(defparameter *aggregate-num* nil)
(defparameter *aggregate-colnames* nil)
(defun aggregate-clear (colnames)
  "Clear aggregation dataset.
COLNAMES is a sequence of strings indicating the
names of variables that will be given as value(s)
and as conditions."
  (setq *aggregate-sum* nil *aggregate-num* nil
	*aggregate-colnames* colnames))
(defun aggregate (value condition-list)
  "Add value to aggregation dataset.
VALUE may be a number or a sequence of numbers.
CONDITION-LIST is a sequence indicating the conditions that
VALUE will be associated with. 
VALUE is usually a dependent variable (measurement) obtained
when conditions CONDITION-LIST were present.
Aggregation will occur over all VALUEs in this combination of conditions."

  (if (assoc condition-list *aggregate-sum*  :test 'equal)
      (progn
	(setf (cdr (assoc condition-list *aggregate-sum*  :test 'equal))
	      (vector-add value (cdr (assoc condition-list *aggregate-sum* :test 'equal))))
	(incf (cdr (assoc condition-list *aggregate-num*  :test 'equal))))
      (progn
	(push (cons condition-list value)
	      *aggregate-sum*)
	(push (cons condition-list 1)
	      *aggregate-num*)))
nil)
(defun vector-add (v1 v2)
  (if (and (numberp v1) (numberp v2))
      (+ v1 v2)
      (loop for i1 in v1 for i2 in v2 collect (+ i1 i2))))
  

;; (defvar tree nil)
;; (defun get-tree (tree path)
;;   (declare (special tree))
;;   (if path
;;       (let ((a (assoc (car path) tree)))
;; 	(if a
;; 	    (get-tree (cdr a) (cdr path))
;; 	    (let ((new-tree '(nil)))
;; 	      (format t "before ~a " new-tree)
;; 	      (get-tree new-tree (cdr path))

;; 	      (format t "after ~a " new-tree)
;; 	      (push (cons (car path) new-tree) tree)
;; 	      (print tree)
;; 	      nil)))
;;       tree))

;; (setq uu '((q . ((u . ((k . 11)))))))
;; (get-tree uu '(q u k))
;; (setq xx '(a b c))
;; (get-tree uu xx)


(defun print-aggregates (&optional file)
  "Print aggregation set as a table.
Output is printed to FILE if given, standard out otherwise.
In this implementation, only mean values are printed."
  (let ((str (if file 
		 (open file 
		       :direction :output :if-exists :supersede :if-does-not-exist :create)
		 t)))
    (format str "~{~A~#[~:;~t~]~}~%" *aggregate-colnames*)
    (loop for s in (reverse *aggregate-sum*)
       for n in (reverse *aggregate-num*)
       do
	 (if (eq (car s) (car n))
	     (when (> (cdr n) 0)
		 (format str "~{~A~#[~:;~t~]~}" (car s) )
		 (if (numberp (cdr s))
		     (format str " ~A"  (float (/ (cdr s) (cdr n))))
		     (loop for i in (cdr s) do
			  (format str " ~A" (float (/ i (cdr n))))))
		 (format str "~%"))
	     (format str "ERROR WHILE PRINTING~%")))

    (if file (close str))))


;; (defun show-aggregates ()
;;   (show-aggregates-1 *aggregate-sum* *aggregate-num* nil))

;; (defun show-aggregates-1 (sum num filter)

;;   (cond ((= (length (caar sum)) 1)
;; 	 (loop for s in sum for n in num 
;; 	    do (format t "~a~t" (/ sum num)))
;; 	 (format t "~%"))
;; 	(t

;; 	 (let ((keys (loop for n in num
;; 		       collect
;; 			 (caar n))))
;; 	   (loop for key in keys do
;; 		(loop for s in sum for n in num do
		     
;; 	      (cons (cdr (car s))
;; 		    (cdr (car s))

;; )))))))



