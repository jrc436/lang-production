;;let's try to acquire correct coocc and occ frequency stats from the corpus


(declaim (optimize (speed 03) (space 0) (debug 0)))


;; load ACT-UP library
(load "act-up.lisp")
(require "act-up" "act-up.lisp")
(use-package :act-up)


(defparameter act-up::*debug* *critical*)

;; Util

(defun add-chunk (chunk &optional presentations)
 
  (if (act-up::chunk-name-not-unique (act-up::actup-chunk-name chunk))
      (progn
	(format t "Chunk ~a of same name already in DM. Not added." (or (act-up::actup-chunk-name chunk) chunk))
	(act-up::get-chunk-by-name (act-up::actup-chunk-name chunk)))
      (progn
	(setf (act-up::actup-chunk-first-presentation chunk) -473040000 )  ; 15 years back
	(setf (act-up::actup-chunk-recent-presentations chunk) '(-10022400 -5011200))
	(setf (act-up::actup-chunk-total-presentations chunk) (or presentations 30000))
	(push (actUP-time) (act-up::actup-chunk-presentations chunk))
	(push chunk (model-chunks (current-actup-model)))
	chunk)))


(defun mean (list)
  (/ (sum list) (length list)))

(defun sum (list)
  (loop for x in list sum x))


;; define a ditransitive verb
(defvar corpus-years-factor 250)
(DEFPARAMETER sji-factor corpus-years-factor);; war 100.0       about 15 years

(defun add-ditrans-verb (name-r lex)
  (let ((name (string-upcase name-r)))

    (let ((ditrans-order-1
	   (add-chunk
	    (make-lex-syn-arg-order
	     :name (intern (concatenate 'string name "-DITRANS-ORDER"))
	     :for-lexeme (intern (concatenate 'string name "SEM"))
	     :for-syn 'ditrans)))
	  (ditrans-order-2
	   (add-chunk
	    (make-lex-syn-arg-order
	     :name (intern (concatenate 'string name "-DITRANSTO-ORDER"))
	     :for-lexeme (intern (concatenate 'string name "SEM"))
	     :for-syn 'ditrans-to)))
	  (lex-chunk
	   (add-chunk
	    (make-lexeme
	     :name (intern (concatenate 'string name "SEM"))
	     :sem (intern name)
	     :syn 'nullsyn
	     :lex lex))))

      (let ((sji-factor (* 10 sji-factor)))
	(add-sji-fct
	 (list
	  (list ditrans-order-1
		'agent (* sji-factor 2.5))
	  (list ditrans-order-1
		'functor (* sji-factor 1.5))
	  (list ditrans-order-1
		'goalrole (* sji-factor 1.0))
	  (list ditrans-order-1
		'theme (* sji-factor 0.5))
	  (list ditrans-order-2
		'agent (* sji-factor 2.5))
	  (list ditrans-order-2
		'functor (* sji-factor 1.5))
	  (list ditrans-order-2
		'goalrole (* sji-factor 0.5))
	  (list ditrans-order-2
		'theme (* sji-factor 1.0)))))


      ; joint frequencies are relative to the frequencies of the lexical items and the syntactic chunks
      (let ((sji-base 0)
	    (sji-factor (* 1 sji-factor)))
	(loop for c in (list (intern name) lex-chunk) do
	     ;; semantics and lexemes
	   ;; we're assuming that the verb semantics are unambiguous
	     (add-sji-fct
	      (list
	       (list c
		     'ditrans
		     (list (floor (+ sji-base (* (* sji-factor) (+ 1 (or (synfreq-raw lex "((S[dcl]\\NP)/NP)/NP") 0)))))
			   'some-time))
	       (list c
		     'ditrans-to
		     (list (floor (+ sji-base (* (* sji-factor) (+ 1 (or (synfreq-raw lex "((S[dcl]\\NP)/PP)/NP") 0)))))
			   'some-time)))))))))

(defun add-trans-verb (name-r lex)
  (let ((name (string-upcase name-r)))

    (let ((ditrans-order-1
	   (add-chunk
	    (make-lex-syn-arg-order
	     :name (intern (concatenate 'string name "-TRANS-ORDER"))
	     :for-lexeme (intern (concatenate 'string name "SEM"))
	     :for-syn 'ditrans)))
	  (ditrans-order-2
	   (add-chunk
	    (make-lex-syn-arg-order
	     :name (intern (concatenate 'string name "-TRANSTO-ORDER"))
	     :for-lexeme (intern (concatenate 'string name "SEM"))
	     :for-syn 'ditrans-to)))
	  (lex-chunk
	   (add-chunk
	    (make-lexeme
	     :name (intern (concatenate 'string name "SEM"))
	     :sem (intern name)
	     :syn 'nullsyn
	     :lex lex))))

      (let ((sji-factor (* 10 sji-factor)))
	(add-sji-fct
	 (list
	  (list ditrans-order-1
		'agent (* sji-factor 2.5))
	  (list ditrans-order-1
		'functor (* sji-factor 1.5))
	  (list ditrans-order-1
		'goalrole (* sji-factor 1.0)))))
	  


      ; joint frequencies are relative to the frequencies of the lexical items and the syntactic chunks
      (let ((sji-base 0)
	    (sji-factor (* corpus-years-factor sji-factor)))
	(add-sji-fct
	 (list
	  (list lex-chunk lex-chunk
		'trans-to
		(list (floor (+ sji-base (* (* sji-factor) (+ 1 (or (synfreq-raw lex "(S[dcl]\\NP)/PP") 0)))))
		      'some-time))
	  ))))))

;; Prob distribution from  corpus

(load "../split-sequence.lisp")
(import 'SPLIT-SEQUENCE::split-sequence)

(DEFPARAMETER *freq-hash* (make-hash-table :test 'equal))
(DEFPARAMETER *freq-sum* 0)
(DEFPARAMETER *synfreq-hash* (make-hash-table :test 'equal)) 

(defun load-wsj-frequencies ()
;; reads table generated with ew.perl
;  (import 'SPLIT-SEQUENCE::split-sequence)
; cat `find ../ccgbank/data/AUTO/ -name *.auto`  
  (with-open-file (stream "../ccgbank-inc-freq.txt")
    (do ((line (read-line stream nil)
               (read-line stream nil)))
        ((null line))
      (let* ((tok (split-sequence #\Tab line))
	    (f (parse-integer (car (cdr tok)))))
	(setq *freq-sum* (+ *freq-sum* f))
	(setf (gethash   (string-downcase (car tok)) *freq-hash*) f ))))
  (with-open-file (stream "../ccgbank-inc-lex-freq.txt")
    (do ((line (read-line stream nil)
               (read-line stream nil)))
        ((null line))
      (let* ((tok (split-sequence #\Space line))
	    (f (parse-integer (car tok))))
	(setq *freq-sum* (+ *freq-sum* f))
	(push
	 (cons (string-downcase (car (cdr (cdr tok))))
	       f)
	 (gethash (string-downcase (car (cdr tok)))
		  *synfreq-hash*)))))

  (setq corpus-years-factor 39)  ;; 225M/5.8M    15-year word exposure / corpus count
  (setq sji-factor corpus-years-factor))



(defun load-swbd-frequencies ()
  ;; reads table generated with this:
  ;; cat switchboard-ccg-rules.txt | awk '{if($7 == "I") { print $8;}}' -  | sort | uniq -c >~/PhD/ACT-R/swbd-freq.txt
;; cat switchboard-ccg-rules.txt | awk '{if($7 == "I") { print $9, $8;}}' -  | sort | uniq -c | awk '{if($1>10) {print $1, $2, $3}}' - >~/PhD/ACT-R/swbd-lex-freq.txt

 
  (with-open-file (stream "../swbd-freq.txt")
    (do ((line (read-line stream nil)
               (read-line stream nil)))
        ((null line))
      (let* ((tok (split-sequence #\Space (string-trim " " line)))
	    (f (parse-integer (car tok))))
	(setq *freq-sum* (+ *freq-sum* f))
	
	(setf (gethash   (string-downcase (car (cdr tok))) *freq-hash*) f ))))
  (with-open-file (stream "../swbd-lex-freq.txt")
    (do ((line (read-line stream nil)
               (read-line stream nil)))
        ((null line))
      (let* ((tok (split-sequence #\Space line))
	    (f (parse-integer (car tok))))
	(setq *freq-sum* (+ *freq-sum* f))
	(push
	 (cons (string-downcase (car (cdr (cdr tok))))
	       f)
	 (gethash (string-downcase (car (cdr tok)))
		  *synfreq-hash*)))))
  
  (setq corpus-years-factor 329)  ;; 225M/684K
  (setq sji-factor corpus-years-factor))

; to reset: (setq  *synfreq-hash* (make-hash-table :test 'equal))
(load-swbd-frequencies)  ;; must be loaded, otherwise you can't define the model below
;(load-wsj-frequencies)

(defun freq-raw (sym-string)
  (gethash (string-downcase sym-string) *freq-hash*))

(defun freq (sym-string)
  (/ (freq-raw sym-string) *freq-sum*))

(defun lexfreq-raw (word)
  (loop for c in (gethash (string-downcase word) *synfreq-hash*) sum (cdr c)))

(defun synfreq-raw (word syn)
 (cdr (assoc (string-downcase syn) 
	     (gethash (string-downcase word) *synfreq-hash*) :test 'equal)))

(defun synfreq-p (word syn)
  (let* ((alist (gethash (string-downcase word) *synfreq-hash*))
	 (wfreq (cdr (assoc (string-downcase syn) 
			    alist :test 'equal))))
    (if (and alist wfreq) ;; not nil!
	(/ wfreq (sum (mapcar 'cdr alist)))
	0)))


;; 

;; Subsymbolic

(defun init-base-levels ()

  ;; initialize base levels
  ;; both syn categories and words are set to their WSJ (CCGBank) frequencies

  ;; CCG BAnk contains 985 non-hapax (freq>1) rules 22670 word forms (freq>1).

  (defvar seconds-per-year 30758400) ; 1000000

  (let ((factor corpus-years-factor) ; works fine with 50  ;  25 = about 15 years
	(decay (- (* 15 seconds-per-year))) ; 0.03 1000000
	(freqs nil) (avg-chunks nil))
    (loop for c in (filter-chunks (model-chunks (current-actUP-model)) '(:chunk-type syntype)) do
    	 (let ((f (freq-raw (syntype-ccgbank-type c))))
    	   (if (and f (> f 0))
    	       (progn
    		 (set-base-levels-fct (list (list c (* factor f) decay)))
    		 (setq freqs (cons f freqs)))
    	       (setq avg-chunks (cons c avg-chunks)))))
    
    (loop for c in (filter-chunks (model-chunks (current-actUP-model)) '(:chunk-type lexeme)) do
    	 (let ((f (lexfreq-raw (lexeme-lex c))))
    	   (if (and f (> f 0))
    	       (progn
    		 (set-base-levels-fct (list (list c (* factor f) decay)))
    		 (setq freqs (cons f freqs)))
    	       (setq avg-chunks (cons c avg-chunks)))))

    
    ;; set remaining chunks to the average frequency
    (let ((mean-pres (floor (* factor (mean freqs)))))
      (mapc (lambda (c)
	      (set-base-levels-fct (list (list c mean-pres decay))))
	    avg-chunks)))

					; we don't want the delay to be so big that chunks go "forgotten"
					;(get-base-level offerSem)  ; 2.54   0.24
					;(get-base-level giveSem) ; 1.68    -0.62





					; lexeme -> syntax node associations

  ;; for the NPs
  (mapcar (lambda (x)
	    (add-sji-fct (list (list (act-up::get-chunk-by-name x) 'np (* sji-factor 10.0)) (list x 'np-typeraised (* sji-factor 10.0)))))
	  '(doc1 doc2 girl1 cheerleader1 friend1 cop1 cop2 flower1 flower2 seat1 hammer1))


  ;; combinatory spreading activation

  ;; we don't want any default association between chunks 

  (let ((syntype-sijs
	 (loop for c in (filter-chunks (model-chunks (current-actUP-model)) '(:chunk-type syntype))
	      collect
	      (list c 0.0))))
    
    (loop for c in (filter-chunks (model-chunks (current-actUP-model)) '(:chunk-type syntype))
	 do
	 (add-sji-fct (mapcar (lambda (sij) (cons c sij)) syntype-sijs))))

  (add-sji-fct `( (intrans np ,(* sji-factor 0.5))
		  (trans np ,(* sji-factor 0.5))
		  (trans-to pp-to ,(* sji-factor 0.5))  ; might spread to expected preposition instead?
		  (ditrans np ,(* sji-factor 0.5))
		  (ditrans-to pp ,(* sji-factor 0.5))
					; (sfnpfppto pp ,(* sji-factor 1.0)) (sfnpfppto NP-typeraised ,(* sji-factor -0.5))
		  (sfpptofnp np ,(* sji-factor 1.5)) (sfpptofnp NP-typeraised ,(* sji-factor -1.0))
		  (SFNPFNP np ,(* sji-factor 1.5)) (SFNPFNP NP-typeraised ,(* sji-factor -1.0))
		  (SFNP np ,(* sji-factor 1.5)) (SFNP NP-typeraised ,(* sji-factor -1.0))
		  (pp-to prep-to ,(* sji-factor 1.0))
		  (beginning-of-sentence NP-typeraised ,(* sji-factor 1.0)) 
		  (beginning-of-sentence NP 0.0)))

  ;; at the beginning of sentences, we want typeraised NPs rather than NPs
					; however, this activation spreading can't work, because lex. entries are activated based on whether their content chunks are 
  ;; in the buffers directly.
  ;; perhaps we need a more direct "expectation"?  



  ;; this forces syn choice during prime step (via preferred-predicate)
  ;; but if we do this, then it'll retrieve this as long as any ditrans
  ;; is in the buffer. we don't want that kind of spread.
  ;; (add-sji (ditrans ditrans 250.0) (ditrans-to ditrans-to 250.0)  )


  ;; these are adjusted so that the spreading activation coming from
  ;; the *-ROLE slots is stronger (but the same for all).
  ;; (untested)
  ;; (add-sji (give1 agent 25.0) (give1 functor 15.0) (give1 goalrole 10.0) (give1 theme 5.0)) 
  ;; (add-sji (give2 agent 25.0) (give2 functor 15.0) (give2 goalrole 5.0) (give2 theme 10.0)) 
					;(add-sji (giveSem agent 25.0) (giveSem functor 15.0) (giveSem goalrole 10.0) (giveSem theme 5.0)) 


  ;; ordering information
  (add-sji-fct '((beginning-of-sentence beginning-of-sentence -10)))



  (add-sji-fct `((talk-transto-order agent ,(* sji-factor 2.5)) 
		 (talk-transto-order functor ,(* sji-factor 1.5)) 
		 (talk-transto-order goalrole ,(* sji-factor 0.5)) 
		 (talk-transto-order theme ,(* sji-factor 0.0))))


  (add-sji-fct `((agent agent ,(* sji-factor 0.1)) (goalrole goalrole ,(* sji-factor 0.1)) (theme theme ,(* sji-factor 0.1)) (functor functor ,(* sji-factor 0.1)))) 
  (add-sji-fct '((agent-done agent -50000) (goalrole-done goalrole -50000) (theme-done theme -50000) (functor-done functor -50000)))

  (add-sji-fct '((agent ditrans 30) (goalrole ditrans 30) (theme ditrans 30)))
  (add-sji-fct '((agent ditrans-to 30) (goalrole ditrans-to 30) (theme ditrans-to 30)))
  (add-sji-fct '((agent trans 30) (goalrole trans 30) (theme trans -100)))
  (add-sji-fct '((agent trans-to 30) (goalrole trans-to 30) (theme trans-to -100)))
  (add-sji-fct '((agent intrans 30) (goalrole intrans -100) (theme intrans -100)))

  ;; some Sijs to be able to force particular verb choices during priming
  (add-sji-fct `((prefer-to-pp ditrans-to ,(* sji-factor 100.0))
  		 (prefer-to-pp ditrans ,(* sji-factor -100.0))
  		 (prefer-to-pp trans-to ,(* sji-factor -100.0))
  		 (prefer-to-pp trans ,(* sji-factor -100.0))
  		 (prefer-np-np ditrans-to ,(* sji-factor -100.0))
  		 (prefer-np-np ditrans ,(* sji-factor 100.0))
  		 (prefer-np-np trans-to ,(* sji-factor -100.0))
  		 (prefer-np-np trans ,(* sji-factor -100.0))))


  )

;; Model
(define-chunk-type syntype
      class
    comb
    left
    right
    attract
    ccgbank-type)

  (define-chunk-type lexeme
      syn
    sem
    lex
    ccgbank-type
    )


  (define-chunk-type synsem
      ;; to do: how many of these do we actually need?
      state
    class
    role
    intention

    functor
    functor-lexform
    functor-synform
    functor-sfcform
    functor-argorder
    functor-role
    functor-done

    agent
    agent-lexform
    agent-role
    agent-done

    theme
    theme-lexform
    theme-role
    theme-done

    goalrole
    goalrole-lexform
    goalrole-role
    goalrole-done

    context-type
    context-type-left
    context-type-comb
    context-type-right


    type-cat
    type-left
    type-comb
    type-right

    lex ;; current phrase lex form

    attract
    attract-lexform  ;; function words that are attracted by rules

    stacked-context-type
    stacked-context-type-left
    stacked-context-type-comb
    stacked-context-type-right

    current-sem
    current-sem2
    current-sem3

    preferred-predicate
    )

  (define-chunk-type lex-syn-arg-order
      for-lexeme
    for-syn
    )

  (define-chunk-type themerole
      slot marker marker-done lexform)


  (define-chunk-type combination left left-comb left-left left-right right right-comb right-left right-right comb result)

(defun init-model ()
  (reset-model)
  
;;; DM

  (add-chunk (make-themerole :name 'theme :slot 'theme :marker 'theme-role :marker-done 'theme-done :lexform 'theme-lexform))
  (add-chunk (make-themerole :name 'agent :slot 'agent :marker 'agent-role :marker-done 'agent-done :lexform 'agent-lexform))
  (add-chunk (make-themerole :name 'goalrole :slot 'goalrole :marker 'goalrole-role :marker-done 'goalrole-done :lexform 'goalrole-lexform))
  (add-chunk (make-themerole :name 'functor :slot 'functor :marker 'functor-role :marker-done 'functor-done :lexform 'functor-lexform))

  (add-chunk (make-combination :name 'combinable-np-dit :left 'NP-typeraised :left-comb '/ :left-left 's :left-right 'intrans
			       :right 'ditrans :right-comb '/ :right-left 'trans :right-right 'np
			       :comb 'backward-composition :result 'sfnpfnp))
  (add-chunk (make-combination :name 'combinable-np-ditto :left 'NP-typeraised :left-comb '/ :left-left 's :left-right 'intrans
			       :right 'ditrans-to :right-comb '/ :right-left 'trans-to :right-right 'np
			       :comb 'backward-composition :result 'sfpptofnp))
  (add-chunk (make-combination :name 'combinable-np-tr :left 'NP-typeraised :left-comb '/ :left-left 's :left-right 'intrans
			       :right 'trans :right-comb '/ :right-left 'intrans :right-right 'np
			       :comb 'backward-composition :result 'sfnp))
  (add-chunk (make-combination :name 'combinable-np-trto :left 'NP-typeraised :left-comb '/ :left-left 's :left-right 'intrans
			       :right 'trans-to :right-comb '/ :right-left 'intrans :right-right 'pp-to
			       :comb 'backward-composition :result 'sfppto))
  (add-chunk (make-combination :name 'combinable-np-int :left 'NP-typeraised :left-comb '/ :left-left 's :left-right 'intrans
			       :right 'intrans :right-comb '\\ :right-left 's :right-right 'np
			       :comb 'backward-composition :result 'sfnpfnp))

  ;; (TYPE COMBINATION LEFT-COMB / LEFT-LEFT S LEFT-RIGHT INTRANS
  ;;         RIGHT-COMB / RIGHT-LEFT TRANS RIGHT-RIGHT NP)

  ;; (add-chunk (make-combination :name 'combinable-np-dit :left 'NP-typeraised :right 'ditrans :comb 'backward-composition :result 'sfnpfnp))
  ;; (add-chunk (make-combination :name 'combinable-np-tr :left 'NP-typeraised :right 'trans :comb 'backward-composition :result 'sfnp))
  ;; (add-chunk (make-combination :name 'combinable-np-trto :left 'NP-typeraised :right 'trans-to :comb 'backward-composition :result 'sfppto))
  ;; (add-chunk (make-combination :name 'combinable-np-int :left 'NP-typeraised :right 'intrans :comb 'backward-composition :result 'sfnpfnp))

  ;; (THEME-DONE ISA donerole)
  ;; (AGENT-DONE ISA donerole)
  ;; (GOALROLE-DONE ISA donerole)
  ;; (FUNCTOR-DONE ISA donerole)

  (add-chunk (make-syntype :name 'beginning-of-sentence) 1000)

;;; changed this from NP0 to NP --- UNTESTED
  (mapc 'add-chunk
	(list
	 (make-syntype :class 'basic :name 'np)
	 (make-syntype :class 'complex :name 'NP-typeraised :comb '/ :left 's :right 'intrans) ; S/(S\NP) 

	 ;; this was intrans -- how do we decide between intrans / ditrans?
	 ;; theoretically this could be done because the verb is decided first, 

	 (make-syntype :class 'basic :name 'pp-to)
	 (make-syntype :class 'basic :name 'vp)
	 (make-syntype :class 'basic :name 's)

	 ;; to do: implement type-raising and forward combination

	 (make-syntype :class 'complex :name 'intrans :comb '\\ :left 's :right 'np :ccgbank-type "S[dcl]\\NP")  ;; S\\NP
	 (make-syntype :class 'complex :name 'trans :comb '/ :left 'intrans :right 'np :ccgbank-type "(S[dcl]\\NP)/NP") ;; (S\\NP)/NP
	 (make-syntype :class 'complex :name 'trans-to :comb '/ :left 'intrans :right 'pp-to :attract 'lex_prep_to :ccgbank-type "(S[dcl]\\NP)/PP") 
	 (make-syntype :class 'complex :name 'ditrans :comb '/ :left 'trans :right 'np  :ccgbank-type "((S[dcl]\\NP)/NP)/NP")  ;; 
	 (make-syntype :class 'complex :name 'ditrans-to :comb '/ :left 'trans-to :right 'np  :ccgbank-type "((S[dcl]\\NP)/PP)/NP") ;;  S[dcl]\NP)/PP)/NP 

					; this is an incremental version -- needs to be fixed once we have type-raising
					; (ditrans make-syntype :class 'complex :name 'ditrans :comb '\\ :left 'sfnpfnp :right 'np ) ;; ((S/NP)/NP)\\NP
					; (ditrans-to make-syntype :class 'complex :name 'ditrans-to :comb '\\ :left 'sfnpfppto :right 'np ) ;; ((S/NP)/PP)\\NP
					; (ditrans-to make-syntype :class 'complex :name 'ditrans-to :comb '\\ :left 'sfpptofnp :right 'np ) ;; ((S/PP)/NP)\\NP

	 (make-syntype :class 'complex :name 'prep-to :comb '/ :left 'pp-to :right 'np :ccgbank-type "((S\\NP)\\(S\\NP))/NP") ;; ((s\np)\(s\np))/np	27357
	 (make-syntype :class 'complex :name 'sfnp :comb '/ :left 's :right 'np :ccgbank-type "S/NP") ;; S/NP  pretty rare anyways
	 (make-syntype :class 'complex :name 'sfppto :comb '/ :left 's :right 'pp-to :attract 'lex_prep_to :ccgbank-type "S/PP")
	 (make-syntype :class 'complex :name 'sfnpfnp :comb '/ :left 'sfnp :right 'np :ccgbank-type "S/NP/NP")
	 (make-syntype :class 'complex :name 'sfnpfppto :comb '/ :left 'sfnp :right 'pp-to :attract 'lex_prep_to :ccgbank-type "S[dcl]/NP/PP") 
	 (make-syntype :class 'complex :name 'sfpptofnp :comb '/ :left 'sfppto :right 'np :ccgbank-type "(s[dcl]/pp)/np")
	 (make-syntype :class 'complex :name 'adjunct1 :comb '\\ :left 'intrans :right 'intrans :ccgbank-type "(S[dcl]\\NP)\\(S[dcl]\\NP)")

	 (make-syntype :class 'complex :name 'prep :comb '/ :left 'adjunct1  :right 'np  :ccgbank-type "((S[dcl]\\NP)\\(S[dcl]\\NP))/NP") 

	 ;; lexicon



	 ;; lexeme-syntax dependant argument ordering 
	 ;; these are normally defined using the macro below
	 (make-lex-syn-arg-order 
	  :name 'talk-transto-order
	  :for-lexeme 'talkSem 
	  :for-syn 'trans-to)



	 (make-lexeme :name 'doc1 :sem 'doc :syn 'nullsyn :lex "the doctor" :ccgbank-type "doctor" ) 
	 (make-lexeme :name 'doc2 :sem 'doc :syn 'nullsyn :lex "the physician" :ccgbank-type "physician") 
	 (make-lexeme :name 'girl1 :sem 'girl :syn 'nullsyn :lex "the girl" :ccgbank-type "girl") 
	 (make-lexeme :name 'cheerleader1 :sem 'girl :syn 'nullsyn :lex "a cheerleader" :ccgbank-type "cheerleader") 
	 (make-lexeme :name 'friend1 :sem 'friend :syn 'nullsyn :lex "a friend" :ccgbank-type "friend") 
	 (make-lexeme :name 'cop1 :sem 'cop :syn 'nullsyn :lex "the policeman" :ccgbank-type "policeman")
	 (make-lexeme :name 'cop2 :sem 'cop :syn 'nullsyn :lex "the cop" :ccgbank-type "cop") 
	 (make-lexeme :name 'flower1 :sem 'flower :syn 'nullsyn :lex "a flower" :ccgbank-type "flower") 
	 (make-lexeme :name 'flower2 :sem 'flower :syn 'nullsyn :lex "a rose" :ccgbank-type "rose")
	 (make-lexeme :name 'seat1 :sem 'seat :syn 'nullsyn :lex "the seat" :ccgbank-type "seat")
	 (make-lexeme :name 'hammer1 :sem 'hammer :syn 'nullsyn :lex "a hammer" :ccgbank-type "hammer")


	 (make-lexeme :name 'talkSem :sem 'talk  :lex "talked")

	 ;; these need fixing
	 (make-lexeme :name 'cry1 :sem 'cry :syn 'intrans :lex "cried" :ccgbank-type "cry") ;; cried doesn't occur in WSJ


	 (make-lexeme :name 'lex_prep_to :sem 'lex_prep_to :syn 'prep-to :lex "to") 
	 ))
  (add-ditrans-verb "give" "gave")
 ; showed, lent, loaned, brought - not sufficient evidence in WSJ
  (add-ditrans-verb "offer" "offered")
  ;(add-trans-verb "talk" "talked")


  ;; Set values from ICE corpus
  ;; ratios for give and offer were taken from Gries 2005
 
  (let ((sji-base 0)
	(sji-factor (* 1 sji-factor)))
    (add-sji-fct
     (list
      ;; (list 'giveSem
      ;; 	    'ditrans
      ;; 	    (list (floor (+ sji-base (* (* sji-factor) (* 461 (/ (* corpus-years-factor (lexfreq-raw "gave")) (+ 461 146))))))  ; DO
      ;; 		  'some-time)) 
      ;; (list 'giveSem
      ;; 	    'ditrans-to
      ;; 	    (list (floor (+ sji-base (* (* sji-factor) (* 146 (/ (* corpus-years-factor (lexfreq-raw "gave")) (+ 461 146))))))  ; PO
      ;; 		  'some-time))
      (list 'offerSem
	    'ditrans
	    (list (floor (+ sji-base (* (* sji-factor) (* 43 (/ (* corpus-years-factor (lexfreq-raw "offered")) (+ 43 15))))))  ; DO
		  'some-time)) 
      (list 'offerSem
	    'ditrans-to
	    (list (floor (+ sji-base (* (* sji-factor) (* 15 (/ (* corpus-years-factor (lexfreq-raw "offered")) (+ 43 15))))))  ; PO
		  'some-time)))))


  (init-base-levels)
  )

(init-model)



(defmacro ensure (comment  &body body)
  `(progn
     (if (and act-up::*debug* (> act-up::*debug* 0)) (print ',comment))
     (let ((result (progn ,@body)))
       (unless result
	 (signal
	  (progn (format t "assertion (ensure) ~s invalid." ',comment)
		 
		 'error)))
       result)))
(defvar *sentence* nil)

(defmacro debug-detail (&body body)
  `(let  ((act-up::*debug* *all*))
    ,@body))


(setq *rt* -10)

(defun speak (semantics &optional preference)
  (setq *sentence* nil)
  (debug-clear)
  ;;(print (act-up::actup-chunk-attrs semantics))
  (let* ((sem-list ;; (loop for s  in (act-up::actup-chunk-attrs semantics) append
		   ;; 	(let ((v (slot-value semantics s))) (if v (list v))))
	  (list (synsem-functor semantics)
		(synsem-agent semantics)
		(synsem-goalrole semantics)
		(synsem-theme semantics)))
	 (functor-lexeme  (ensure "functor-lexeme" (retrieve-chunk (list :chunk-type 'lexeme :sem (synsem-functor semantics)))))
	 (role-cues (append (if (synsem-agent semantics) '(agent))
			    (if (synsem-goalrole semantics) '(goalrole))
			    (if (synsem-theme semantics) '(theme))))
	 (sem-cues (append sem-list 
			   role-cues   ; not mentioned in paper.
			   (list
			    functor-lexeme; functor syntax
			    (lexeme-lex functor-lexeme))
			   ))
	 
	 (functor-syn (ensure "functor-syn" (retrieve-chunk '(act-up:chunk-type syntype)
							    ;; cues:
							    (append 
							     (if preference (list preference)) 
							     sem-cues)))) ; surface form
	 (arg-order (ensure "arg-order"  (retrieve-chunk (list  :chunk-type 'lex-syn-arg-order
							       :for-lexeme functor-lexeme
							       :for-syn functor-syn))))
	 (context-type nil)
	 (done-markers nil))
    (loop for s in sem-list do (learn-chunk s))
    (learn-chunk functor-syn sem-cues)
    (when arg-order ;; an occasional failure...
      (if (and act-up::*debug* (> act-up::*debug* 0)) (format t "arg-order: ~A~%" (act-up::chunk-name arg-order)))
      ;; retrieving/discarding functor syntax is a presentation
      
      ;;(learn-chunk functor-syn sem-list)
      (loop while (not (eq (chunk-ensure-name context-type) 's))
	 do

	 (let* ((role (ensure "role" (retrieve-chunk (list :chunk-type 'themerole)
								    ;; cues:
								    (append (list (act-up::chunk-name arg-order)
										  (act-up::chunk-name arg-order)
										  (act-up::chunk-name arg-order) )
									  done-markers))))
		(sem (if (and context-type (syntype-attract context-type))
			 (syntype-attract context-type)
			 (progn
			   (if (and act-up::*debug* (> act-up::*debug* 0)) (format t "role: ~A~%" (act-up::chunk-name role)))
			   (setq done-markers (cons (themerole-marker-done role) done-markers))
			   (slot-value semantics  (themerole-slot role)))))
		(lex (ensure "lexeme" (if (eq role 'functor) functor-lexeme (if sem (retrieve-chunk (list :chunk-type 'lexeme :sem sem)))))))
	   (when lex
	   (learn-chunk lex)
	   (learn-chunk arg-order)

	   (setq context-type 
		 (if (eq (lexeme-syn lex) 'nullsyn)
		     ;; get whatever is most active now (via spreading activation and priming)
		     (let ((syn (ensure "active syn" 
				  (if (eq lex functor-lexeme)
				      functor-syn
				      (retrieve-chunk '(:chunk-type syntype) (list lex lex lex (or context-type 'beginning-of-sentence)))))))
		       (when syn
		       ;; split up semantics and learn with context
		       (learn-chunk syn (cons lex sem-cues))

		       (adjoin2 context-type (chunk-name syn) nil)))
		     ;;realize-next-argument-3a and ;; realize-next-argument-3functor
		     (adjoin2 context-type (lexeme-syn lex) (lexeme-lex lex))))
	   (push (lexeme-lex lex) *sentence*)
	   ;(print (chunk-name context-type))
	   )
	   (pass-time 0.5)
	   ;; (format t "new context type: ~A~%" context-type)
	   ))
    ))  
  ;; simulate random lag 60-1,000
  (pass-time 4)
  ;;(pass-time (+ 5 (random (- 60 5))))
 ;;(pass-time (+ 60 (random (- 1000 60))))

;(pass-time 20)
)

 
(defun chunk-ensure-name (chunk)
  (if (act-up::actup-chunk-p chunk) (chunk-name chunk) chunk))

(defun adjoin2 (current type surface)
  (let ((syn (or (retrieve-chunk (list :chunk-type 'syntype :name type)) ; complex syn
		 type ;; simple 
		 )))
    ;; (if act-up::*debug* (format t "adjoining ~a and ~a~%" (if current (chunk-name current) ) (chunk-name syn)))
    (let ((res (ccg-adjoin (if (and current (equal (syntype-class current) 'complex))
			       (list (syntype-comb current) 
				     (chunk-ensure-name (syntype-left current))
				     (chunk-ensure-name (syntype-right current)))
			       (chunk-ensure-name current))
			   (if (and syn (equal (syntype-class syn) 'complex))
			       (list (syntype-comb syn) 
				     (chunk-ensure-name (syntype-left syn))
				     (chunk-ensure-name (syntype-right syn)))
			        (chunk-ensure-name syn)))))
      (if (syntype-p res) res
	  (if (complex-syn-p res)
	      (retrieve-chunk (list :chunk-type 'syntype :comb (first res) :left (second res) :right (third res)))
	      (retrieve-chunk (list :chunk-type 'syntype :name res)))))))


(defun ccg-adjoin (left right)
  (or
   (ccg-at-sentence-start left right)
   (ccg-adjoin-forward-application left right)
   (ccg-adjoin-backward-application left right)
   (adjoin-forward-composition left right)
   (adjoin-backward-composition left right)
   (adjoin-using-stored-combination left right)
   (progn (print 'adjoin-failed) left)))

(defun complex-syn-p (syn)
  (consp syn))

(defun ccg-at-sentence-start (left right)
  (and (null left)
       right))
(defun ccg-adjoin-forward-application (left right)
  (and (complex-syn-p left)
       (eq (car left) '/)
       (equal (third left) right)
       (second left)))
(defun ccg-adjoin-backward-application (left right)
  (and (complex-syn-p right)
       (eq (car right) '\\)
       (equal (third right) left)
       (second right)))
(defun adjoin-forward-composition (left right)
  (and (complex-syn-p left)
       (complex-syn-p right)
       (eq (car left) '/)
       (eq (car right) '/)
       (equal (third left) (second right))
       (list '/ (second left) (third right))))
(defun adjoin-backward-composition (left right)
  (and (complex-syn-p left)
       (complex-syn-p right)
       (eq (car left) '\\)
       (eq (car right) '\\)
       (equal (second left) (third right))
       (list '\\ (second right) (third left))))


;;   (add-chunk (make-combination :name 'combinable-np-int :left 'NP-typeraised :right 'intrans :comb 'backward-composition :result 'sfnpfnp))

(defun adjoin-using-stored-combination (left right)
  (and  (let ((comb (retrieve-chunk (append '(:chunk-type combination) 
					    (if (complex-syn-p left)
						(list :left-comb (first left) :left-left (second left) :left-right (third left))
						(list :left left))
					    (if (complex-syn-p right)
						(list :right-comb (first right) :right-left (second right) :right-right (third right))
						(list :right right))
					    ))))
	  (when comb
	      (act-up::get-chunk-by-name (combination-result comb))
	    ))))
	      


;;;; 

(defun test (number &optional preference)

  (speak
   (cond ((eq number 1)
	  (make-synsem :state 'start :intention 'test-utterance
		       :functor 'give
		       :functor-role 'functor
		       :agent 'doc
		       :agent-role 'agent
		       :goalrole 'cop
		       :goalrole-role 'goalrole
		       :theme 'flower
		       :theme-role 'theme))
	 ((eq number 2)
	  (make-synsem :state 'start :intention 'test-utterance
		       :functor 'offer
		       :functor-role 'functor
		       :agent 'girl
		       :agent-role 'agent
		       :goalrole 'friend
		       :goalrole-role 'goalrole
		       :theme 'seat
		       :theme-role 'theme))
	 
	 ((eq number 3)
	  (make-synsem :state 'start :intention 'test-utterance
		       :functor 'give
		       :functor-role 'functor
		       :agent 'girl
		       :agent-role 'agent
		       :goalrole 'friend
		       :goalrole-role 'goalrole
		       :theme 'seat
		       :theme-role 'theme
		       :preferred-predicate 'prefer-to-pp))
	 ((eq number 4)
	  (make-synsem :state 'start :intention 'test-utterance
		       :functor 'talk
		       :functor-role 'functor
		       :agent 'doc
		       :agent-role 'agent
		       :goalrole 'girl
		       :goalrole-role 'goalrole)))
   preference)
  (if (and act-up::*debug* (> act-up::*debug* 0)) (print (reverse *sentence*)))
  (reverse *sentence*))

(defun po-p (list) (member "to" list :test #'equal))
(defun dopo (ex1 &optional (rep 1))

  (let ((*debug-to-log* t))
  (loop with po = 0
   with count = 0
   for trial from 1 to rep do
     (init-model) 
       
       (let* (
	      (ptype (if (po-p (test ex1))
			 'po nil))
	      )
	 (if ptype
	     (incf po))
	 (incf count))
     finally (return (float (/ po count))))))


(defun either (this that)
  (if (< (random 2) 1)
      this that))

(defun priming (ex-1 ex-2 &optional (rep 1) (lexrep nil) (control-prime t))

  (let ((*debug-to-log* t))
  (loop with repetitions-po = 1 with repetitions-do = 1 with p-po = 1 with r-po = 1 with p-do = 1 with r-do = 1
       with do-do = 0 with do-po = 0 with po-do = 0 with po-po = 0
       with failed = 0
     with count = 1
     for trial from 1 to rep
     for prime-ctl = (if control-prime (either 'prefer-to-pp 'prefer-np-np))  ; 50/50
       do
       (let*
	   (( ex1 (either ex-1 ex-2))   ;; both directions
	    ( ex2 (if lexrep ex1 (if (eq ex1 ex-1) ex-2 ex-1))))
	    
	    (init-model)
	    (let* (
		   (ptype (if (po-p (test ex1 prime-ctl))
			      'po nil))
		   (prime-sentence *sentence*)
		   (rtype (if (po-p (test ex2))
			      'po nil))
		   (target-sentence *sentence*))

	      (if (and prime-sentence target-sentence)
		  (progn
		    (if ptype
			(if rtype (incf po-po) (incf po-do))
			(if rtype (incf do-po) (incf do-do)))   
		    
		    (if ptype (incf p-po) (incf p-do))
		    (if rtype (incf r-po) (incf r-do))
		    (if (eq ptype rtype)
			(if (eq ptype 'po)
			    (incf repetitions-po)
			    (incf repetitions-do)))
		    (incf count))
		  (incf failed))))
     finally   (return (list :failed failed :prime-po (float (/ p-po 1)) :target-po (float (/ r-po 1))
			   :prime-do (float (/ p-do 1)) :target-do (float (/ r-do 1))
			   :po-do po-do
			   :po-po po-po
			   :do-do do-do
			   :do-po do-po

			   )) 
       ;; (return (list :prime-po (float (/ p-po count)) :target-po (float (/ r-po count))
       ;; 			   :prime-do (float (/ p-do count)) :target-do (float (/ r-do count))
       ;; 			   :po-target-after-po-prime (float (/ repetitions-po p-po)) 
       ;; 			   :do-target-after-do-prime (float (/ repetitions-do p-do))))
       )))


(defun lexboost (&optional (rep 1000))

  (let ((act-up::*debug* 0))
  (let ((lexrep (priming 1 2 rep t))  ; everything is repeated
	(verbrep (priming 1 3 rep nil)) ; only verb is repeated
	(nonrep (priming 1 2 rep nil)))  ; nothing is repeated
    (format t " ~%fullrep: ~a    ~%verbrep: ~a   ~%norep: ~a   ~%"
	    lexrep verbrep nonrep
	    ))))
 