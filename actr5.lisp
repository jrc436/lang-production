;;;  -*- mode: LISP; Package: CL-USER; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Authors     : Christian Lebiere, John R. Anderson & Dan Bothell
;;; Address     : Carnegie Mellon University
;;;             : Psychology Department
;;;             : Pittsburgh,PA 15213-3890
;;;             : db30+@andrew.cmu.edu
;;; 
;;; Copyright   : (c) 2000-2004 Christian Lebiere & John R Anderson
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : actr5.lisp
;;; Version     : 5.1
;;;
;;; Description : The core components of the ACT-R 5 cognitive system -
;;;               the goal manipulation, procedural and declarative memories. 
;;;
;;; Bugs        : 
;;; ----- History -----
;;; 04.04.13 Dan [5.1] 
;;;             : Added the header, the copyright notice and the LGPL stuff.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *actr-version-and-date-stamp* "5.1 - 4/13/2004")

;;;
;;; COPYRIGHT
;;;

(defun act-r-copyright ()
  (format t "~&ACT-R VERSION 5.1~%REPORT BUGS TO CL+@CMU.EDU~%~
             +------------------------------------------------------------+~%~
             |                           ACT-R 5.0                        |~%~
             |                           (c) 2000-2004                    |~%~
             |            Christian Lebiere   John R. Anderson            |~%~
             |    Department of Psychology, Carnegie Mellon University    |~%~
             |         Supported by ONR Contract N00014-95-10223          |~%~
             +------------------------------------------------------------+~%")
  (values))

(act-r-copyright)

;;;
;;; VARIABLE, STRUCTURE AND ACCESS MACRO DEFINITIONS
;;;

;;; Global System Variables

(defvar *compile-eval-calls* nil
  "Do not compile !eval! and other Lisp calls at production definition.")

(defvar *model* nil
  "Holds the file name of the current model and its contents.
   Set by clear-all and used by reset and reload.")

(defvar *time* 0.0
  "The real-time counter.")

(defvar *start-time* 0.0
  "The real-time counter.")

(defvar *default-action-time* 0.05
  "The default time to execute a right-hand side is 50 milliseconds.")

(defvar *latency* 0.0
  "Latency of conflict resolution process.")

(defvar *cycle* 0
  "The production firing cycle counter.")

(defvar *spread-stamp* 0
  "The stamp for activation spreading.  Similar to *cycle*,
   but may clock faster due to interactive manipulation of sources,
   or slower when rational analysis is turned off.")

(defvar *wme-number* 0.0
  "Holds the count of the total number of wmes in declarative memory.")

(defvar *declarative-memory* nil
  "Holds a list of wme types indexed by name, with for each type its
   definition (with possible subtypes) and the list of wmes of that type.")

(defvar *hash-names* nil
  "Holds for each wme name a pointer to its structure.  Primarily used
   to load the data base and for interfacing, not at run-time when
   direct pointers are used instead of names to hash.")

(defvar *used-ACT-R-symbols* nil
  "Holds a list of all the temporary symbols interned
   by ACT-R so they can be freed on a reset.")

(defvar *procedural-memory* nil
  "Holds a list of productions indexed by names.")

(defvar *goal-activation* 1.0
  "Amount of activation source level to be evenly split among goal elements.")

(defvar *goal-sources* nil
  "Activation sources existing after the current goal is pushed.")

(defvar *wmfocus* nil
  "The top goal.")

(defvar *wmfocus-list* nil
  "The list of goals to focus on specified by the expanded form of wmfocus.")

(defvar *goal-stack* nil
  "Holds the stack of goal, with the top-most at the front of the list.")

(defvar *goal-depth* 1
  "The depth of the goal stack, times 3 plus 1, to be used for indenting traces.")

(defvar *g* 20.0
  "The value of G in PG-C.  Default is 20.0 for historical reasons.")

(defvar *exp-gain-noise* nil
  "Variance of the zero-mean gaussian noise added to instantiation values.")

(defvar *retrieval* nil
  "Holds the last chunk retrieved from declarative memory.")

(defvar *retrieval-scheduler* nil
  "Holds the result of the next retrieval and the time at which it will be available.")

(defvar *visual-location* nil
  "Holds the result of the last find-location.")

(defvar *aural-location* nil
  "Holds the result of the last find-sound")

(defvar *previous-instantiations* nil
  "The list of instantiations from the previous resetting.")

(defvar *previous-instantiation* nil
  "The previous instantiation and its time for use in production compilation.")

(defvar *instantiation* nil
  "The current instantiation as a global variable to limit parameter passing
   and allow references in eval statements.")

(defvar *extra-instantiation* nil
  "List of previously generated instantiations of a given production,
   or instantiation to store activations if partial matching is on.")

(defvar *conflict-set* nil
  "The conflict set holds the list of current instantiations.")

(defvar *sum-exp-act* 0.0
  "Holds the sum of the exponential of activations for scaling latencies.")

(defvar *latency-fn* 'old-latency
  "The function used in computing retrieval latency from chunk(s) activation.")

(defvar *latency-factor* 1.0
  "The latency scaling factor B in the latency equation ROM (3.3).")

(defvar *latency-exponent* 1.0
  "The activation scaling factor b in the latency equation ROM (3.3).")

(defvar *base-level-constant* 0.0
  "The base level constant, i.e. B in ROM Equation 4.3")

(defvar *activation-sources* nil
  "List of activation sources and their source level.")

(defvar *activation-noise* nil
  "Variance of the zero-mean gaussian noise added to activation.")

(defvar *permanent-activation-noise* nil
    "Variance of the zero-mean gaussian noise permanently added to activation.")

(defvar *mismatch-penalty* 1.0
  "Maximum mismatch penalty, in activation units, in partial matching.")

(defvar *retrieval-threshold* nil
  "Minimum amount of activation needed for retrieval.
   Nil by default (N/A) but switch to default of 0.0 for partial matching.")

(defvar *threshold-noise* nil
  "If set to t, add noise to the retrieval and utility thresholds.
   Defaults to nil to implement full 4.0 activation compatibility.")

(defvar *partial-matching* nil
  "Triggers partial matching.")

(defvar *max-sim* 0.0
  "Maximum similarity between two values, presumably in case of identity.")

(defvar *max-dif* -1.0
  "Maximum difference between two values, presumably for complete mismatches.")

(defvar *temperature* 1.0
  "When set (symbol :tmp) to a positive number, it is used instead of (sqrt 2 *)
   the activation noise as the scaling factor in the blending process.")

(defvar *blending* nil
  "Triggers blending (symbol :bln).  If set to 'rt, then blending
   occurs only after standard retrieval fails to reach threshold.")

(defvar *blending-trace* nil
  "Triggers the blending trace (symbol :blt).")

(defvar *blending-activation-fn* 'first-equation
  "Defines the activation of the blended value as a function of the activations
  of the individual chunks and the similarities between their values.")

(defvar *break-productions* nil
  "Holds the productions on which to break before firing.")

(defvar *failed-productions* nil
  "Holds the failed productions.")

(defvar *cost-penalty* 1.0
  "Cost Penalty for new productions.")

(defvar *initial-experience* 10.0
  "Initial Experience (total successes and failures) for new productions.")

(defvar *threshold-time* 2.0
  "Threshold of time between production firings to allow production rule learning.")

(defvar *enable-production-learning* nil
  "Triggers production rule learning.")

(defvar *reinforce-analogized-production* t
  "Whether or not to reinforce a production when it is re-analogized.")

(defvar *production-compilation-parameters* nil
  "Holds the argument list to the parameters command for compiled productions")

(defvar *enable-rational-analysis* nil
  "Triggers the activation and rational analysis computation.")

(defvar *enable-randomness* nil
  "When on, ranks instantiations randomly instead of according to
   some deterministic but arbitrary order.")

(defvar *utility-threshold* 0.0
  "Minimum utility (pg-c) needed for production selection.  0.0 by default,
   but can be set to any number or even NIL when no thresholding is desired.")

(defvar *optimized-learning* t
  "Mode of learning base levels and production strength which ensures
   constant space and time demands.  Generalized to take a fixed number of references.")

(defvar *base-level-learning* nil
  "The trigger for base level learning, d in ROM Equation 4.4.
   Should be less than 1.0 for the optimized learning formula to apply.")

(defvar *maximum-associative-strength* nil
  "If set to a number, interpreted as S replacing ln(m)
   in associative learning equation, i.e. Sji = S - ln(n).")

(defvar *associative-learning* nil
  "The trigger for associative learning, a in ROM Equation 4.5.
   Can be any non-negative value.")

(defvar *strength-learning* nil
  "The trigger for learning production strength, d in ROM Equation 4.6.
   Should be less than 1.0 for the optimized learning formula to apply.")

(defvar *parameters-learning* nil
  "The trigger for learning production parameters in ROM Equations 4.7 and 4.8.
   True or false.")

(defvar *command-trace* t
  "Directs the output of the user-level commands.  Not included in sgp.")

(defvar *output-trace* t
  "Directs the printing of !output! command.")

(defvar *cycle-trace* t
  "Prints the name of production to fire at every cycle.")

(defvar *latency-trace* nil
  "Prints the latency of matching and action side of firing production.")

(defvar *partial-matching-trace* nil
  "Prints all information relevant to partial matching.")

(defvar *production-compilation-trace* t
  "Prints the trace of the production compilation process.")

(defvar *activation-trace* nil
  "Prints the activation computation for wmes and instantiations.")

(defvar *conflict-resolution-trace* nil
  "Prints the conflict resolution at work.")

(defvar *conflict-set-trace* nil
  "Prints the number of instantiations considered out of the conflict set
   total, and the expected gain of the winning instantiation.")

(defvar *goal-trace* nil
  "Prints goal pushing and popping.")

(defvar *dm-trace* nil
  "Prints addition or deletion of wmes during execution.")

(defvar *production-trace* nil
  "Prints a description of each production instantiation fired.")

(defvar *matches-trace* nil
  "Prints a description of all instantiations.")

(defvar *exact-matching-trace* nil
  "Prints a trace of every (non-partial) match.")

(defvar *verbose* t
  "When off, turns off all but essential printing.")

(defvar *abort-instantiation* t
  "Determines whether or not an instantiation can be aborted by a time limit
   or allowed to complete.")

(defvar *parameter-sets* nil
  "Holds the parameter sets in a-list format.")

(defvar *similarity-hook-fn* nil
  "If non-nil, call this function to get similarity values.  If it returns nil,
   then the usual process of looking up similarities in wmes and comparing
   non-wmes for equality is used.")

(defvar *linear-similarity-scale* 1.0
  "Scale for linear-similarity: similarity = difference / scale.")   

(defvar *blending-hook-fn* 'blending-arithmetic-mean
  "Defines the direct averaging for number values.")

(defvar *conflict-set-hook-fn* nil
  "If non-nil, call this function before conflict resolution with the conflict set.
   If it returns an instantiation, then fires that one, otherwise run as usual.")

(defvar *firing-hook-fn* nil
  "if non-nil, called for side-effect with the instantiation before its firing.")

(defvar *cycle-hook-fn* nil
  "If non-nil, call this function after each cycle with the instantiation fired.")

(defvar *web-hook-fn* nil
  "If non-nil, call this function after each cycle with the instantiation fired.
   Same as *cycle-hook-fn*, but used specifically by ACT-R-on-the-web.")

(defvar *end-run-hook-fn* nil
  "If non-nil, call this function at the end of a run with the total run latency.")

(defvar *init-hook-fn* nil
  "if non-nil, call this function after clear-all with no arguments.")

(defvar *step-fn* 'step-fct
  "If non-nil, call this function at each step of pstep to get user instruction.")

(defvar *stop* nil
  "If non-nil, stops the run function before the next cycle.")

(defvar *save-state-changes* nil
  "If non-nil, the state changes are saved for later undoing.")

(defvar *pop-upon-failure* nil
  "If non-nil, then pop the focus when no suitable instantiation can be found.
   If nil, then stop the run as in previous versions without taking any action.")

(defconstant *buffer-keywords*
  '((=goal . *wmfocus*)
    (goal . *wmfocus*)
    (=retrieval . *retrieval*)
    (retrieval . *retrieval*)
    (=visual-location . *visual-location*)
    (visual-location . *visual-location*)
    (=aural-location . *aural-location*)
    (aural-location . *aural-location*))
  "Holds the alist of special buffers with their associated special variables.
   Works for both variable and constant names.")

(defparameter *buffers* nil
  "Buffers with their global variable and function calls.")

(defconstant *command-mappings*
  '((ACTIVATION-SOURCES ACTIVATIONSOURCES) (ACTR-TIME ACTRTIME)
    (ADD-DM ADDWM) (ADD-IA ADDIA) (CHUNK-SLOT-VALUE WMESLOTVALUE)
    (CHUNK-TYPE WMETYPE) (CLEAR-ALL CLEARALL) (CLEAR-DM CLEARWM)
    (CLEAR-GOAL-STACK CLEARGOALSTACK)
    (CLEAR-PRODUCTIONS CLEARPRODUCTIONS) (CLOSE-OUTPUT CLOSEOUTPUT)
    (CLOSE-TRACE CLOSETRACE) (COPY-CHUNK COPYWME)
    (DELETE-CHUNK DELETEWM) (DM WM) (FOCUS-ON-GOAL FOCUS-ON-WME)
    (GET-BASE-LEVEL GETBASELEVEL) (GOAL-FOCUS WMFOCUS)
    (GOAL-STACK GOALSTACK) (HELP NIL) (IA IA)
    (IMPORT-MODEL IMPORT-MODEL) (LOAD-MODEL LOAD-MODEL)
    (MOD-CHUNK MODWME) (MOD-FOCUS MODFOCUS) (NAME NAME)
    (NO-OUTPUT NO-OUTPUT) (OUTPUT-STREAM OUTPUTSTREAM)
    (P P) (PARAMETERS PARAMETERS) (PBREAK PBREAK)
    (PDISABLE PDISABLE) (PENABLE PENABLE) (PMATCHES PMATCHES)
    (POP-GOAL POP-WME) (PP PP)
    (PRODUCTION-PARAMETER PRODUCTION-PARAMETER) (PSET PSET)
    (PSTEP PSTEP) (PUNBREAK PUNBREAK) (PUNDO PUNDO)
    (PUSH-GOAL PUSH-WME) (RELOAD RELOAD) (RESET RESET)
    (RESET-IA RESETIA) (RUN RUN) (RUN-MANY RUN-MANY) (SDM SWM)
    (SDP SWP) (SET-ALL-BASE-LEVELS SETALLBASELEVELS)
    (SET-COMPILATION-PARAMETERS SETANALOGIZEDPARAMETERS)
    (SET-BASE-LEVELS SETBASELEVELS) (SET-DM SETWM) (SET-G SETG)
    (SET-GENERAL-BASE-LEVELS SETGENERALBASELEVELS) (SET-IA SETIA)
    (SET-SIMILARITIES SETSIMILARITIES) (SGP SGP) (SIMILARITY SIMILARITY)
    (SPP SPP) (TRACE-STREAM TRACESTREAM) (UPDATE-ACTIVATION UPDATE-ACTIVATION)
    (WHYNOT WHYNOT) (WHYNOT-DEPENDENCY WHYNOT-ANALOGY)))


;;; Error, Abort, Warning and Output functions.  Plus Save State Changes.

(defmacro signal-error (message &rest arguments)
  "Signals an error and stops."
  `(error ,message ,@arguments))

(defmacro signal-warn (message &rest arguments)
  "Outputs a warning of message and arguments."
  `(format *error-output* ,(concatenate 'string "~&~VT" message)
           ,@(cons '*goal-depth* arguments)))

(defmacro signal-output (stream message &rest arguments)
  "Outputs message and arguments when verbose.  Prints by default in lowercase."
  `(when (and *verbose* ,stream)
     (let ((*print-case* :downcase))
       (format ,stream ,(concatenate 'string "~&~VT" message)
               ,@(cons '*goal-depth* arguments)))))

(defmacro save-state-change (&rest change)
  "Save the state change."
  `(when *save-state-changes*
     (push (list ,@change) *save-state-changes*)))

;;; WME Types as structures

(defun print-wme-type (wmetype stream depth)
  "Print a wme type as just its name, in uppercase."
  (declare (ignore depth))
  (format stream "~:@(~A~)" (wme-type-name wmetype)))

(defstruct (wme-type (:print-function print-wme-type))
  "Holds the wmetype name, lists of subtypes and supertypes including itself
   as the first of each, the size of the wme (number of slots),
   a list of slots containing name, index and default value,
   a list of productions in which this type is used as goal type,
   and the list of wmes of that type.  Also the documentation string."
  name subtypes supertypes size slots productions wmes documentation)


;;; Declarative Memory as an a-list of types

(defmacro get-type (type)
  "Returns the structure corresponding to type."
  `(cdr (assoc ,type *declarative-memory* :test #'eq)))

(defmacro get-safe-type (type &optional context)
  "Calls get-type, and prints an error if undefined."
  `(if (wme-type-p ,type) ,type
       (or (get-type ,type)
           (signal-warn "CHUNK TYPE ~S IS UNDEFINED IN ~A." ,type ,context))))

(defmacro for-all-wmes (wme &rest form)
  "Execute forms where wme cycles through all wmes in declarative memory."
  `(dolist (wme-type *declarative-memory*)
     (dolist (,wme (wme-type-wmes (cdr wme-type)))
       ,@form)))


;;; WMEs as structures

(defun print-wme (wme stream depth)
  "Print a wme as just its name, capitalized."
  (declare (ignore depth))
  (format stream "~:(~A~)" (wme-name wme)))

(defstruct (wme (:print-function print-wme))
  "WMEs are represented as structures, with the slots as a slot array."
  name type fan (slot-wmes (list 0.0)) ias similarities
  (creation-time *time*) (creation-cycle *cycle*) (time-stamp (- *time* 1.0))
  (activation *base-level-constant*) (base-level *base-level-constant*)
  (spread-stamp (- *spread-stamp* 1.0)) (source-spread 0.0)
  (references (cons 1.0 (when (and *base-level-learning* (not (eq *optimized-learning* t)))
                          (list *time*))))
  (contexts 0.0) (needed 0.0) slots source (permanent-noise 0.0) documentation)

(defmacro slots-slot (slots index)
  "Given the slots subarray, return the index value."
  `(svref ,slots ,index))

(defmacro wme-slot (wme index)
  "Given a wme and a slot index, return the slot value."
  `(slots-slot (wme-slots ,wme) ,index))

(defmacro wmep (wme)
  "Determines if argument is a wme."
  `(wme-p ,wme))

;;; Slot Descriptions as lists to gain on search time (assoc)

(defmacro make-slot (&key name index default)
  "A slot is a list of its name, index, and default value."
  `(list ,name ,index ,default))

(defmacro slot-name (slot)
  "Given a slot description, return its name."
  `(first ,slot))

(defmacro slot-index (slot)
  "Given a slot description, return its index."
  `(second ,slot))

(defmacro slot-default (slot)
  "Given a slot description, return its default."
  `(third ,slot))

(defmacro get-slot (name slots)
  "Get description of slot name from list of slots."
  `(assoc ,name ,slots :test #'eq))

(defmacro get-safe-slot (name type &optional context)
  "Get slot name from type, or print an error if non-existant.  Cache name."
  `(or (get-slot ,name (wme-type-slots ,type))
       (signal-output *command-trace*
                      "SLOT ~S OF TYPE ~S IS UNDEFINED IN ~A."
                      ,name ,type ,context)))

(defmacro get-slot-value (wme slot)
  "Given a wme and a slot name, return its value if the slot exists."
  `(let ((index (get-slot ,slot (wme-type-slots (wme-type ,wme)))))
     (when index (wme-slot ,wme (slot-index index)))))

(defmacro get-safe-slot-value (wme slot)
  "Given a wme and a slot name, return its value if the slot exists.
   Complain otherwise."
  `(let ((index (get-safe-slot ,slot (wme-type ,wme) ,wme)))
     (when index (wme-slot ,wme (slot-index index)))))


;;; Hash Table of wme names

(defmacro get-wme (name)
  "Given a wme name, returns the wme itself."
  `(gethash ,name *hash-names*))

(defun get-wme-name (wme)
  "Get the name of wme, or return directly if not a wme."
  (if (wmep wme) (wme-name wme) wme))

(defmacro get-safe-wme (name &optional (warn t))
  "Gets the wme corresponding to name.  Prints an error message if undefined."
  `(if (wmep ,name) ,name
       (or (get-wme ,name)
           ,(if warn
              `(signal-warn "CHUNK ~S IS UNDEFINED." ,name)
              `(signal-output *command-trace* "CHUNK ~S IS UNDEFINED." ,name)))))

(defun get-wme-or-constant (name)
  "Returns the wme corresponding to name, or the name if none.  Detects nil.
   Now automatically defines non-wme symbols as of the default type wme."
  (cond ((null name) nil)
         ((eq name t) t)  ;; t, like nil, is also given special status
         ((wmep name) name)
         ((get-wme name))
         ((symbolp name)
;          (signal-output *command-trace* "UNDEFINED CHUNK ~S IS BEING CREATED AS OF DEFAULT TYPE CHUNK."
;                       name)
          (create-wme name (cdar *declarative-memory*)))
         (t name)))

(defmacro remove-name (name)
  "Given a wme name, remove its entry from the hash table."
  `(remhash ,name *hash-names*))


;;; IA values as structures of 3 elements, indexed by wme in an a-list

(defun print-ia (ia stream depth)
  "Print an ia as just its sji."
  (declare (ignore depth))
  (format stream "~6,3F" (ia-sji ia)))

(defstruct (ia (:print-function print-ia))
  "An ia-link to wme i is indexed by the wme j and composed of its count
   (the number of times it appears in wme i), the current Sji value,
   the Rji* prior, and the F(Ni&Cj) statistics."
  (count 1)
  (sji 0.0)
  (rji* 1.0)
  (fnicj 0.0))

(defmacro default-rji* (wmej)
  "The default value of Rji* for that wme, i.e. m/n.  
   If S is specified, then m defaults to e^S."
  `(/ (if (numberp *maximum-associative-strength*)
        (exp *maximum-associative-strength*)
        *wme-number*)
      (first (wme-fan ,wmej))))

(defmacro count-rji* (ia value)
  "Computes the value of Rji* of ia given the default value."
  `(if (<= (ia-count ,ia) 1)
     ,value
     (* ,value (ia-count ,ia))))

(defmacro rji-sji (rji)
  "Sji in terms of rji, i.e. log of rji."
  `(log ,rji))

(defmacro count-sji (ia value)
  "Computes Sji of ia from the default value."
  `(if (<= (ia-count ,ia) 1)
     ,value
     (+ ,value (log (coerce (ia-count ,ia) *read-default-float-format*)))))

(defmacro learn-rji (ia wmej wmei)
  "Computes rji for ia between wmej and wmei based on ROM Equation 4.5."
  `(/ (+ (* *associative-learning* (ia-rji* ,ia))
         (if (= (wme-needed ,wmei) 0.0)
           ; Eji defaults to 1 so Rji defaults to 0
           (wme-contexts ,wmej)
           (* (ia-fnicj ,ia)
              (/ (- *cycle* (wme-creation-cycle ,wmei))
                 (wme-needed ,wmei)))))
      (+ *associative-learning* (wme-contexts ,wmej))))

(defmacro compute-ia (wmej ia wmei)
  "Initializes the IA connection from wmej to wmei."
  `(let ((rji* (count-rji* ,ia (default-rji* ,wmej))))
     (setf (ia-rji* ,ia) rji*)
     (setf (ia-sji ,ia)
           (rji-sji (if *associative-learning* (learn-rji ,ia ,wmej ,wmei) rji*)))))

(defmacro create-ia (wmej wmei &key (count 1) (sji 0.0) (rji* 1.0))
  "Creates an IA connection between wmej and wmei.
   Count indicates whether the wmes are connected or not (learned)."
  `(let ((ia (make-ia :count ,count :sji ,sji :rji* ,rji*)))
     (push (cons ,wmej ia) (wme-ias ,wmei))
     ia))

(defmacro ia-value (ia wmej wmei)
  "Compute new IA if necessary, and return the ia value."
  `(if (and *associative-learning* (< (wme-spread-stamp ,wmei) *spread-stamp*))
     (setf (ia-sji ,ia) (rji-sji (learn-rji ,ia ,wmej ,wmei)))
     (ia-sji ,ia)))

(defmacro delete-ia (wmej wmei)
  "Deletes the ia-link between two wmes."
  `(setf (wme-ias ,wmei)
         (delete ,wmej (wme-ias ,wmei) :test #'eq :key #'car :count 1)))

(defmacro get-ia (wmej wmei)
  "Retrieves the ia-link between two wmes."
  `(cdr (assoc ,wmej (wme-ias ,wmei) :test #'eq)))

(defmacro get-safe-ia (wmej wmei)
  "Gets the ia between wmej and wmei, or prints an error message if none."
  `(or (get-ia ,wmej ,wmei)
       (signal-warn "IA BETWEEN ~S AND ~S IS UNDEFINED." ,wmej ,wmei)))

(defmacro get-make-ia (wmej wmei)
  "Gets the ia between wmej and wmei, or adds it if none."
  `(or (get-ia ,wmej ,wmei)
       (create-ia ,wmej ,wmei :count 0)))

(defmacro remove-connection (wmej wmei &key (compute-ia t))
  "Remove the connection between two wmes."
  `(let ((ia (get-safe-ia ,wmej ,wmei)))
     (decf (first (wme-fan ,wmej)) 1.0)
     (delete ,wmei (wme-fan ,wmej) :test #'eq :count 1)
     (decf (first (wme-slot-wmes ,wmei)) 1.0)
     (delete ,wmej (wme-slot-wmes ,wmei) :test #'eq :count 1)
     (if (= (decf (ia-count ia) 1) 0)
       (delete-ia ,wmej ,wmei)
       ,@(when compute-ia
           `((compute-ia ,wmej ia ,wmei))))))

(defmacro add-connection (wmej wmei &key (compute-ia t))
  "Adds a connection between two wmes."
  `(let ((ia (get-ia ,wmej ,wmei)))
     (incf (first (wme-fan ,wmej)) 1.0)
     (push ,wmei (rest (wme-fan ,wmej)))
     (incf (first (wme-slot-wmes ,wmei)) 1.0)
     (push ,wmej (rest (wme-slot-wmes ,wmei)))
     (if ia
       (incf (ia-count ia) 1)
       (setf ia (create-ia ,wmej ,wmei)))
     ,@(when compute-ia
         `((compute-ia ,wmej ia ,wmei)))))

(defmacro set-slot-value (wme slot-index value &key (compute-ia t))
  "Sets wme slot index to value.  Maintains the proper connections.
   Tests for analogy special slots."
  `(let ((old-value (wme-slot ,wme ,slot-index))
         (new-value ,value))
     (save-state-change :set-slot-value ,wme ,slot-index old-value)
     (when (wmep old-value)
       (remove-connection old-value ,wme :compute-ia ,compute-ia))
     (when (wmep new-value)
       (add-connection new-value ,wme :compute-ia ,compute-ia))
     (setf (wme-slot ,wme ,slot-index) new-value)))


;;; Similarities as a-lists

(defmacro find-similarity (wmej wmei)
  "Returns the cons-cell holding the similarity between wmej and wmei, or nil."
  `(assoc ,wmei (wme-similarities ,wmej) :test #'eq))

(defmacro get-similarity (wmej wmei)
  "Retrieves the similarity between wmej and wmei, in that order, or 0.0."
  `(or (and *similarity-hook-fn* (funcall *similarity-hook-fn* ,wmej ,wmei))
       (if (and (wmep ,wmej) (wmep ,wmei))
         (or (cdr (find-similarity ,wmej ,wmei)) *max-dif*)
         (if (equal ,wmej ,wmei) *max-sim* *max-dif*))))

(defmacro set-similarity (wmej wmei similarity)
  "Sets the similarity between wmej and wmei to similarity."
  `(let ((existing (find-similarity ,wmej ,wmei)))
     (if existing
       (rplacd existing ,similarity)
       (push-last (cons ,wmei ,similarity) (wme-similarities ,wmej)))))

;;; Productions as structures

(print "print-prod defining")
(defun print-production (production stream depth)
  "Print a production as just its name, capitalized."
  (declare (ignore depth))
  (format stream "~:(~A~)" (production-name production)))

#|
(defstruct (production (:print-function print-production) (:predicate productionp))
  "Productions are represented as structures which hold the usual info,
   plus a list of instantiations for reuse."
  name goal-type text lhs rhs bindings retrievals initializations 
  size instantiation extra-instantiation
  (creation-time *time*) (time-stamp (- *time* 1.0))
  (references (cons 1.0 (when (and *strength-learning* (not *optimized-learning*))
                          (list *time*))))
  (strength 0.0) success failure (chance 1.0) (effort *default-action-time*)
  (value 0.0) (p 1.0) (c *default-action-time*) pg-c
  (successes (cons 1.0 (when (numberp *parameters-learning*) (list *time*))))
  (failures (list 0.0))
  (efforts (cons *default-action-time* (when (numberp *parameters-learning*)
                                         (list *default-action-time*))))
  documentation)
|#

;;; Updated definition from prod-comp-update.lisp

(defstruct (production (:print-function print-production) (:predicate productionp))
  "Productions are represented as structures which hold the usual info,
   plus a list of instantiations for reuse."
  name goal-type text lhs rhs bindings retrievals initializations 
  size instantiation extra-instantiation
  (creation-time *time*) (time-stamp (- *time* 1.0))
  (references (cons 1.0 (when (and *strength-learning* (not *optimized-learning*))
                          (list *time*))))
  (strength 0.0) success failure (chance 1.0) (effort *default-action-time*)
  (value 0.0) (p 1.0) (c *default-action-time*) pg-c
  (successes (cons 1.0 (when (numberp *parameters-learning*) (list *time*))))
  (failures (list 0.0))
  (efforts (cons *default-action-time* (when (numberp *parameters-learning*)
                                         (list *default-action-time*))))
  (priorC 20.0) ; NT: newly learned productions start with pessimistic values for C and P 
  (priorP 0.0) ; NT:
  documentation)

;;; Productions stored in a-list of name-structure pair for easy search

(defmacro get-production (name &optional (productions '*procedural-memory*))
  "Returns the production of a given name, if any, or nil."
  `(cdr (assoc ,name ,productions :test #'eq)))

(defmacro get-safe-production (name &optional (productions '*procedural-memory*))
  "Returns the production of a given name, or prints a warning."
  `(if (productionp ,name) ,name
       (or (get-production ,name ,productions)
           (signal-warn "PRODUCTION ~S IS UNDEFINED." ,name))))


;;; Instantiations as arrays

(defconstant *instantiation-slots* 3
  "Specifies the number of common slots for each instantiation.
   Currently, the production, latency and gain.")

(defmacro instantiation-production (instantiation)
  "Given an instantiation, returns the production."
  `(svref ,instantiation 0))

(defmacro instantiation-latency (instantiation)
  "Given an instantiation, returns its latency."
  `(svref ,instantiation 1))

(defmacro instantiation-gain (instantiation)
  "Given an instantiation, returns its expected gain."
  `(svref ,instantiation 2))

(defmacro instantiation-variable (instantiation index)
  "Given an instantiation and a variable index, returns the binding value."
  `(svref ,instantiation ,index))

(defmacro make-instantiation (production)
  "Create a new instantiation with the given production size."
  `(let ((instantiation (make-array (production-size ,production)
                                    :initial-element nil)))
     (setf (instantiation-production instantiation) ,production)
     instantiation))

(defmacro get-next-instantiation (production)
  "Picks the next instantiation from *extra-instantiation*,
   or generate a new one and add it to the list for production."
  `(or (pop *extra-instantiation*)
       (first (push (make-instantiation ,production)
                    (production-extra-instantiation ,production)))))

(defmacro copy-instantiation (instantiation)
  "Makes a copy of an instantiation for keeping in *previous-instantiations*."
  `(copy-seq ,instantiation))


;;; Variable Bindings

(defmacro make-variable-binding (name index &key (type-or-slot nil))
  "Creates a binding for variable name of stack index.  Type-or-slot
   indicates the wme type for actual retrievals and slot number for others.
   List to make it searchable by assoc.
   NOTE: Return arguments depend on this format to work correctly!"
  `(list ,name ,index ,type-or-slot))

(defmacro variable-name (binding)
  "Returns the variable name from binding."
  `(first ,binding))

(defmacro variable-index (binding)
  "Returns the variable stack index from binding."
  `(second ,binding))

(defmacro variable-type (binding)
  "Returns the variable type from binding."
  `(third ,binding))

(defmacro variable-slot (binding)
  "Returns the index of the slot from which this variable is bound."
  `(third ,binding))

(defmacro variable-slot-and-returns (binding)
  "Returns the index of the slot from which this variable is bound,
   and the following return indices."
  `(cddr ,binding))  

(defmacro variable-returns (binding)
  "Returns the return bindings which are pushed at the end of the list."
  `(cdddr ,binding))

(defmacro get-variable-binding (variable bindings)
  "Retrieves the binding of variable from the a-list of variable bindings."
  `(assoc ,variable ,bindings :test #'eq))

(defmacro get-safe-variable-binding (variable bindings &optional context)
  "Retrieves the binding of variable from the a-list of variable bindings.
   Prints a warning if no such variable binding exists."
  `(or (get-variable-binding ,variable ,bindings)
       (signal-warn "VARIABLE ~S IS UNBOUND IN ~A." ,variable ,context)))

(defmacro get-index-binding (index bindings)
  "Retrieves the variable binding index from the a-list of variable bindings."
  `(rassoc ,index ,bindings :test #'eq :key #'car))

(defmacro with-binding (binding existing variable bindings index &rest form)
  "Gets the binding of variable, or add a new one to bindings at index.
   Existing is set as to whether binding existed previously.
   Then form is evaluated with binding and existing bound."
  `(let ((,binding (get-variable-binding ,variable ,bindings))
         (,existing t))
     (unless ,binding
       (setf ,binding (make-variable-binding ,variable (incf ,index 1)))
       (push-last ,binding ,bindings)
       (setf ,existing nil))
     ,@form))


;;; Action Description

(defstruct action
  "Creates an action consisting of a slot name and index, a value to be bound
   or tested, a dispatch keyword {:stack, :literal, or :eval} and a negation flag."
   name slot value dispatch negation)
  

;;; Goal Stack Frames

(defun print-goal-frame (goal-frame stream depth)
  "Print a goal frame as just its focus."
  (declare (ignore depth))
  (format stream "~A" (goal-frame-focus goal-frame)))  

(defstruct (goal-frame (:print-function print-goal-frame))
  "Creates a goal stack frame containg the focus,
   and a description of which return values to pass back and how.
   Also keep the sources as existing when the chunk was created."
  focus return-values sources)


;;; Functional parameters

(defun print-functional-parameter (parameter stream depth)
  (declare (ignore depth))
  (format stream "~S" (functional-parameter-expression parameter)))

(defstruct (functional-parameter (:print-function print-functional-parameter))
  "Stores a functional production parameter as the original expression
   and the compiled function."
  expression function)


;;; Modular buffer structures

(defstruct buffer-calls "Keeps the calls to buffers in a structure"
  equal-lhs plus-rhs equal-rhs minus-rhs)


;;; Syntax Macros

(defmacro first-char (string)
  "First character of the string."
  `(schar ,string 0))

(defmacro last-char (string)
  "Last character of the string."
  `(schar ,string (- (length ,string) 1)))

(defmacro var> (string)
  "Determines if there is a > character at the end of the string."
  `(and (eq #\> (last-char ,string)) (> (length ,string) 1)))

(defmacro !keyword! (string)
  "Determines if the first and last character of the string is a !."
  `(and (eq #\! (first-char ,string))
        (eq #\! (last-char ,string))))

(defmacro =var (string)
  "Determines if the first character of the string is a =."
  `(eq #\= (first-char ,string)))

(defmacro +var (string)
  "Determines if the first character of the string is a +."
  `(eq #\+ (first-char ,string)))

(defmacro -var (string)
  "Determines if the first character of the string is a -."
  `(eq #\- (first-char ,string)))

(defmacro delimiter (string)
  "Determines if string is either a var> or a !keyword!."
  `(or (var> ,string) (!keyword! ,string)))

(defmacro symbol-name-test (symbol test)
  "Gets the name of symbol and apply test to it."
  `(and (symbolp ,symbol)
        (let ((name (symbol-name ,symbol)))
          (,test name))))

(defmacro retrievalp (symbol)
  "Determines whether symbol is of the form var>."
  `(symbol-name-test ,symbol var>))

(defmacro commandp (symbol)
  "Determines whether symbol is of the form !keyword!."
  `(symbol-name-test ,symbol !keyword!))

(defmacro delimiterp (symbol)
  "Determines whether symbol is of the form var> or !keyword!."
  `(symbol-name-test ,symbol delimiter))

(defmacro separatorp (symbol)
  "Determines if symbol is the separator ==>."
  `(eq '==> ,symbol))

(defmacro variablep (symbol)
  "Determines whether symbol is of the form =var."
  `(symbol-name-test ,symbol =var))

(defmacro actionp (symbol)
  "Determines whether symbol is of the form +var."
  `(symbol-name-test ,symbol +var))

(defmacro clearp (symbol)
  "Determines whether symbol is of the form -var."
  `(symbol-name-test ,symbol -var))

(defmacro position-or-last (list test)
  "Determines the index of the first element in list to satisfy test, or last."
  `(let ((index 0))
     (dolist (symbol ,list index)
       (when (,test symbol) (return index))
       (incf index 1))))

(defmacro next-delimiter (list)
  "Determines the index of the position of the next delimiter,
   the current one not included."
  `(position-or-last (rest ,list) delimiterp))

(defmacro test-modifier (symbol)
  "Determines if symbol is a test modifier such as negation or comparison."
  `(member ,symbol '(- < > <= >=) :test #'eq))

(defmacro next-slot (list)
  "Returns 1 for positive tests, 2 for negative or comparison tests."
  `(if (test-modifier (first ,list)) 2 1))

(defmacro var-var> (var)
  "Adds a final > to var."
  `(intern (concatenate 'string (coerce-string ,var) ">")))

(defmacro var>var (var)
  "Strips the final > off the var."
  `(let* ((name (symbol-name ,var))
          (length (- (length name) 1))
          (new-string (make-string length)))
     (dotimes (i length (intern new-string))
       (setf (schar new-string i) (schar name i)))))

(defmacro var=var (var)
  "Strips the initial = off a var."
  `(let* ((name (symbol-name ,var))
          (length (- (length name) 1))
          (new-string (make-string length)))
     (dotimes (i length (intern new-string))
       (setf (schar new-string i) (schar name (+ i 1))))))

(defun coerce-string (expr)
  "Returns the string corresponding to expr.  Tries to avoid using format."
  (when (wmep expr) (setf expr (wme-name expr)))
  (cond ((stringp expr) expr)
        ((symbolp expr) (symbol-name expr))
        (t (format nil "~S" expr))))



;;; 
;;; UTILITIES
;;;

(defmacro push-last (item place)
  `(setf ,place (nconc ,place (list ,item))))

(defun noise (s)
  "Approximates a sample from a normal distribution with mean zero and
   the given s-value (/ (sqrt (* 3.0 variance)) 3.1416)."
  ;; Need to test bound because of short-float lack of precision
  (let ((p (max 0.0001 (min (random 1.0) 0.9999))))
    (* s (log (/ (- 1.0 p) p)))))

(defmacro exp-log (base power)
  "Computes expt using exp and log to prevent getting out of short-float,
   if necessary."
  (if (typep (expt 1.0 1.0) *read-default-float-format*)
    `(expt ,base ,power)
    `(exp (* (log ,base) ,power))))

(defun quote-arguments (argument-list)
  "Return a new argument list with the non-constants quoted."
  (let ((new-list nil))
    (dolist (argument argument-list new-list)
      (push-last (if (or (constantp argument) (variablep argument)
                         (and (symbolp argument) (boundp argument)))
                   argument (list 'quote argument))
                 new-list))))

;;;
;;; MAIN FUNCTION DEFINITIONS
;;;

;;; Clearing, Resetting and Deleting

(defmacro safe-gentemp (prefix)
  "gentemp a new symbol and save it for later removal."
  `(car (push (gentemp ,prefix) *used-ACT-R-symbols*)))

(defun clear-used-symbols-fct ()
  "Uninterns all of the symbols that have been safe-gentemp'ed
   since the last reset."
  (dolist (x *used-ACT-R-symbols*)
    (unintern x))
  (setf *used-ACT-R-symbols* nil))

(defun init-types ()
  "Initializes the init types"
  (chunk-type-fct '(chunk))
  (chunk-type-fct '(error condition))
  (chunk-type-fct '(dependency goal modified stack constraints actions
                    generals specifics dont-cares differents))
  (let ((type-t (make-wme-type)))
    (setf (wme-type-name type-t) t)
    (setf (wme-type-documentation type-t) "The default type for productions without goals.")
    (setf (wme-type-size type-t) 0)
    (push-last (cons t type-t) *declarative-memory*))
  (add-dm-fct '((failure isa error condition failure))))

(defun clear-all-fct (&optional (save-model t))
  "Clears everything."
  (clear-used-symbols-fct)
  (setf *compile-eval-calls* nil)
  (when *load-pathname*
    (setf *model* (when save-model (list *load-pathname*)))
    (when (eq save-model t)
      (load-model-list *load-pathname*)))
  (setf *time* 0.0)
  (setf *start-time* 0.0)
  (setf *default-action-time* 0.05)
  (setf *latency* 0.0)
  (setf *cycle* 0)
  (setf *spread-stamp* 0)
  (setf *wme-number* 0.0)
  (setf *buffers* nil)
  (setf *declarative-memory* nil)
  (setf *hash-names* (make-hash-table :test #'eq :size 1000))
  (init-types)
  (setf *procedural-memory* nil)
  (setf *goal-activation* 1.0)
  (setf *goal-sources* nil)
  (setf *wmfocus* nil)
  (setf *wmfocus-list* nil)
  (setf *goal-stack* nil)
  (setf *goal-depth* 1)
  (setf *g* 20.0)
  (setf *exp-gain-noise* nil)
  (setf *retrieval* nil)
  (setf *retrieval-scheduler* nil)
  (setf *visual-location* nil)
  (setf *aural-location* nil)
  (setf *previous-instantiations* nil)
  (setf *previous-instantiation* nil)
  (setf *instantiation* nil)
  (setf *extra-instantiation* nil)
  (setf *conflict-set* nil)
  (setf *sum-exp-act* 0.0)
  (setf *latency-fn* 'old-latency)
  (setf *latency-factor* 1.0)
  (setf *latency-exponent* 1.0)
  (setf *base-level-constant* 0.0)
  (setf *activation-sources* nil)
  (setf *activation-noise* nil)
  (setf *permanent-activation-noise* nil)
  (setf *mismatch-penalty* 1.0)
  (setf *retrieval-threshold* nil)
  (setf *partial-matching* nil)
  (setf *max-sim* 0.0)
  (setf *max-dif* -1.0)
  (setf *maximum-associative-strength* nil)
  (setf *temperature* 1.0)
  (setf *blending* nil)
  (setf *break-productions* nil)
  (setf *failed-productions* nil)
  (setf *cost-penalty* 1.0)
  (setf *initial-experience* 10.0)
  (setf *threshold-time* 2.0)
  (setf *enable-production-learning* nil)
  (setf *reinforce-analogized-production* t)
  (setf *production-compilation-parameters* nil)
  (setf *enable-rational-analysis* nil)
  (setf *enable-randomness* nil)
  (setf *utility-threshold* 0.0)
  (setf *optimized-learning* t)
  (setf *base-level-learning* nil)
  (setf *associative-learning* nil)
  (setf *strength-learning* nil)
  (setf *parameters-learning* nil)
  (setf *command-trace* t)
  (setf *output-trace* t)
  (setf *cycle-trace* t)
  (setf *latency-trace* nil)
  (setf *partial-matching-trace* nil)
  (setf *blending-trace* nil)
  (setf *production-compilation-trace* t)
  (setf *activation-trace* nil)
  (setf *conflict-resolution-trace* nil)
  (setf *conflict-set-trace* nil)
  (setf *goal-trace* nil)
  (setf *dm-trace* nil)
  (setf *production-trace* nil)
  (setf *matches-trace* nil)
  (setf *exact-matching-trace* nil)
  (setf *verbose* t)
  (setf *abort-instantiation* t)
  (setf *parameter-sets* nil)
  (setf *similarity-hook-fn* nil)
  (setf *blending-hook-fn* 'blending-arithmetic-mean)
  (setf *conflict-set-hook-fn* nil)
  (setf *firing-hook-fn* nil)
  (setf *cycle-hook-fn* nil)
  (setf *web-hook-fn* nil)
  (setf *end-run-hook-fn* nil)
  (setf *step-fn* 'step-fct)
  (setf *stop* nil)
  (setf *save-state-changes* nil)
  (setf *pop-upon-failure* nil)
  (when *init-hook-fn* (funcall *init-hook-fn*)))

(defun clear-dm-fct ()
  "Clears all working memory elements."
  (dolist (wmetype *declarative-memory*)
    (setf (wme-type-wmes (cdr wmetype)) nil))
  (clrhash *hash-names*)
  (setf *wme-number* 0.0))

(defun clear-productions-fct ()
  "Clears all productions from memory."
  (dolist (wmetype *declarative-memory*)
    (setf (wme-type-productions (cdr wmetype)) nil))
  (setf *procedural-memory* nil)
  (setf *break-productions* nil)
  (setf *failed-productions* nil))

(defun actr-time-fct (&optional inc)
  "Returns the current act-r time, or adds inc to it if specified."
  (if inc
    (incf *time* inc)
    *time*))

(defparameter *merge* nil)

(defun delete-wme (wme &optional (identical nil))
  "Deletes a given wme node from the type memory and the hash table.
   Zeroes all its slots, and only proceed when it is not referenced.  If identical
   is specified, any remaining reference to wme will be replaced with identical.
   Also replace occurrences of wme in buffers, retrieval scheduler and
   instantiation variables."
  (dolist (parent (rest (wme-fan wme)))
    (unless (eq parent wme)
      (if identical  ; replace references to wme in chunk slots with identical
        (dotimes (index (wme-type-size (wme-type parent)))
          (when (eq (wme-slot parent index) wme)
            (set-slot-value parent index identical)))
        (signal-output *command-trace* "CHUNK ~S IS STILL REFERENCED IN CHUNK ~S WHEN DELETED."
                       wme parent))))
  ;; Clear occurences of wme in buffers
  (dolist (buffer *buffer-keywords*)
    (setf buffer (cdr buffer))
    (when (eq (symbol-value buffer) wme)
      (setf (symbol-value buffer) identical)))
  ;; same for retrieval scheduler but make sure it works whether pair or wme
  (when *retrieval-scheduler*
    (if (consp *retrieval-scheduler*)
      (when (eq (cdr *retrieval-scheduler*) wme)
        (rplacd *retrieval-scheduler* identical))
      (when (eq *retrieval-scheduler* wme)
        (setf *retrieval-scheduler* wme))))
  ;; Clear occurences of wme in instantiation variables
  (let ((instantiation *instantiation*))
    (when instantiation
      (dotimes (variable (production-size (instantiation-production instantiation)))
        (when (eq (instantiation-variable instantiation variable) wme)
          (setf (instantiation-variable instantiation variable) identical)))))
  ;; When merging print a warning message
  (when identical
    (signal-output *cycle-trace* "Merging chunk ~S into chunk ~S" wme identical)
    ;; added for production compilation purposes
    (setf *merge* (list wme identical)))
  (let ((wmetype (wme-type wme)))
    (decf *wme-number* 1.0)
    (dolist (slot (wme-type-slots wmetype))
      (set-slot-value wme (slot-index slot) nil))
    (remove-name (wme-name wme))
    (setf (wme-type-wmes wmetype)
          (delete wme (wme-type-wmes wmetype) :test #'eq :count 1))
    (save-state-change :delete-wme wme)))

(defun delete-chunk-fct (wmes)
  "Deletes wmes."
  (let ((structures nil))
    (dolist (wme wmes structures)
      (setf wme (get-safe-wme wme))
      (when wme
        (push-last (wme-name wme) structures)
        (delete-wme wme)))))

(defun delete-production (production)
  "Removes a production from procedural memory and type memory."
  (let ((type (production-goal-type production)))
    (save-state-change :delete-production production)
    (setf (wme-type-productions type) (delete production (wme-type-productions type)
                                              :test #'eq :count 1))
    (setf *procedural-memory* (delete (production-name production) *procedural-memory*
                                      :test #'eq :key #'car :count 1))))

(defun remove-wmetype-definition (definition)
  "Removes the structure definitions pertaining to a wme type definition."
  (dolist (subtype (rest (wme-type-subtypes definition)))
    (setf (wme-type-supertypes subtype)
          (delete definition (wme-type-supertypes subtype) :test #'eq :count 1)))
  (dolist (supertype (wme-type-supertypes definition))
    (setf (wme-type-subtypes supertype)
          (delete definition (wme-type-subtypes supertype) :test #'eq :count 1)))
  (setf *declarative-memory*
          (delete definition *declarative-memory*
                  :test #'eq :key #'cdr :count 1)))


;;; WME Type Creating and Printing

(defun pprint-wmetype (wmetype)
  "Pretty prints a wmetype."
  (signal-output *command-trace* "~S" wmetype)
  (when (wme-type-documentation wmetype)
    (signal-output *command-trace* "~S" (wme-type-documentation wmetype)))
  (let ((super-type (second (wme-type-supertypes wmetype))))
    (when super-type
      (signal-output *command-trace* " <- ~S" super-type)))
  (dolist (slot (wme-type-slots wmetype))
    (signal-output *command-trace* "   ~S" (slot-name slot))
    (when (slot-default slot)
      (signal-output *command-trace* " <- ~S" (slot-default slot))))
  (wme-type-name wmetype))

(defun print-wmetypes (&optional (wmetypes nil))
  "Prints definitions for a list of types, possibly empty."
  (let ((*verbose* t)
        (*goal-depth* 1)
        (structures nil))
    (if wmetypes
      (dolist (wmetype wmetypes)
        (setf wmetype (get-safe-type wmetype "CHUNK-TYPE"))
        (when wmetype
          (push-last (pprint-wmetype wmetype) structures)))
      (dolist (wmetype *declarative-memory*)
        (setf wmetype (cdr wmetype))
        (push-last (pprint-wmetype wmetype) structures)))
    structures))

(defun chunk-type-fct (name-and-slots)
  "Defines a new wme type.  Checks that the syntax is correct.
   Clears the previous definition if wme type is already defined."
  (if name-and-slots
    (let* ((name-description (car name-and-slots))
           (name (if (consp name-description)
                   (car name-description) name-description))
           (super-type (if (consp name-description)
                         (cdr name-description) nil))
           (documentation (when (stringp (second name-and-slots))
                            (second name-and-slots)))
           (slots (if documentation (cddr name-and-slots) (cdr name-and-slots)))
           (definition nil)
           (slot-index 0))
      ; check type hierarchy
      (when super-type
        (unless (null (cdr super-type))
          (signal-warn "TOO MANY OPTIONS SPECIFIED FOR TYPE ~S: ~S.  ALL BUT THE FIRST ARE IGNORED."
                       name super-type))
        (if (and (eq (caar super-type) :include) (null (cddar super-type)))
          (setf super-type (get-safe-type (cadar super-type) name))
          (signal-warn "UNKNOWN OPTION ~S SPECIFIED FOR TYPE ~S." (car super-type) name)))
      ; check for redefinition
      (setf definition (get-type name))
      (cond (definition
              (signal-warn "TYPE ~S IS BEING REDEFINED.  RESET STRONGLY SUGGESTED." name)
              (remove-wmetype-definition definition))
            (t
             (setf definition (make-wme-type))))
      (setf (wme-type-name definition) name)
      (setf (wme-type-documentation definition) documentation)
      (setf (wme-type-supertypes definition)
            (when super-type (copy-list (wme-type-supertypes super-type))))
      (setf (wme-type-size definition) (if super-type (wme-type-size super-type) 0))
      (setf (wme-type-slots definition)
            (when super-type (copy-list (wme-type-slots super-type))))
      (push-last (cons name definition) *declarative-memory*)
      (push definition (wme-type-subtypes definition))
      (dolist (super-type (wme-type-supertypes definition))      
        (push-last definition (wme-type-subtypes super-type)))
      (push definition (wme-type-supertypes definition))
      (setf slot-index (wme-type-size definition))
      (dolist (slot slots)
        (cond ((or (atom slot) (and (listp slot) (= (length slot) 2)))
               (push-last (make-slot :name (if (atom slot) slot (first slot))
                                     :index slot-index
                                     :default (if (atom slot) nil
                                                  (get-wme-or-constant (second slot))))
                          (wme-type-slots definition))
               (incf slot-index 1))
              (t
               (signal-warn "UNKNOWN SLOT FORMAT ~S FOR TYPE ~S." slot name))))
      (setf (wme-type-size definition) slot-index)
      name)
    (print-wmetypes)))

(defun pprint-instantiation (&optional (instantiation *instantiation*)
                                       (trace *command-trace*)
                                       (format *production-trace*))
  "Pretty-prints an instantiation."
  (let ((production (instantiation-production instantiation)))
    (when (eq trace 'short) (setf trace t))
    (signal-output trace "~S  ~8,3F  ~8,3F" production
                   (instantiation-latency instantiation)
                   (instantiation-gain instantiation))
    (if (eq format 'short)
      (dolist (binding (production-bindings production))
        (signal-output trace "   ~A: ~A" (variable-name binding)
                       (if (integerp (variable-index binding))
                         (or (instantiation-variable instantiation (variable-index binding))
                             "VARIABLE STILL UNBOUND")
                         "RETURN VARIABLE STILL UNDETERMINED")))
      (let ((bindings nil)
            (text nil))
        (dolist (binding (production-bindings production))
          (when (integerp (variable-index binding))
            (let ((value (instantiation-variable instantiation (variable-index binding))))
              (when value
                (push (cons (variable-name binding) value) bindings)
                (push (cons (var-var> (variable-name binding))
                            (var-var> value))
                  bindings)))))
        (setf text (sublis bindings (production-text production)))
        (pprint-production production trace text)))))

(defun pprint-instantiations (&optional (instantiations *conflict-set*)
                                        (trace *command-trace*)
                                        (format *production-trace*))
  "Pretty-prints instantiations on trace."
  (let ((index 0))
    (when (eq trace 'short) (setf trace t))
    (dolist (instantiation instantiations)
      (incf index 1)
      (signal-output trace "Instantiation ~D:" index)
      (pprint-instantiation instantiation trace format))))

;;; WME Creation

(defun create-wme (name type &key (given nil) (compute-ia t))
  "Creates a wme of a given name and type, with the default slot values.
   If wme is given, then reuse it.  If not, then initialize the structure.
   Install wme in the type memory and the hash table."
  (let ((wme (or given (make-wme)))
        (rji* 1.0)
        (sji 0.0))
    (incf *wme-number* 1.0)
    (setf (wme-name wme) name)
    (setf (wme-type wme) type)
    (setf (wme-slots wme) (make-array (wme-type-size type) :initial-element nil))
    (unless given
      (when *permanent-activation-noise*
        (setf (wme-permanent-noise wme) (noise *permanent-activation-noise*)))
      (setf (wme-fan wme) (list 1.0 wme))
      (when compute-ia
        (setf rji* (default-rji* wme))
        (setf sji (rji-sji rji*)))
      (create-ia wme wme :count 1 :sji sji :rji* rji*)
      (setf (wme-similarities wme) (list (cons wme *max-sim*))))
    (dolist (slot (wme-type-slots type))
      (when (slot-default slot)
        (set-slot-value wme (slot-index slot) (slot-default slot)
                        :compute-ia compute-ia)))
    (setf (get-wme name) wme)
    (push-last wme (wme-type-wmes type))
    (save-state-change :create-wme wme)
    wme))

(defun new-name-fct (name)
  "Returns a unique symbol as generated by gentemp based on name,
   which can be either a string, a symbol, or else defaults to CHUNK."
  (cond ((stringp name) (safe-gentemp (string-upcase name)))
        ((symbolp name) (safe-gentemp (symbol-name name)))
        (t (signal-warn "ARGUMENT ~S TO NEW-NAME IS NOT A SYMBOL OR STRING." name)
           (safe-gentemp "CHUNK"))))

(defun add-dm-fct (wmes &key (reset-ia t))
  "Adds the wmes to working memory."
  ; First pass: create the wme structures
  (let ((structures nil)
        (names nil))
    ;; copy once for more effective modification
    (setf wmes (copy-tree wmes))
    (do ((rest-wmes wmes (rest rest-wmes)))
        ((null rest-wmes))
      (let ((wme (first rest-wmes)))
        (when (and (eq (first wme) 'isa) (not (eq (second wme) 'isa)))
          (setf wme (cons (new-name-fct (second wme)) wme))
          (rplaca rest-wmes wme))
        (let* ((wme-name (first wme))
               (slots (if (stringp (second wme)) (cddr wme) (cdr wme))))
          ; check that the format is correct
          (cond ((eq (first slots) 'isa)
                 (let ((wme-structure (get-wme wme-name))
                       (wmetype (get-safe-type (second slots) wme-name)))
                   (when (and wme-structure wmetype)
                     (signal-output *command-trace* "CHUNK ~S ALREADY EXISTS.  DELETING IT FIRST."
                                    wme-name)
                     (delete-wme wme-structure))
                   ; create the new wme and install it in declarative memory and the hash
                   (if wmetype
                     (setf wme-structure
                           (create-wme wme-name wmetype :given wme-structure :compute-ia nil))
                     (setf wmes (delete wme wmes :test #'eq)))))
                (t
                 (signal-warn "NO TYPE SPECIFIED FOR CHUNK ~S." wme-name)
                 (setf wmes (delete wme wmes :test #'eq)))))))
    ; Second pass: fill the slots with the cross-references
    (dolist (wme wmes)
      (let* ((wme-name (first wme))
             (wme-structure (get-wme wme-name))
             (wmetype (wme-type wme-structure))
             (documentation (when (stringp (second wme)) (second wme)))
             (all-slots (if documentation (cddddr wme) (cdddr wme)))
             (slot nil))
        (push-last wme-structure structures)
        (push-last wme-name names)
        (when documentation
          (setf (wme-documentation wme-structure) documentation))
        (do ((slots all-slots (cddr slots)))
            ((null slots))
          (setf slot (get-safe-slot (first slots) wmetype wme-name))
          (when slot (set-slot-value wme-structure
                                     (slot-index slot)
                                     (get-wme-or-constant (second slots))
                                     :compute-ia nil)))))
    ; Reset all IAs, or just those for the wmes
    (if reset-ia
      (reset-ia-fct)
      (dolist (structure structures)
        (reset-ia-wme structure)))
    names))

(defun set-dm-fct (wmes)
  "Same as addwm-fct."
  (add-dm-fct wmes))

(defun copy-chunks (wmes)
  "Makes a clean copy of wmes and return the copies."
  (let ((copies nil)
        (copy nil))
    (dolist (wme wmes copies)
      (setf wme (get-safe-wme wme))
      (when wme
        (setf copy (create-wme (safe-gentemp (symbol-name (wme-name wme)))
                               (wme-type wme)))
        (dolist (slot (wme-type-slots (wme-type copy)))
          (set-slot-value copy (slot-index slot)
                          (get-slot-value wme (slot-name slot))))
        (push-last copy copies)))))

(defun copy-chunk-fct (wmes)
  "Makes a clean copy of wmes and return the names."
  (get-name-fct (copy-chunks wmes)))    

;;; WME Activation

(defun reset-ia-wme (wmei)
  "Resets all the ia values leading to wmei."
  (let ((wmej nil)
        (ia nil))
    (dolist (wme-ia (wme-ias wmei))
      (setf wmej (car wme-ia))
      (setf ia (cdr wme-ia))
      (compute-ia wmej ia wmei))))

(defun reset-ia-fct ()
  "Resets all the ia values, preserving user-specified and learned values.
   Make sure that spreading activation is recomputed."
  (let ((rji* 0.0)
        (sji 0.0)
        (ia nil))
    (for-all-wmes wmej
                  (setf rji* (default-rji* wmej))
                  (setf sji (rji-sji rji*))
                  (dolist (wmei (rest (wme-fan wmej)))
                    (setf ia (get-ia wmej wmei))
                    (setf (ia-rji* ia) (count-rji* ia rji*))
                    (setf (ia-sji ia) (count-sji ia sji))))
    (incf *spread-stamp* 1)
    nil))

(defun ia-fct (wmej wmei)
  "Returns the ia value between wmej and wmei."
  (let* ((*verbose* t)
         (*goal-depth* 1)
         (ia nil))
    (setf wmej (get-safe-wme wmej))
    (setf wmei (get-safe-wme wmei))
    (when (and wmej wmei)
      (setf ia (get-ia wmej wmei)))
    (setf ia (if ia (ia-value ia wmej wmei) 0.0))
    (signal-output *command-trace* "~6,3F" ia)
    ia))

(defun update-activation-spread (&key (focus *wmfocus*))
  "Updates the activation sources to be the slot values of the focus wme."
  (incf *spread-stamp* 1)
  (dolist (source *activation-sources*)
    (setf (wme-source source) nil))
  (setf *activation-sources* nil)
  (when focus
    (let ((level (first (wme-slot-wmes focus))))
      (when (> level 0.0)
        (setf level (/ *goal-activation* level))
        (dolist (source (rest (wme-slot-wmes focus)))
          (cond ((wme-source source)
                 (incf (wme-source source) level))
                (t
                 (setf (wme-source source) level)
                 (push source *activation-sources*))))))))

(defun compute-spreading-activation (wmei)
  "Updates the spreading activation of wmei.  Maintains activation."
  (let ((wmej nil)
        (ia nil)
        (spread 0.0))
    (decf (wme-activation wmei) (wme-source-spread wmei))
    (setf (wme-source-spread wmei) 0.0)
    (dolist (wme-ia (wme-ias wmei))
      (setf wmej (car wme-ia))
      (when (wme-source wmej)
        (setf ia (cdr wme-ia))
        (setf ia (ia-value ia wmej wmei))
        (setf spread (* (wme-source wmej) ia))
        (signal-output *activation-trace* "   Spreading activation ~6,3F from source ~S level ~6,3F times IA ~6,3F"
                       spread wmej (wme-source wmej) ia)
        (incf (wme-source-spread wmei) spread)))
    (setf (wme-spread-stamp wmei) *spread-stamp*)
    (incf (wme-activation wmei) (wme-source-spread wmei))
    (wme-source-spread wmei)))

(defun compute-references (n references creation-time minus-decay)
  "Computes generalized decay formula from number and list of references,
   creation time and minus the decay rate."
  (let ((value 0.0)
        (last-reference 0.0))
    (when references
      (dolist (reference references)
        (incf value (exp-log (max *default-action-time* (- *time* reference))
                             minus-decay))
        (setf last-reference reference)))
    (when *optimized-learning*
      (let ((denominator (+ 1.0 minus-decay)))
        (if (numberp *optimized-learning*)
          (when (> n *optimized-learning*)
            (incf value (/ (* (- n *optimized-learning*)
                              (- (exp-log (- *time* creation-time) denominator)
                                 (exp-log (- *time* last-reference) denominator)))
                           (* (max *default-action-time* (- last-reference creation-time))
                              denominator))))
          (setf value (/ (* n (exp-log (max *default-action-time* (- *time* creation-time))
                                       minus-decay))
                         denominator)))))
    (log value)))

(defun compute-base-level-activation (wme)
  "Updates the base level activation of wme when base level learning."
  (setf (wme-base-level wme)
        (+ *base-level-constant*
           (compute-references (car (wme-references wme)) (cdr (wme-references wme))
                               (wme-creation-time wme) (- *base-level-learning*))))
  (signal-output *activation-trace* "   Computing a base level of ~6,3F from ~D references from creation time ~6,3F"
                 (wme-base-level wme) (round (first (wme-references wme))) (wme-creation-time wme))
  (wme-base-level wme))

(defun activation (wme)
  "Returns the activation of wme.  May involve recomputing the spreading activation,
   the base level and adding some noise."
  (declare (inline compute-spreading-activation compute-base-level-activation noise))
  (when (and *enable-rational-analysis*
             (< (wme-spread-stamp wme) *spread-stamp*))
    (compute-spreading-activation wme))
  (when (and (or *base-level-learning* *activation-noise*)
             (< (wme-time-stamp wme) *time*))
    (when *base-level-learning*
      (compute-base-level-activation wme))
    (setf (wme-activation wme) (+ (wme-permanent-noise wme)
                                  (wme-base-level wme)
                                  (wme-source-spread wme)))
    (when *activation-noise*
      (let ((noise (noise *activation-noise*)))
        (signal-output *activation-trace* "   Adding noise ~6,3F" noise)
        (incf (wme-activation wme) noise)))
    (setf (wme-time-stamp wme) *time*))
  (wme-activation wme))

(defun update-activation-fct ()
  "Updates the activation of all wmes by recomputing it.
   Also updates the IAs when associative learning is on."
  (let ((*verbose* t)
        (*goal-depth* 1))
    (incf *spread-stamp* 1)
    (for-all-wmes wme
                  (when *associative-learning*
                    (dolist (wme-ia (wme-ias wme))
                      (ia-value (cdr wme-ia) (car wme-ia) wme)))
                  (activation wme))))


;;; WME Parameters and Printing

(defun even-references (start end n &optional (m n))
  "Distributes m references evenly along n intervals between start and end."
  (when (plusp n)
    (let ((decrement (/ (- end start) n))
          (time end)
          (times nil))
      (dotimes (i (round m) times)
        (decf time decrement)
        (push-last time times)))))

(defun adapt-references (references creation-time &optional (optimized-learning *optimized-learning*))
  "If optimized learning is off, then erase all reference times.
   If on, then generate equidistant references since creation time.
   Generalize to a fixed number of references."
  (cond ((eq optimized-learning t) (rplacd references nil))
        ((null optimized-learning)
         (nconc references (even-references creation-time
                                            (if (null (rest references)) *time*
                                                (first (last references)))
                                            (- (first references)
                                               (length (rest references))))))
        ((<= optimized-learning (length (rest references)))
         (rplacd (nthcdr optimized-learning references) nil))
        (t
         (nconc references (even-references creation-time
                                            (if (null (rest references)) *time*
                                                (first (last references)))
                                            (- (first references)
                                               (length (rest references)))
                                            (- (min optimized-learning (first references))
                                               (length (rest references))))))))

(defun get-base-level-fct (wmes)
  "Returns the base level of wmes."
  (let* ((*verbose* t)
         (*goal-depth* 1)
         (baselevel 0.0)
         (baselevels nil))
    (dolist (wme wmes baselevels)
      (setf wme (get-safe-wme wme))
      (when wme
        (setf baselevel
              (if (and *base-level-learning* (< (wme-time-stamp wme) *time*))
                (compute-base-level-activation wme)
                (wme-base-level wme)))
        (signal-output *command-trace* "~6,3F" baselevel)
        (push-last baselevel baselevels)))))

(defun set-base-level (wme setting)
  "Sets the base level of wme.  setting is either the base level or the
   references, followed perhaps by the creation time.  Increments activation."
  (let ((old-base-level (wme-base-level wme)))
    (when (second setting)
      (setf (wme-creation-time wme) (coerce (second setting)
                                            *read-default-float-format*)))
    (cond (*base-level-learning*
           (setf (wme-references wme)
                 (list (coerce (first setting) *read-default-float-format*)))
           (unless *optimized-learning*
             (adapt-references (wme-references wme) (wme-creation-time wme)))
           (compute-base-level-activation wme))
          (t
           (setf (wme-base-level wme) (coerce (first setting) *read-default-float-format*))))
    (incf (wme-activation wme) (- (wme-base-level wme) old-base-level))
    (wme-base-level wme)))
  
(defun set-all-base-levels-fct (references &optional (creation-time nil))
  "Sets all individual base levels to these references and creation-time."
  (let ((baselevel nil)
        (*activation-trace* nil))
    (for-all-wmes wme
                  (setf baselevel
                        (set-base-level wme (list references creation-time))))
    baselevel))

(defun set-base-levels-fct (settings)
  "Sets individual base levels.  settings specify either the base level,
   if base level learning is off, or the references and perhaps
   the creation time."
  (let ((wme-or-type nil)
        (baselevels nil)
        (*activation-trace* nil))
    (dolist (setting settings baselevels)
      (setf wme-or-type (first setting))
      (unless (or (wmep wme-or-type) (wme-type-p wme-or-type))
        (setf wme-or-type (get-type wme-or-type))
        (unless wme-or-type
          (setf wme-or-type (get-safe-wme (first setting)))))
      (if (wmep wme-or-type)
        (push-last (set-base-level wme-or-type (rest setting)) baselevels)
        (when (wme-type-p wme-or-type)
          (dolist (wme (wme-type-wmes wme-or-type))
            (push-last (set-base-level wme (rest setting)) baselevels)))))))

(defun set-general-base-levels-fct (settings)
  "Sets individual base levels.  settings specify either the base level,
   if base level learning is off, or the references and perhaps
   the creation time.  Same as set-base-levels-fct."
  (set-base-levels-fct settings))

(defun set-ia-value (wmej wmei sji)
  "Sets ia value from wmej to wmei to sji.
   When wmej is a source, recompute spreading activation for wmei."
  (let ((ia (get-make-ia wmej wmei)))
    (when (wme-source wmej) (decf (wme-spread-stamp wmei) 1))
    (setf (ia-rji* ia) (exp sji))
    (setf (ia-sji ia) sji)
    sji))

(defun add-ia-fct (settings)
  "Sets individual ias.  settings specify Sji as (wmej wmei sji)."
  (let ((wmej nil)
        (wmei nil)
        (ias nil))
    (dolist (setting settings ias)
      (setf wmej (get-safe-wme (first setting)))
      (setf wmei (get-safe-wme (second setting)))
      (when (and wmej wmei)
        (push-last (set-ia-value wmej wmei (third setting)) ias)))))

(defun set-ia-fct (settings)
  "Sets individual ias.  settings specify Sji as (wmej wmei sji).  Same as add-ia-fct."
  (add-ia-fct settings))

(defun activation-sources-fct ()
  "Displays and returns activation sources."
  (let* ((*verbose* t)
         (*goal-depth* 1)
         (sources nil))
    (dolist (source *activation-sources* sources)
      (push-last (wme-name source) sources)
      (signal-output *command-trace* "~S: ~6,3F" source (wme-source source)))))

(defun chunk-slot-value-fct (wme slot)
  "Returns the slot value of wme."
  (setf wme (get-safe-wme wme))
  (when wme
    (get-wme-name (get-safe-slot-value wme slot))))

(defun mod-chunk-fct (name slot-values)
  "Sets slots of wme name to values.
   Make sure that activation spreading is recomputed for all or just this wme."
  (let* ((wme (get-safe-wme name))
         (type (when wme (wme-type wme)))
         (slot nil))
    (when wme
      (do ((rest-slot-values slot-values (cddr rest-slot-values)))
          ((null rest-slot-values))
        (setf slot (get-safe-slot (first rest-slot-values) type name))
        (when slot (set-slot-value wme
                                   (slot-index slot)
                                   (get-wme-or-constant (second rest-slot-values)))))
      (if (eq wme *wmfocus*)
        (update-activation-spread)
        (decf (wme-spread-stamp wme) 1))
      (wme-name wme))))

(defun mod-focus-fct (slot-values)
  "Sets slots of focus to values."
  (mod-chunk-fct *wmfocus* slot-values))

(defun get-name-fct (wmes-or-productions)
  "Given a list of wmes or productions, returns the list of names."
  (let ((*verbose* t)
        (*goal-depth* 1)
        (names nil))
    (dolist (structure wmes-or-productions names)
      (cond ((wmep structure)
             (push-last (wme-name structure) names))
            ((productionp structure)
             (push-last (production-name structure) names))
            ((listp structure)
             (dolist (sublist structure)
               (push-last (get-name-fct sublist) names)))
            (t
             (signal-output *command-trace* "OBJECT ~S IN COMMAND NAME IS NEITHER A CHUNK NOR A PRODUCTION."
                            structure))))))

(defun pprint-wme (wme)
  "Given a wme, prints the name, type, and slot values, and returns its name."
  (let ((wmetype (wme-type wme)))
    (signal-output *command-trace* "~A~S   ~6,3F"
                   (if (eq wme *wmfocus*) "**" "") wme (activation wme))
    (when (wme-documentation wme)
      (signal-output *command-trace* "~S" (wme-documentation wme)))
    (signal-output *command-trace* "   isa ~S" wmetype)
    (dolist (slot (wme-type-slots wmetype))
      (let ((slot-value (wme-slot wme (slot-index slot))))
        (signal-output *command-trace* "   ~S ~S" (slot-name slot)
                       slot-value)))
    (wme-name wme)))

(defun dm-fct (wmes)
  "Pretty prints a list of wmes, or all of them if not specified."
  (let ((*verbose* t)
        (*goal-depth* 1)
        (structures nil))
    (if wmes
      (dolist (wme wmes)
        (setf wme (get-safe-wme wme nil))
        (when wme
          (push-last (pprint-wme wme) structures)))
      (for-all-wmes wme
                    (push-last (pprint-wme wme) structures)))
    structures))

(defun sdm-fct (slot-values)
  "Prints wmes having slot values."
  (let ((*verbose* t)
        (*goal-depth* 1)
        (structures nil))
    (dolist (type *declarative-memory* structures)
      (let ((slots (wme-type-slots (cdr type)))
            (slot nil)
            (index nil)
            (index-values nil))
        (when (do ((rest-slot-values slot-values (cddr rest-slot-values)))
                  ((null rest-slot-values) t)
                (setf slot (first rest-slot-values))
                (cond ((eq slot 'isa)
                       (unless (eq (second rest-slot-values) (car type))
                         (return nil)))
                      (t
                       (setf index (slot-index (get-slot slot slots)))
                       (if index
                         (push (cons index (or (get-wme (second rest-slot-values))
                                               (second rest-slot-values)))
                               index-values)
                         (return nil)))))
          (dolist (wme (wme-type-wmes (cdr type)))
            (dolist (test index-values (push-last (pprint-wme wme) structures))
              (unless (equal (cdr test) (wme-slot wme (car test)))
                (return nil)))))))))


;;; WME parameters setting and printing

(defun wme-parameters-fct (wme &optional parameters)
  "Returns the value of the wme parameter(s), or print them all if none specified."
  (let ((*verbose* t)
        (*goal-depth* 1)
        (value nil)
        (values nil))
    (setf wme (get-safe-wme wme))
    (cond (wme
           (signal-output *command-trace* "Parameters for chunk ~S:" wme)
           ;;; Update activations!!
           (when *enable-rational-analysis*
             (when *associative-learning*
               (dolist (wme-ia (wme-ias wme))
                 (ia-value (cdr wme-ia) (car wme-ia) wme)))
             (activation wme))
           (cond (parameters
                  (dolist (parameter parameters)
                    (setf value 
                          (case parameter
                            (:name (wme-name wme))
                            (:activation (wme-activation wme))
                            (:source (wme-source wme))
                            (:base-level (wme-base-level wme))
                            (:creation-time (wme-creation-time wme))
                            (:references (wme-references wme))
                            (:source-spread (wme-source-spread wme))
                            (:ias (wme-ias wme))
                            (:creation-cycle (wme-creation-cycle wme))
                            (:needed (wme-needed wme))
                            (:contexts (wme-contexts wme))
                            (:permanent-noise (wme-permanent-noise wme))
                            (:similarities (wme-similarities wme))
                            (t (signal-warn "NO PARAMETER ~A DEFINED FOR CHUNK (TRY : IN FRONT)."
                                            parameter)
                               :error)))
                    (push-last value values)
                    (signal-output *command-trace* "~S ~6,3F" parameter value))
                  (signal-output *command-trace* "")
                  values)
                 (t
                  (when *enable-rational-analysis*
                    (signal-output *command-trace*
                                   ":Activation ~6,3F~% :Source ~6,3F~% :Base-Level ~6,3F"
                                   (wme-activation wme) (wme-source wme) (wme-base-level wme))
                    (when *base-level-learning*
                      (signal-output *command-trace*
                                     ":Creation-Time ~6,3F~% :References ~6,3F"
                                     (wme-creation-time wme) (wme-references wme)))
                    (signal-output *command-trace*
                                   ":Source-Spread ~6,3F~% :IAs ~6,3F"
                                   (wme-source-spread wme) (wme-ias wme))
                    (when *associative-learning*
                      (signal-output *command-trace*
                                     ":Creation-Cycle ~6,3F~% :Needed ~6,3F~% :Contexts ~6,3F"
                                     (wme-creation-cycle wme) (wme-needed wme) (wme-contexts wme)))
                    (when *permanent-activation-noise*
                      (signal-output *command-trace* ":Permanent-Noise ~6,#F"
                                     (wme-permanent-noise wme)))
                    (when *partial-matching*
                      (signal-output *command-trace*
                                     ":Similarities ~6,3F"
                                     (wme-similarities wme))))
                  (signal-output *command-trace* "")
                  wme)))
          (t :error))))

(defun set-wme-parameters-fct (wme parameters)
  "Sets the parameters of wme."
  (let ((values nil))
    (setf wme (get-safe-wme wme))
    (if wme
      (loop
        (unless parameters (return values))
        (let* ((parameter (pop parameters))
               (value (pop parameters)))
          (when (and (listp value) (eq (first value) 'quote))
            (setf value (second value)))  ;; for compatibility with evaluating versions
          (push-last
           (case parameter
             (:name
              (signal-warn "CHUNK NAME CANNOT BE SET.")
              :error)
             (:activation
              (signal-warn "CHUNK ACTIVATION CANNOT BE SET DIRECTLY: SET BASE-LEVEL AND/OR SOURCE INSTEAD.")
              :error)
             (:base-level
              (cond (*base-level-learning*
                     (signal-warn "CHUNK BASE-LEVEL CANNOT BE SET DIRECTLY WHEN BASE LEVEL LEARNING IS ENABLED: SET CREATION-TIME AND/OR REFERENCES INSTEAD.")
                     :error)
                    ((numberp value)
                     (incf (wme-activation wme) (- value (wme-base-level wme)))
                     (setf (wme-base-level wme) value))
                    (t
                     (signal-warn "CHUNK BASE-LEVEL MUST BE SET TO A NUMBER.")
                     :error)))
             (:source
              (cond ((or (numberp value) (null value))
                     (incf *spread-stamp* 1)
                     (setf (wme-source wme) value))
                    (t
                     (signal-warn "CHUNK SOURCE MUST BE SET TO NIL OR A NUMBER.")
                     :error)))
             (:creation-time
              (cond ((numberp value)
                     (decf (wme-time-stamp wme) 1.0)
                     (setf (wme-creation-time wme) value))
                    (t
                     (signal-warn "CHUNK CREATION-TIME MUST BE SET TO A NUMBER.")
                     :error)))
             (:references
              (cond ((listp value)
                     (decf (wme-time-stamp wme) 1.0)
                     (setf (wme-references wme)
                           (cons (length value)
                                 (unless *optimized-learning* (copy-list value))))
                     (adapt-references (wme-references wme) (wme-creation-time wme))
                     (wme-references wme))
                    ((numberp value)
                     (decf (wme-time-stamp wme) 1.0)
                     (setf (first (wme-references wme)) value)
                     (adapt-references (wme-references wme) (wme-creation-time wme))
                     (wme-references wme))
                    (t
                     (signal-warn "CHUNK REFERENCES MUST BE SET TO A NUMBER OR A LIST.")
                     :error)))
             (:source-spread
              (signal-warn "CHUNK SOURCE-SPREAD CANNOT BE SET DIRECTLY: SET SOURCE AND/OR IAS INSTEAD.")
              :error)
             (:ias
              (cond ((listp value)
                     (incf *spread-stamp* 1)
                     (dolist (ia-pair value)
                       (let ((wmei (get-safe-wme (car ia-pair)))
                             (ia (if (numberp (cdr ia-pair))
                                   (cdr ia-pair) (cadr ia-pair))))
                         (if (numberp ia)
                           (when wmei (set-ia-value wmei wme ia))
                           (signal-warn "CHUNK IA VALUE ~S IS NOT A NUMBER."
                                          ia))))
                     (wme-ias wme))
                    (t
                     (signal-warn "CHUNK IAS MUST BE SET TO A LIST OF CHUNK-NUMBER PAIRS.")
                     :error)))
             (:needed
              (cond ((numberp value)
                     (incf *spread-stamp* 1)
                     (setf (wme-needed wme) value))
                    (t
                     (signal-warn "CHUNK NEEDED MUST BE SET TO A NUMBER.")
                     :error)))
             (:contexts
              (cond ((numberp value)
                     (incf *spread-stamp* 1)
                     (setf (wme-contexts wme) value))
                    (t
                     (signal-warn "CHUNK CONTEXTS MUST BE SET TO A NUMBER.")
                     :error)))
             (:creation-cycle
              (cond ((numberp value)
                     (incf *spread-stamp* 1)
                     (setf (wme-creation-cycle wme) value))
                    (t
                     (signal-warn "CHUNK CREATION-CYCLE MUST BE SET TO A NUMBER.")
                     :error)))
             (:permanent-noise
              (cond ((numberp value)
                     (incf (wme-activation wme) (- value (wme-permanent-noise wme)))
                     (setf (wme-permanent-noise wme) value))
                    (t
                     (signal-warn "CHUNK PERMANENT-NOISE MUST BE SET TO A NUMBER.")
                     :error)))
             (:similarities
              (cond ((listp value)
                     (dolist (similarity-pair value)
                       (let ((wmei (get-safe-wme (car similarity-pair)))
                             (similarity (if (numberp (cdr similarity-pair))
                                           (cdr similarity-pair)
                                           (cadr similarity-pair))))
                         (if (numberp similarity)
                           (when wmei
                             (set-similarity wme wmei similarity))
                           (signal-warn "CHUNK SIMILARITY VALUE ~S IS NOT A NUMBER."
                                          similarity))))
                     (wme-similarities wme))
                    (t
                     (signal-warn "CHUNK SIMILARITIES MUST BE SET TO A LIST OF CHUNK-NUMBER PAIRS.")
                     :error)))
             (t (signal-warn "NO PARAMETER ~A DEFINED FOR CHUNKS (TRY : IN FRONT)."
                             parameter)
                :error))
           values)))
      :error)))

(defun sdp-fct (parameters)
  "Inspects and sets wme parameters."
  (let ((results nil))
    (if (null parameters) ; print all parameters for all wmes
      (for-all-wmes wme (push-last (wme-parameters-fct wme) results))
      (dolist (description (if (or (keywordp (first parameters))
                                   (keywordp (second parameters))
                                   (and (listp (first parameters))
                                        (null (second parameters))
                                        (not (keywordp (second (first parameters))))))
                             (list parameters) parameters))
        (when (atom description) (setf description (list description)))
        (if (keywordp (first description))
          (for-all-wmes
           wme
           (push-last
            (if (and (cdr description)
                     (not (keywordp (second description))))
              (set-wme-parameters-fct wme description)
              (wme-parameters-fct wme description))
            results))
          (dolist (wme (if (atom (first description))
                         (list (first description))
                         (first description)))
            (push-last
             (if (and (cddr description)
                      (not (keywordp (third description))))
               (set-wme-parameters-fct wme (rest description))
               (wme-parameters-fct wme (rest description)))
             results)))))
    results))


;;; Common similarity and blending functions.

(defun linear-similarity (wmej wmei)
  "Implements the linear similarity scale scaled by *linear-similarity-scale*.
   Suitable for use as the value of *similarity-hook-fn*.
   Returns the similar defined as the difference divided by scale,
   or nil if both arguments are not numbers."
  (when (and (numberp wmej) (numberp wmei))
    (/ (abs (- wmej wmei)) *linear-similarity-scale*)))

(defun blending-arithmetic-mean (value-probs)
  "Averages the value-probabilities using arithmetic mean.
   Suitable for linear similarities between numbers."
  (let ((sum 0.0))
    ;; Return the sum of all values multiplied by their probabilities
    (dolist (value-prob value-probs sum)
      (incf sum (* (car value-prob) (cdr value-prob))))))

(defun ratio-similarity (wmej wmei)
  "Implements the ratio similarity scale (no need for scaling).
   Suitable for use as the value of *similarity-hook-fn*.
   Returns the similar defined as the ratio of smallest to largest,
   or nil if both arguments are not numbers."
  (when (and (numberp wmej) (numberp wmei))
    (let ((min (min (abs wmej) (abs wmei)))
          (max (max (abs wmej) (abs wmei))))
      (if (zerop max) 1.0 ;; if max is 0 then wmej=wmei=0
          (/ min max)))))

(defun blending-geometric-mean (value-probs)
  "Averages the value-probabilities using geometric mean.
   Suitable for ratio similarities between numbers"
  (let ((sum 1.0))
    ;; Return the product of all values to the exponent of their probabilities
    ;; This will not work for negative values, but the ratio scale really only
    ;; applies to positive numbers anyway.
    (dolist (value-prob value-probs sum)
      (setf sum (* sum (expt (car value-prob) (cdr value-prob)))))))

(defun first-equation (best-value value-probs total-prob)
  "First activation equation (see ACT-R Workshop 99 slides)."
  (let ((activation 0.0))
    (dolist (value-prob value-probs)
      (let ((temperature (or *temperature* (* (sqrt 2.0) *activation-noise*)))
            (match-score (cdr value-prob)))
        ;; recover chunk matching score from its probability
        (unless (zerop total-prob)
          (setf match-score (* match-score total-prob)))
        (setf match-score (* temperature (log match-score)))
        ;; apply the mistatch penalty between values
        (decf match-score (- 1.0 (or (get-similarity (car value-prob)
                                                     best-value) 0.0)))
        ;; add the exponential of the new match scores
        (incf activation (exp match-score))))
    (if (zerop activation) ;; just in case
      *retrieval-threshold*
      (log activation))))

(defun second-equation (best-value value-probs total-prob)
  "Second activation equation (see ACT-R Workshop 99 slides)."
  (let ((activation 0.0))
    (dolist (value-prob value-probs)
      (let ((temperature (or *temperature* (* (sqrt 2.0) *activation-noise*)))
            (match-score (cdr value-prob)))
        ;; recover chunk matching score from its probability
        (unless (zerop total-prob)
          (setf match-score (* match-score total-prob)))
        (setf match-score (* temperature (log match-score)))
        ;; add the exponential of the new match scores weighted by similarity
        (incf activation (* (exp match-score)
                            (or (get-similarity (car value-prob)
                                                best-value) 0.0)))))
    (if (zerop activation) ;; just in case
      *retrieval-threshold*
      (log activation))))

(defun third-equation (best-value value-probs total-prob)
  "Third activation equation (see ACT-R Workshop 99 slides)."
  (declare (ignore total-prob))
  (let ((activation 0.0))
    (dolist (value-prob value-probs)
      (incf activation (* (cdr value-prob)
                          (or (get-similarity (car value-prob)
                                              best-value) 0.0))))
    (if (>= activation 1.0) ;; perfect agreement on the same value
      ;; which is only possible if it is retrieval threshold
      *retrieval-threshold*
      (log (/ activation (- 1.0 activation))))))

;;; Latencies

(defun old-latency (activation)
  "Old latency equation: scaled negative exponential of chunk activation.
   The activation of other chunks and threshold is not used for scaling."
  (* *latency-factor* (exp (- (* *latency-exponent*
                                 (+ (if *instantiation*
                                      (strength (instantiation-production *instantiation*))
                                      0.0)
                                    activation))))))

(defun competitive-latency-bounded (activation)
  "Competitive latency equation: scaled the old latency (exponential of
   activation) by the sum of the exponential of other chunks activation
   passed as second argument. Includes the chunk itself and the threshold
   and thus will always be greater than the latency factor."
  (* *latency-factor*
     (expt (/ *sum-exp-act* (exp (/ activation *temperature*)))
           *latency-exponent*)))

(defun competitive-latency (activation)
  "Competitive latency equation: scaled the old latency (exponential of
   activation) by the sum of the exponential of other chunks activation
   passed as second argument. Includes the threshold but not the chunk
   itself and thus can be arbitrarily small."
  (let ((chunk-itself (exp (/ activation *temperature*))))
    (* *latency-factor*
       (expt (/ (- *sum-exp-act* chunk-itself) chunk-itself)
             *latency-exponent*))))

(defmacro activation-latency (activation)
  "Given the activation, returns the latency according *latency-fn*."
  `(funcall *latency-fn* ,activation))


;;; Learning

(defmacro build-functional-parameter (expression bindings)
  "Builds a functional parameter from the expression and variable bindings."
  `(make-functional-parameter :expression ,expression
                              :function (sub-eval-vars ,expression ,bindings)))

(defmacro get-functional-parameter (production-parameter)
  "Returns the value of parameter for production.  Tests for functional values."
  `(if (functional-parameter-p ,production-parameter)
     (funcall (functional-parameter-function ,production-parameter))
     ,production-parameter))

(defmacro production-action-probability (production)
  "The probability that the action of production executes successfully
   is either chance if specified or P."
  `(or (get-functional-parameter (production-chance ,production))
       (production-p ,production)))

(defmacro production-action-cost (production)
  "The action cost of a production is either the effort if specified or C."
  `(or (get-functional-parameter (production-effort ,production))
       (production-c ,production)))

(defun add-reference (references &optional (optimized-learning *optimized-learning*)
                                 (count 1.0) (reference *time*))
  "Increments the reference count and pushes an extra reference if
   optimized learning is off.  Works for both wme and production."
  (save-state-change :add-reference references)
  (incf (first references) count)
  (unless (eq optimized-learning t)
    (dotimes (i (round count))
      (push reference (rest references))))
  ;;; when keeping a fixed number of references and there are too many, lose the oldest
  (when (and (numberp optimized-learning) (> (first references) optimized-learning))
    (nbutlast references)))

(defun learn-matching (&optional (instantiation *instantiation*))
  "Applies the learning equations 4.3, 4.5 and 4.6 to instantiation."
  (let ((production (instantiation-production instantiation))
        (retrievals nil)
        (ia nil)
        (level 1.0))
    (unless *blending*
      (when (or *base-level-learning* *associative-learning*)
        (dolist (retrieval (production-retrievals production))
          (push-last (instantiation-variable instantiation retrieval)
                     retrievals)))
      ;; For every WME matched on the LHS, accumulate the reference times
      ;; for use in Equation 4.3 (and Equation 4.5)
      (when *base-level-learning*
        (dolist (retrieval retrievals)
          (add-reference (wme-references retrieval))))
      ;; For every context element (activation source) and every needed WME,
      ;; increment the number of times in context and needed, respectively,
      ;; and the number of coincidences for every pair for use in Equation 4.5.
      ;; DO NOT update the Sji since it changes every epoch
      (when *associative-learning*
        (dolist (retrieval retrievals)
          (incf (wme-needed retrieval) 1.0))
        (dolist (wmej *activation-sources*)
          (incf (wme-contexts wmej) level)
          (dolist (retrieval retrievals)
            (setf ia (get-make-ia wmej retrieval))
            (incf (ia-fnicj ia) level)))))
    ;; Adds the reference time for the selected production in Equation 4.6
    (when *strength-learning*
      (add-reference (production-references production)))))

(defun compute-probabilities (successes-list failures-list)
  "Computes probabilities by taking the ratio of successes to the sum of successes and
  failures.  Computes those as decaying ratios if parameters learning is a number."
  (let ((successes 0.0)
        (failures 0.0))
    (cond ((numberp *parameters-learning*)
           (let ((minus-d (- *parameters-learning*)))
             (dolist (success (rest successes-list))
               (incf successes (exp-log (max *default-action-time* (- *time* success))
                                        minus-d)))
             (dolist (failure (rest failures-list))
               (incf failures (exp-log (max *default-action-time* (- *time* failure))
                                       minus-d)))))
          (t
           (setf successes (first successes-list))
           (setf failures (first failures-list))))
    (/ successes (+ successes failures))))

#|

Replaced 10/30/03 by Dan because new definition for compilation

(defun compute-p (p)
  "Computes the p parameter according to ACT Parameters Learning Equation."
  (setf (production-p p)
        (compute-probabilities (production-successes p)
                               (production-failures p))))
|#

(defun compute-costs (successes-list failures-list efforts-list)
  "Computes probabilities by taking the ratio of efforts to the sum of successes and
  failures.  Computes those as decaying ratios if parameters learning is a number."
  (let ((successes 0.0)
        (failures 0.0)
        (efforts 0.0))
    (cond ((numberp *parameters-learning*)
           (let ((minus-d (- *parameters-learning*))
                 (rest-successes (rest successes-list))
                 (rest-failures (rest failures-list))
                 (rest-efforts (rest efforts-list))
                 (decay 0.0))
             (loop
               (when (or (and (null rest-successes) (null rest-failures))
                         (null rest-efforts))
                 (return))
               (setf decay
                     (if (and rest-successes
                              (or (null rest-failures)
                                  (> (first rest-successes) (first rest-failures))))
                       (pop rest-successes) (pop rest-failures)))
               (setf decay (exp-log (max *default-action-time* (- *time* decay))
                                    minus-d))
               (incf successes decay)
               (incf efforts (* (pop rest-efforts) decay)))))
          (t
           (setf successes (first successes-list))
           (setf failures (first failures-list))
           (setf efforts (first efforts-list))))
    (/ efforts (+ successes failures))))

#|

Replaced 10/30/03 by Dan because new definition for compilation

(defun compute-c (p)
  "Computes the c parameter according to ACT Parameters Learning Equation."
  (setf (production-c p)
        (compute-costs (production-successes p) (production-failures p)
                       (production-efforts p))))
|#

(defmacro pg-c (production)
  "Computes PG-C of a production."
  `(setf (production-pg-c ,production)
         (- (* (production-p ,production) *g*)
            (production-c ,production))))

(defun all-pg-c ()
  "Recomputes the pg-c of all productions."
  (dolist (production *procedural-memory*)
    (let ((p (cdr production)))
      (pg-c p))))

(defun recompute-production-parameters (production)
  "Recomputes all parameters for production."
  (when *parameters-learning*
    (compute-p production)
    (compute-c production))
  (pg-c production))

(defun get-pg-c (production)
  "Returns the current pg-c of a production, which means the value of slot pg-c,
   unless *parameters-learning* is a number, in case the decaying value is computed."
  (when (numberp *parameters-learning*)
    (recompute-production-parameters production))
  (production-pg-c production))

(defun learn-parameters (success failure &key (latency 0.0) (instantiation *instantiation*))
  "Applies the parameters learning equations to instantiation.  The success and/or
   failure of the goal has already been determined and is passed along with the latency
   of this instantiation and the instantiation itself (if it exists)."
  (let ((production (when instantiation (instantiation-production instantiation))))
;    (when (and (not success) (not failure) instantiation) ;; see if goal changes: success
;      (let ((goal-index (car (rassoc '*wmfocus* (production-initializations production)))))
;        (when (and goal-index  ;; not always mention of the goal in the production
;                   (not (equal *wmfocus* (instantiation-variable instantiation goal-index))))
;          (setf success t))))  ;;; success by default upon goal changes
    (cond ((and success failure)  ;; clear the history
           (setf *previous-instantiations* nil))
          ((or success failure)  ;; register success or failure for all productions
           (when production
             (push (cons production (- *time* latency)) *previous-instantiations*))
           (dolist (production-time *previous-instantiations*)
             (let* ((previous-production (car production-time))
                    (previous-time (cdr production-time))
                    (effort (- *time* previous-time)))
               (add-reference (if success (production-successes previous-production)
                                  (production-failures previous-production))
                              (not (numberp *parameters-learning*))
                              1.0 previous-time)
               (add-reference (production-efforts previous-production)
                              (not (numberp *parameters-learning*))
                              effort effort)
               (recompute-production-parameters previous-production)))
           (setf *previous-instantiations* nil))
          (t  ;; simply add the production
           (when production
             (push (cons production (- *time* latency)) *previous-instantiations*))))))

(defun strength (production)
  "Returns the production strength."
  (when (and *strength-learning*
           (< (production-time-stamp production) *time*))
    (setf (production-strength production)
          (compute-references (car (production-references production))
                              (cdr (production-references production))
                              (production-creation-time production) (- *strength-learning*)))
    (setf (production-time-stamp production) *time*)
    (signal-output *activation-trace* "Computing a production strength of ~6,3F from ~D references from creation time ~6,3F"
                   (production-strength production)
                   (round (first (production-references production)))
                   (production-creation-time production)))
  (production-strength production))

;;; Default modular buffer functions

(defun get-buffer-content (arguments)
  "Argument is a list containing the name of the buffer and its global variable."
  (let* ((name (pop arguments))
         (buffer (pop arguments))
         (chunk (symbol-value buffer)))
    (signal-output *exact-matching-trace* "BUFFER ~A holds CHUNK ~A." name chunk)    
    (symbol-value buffer)))

(defun clear-buffer (arguments)
  "Argument is a list containing the name of the buffer and its global variable."
  (let ((name (pop arguments))
        (buffer (pop arguments)))
    (signal-output *dm-trace* "Clearing BUFFER ~A." name)    
    (setf (symbol-value buffer) nil)))

(defun create-buffer-chunk (arguments)
  "Arguments are a list containing the name of the buffer, the global variable
   holding the buffer contents, then the chunk description with isa and slots.
   THIS LAST ARGUMENT MUST BE FUNCALLED TO RESOLVE THE VALUE OF PRODUCTION VARIABLES."
  (let* ((name (pop arguments))
         (buffer (pop arguments))
         (chunk (new-name-fct name))
         (specs (funcall (pop arguments))))
    (signal-output *dm-trace* "Creating CHUNK ~A in BUFFER ~A with specs ~S."
                   chunk name specs)
    (add-dm-fct (list (cons chunk specs)))
    (setf (symbol-value buffer) (get-wme chunk))))

(defun modify-buffer-chunk (arguments)
  "Arguments are a list containing the name of the buffer, the global variable
   holding the buffer contents, then the chunk description with slots but no isa.
   THIS LAST ARGUMENT MUST BE FUNCALLED TO RESOLVE THE VALUE OF PRODUCTION VARIABLES."
  (let* ((name (pop arguments))
         (buffer (pop arguments))
         (chunk (symbol-value buffer))
         (specs (funcall (pop arguments))))
    (signal-output *dm-trace* "Modifying CHUNK ~A in BUFFER ~A with specs ~S."
                   chunk name specs)
    (mod-chunk-fct chunk specs)))

(defun define-buffer-fct (buffer-name variable-name &key
                                      (equal-lhs 'get-buffer-content)
                                      (plus-rhs 'create-buffer-chunk)
                                      (equal-rhs 'modify-buffer-chunk)
                                      (minus-rhs 'clear-buffer))
  (push-last (list buffer-name variable-name
                   (make-buffer-calls :equal-lhs equal-lhs :plus-rhs plus-rhs 
                                      :equal-rhs equal-rhs :minus-rhs minus-rhs))
             *buffers*)
  )

;;; Production Parsing and Compiling

(defmacro sublist (list index)
  "Makes the sublist from 0 to index the first element of list,
   with the sublist from index to the end being the rest.  Destructively.
   Needs a special handling when index is zero."
  `(if (zerop ,index)
     (rplaca ,list (list (first ,list)))
     (let ((previous (nthcdr ,index ,list)))
       (rplaca ,list (cons (first ,list) (rest ,list)))
       (rplacd ,list (cdr previous))
       (when previous (rplacd previous nil)))))
  
(defmacro unflatten (expr index &key (subproc nil))
  "Makes a list of sublists of expr by index.
   Every sublist is processed by subproc is present."
  `(let ((list ,expr))
     (loop
       (unless list (return ,expr))
       (sublist list (,index list))
       ,@(when subproc (list `(,subproc (first list))))
       (setf list (rest list)))))

(defun sub-eval-vars (expr bindings)
  "Substitute stack reference for production variables in evaled expressions.
   Return a function to be funcalled rather than a form to be evaled, which for all
   but the simplest ones causes the compiler to be called every time, unless
   *compile-eval-calls* has been turned off."
  (let ((new-expr (list (copy-tree expr))))
    (dolist (binding bindings (if *compile-eval-calls*
                                (compile nil `(lambda () ,(first new-expr)))
                                (eval `(function (lambda () ,(first new-expr))))))
      (nsubst `(get-wme-name (svref *instantiation* ,(variable-index binding)))
              (variable-name binding) new-expr :test #'eq))))

(defmacro process-clause (clause)
  `(when (retrievalp (first ,clause))
     (unflatten (rest ,clause) next-slot)))

(defun structure-production (side)
  "Structures the definition of a production side."
  (unflatten side next-delimiter :subproc process-clause))

(defun parse-production (definition)
  "Parses a production on the basic format of name-lhs-==>-rhs,
   where lhs and rhs are lists of retrievals and commands.
   Each retrieval is assembled into the wme name, and a list
   of attribute-value pairs.  Values can be negative or !eval!.
   Returns the name, the goal type, and the lhs and rhs."
  ; basic parsing; also copies the definition
  (let ((delimiter (position-or-last definition separatorp))
        (documentation (when (stringp (second definition)) (second definition))))
    (values (first definition)
;            (if documentation (fifth definition) (fourth definition))
            (structure-production (subseq definition (if documentation 2 1) delimiter))
            (if (= delimiter (length definition))
              (signal-warn "NO SEPARATOR ==> DEFINED IN PRODUCTION ~S"
                           (first definition))
              (structure-production (subseq definition (+ delimiter 1))))
            documentation)))

(defmacro set-variable-type (production type key pair)
  "Sets the type of variable key, or prints warning message if non-existent.
   First assigns type because usually given by pop.
   Also replaces type name with structure in text for better printing."
  `(let ((type-info ,pair))
     (if (eq 'isa (first type-info))
       (let ((type-structure (get-safe-type (second type-info) ,production)))
         (setf ,type type-structure)
         (when type-structure
           (setf (second type-info) type-structure)))
       (signal-warn "NO TYPE DEFINED FOR VARIABLE ~S IN ~A." ,key ,production))))

(defmacro compile-lhs-slots (bound-list unbound-list)
  "Compile slot-value pairs into a bound list and unbound list.
   Also update the text to replace constants with actual chunks for printing."
  `(dolist (slot-value clause)
     (setf negation (if (test-modifier (first slot-value)) (pop slot-value) nil))
     (setf slot (first slot-value))
     (setf slot (get-safe-slot slot type production))
     (setf value (second slot-value))
     (cond ((null slot))
           ((and (listp value) (eq '!eval! (first value)))
            (push-last (make-action :name (slot-name slot) :slot (slot-index slot)
                                    :value (sub-eval-vars (second value) bindings)
                                    :dispatch :eval :negation negation)
                       ,bound-list))
           ((and (not (variablep value)) (not (assoc value *buffer-keywords*)))
            (let ((wme-or-constant (get-wme-or-constant value)))
              (push-last (make-action :name (slot-name slot) :slot (slot-index slot)
                                      :value wme-or-constant
                                      :dispatch :literal :negation negation)
                         ,bound-list)
              (setf (second slot-value) wme-or-constant)))
           (t
            (with-binding value-binding bound value bindings index
              (cond (bound
                     (push-last
                      (make-action :name (slot-name slot) :slot (slot-index slot)
                                   :value (variable-index value-binding)
                                   :dispatch :stack :negation negation)
                      ,bound-list))
                    (t
                     (when negation
                       (signal-warn "TEST MODIFIER ~S CANNOT BE USED TO SET ~S IN SLOT ~S IN PRODUCTION ~A: IGNORED." 
                                    negation value slot production))
                     (setf (variable-slot value-binding) (slot-index slot))
                     (push-last
                      (make-action :name (slot-name slot) :slot (slot-index slot)
                                   :value (variable-index value-binding))
                      ,unbound-list))))))))

(defun first-retrieval-index (lhs initializations)
  "Returns the index of the first retrieval in lhs."
  (let ((index 0)
        (form lhs))
    (loop
      (unless form
        (return (length lhs)))
      (if (and (member (first form) '(direct-test-and-bind indirect-test-and-bind
                                      direct-test-and-bind-pm indirect-test-and-bind-pm)
                       :test #'eq)
               (not (assoc (second form) initializations :test #'=)))
        (return index)
        (incf index 1))
      (setf form (rest form)))))

(defun leaves (tree &optional (list nil))
  "Return a list of the leaves of tree in order."
  (if (and tree (listp tree))
    (dolist (branch tree list)
      (setf list (leaves branch list)))
    (setf list (push-last tree list))))

(defun compile-lhs (production lhs initializations bindings index)
  "Compiles the left-hand side of production.
   Add the instantiation-adding call."
  (let ((code nil)
        (form nil))
    (dolist (clause lhs)
      (let ((key (pop clause))
            (type nil)
            (tests nil)
            (binds nil)
            (slot nil)
            (value nil)
            (negation nil))
        (setf form
              (cond
               ((retrievalp key)
                (setf key (var>var key))
                (with-binding binding direct key bindings index
                  (set-variable-type production type key (pop clause))
                  (when type
                    (setf (variable-type binding) type)
                    (compile-lhs-slots tests binds)
                    (if (assoc (var=var key) *buffers*)   ;;; lhs buffer matching
                      (let ((buffer (assoc (var=var key) *buffers*)))
                        (list 'buffer-test-and-bind
                              (variable-index binding)
                              (list (buffer-calls-equal-lhs (third buffer))
                                    (list (first buffer) (second buffer)))
                              type tests binds))
                      (list (if (assoc (variable-index binding) initializations :test #'=)
                              ;; Fixed buffers have their special test to avoid activation
                              'direct-test-and-bind-buffer
                              (if *enable-rational-analysis*
                                (if direct 'direct-test-and-bind-pm
                                    'indirect-test-and-bind-pm)
                                (if direct 'direct-test-and-bind
                                    'indirect-test-and-bind)))
                            (variable-index binding) type tests binds)))))
               ((eq '!eval! key)
                (list 'eval-test (sub-eval-vars (first clause) bindings)))
               ((eq '!bind! key)
                (with-binding binding bound (first clause) bindings index
                  (when bound
                    (signal-warn "VARIABLE ~S ALREADY BOUND IN PRODUCTION ~A."
                                 (first clause) production))
                  (list 'bind-lhs (variable-index binding)
                        (sub-eval-vars (second clause) bindings))))
               ((eq '!find-location! key)
                (with-binding binding bound (first clause) bindings index
                  (when bound
                    (signal-warn "VARIABLE ~S ALREADY BOUND IN PRODUCTION ~A."
                                 (first clause) production))
                  (list 'bind-find-location (variable-index binding)
                        (sub-eval-vars (cons 'list (quote-arguments (rest clause)))
                                       bindings))))
               (t (signal-warn "UNKNOWN COMMAND ~S IN PRODUCTION ~A."
                               key production))))
        (setf code (nconc code form))))
    ;; FIX: split into goal and retrievals when ERA is enabled.
    (values (if *enable-rational-analysis*
              (let ((direct-length (first-retrieval-index code initializations)))
                (cons (nconc (subseq code 0 direct-length) (list 'add-instantiation-to-conflict-set))
                      (nconc (subseq code direct-length) (list 'not))))
              (nconc code (list 'add-instantiation-to-conflict-set)))
            bindings index)))

(defmacro compile-rhs-slots (bound-list unbound-list parent)
  "Compile slot-value pairs into a bound list and unbound (returns) list.
   Parent is used to know which goal this stack slot belongs to.
   Also update the text to replace constants with actual chunks for printing."  
  `(dolist (slot-value clause)
     (setf negation (if (test-modifier (first slot-value)) (pop slot-value) nil))
     (setf slot (first slot-value))
     (setf slot (get-safe-slot slot type production))
     (setf value (second slot-value))
     (cond ((null slot))
           ((and (listp value) (eq '!eval! (first value)))
            (push-last (make-action :name (slot-name slot) :slot (slot-index slot)
                                    :value (sub-eval-vars (second value) bindings)
                                    :dispatch :eval :negation negation)
                       ,bound-list))
           ((and (not (variablep value)) (not (get-variable-binding value bindings)))
            (let ((wme-or-constant (get-wme-or-constant value)))
              (push-last (make-action :name (slot-name slot) :slot (slot-index slot)
                                      :value wme-or-constant
                                      :dispatch :literal :negation negation)
                         ,bound-list)
              (setf (second slot-value) wme-or-constant)))             
           (t
            (with-binding value-binding bound value bindings index
              (cond (bound
                     (if (numberp (variable-index value-binding))
                       ; if a normal variable, then push the action
                       (push-last
                        (make-action :name (slot-name slot) :slot (slot-index slot)
                                     :value (variable-index value-binding)
                                     :dispatch :stack :negation negation)
                        ,bound-list)
                       ; otherwise, add the reference to the return stack
                       (let ((variable-references
                              (get-variable-binding value ,unbound-list)))
                         (push-last (cons (variable-name ,parent) (slot-index slot))
                                    (variable-returns variable-references)))))
                    (t
                     (when negation
                       (signal-warn "TEST MODIFIER ~S CANNOT BE USED TO SET ~S IN SLOT ~S IN PRODUCTION ~A: IGNORED." 
                                    negation value slot production))
                     ; Return values do not need instantiation representation
                     ; stack index is the parent's name
                     (decf index)
                     (setf (variable-index value-binding) ,parent)
                     (setf (variable-slot value-binding) (slot-index slot))
                     (push-last value-binding ,unbound-list))))))))

(defmacro subgoal-returns (subgoal returns)
  "Select the returns from slots of subgoal."
  `(let ((selected nil))
     (dolist (return ,returns selected)
       (when (eq (variable-name (variable-index return)) ,subgoal)
         (push (variable-slot-and-returns return)
               selected)))))

(defun interpret-output-argument (argument bindings production)
  "Interprets a production variable as its stack index,
   and returns more complex arguments as functions."
  (if (variablep argument)
    (variable-index (get-safe-variable-binding argument bindings production))
    (sub-eval-vars argument bindings)))

(defun compile-output (clause bindings production)
  "Compiles an output clause for a production given bindings."
  (let ((output-string "~&~VT")
        (output-args nil))
    (when (and (listp (first clause)) (null (rest clause)))
      (setf clause (first clause)))
    (cond ((stringp (first clause))
           (setf output-string (concatenate 'string output-string (first clause)))
           (dolist (arg (rest clause))
             (push-last (interpret-output-argument arg bindings production)
                        output-args)))
          (t
           (dolist (arg clause)
             (cond ((or (variablep arg) (listp arg))
                    (setf output-string (concatenate 'string output-string "~S "))
                    (push-last (interpret-output-argument arg bindings production)
                               output-args))
                   (t
                    (setf output-string
                          (concatenate 'string output-string (coerce-string arg) " ")))))))
    (cons output-string output-args)))

(defmacro simulate-call (arguments)
  "Simulate production compilation by funcalling the function with the rest
   of the arguments."
  `(funcall (first ,arguments) (rest ,arguments)))

(defmacro index-or-chunk (reference)
  "Translates reference into either a constant chunk or an instantiation variable."
  `(if (wmep ,reference) ,reference (instantiation-variable *instantiation* ,reference)))

(defun assign-retrieval (arguments)
  "Assigns to the variable *retrieval-scheduler* the result and latency of the retrieval.
   The variable *retrieval* is also reset to nil waiting for the result."
  (let* ((index (first arguments))
         (retrieval (if index
                     (index-or-chunk index)
                     *retrieval-scheduler*)))
    (signal-output *latency-trace* "Latency ~6,3F: ~A Retrieval" *latency* retrieval)
    (setf *retrieval* nil)
    (setf *retrieval-scheduler* (cons (+ *time* *latency*) retrieval))))

(defun handle-failure (arguments)
  "Calls whatever test-and-bind function is given as arguments.  If it fails,
   clean the bindings and set them as well as retrieval to the chunk failure.
   Failure is defined as being of the default type error with itself as condition."
  (unless (simulate-call arguments) ; if matching fails set the bindings to failure
    (let ((failure (get-wme 'failure)))
      (unless failure ; if failure isn't yet defined then add it
        (add-dm-fct '((failure isa error condition failure)) :reset-ia nil)
        (setf failure (get-wme 'failure)))
      (dolist (action (fifth arguments))
        (setf (instantiation-variable *instantiation* (action-value action))
              failure))
      (when (and (second arguments)
                 (member (first arguments) '(indirect-test-and-bind
                                             indirect-test-and-bind-pm)))
        (setf (instantiation-variable *instantiation* (second arguments))
              failure))
      
      (signal-output *latency-trace* "Latency ~6,3F: Failure Retrieval" *latency*)
      (setf *retrieval* nil)
      (setf *retrieval-scheduler* (cons (+ *time* *latency*) failure)))))

(defun assign-buffer (buffer-index)
  "Assigns value of instantiation variable index or function to buffer."
  (buffers-fct (list (first buffer-index)
                     (let ((value (second buffer-index)))
                       (when value
                         (if (wmep value) value
                             (if (numberp value)
                               (instantiation-variable *instantiation* value)
                               (get-wme-or-constant (funcall value)))))))))

(defun compile-rhs (production rhs bindings index)
  "Compiles the right-hand side of production."
  (let ((code nil)
        (returns nil)
        (goal-stack (list (variable-name (first bindings)))))        
    (dolist (clause rhs)
      (let ((key (pop clause))
            (type nil)
            (assigns nil)
            (slot nil)
            (value nil)
            (negation nil))
        (setf code (nconc code
         (cond ((retrievalp key)
                (setf key (var>var key))
                (when (actionp key) (setf key (var=var key))) ;; strip out the +
                (cond ((clearp key)  ;; clearing buffers 
                       (setf key (var=var key))  ;; strip out the -
                       (if (assoc key *buffer-keywords*)  ;; clearing ACT-R buffers
                         (list (list 'assign-buffer key nil))
                         (if (assoc key *buffers*)   ;;; rhs buffer clearing
                           (let ((buffer (assoc key *buffers*)))
                             (list (list (buffer-calls-minus-rhs (third buffer))
                                         (first buffer) (second buffer))))
                           (signal-warn "UNKNOWN BUFFER ~S CANNOT CLEAR." key))))
                      ((and (assoc key *buffer-keywords*) (null (rest clause))
                            (null (rest (first clause))) (listp (first (first clause))))
                       ;; assignment to buffer of result of function call
                       (list (list 'assign-buffer key
                                   (sub-eval-vars (first (first clause)) bindings))))
                      ((and (not (variablep key)) (assoc key *buffer-keywords*)
                            (null (rest clause)) (null (rest (first clause))))
                       ;; direct assignment to buffers, including goal focus and direct retrieval
                       (let ((binding (if (variablep (first (first clause)))
                                        (variable-index (get-variable-binding (first (first clause))
                                                                              bindings))
                                        (get-wme-or-constant (first (first clause))))))
                         (cond ((eq key 'goal)  ;; focus on goal
                                (setf goal-stack (butlast goal-stack))
                                (push-last key goal-stack)
                                (list (list 'focus-fct 
                                            binding
                                            (subgoal-returns key returns))))
                               ((eq key 'retrieval)   ;; direct retrieval
                                (list (list 'handle-failure
                                            (if *enable-rational-analysis*
                                              'direct-test-and-bind-pm
                                              'direct-test-and-bind)
                                            binding
                                            type nil nil
                                            'assign-retrieval
                                            binding)))
                               (t  ;; other buffer assignment
                                (list (list 'assign-buffer key binding))))))
                      (t
                       (with-binding binding modify key bindings index
                         (cond (modify
                                (setf type (variable-type binding))
                                (if (and type (not (integerp type)))
                                  (when (eq 'isa (caar clause))
                                    (set-variable-type production type key (pop clause))
                                    (unless (or (eq type (variable-type binding))
                                                (assoc key *buffer-keywords*))
                                      (signal-warn "TYPE OF VARIABLE ~S IS BEING REDEFINED IN PRODUCTION ~A."
                                                   key production)))
                                  (set-variable-type production type key (pop clause))))
                               (t
                                (set-variable-type production type key (pop clause))))
                         (when type
                           (cond ((or (eq 'retrieval key)
                                      (member (list '!retrieve! key) rhs :test #'equal))
                                  (let ((tests nil)
                                        (binds nil))
                                    (compile-lhs-slots tests binds)
                                    (list (list 'handle-failure
                                                (if *enable-rational-analysis*
                                                  (if modify
                                                    'direct-test-and-bind-pm
                                                    'indirect-test-and-bind-pm)
                                                  (if modify
                                                    'direct-test-and-bind
                                                    'indirect-test-and-bind))
                                                nil
                                                type tests binds
                                                'assign-retrieval
                                                nil))))
                                 ((assoc key *buffers*)  ;;; rhs buffer action
                                  (let ((buffer (assoc key *buffers*))
                                        (arguments (sub-eval-vars (cons 'list (quote-arguments (cons 'isa (cons type (leaves clause)))))
                                                                  bindings)))
                                    (list (list (buffer-calls-plus-rhs (third buffer))
                                                (first buffer) (second buffer)
                                                arguments))))
                                 ((assoc (var=var key) *buffers*)   ;;; rhs buffer modification
                                  (let ((buffer (assoc (var=var key) *buffers*))
                                        (arguments (sub-eval-vars (cons 'list (quote-arguments (leaves clause)))
                                                                  bindings)))
                                    (list (list (buffer-calls-equal-rhs (third buffer))
                                                (first buffer) (second buffer) arguments))))
                                 (t
                                  (compile-rhs-slots assigns returns binding)
                                  (cons
                                   (if modify
                                     (list 'modify-old-wme (variable-index binding)
                                           type assigns)
                                     (list 'create-new-wme (variable-index binding)
                                           (if (variablep key) (var=var key) key) type assigns))
                                   (when (and (or (eq '=newgoal key) (eq 'newgoal key) (eq 'goal key))
                                              (not (member (list '!push! key) rhs :test #'equal))
                                              (not (member (list '!focus-on! key) rhs :test #'equal)))
                                     (setf goal-stack (butlast goal-stack))
                                     (push-last key goal-stack)
                                     (list (list 'focus-fct 
                                                 (variable-index binding)
                                                 (subgoal-returns key returns))))))))))))
               ((eq '!retrieve! key) ;; do nothing since retrieval already done
                nil)
               ((eq '!push! key)
                (let* ((subgoal (first clause))
                       (binding (get-safe-variable-binding subgoal bindings production)))
                  (when binding
                    (push-last subgoal goal-stack)
                    (list (list 'push-fct
                                (variable-index binding)
                                (subgoal-returns subgoal returns))))))
               ((eq '!pop! key)
                ;;; FIX: pop from the end of the stack
                (setf goal-stack (butlast goal-stack))
                (list (list 'pop-fct)))
               ((eq '!focus-on! key)
                (let* ((subgoal (first clause))
                       (binding (get-safe-variable-binding subgoal bindings production)))
                  (when binding
                    ;;; FIX: do not assume that there is something on the stack
                    (setf goal-stack (butlast goal-stack))
                    (push-last subgoal goal-stack)
                    (list (list 'focus-fct 
                                (variable-index binding)
                                (subgoal-returns subgoal returns))))))
               ((eq '!output! key)
                (list (list 'output (compile-output clause bindings production))))
               ((eq '!eval! key)
                (list (list 'eval-side (sub-eval-vars (first clause) bindings))))
               ((eq '!bind! key)
                (with-binding binding bound (first clause) bindings index
                  (when bound
                    (signal-warn "VARIABLE ~S ALREADY BOUND IN PRODUCTION ~A."
                                 (first clause) production))
                  (list (list 'bind-rhs (variable-index binding)
                              (sub-eval-vars (second clause) bindings)))))
               ((eq '!move-attention! key)
                (with-binding binding bound (first clause) bindings index
                  (when bound
                    (signal-warn "VARIABLE ~S ALREADY BOUND IN PRODUCTION ~A."
                                 (first clause) production))
                  (list (list 'bind-move-attention (variable-index binding)
                              (sub-eval-vars (cons 'list (quote-arguments (rest clause)))
                                             bindings)))))
               ((eq '!press-key! key)
                (list (list 'visual-action 'press-key
                            (sub-eval-vars (cons 'list (quote-arguments clause))
                                           bindings))))
               ((eq '!move-mouse! key)
                (list (list 'visual-action 'move-mouse
                            (sub-eval-vars (cons 'list (quote-arguments clause))
                                           bindings))))
               ((eq '!click-mouse! key)
                (list (list 'visual-action 'click-mouse
                            (sub-eval-vars (cons 'list (quote-arguments clause))
                                           bindings))))
               ((eq '!send-command! key)
                (list (list 'action-command
                            (sub-eval-vars (cons 'list (quote-arguments clause))
                                           bindings))))
               ((eq '!delete! key)
                (let ((binding (get-safe-variable-binding (first clause) bindings production)))
                  (when binding
                    (list (list 'delete-wme-variable (variable-index binding))))))
               ((or (eq '!copy! key) (eq '!copywme! key))
                (with-binding binding bound (first clause) bindings index
                  (when bound
                    (signal-warn "VARIABLE ~S ALREADY BOUND IN PRODUCTION ~A."
                                 (first clause) production))
                  (list (list 'copy-chunk-variable (variable-index binding)
                              (sub-eval-vars (cons 'list (rest clause))
                                             bindings)))))
               ((eq '!stop! key)
                (list (list 'stop)))
               ((eq '!restart! key)
                (list (list 'restart-top-goal)))
               (t (signal-warn "UNKNOWN COMMAND ~S IN PRODUCTION ~A." key production)))))))
    ; move retrieval clauses at the end to allow activation spreading
    (dolist (clause code)
      (when (eq (first clause) 'handle-failure)
        (setf code (nconc (delete clause code :test #'eq) (list clause)))))
    ; remove the unknown commands
    ; (setf code (delete nil code :test #'eq))
    ; translate wme variables into relative goal stack indices for value returns
    (dolist (return returns)
      (let ((from-position
             (position (variable-name (variable-index return))
                       goal-stack :test #'eq)))
        (if from-position
          (dolist (destination (variable-returns return))
            (let ((to-position (position (first destination) goal-stack :test #'eq)))
              (if (and (numberp to-position) (> from-position to-position))
                (rplaca destination (- from-position to-position 1))
                (signal-warn "VARIABLE ~A CANNOT BE RETURNED BECAUSE CHUNK ~A IS NOT ON THE STACK BELOW SUBGOAL ~A IN PRODUCTION ~A."
                       (variable-name return) (first destination)
                       (variable-name (variable-index return)) production))))
          (signal-warn "VARIABLE ~A CANNOT BE RETURNED BECAUSE CHUNK ~A IS NOT ON THE STACK IN PRODUCTION ~A."
                       (variable-name return)
                       (variable-name (variable-index return)) production))))
    (values code bindings index)))

(defun compile-buffers (production lhs bindings index)
  "Compiles the left-hand side references to pre-defined buffers such as goal and retrieval."
  (declare (ignore production))
  (let ((initializations nil)
        (type t))
    (dolist (clause lhs)
      (let ((key (first clause)))
        (when (retrievalp key)
          (setf key (var>var key))
          (let ((buffer (assoc key *buffer-keywords*)))
            ;;; sets the type of the goal
            (when (or (eq key '=goal) (eq key 'goal))
              (setf type (second (assoc 'isa (rest clause)))))
            (when buffer
              ;;; When the retrieval refers to a buffer, create a new binding
              (with-binding binding existing key bindings index
                (unless existing
                  (push-last (cons index (rest buffer))
                             initializations))))))))
;    (signal-output *command-trace* "The special buffers in production ~S are ~S"
;                   production initializations)
    (values type initializations bindings index)))

(defun compile-production (name lhs rhs &optional (documentation nil))
  "Compile production name, type, lhs, rhs and optional documentation."
  (let ((production nil)
        (type nil)
        (initializations nil)
        (bindings nil)
        (retrievals nil)
        (lhs-code nil)
        (rhs-code nil)
        (stack-index (1- *instantiation-slots*)))
    (setf production (get-production name))
    (multiple-value-setq (type initializations bindings stack-index)
      (compile-buffers name lhs bindings stack-index))
    (setf type (get-safe-type type name))
    (when type
      (cond (production
             (signal-warn "PRODUCTION ~S IS BEING REDEFINED." name)
             (delete-production production))
            (t
             (setf production (make-production :name name))
             (save-state-change :compile-production production)))
      (push-last (cons name production) *procedural-memory*)
      (setf (production-documentation production) documentation)
      (setf (production-goal-type production) type)
      (push-last production (wme-type-productions type))
      (setf (production-text production) (cons lhs rhs))
;      (setf bindings (list (make-variable-binding (var>var (first (first lhs)))
;                                                  stack-index)))
      (setf (production-initializations production) initializations)
      (multiple-value-setq (lhs-code bindings stack-index)
        (compile-lhs name lhs initializations bindings stack-index))
      (setf (production-lhs production) lhs-code)
      (multiple-value-setq (rhs-code bindings stack-index)
        (compile-rhs name rhs bindings stack-index))
      (setf (production-rhs production) rhs-code)
      (setf (production-bindings production) bindings)
      (dolist (binding bindings)
        (when (and (not (equal (variable-name binding) '=goal))
                   (typep (variable-type binding) 'wme-type))
          (push-last (variable-index binding) retrievals)))
      (setf (production-retrievals production) retrievals)
      (setf (production-size production) (+ stack-index 1))
      (setf (production-instantiation production) (make-instantiation production))
      ;; FIX: no more setting of extra-instantiation
      (pg-c production)
      production)))

(defun p-fct (definition)
  "Defines a production by parsing then compiling it."
  (multiple-value-bind (name lhs rhs documentation)
                       (parse-production definition)
    (let ((production (compile-production name lhs rhs documentation)))
      (when production (production-name production)))))

(defun penable-fct (names)
  "Enables disabled productions."
  (let ((production nil))
    (save-state-change :penable-fct names)
    (dolist (name names (mapcar #'car *failed-productions*))
      (setf production (get-safe-production name *failed-productions*))
      (when production
        (setf *failed-productions* (delete (production-name production) *failed-productions*
                                           :test #'eq :key #'car :count 1))
        (push-last (cons (production-name production) production) *procedural-memory*)
        (push-last production (wme-type-productions (production-goal-type production)))
        (pg-c production)))))

(defun pdisable-fct (names)
  "Disables productions."
  (let ((production nil))
    (save-state-change :pdisable-fct names)
    (dolist (name names (mapcar #'car *failed-productions*))
      (setf production (get-safe-production name))
      (when production
        (delete-production production)
        (push-last (cons (production-name production) production)
                   *failed-productions*)))))

(defun pbreak-fct (names)
  "Sets break points for productions."
  (let ((production nil))
    (dolist (name names *break-productions*)
      (setf production (get-safe-production name))
      (when production
        (push-last (production-name production) *break-productions*)))))

(defun punbreak-fct (names)
  "Removes break points for productions."
  (let ((production nil))
    (dolist (name (or names *break-productions*) *break-productions*)
      (setf production (get-safe-production name))
      (when production
        (setf *break-productions* (delete (production-name production)
                                          *break-productions*
                                          :test #'eq :count 1))))))

(defmacro print-side (side)
  "Prints one side of a production, i.e. a list of commands and wme retrievals."
  `(dolist (clause ,side)
     (cond ((retrievalp (first clause))
            (signal-output trace "   ~S" (first clause) )
            (dolist (slot-value (rest clause))
              (if (null (rest slot-value))
                (signal-output trace "      ~S" (first slot-value))
                (signal-output trace "    ~A ~S ~S"
                               (if (test-modifier (first slot-value)) (pop slot-value) " ")
                               (first slot-value)
                               (second slot-value)))))
           (t
            (signal-output trace "  ~{ ~S~}" clause)))))


(defun pprint-production (production &optional (trace *command-trace*)
                                     (text (production-text production))
                                     (documentation (production-documentation production)))
  "Pretty prints production.  Pretty basic."
  (signal-output trace "(p ~S" production)
  (when documentation
    (signal-output trace "   ~S" documentation))
  (print-side (car text))
  (signal-output trace "==>")
  (print-side (cdr text))
  (signal-output trace ")")
  (production-name production))

(defun pp-fct (productions)
  "Prints the list of productions, or all the active ones if none specified."
  (let ((*verbose* t)
        (*goal-depth* 1)
        (structures nil))
    (if productions
      (dolist (production productions)
        (setf production
              (or (get-production production)
                  (get-safe-production production *failed-productions*)))
        (when production
          (push-last (pprint-production production) structures)))
      (dolist (production *procedural-memory*)
        (setf production (cdr production))
        (push-last (pprint-production production) structures)))
    structures))


;;; Production parameters setting and printing

(defun displayable-production-parameters ()
  "Returns the list of displayable production parameters given
   the current values of global parameters.  Used by the interface."
  (nconc
   (list :chance :effort)
   (if *enable-rational-analysis*
     (nconc
      (list :strength)
      (when (or *strength-learning*
                (numberp *parameters-learning*))
        (list :creation-time))
      (when *strength-learning*
        (list :references))
      (list :q :a :r :b :pg-c)
      (when *parameters-learning*
        (list :successes :failures :efforts
              :eventual-successes :eventual-failures :eventual-efforts
              :success :failure)))
     (list :value))))

(defun settable-production-parameters ()
  "Returns the list of settable production parameters given
   the current values of global parameters.  Used by the interface."
  (nconc
   (list :chance :effort)
   (if *enable-rational-analysis*
     (nconc
      (when (or *strength-learning*
                (numberp *parameters-learning*))
        (list :creation-time))
      (if *strength-learning*
        (list :references)
        (list :strength))
      (if *parameters-learning*
        (list :successes :failures :efforts
              :eventual-successes :eventual-failures :eventual-efforts
              :success :failure)      
        (list :q :a :r :b)))
     (list :value))))

(defun default-production-parameters ()
  "Returns the production parameters with their default values.
   Used by the interface."
  (nconc
   (list (cons :chance 1.0) (cons :effort *default-action-time*))
   (if *enable-rational-analysis*
     (nconc
      (when (or *strength-learning*
                (numberp *parameters-learning*))
        (list (cons :creation-time *time*)))
      (if *strength-learning*
        (list (cons :references (cons 1.0 (when (and *strength-learning* (not *optimized-learning*)) (list *time*)))))
        (list (cons :strength 0.0)))
      (if *parameters-learning*
        (list (cons :successes (cons 1.0 (when (numberp *parameters-learning*) *time*)))
              (cons :eventual-successes (cons 1.0 (when (numberp *parameters-learning*) *time*)))
              (cons :failures (list 0.0)) (cons :eventual-failures (list 0.0))
              (cons :efforts (cons *default-action-time* (when (numberp *parameters-learning*)
                                                            *default-action-time*)))
              (cons :eventual-efforts (cons 1.0 (when (numberp *parameters-learning*) 1.0))))
        (list (cons :q 1.0) (cons :a *default-action-time*)
              (cons :r 1.0) (cons :b 1.0))))
     (list :value))))
     
(defun displayable-declarative-parameters ()
   "Returns the list of displayable declarative parameters given
   the current values of global parameters.  Used by the interface."
   (when *enable-rational-analysis*
     (nconc
      (list :activation :source :base-level)
      (when *base-level-learning*
        (list :creation-time :references))
      (list :source-spread :ias)
      (when *associative-learning*
        (list :creation-cycle :needed :contexts))
      (when *permanent-activation-noise*
        (list :permanent-noise))
      (when *partial-matching*
        (list :similarities)))))

(defun settable-declarative-parameters ()
   "Returns the list of settable declarative parameters given
   the current values of global parameters.  Used by the interface."
   (when *enable-rational-analysis*
     (nconc
      (if *base-level-learning*
        (list :creation-time :references)
        (list :base-level))
      (when *associative-learning*
        (list :creation-cycle :needed :contexts))
      (when *permanent-activation-noise*
        (list :permanent-noise)))))

(defun production-parameter-fct (production &optional parameters)
  "Returns the value of the production parameter(s), or print all if none given."
  (let ((*verbose* t)
        (*goal-depth* 1)
        (value nil)
        (values nil))
    (setf production (get-safe-production production))
    (cond (production
           (signal-output *command-trace* "Parameters for production ~S:" production)
           ;;; updates production strength and parameters.
           (when *strength-learning*
             (strength production))
           (when (numberp *parameters-learning*)
             (recompute-production-parameters production))
           (cond (parameters
                  (dolist (parameter parameters)
                    (setf value 
                          (case parameter
                            (:name (production-name production))
                            (:strength (production-strength production))
                            (:creation-time (production-creation-time production))
                            (:references (production-references production))
                            (:p (production-p production))
                            (:c (production-c production))
                            (:pg-c (production-pg-c production))
                            (:value (production-value production))
                            (:successes (production-successes production))
                            (:failures (production-failures production))
                            (:efforts (production-efforts production))
                            (:chance (production-chance production))
                            (:effort (production-effort production))
                            (:success (production-success production))
                            (:failure (production-failure production))
                            (t (signal-warn "NO PARAMETER ~A DEFINED FOR PRODUCTIONS (TRY : IN FRONT)."
                                            parameter)
                               :error)))
                    (push-last value values)
                    (signal-output *command-trace* "~S ~6,3F" parameter value))
                  (signal-output *command-trace* "")
                  values)
                 (t
                  (signal-output *command-trace* ":Chance ~6,3F~% :Effort ~6,3F"
                                 (production-chance production) (production-effort production))
                  (when *enable-rational-analysis*
;                    (signal-output *command-trace* ":Strength ~6,3F" (strength production))
                    (when (or *strength-learning*
                              (numberp *parameters-learning*))
                      (signal-output *command-trace* ":Creation-Time ~6,3F" (production-creation-time production)))
                    (when *strength-learning*
                      (signal-output *command-trace* ":References ~6,3F" (production-references production))))
                  (if *enable-rational-analysis*
                    (signal-output *command-trace*
                                   ":P ~6,3F~% :C ~6,3F~% :PG-C ~6,3F~%"
                                   (production-p production) (production-c production)
                                   (production-pg-c production))
                    (signal-output *command-trace* ":Value ~6,3F"
                                   (production-value production)))
                  (when *parameters-learning*
                    (signal-output *command-trace*
                                   ":Successes ~6,3F~% :Failures ~6,3F~% :Efforts ~6,3F~%~
                                    ~1T:Success ~6,3F~% :Failure ~6,3F"
                                   (production-successes production) (production-failures production) (production-efforts production)
                                   (production-success production) (production-failure production)))
                  (signal-output *command-trace* "")
                  production)))
          (t :error))))

(defmacro set-parameter (slot parameter test warning &rest housekeeping)
  "Sets parameter of production p in slot if value passes test,
   otherwise issue warning."
  `(cond (,test
          (setf (,slot p) value)
          ,@housekeeping
          value)
         (t
          (signal-warn "PARAMETER ~A CANNOT TAKE VALUE ~S BECAUSE IT MUST BE ~:@(~A~)."
                         ,parameter value ,warning)
          :error)))

(defmacro pre-process-production-references (p value optimized)
  "Pre-process a specification of references into the proper format."
  `(progn
     (decf (production-time-stamp ,p) 1.0)
     ; pre-process references
     (when (listp ,value)
       (setf ,value (cons (length ,value) (copy-list ,value)))
       (let ((*optimized-learning* ,optimized))
         (adapt-references ,value (production-creation-time ,p))))
     (when (numberp ,value)
       (setf ,value (list ,value))
       (let ((*optimized-learning* ,optimized))
         (adapt-references ,value (production-creation-time ,p))))))

(defmacro pre-process-production-efforts (p value optimized references)
  "Pre-process a specification of efforts into the proper format."
  `(progn
     (decf (production-time-stamp ,p) 1.0)
     ; pre-process references
     (when (listp ,value)
       (let ((efforts 0.0))
         (dolist (effort ,value)
           (incf efforts effort))
         (setf ,value (cons efforts (copy-list ,value)))))
     (when (numberp ,value)
       (let ((ratio (round ,references)))
         (setf ,value
               (cons ,value
                     (unless ,optimized
                       (make-list ratio :initial-element (/ ,value ratio)))))))))

(defun parameters-fct (p parameters)
  "Sets the parameters of production.  Keyword arguments have been discontinued."
  (let ((values nil))
    (setf p (get-safe-production p))
    (if p
      (loop
        (unless parameters (recompute-production-parameters p) (return values))
        (let* ((parameter (pop parameters))
               (value (pop parameters)))
          (when (and (listp value) (eq (first value) 'quote))
            (setf value (second value)))  ;; for compatibility with evaluating versions
          (push-last
           (case parameter
             (:name
              (signal-warn "PARAMETER NAME CANNOT BE SET.")
              :error)
             (:strength
              (cond (*strength-learning*
                     (signal-warn "PARAMETER STRENGTH CANNOT BE SET DIRECTLY WHEN STRENGTH LEARNING IS ENABLED: SET CREATION-TIME AND REFERENCES INSTEAD.")
                     :error)
                    (t
                     (set-parameter production-strength :strength (numberp value) "a number"))))
             (:creation-time
              (decf (production-time-stamp p) 1.0)
              (set-parameter production-creation-time :creation-time
                             (numberp value) "a number"))
             (:references
              (pre-process-production-references p value *optimized-learning*)
              (set-parameter production-references :references
                             (or (numberp value) (listp value))
                             "a number or list of numbers"))
             (:value
              (unless (constantp value)
                (setf value (build-functional-parameter value (production-bindings p))))
              (set-parameter production-value :value
                             (or (numberp value) (functional-parameter-p value))
                             "a number of function"))
             (:pg-c
              (signal-warn "PRODUCTION PG-C CANNOT BE SET DIRECTLY: SET INDIVIDUAL PARAMETERS INSTEAD.")
              :error)
             (:p
              (cond (*parameters-learning*
                     (signal-warn "PARAMETER P CANNOT BE SET DIRECTLY WHEN PARAMETERS LEARNING IS ENABLED: SET SUCCESSES AND FAILURES INSTEAD.")
                     :error)
                    (t
                     (set-parameter production-p :p
                                    (and (numberp value) (>= value 0.0) (<= value 1.0))
                                    "a number between 0.0 and 1.0"))))
             (:c
              (cond (*parameters-learning*
                     (signal-warn "PARAMETER C CANNOT BE SET DIRECTLY WHEN PARAMETERS LEARNING IS ENABLED: SET SUCCESSES AND EFFORTS INSTEAD.")
                     :error)
                    (t
                     (set-parameter production-c :c (and (numberp value) (>= value 0.0))
                                    "a non-negative number"))))
             (:successes
              (pre-process-production-references p value
                                                 (not (numberp *parameters-learning*)))
              (set-parameter production-successes :successes
                             (or (numberp value) (listp value))
                             "a number or list of numbers"))
             (:failures
              (pre-process-production-references p value
                                                 (not (numberp *parameters-learning*)))
              (set-parameter production-failures :failures
                             (or (numberp value) (listp value))
                             "a number or list of numbers"))
             (:efforts
              (pre-process-production-efforts p value
                                              (not (numberp *parameters-learning*))
                                              (first (production-successes p)))
              (set-parameter production-efforts :efforts
                             (or (numberp value) (listp value))
                             "a number or list of numbers"))
             (:chance
              (unless (constantp value)
                (setf value (build-functional-parameter value (production-bindings p))))
              (set-parameter production-chance :chance
                             (or (and (numberp value) (>= value 0.0) (<= value 1.0))
                                 (null value) (functional-parameter-p value))
                             "NIL or a function or number between 0.0 and 1.0"))
             (:effort
              (unless (constantp value)
                (setf value (build-functional-parameter value (production-bindings p))))
              (set-parameter production-effort :effort
                             (or (and (numberp value) (>= value 0.0))
                                 (null value) (functional-parameter-p value))
                             "NIL or a non-negative number or function"))
             (:success
              (unless (constantp value)
                (setf value (build-functional-parameter value (production-bindings p))))
              (set-parameter production-success :success
                             (or (null value) (eq value t) (functional-parameter-p value))
                             "T or NIL or a function"))
             (:failure
              (unless (constantp value)
                (setf value (build-functional-parameter value (production-bindings p))))
              (set-parameter production-failure :failure
                             (or (null value) (eq value t) (functional-parameter-p value))
                             "T or NIL or a function"))
             (t
              (signal-warn "NO PARAMETER ~A DEFINED FOR PRODUCTIONS." parameter)
              :error))
           values)))
      :error)))

(defun spp-fct (parameters)
  "Inspects and sets production parameters."
  (let ((results nil))
    (if (null parameters) ; print all parameters for all productions
      (dolist (production *procedural-memory*)
        (push-last (production-parameter-fct (cdr production)) results))
      (dolist (description (if (or (keywordp (first parameters))
                                   (keywordp (second parameters))
                                   (and (listp (first parameters))
                                        (null (second parameters))
                                        (not (keywordp (second (first parameters))))))
                             (list parameters) parameters))
        (when (atom description) (setf description (list description)))
        (if (keywordp (first description))
          (dolist (production *procedural-memory*)
            (push-last
             (if (and (cdr description) (not (keywordp (second description))))
               (parameters-fct (cdr production) description)
               (production-parameter-fct (cdr production) description))
             results))
          (dolist (production (if (atom (first description))
                                (list (first description))
                                (first description)))
            (push-last
             (if (and (cddr description) (not (keywordp (third description))))
               (parameters-fct production (rest description))
               (production-parameter-fct production (rest description)))
             results)))))
    results))


;;; Run Time

(defmacro subtype (sub type)
  "Tests whether sub is a subtype of supertype.  Actually, the other way
   around since the listof supertypes is likely to be shorter."
  `(member ,type (wme-type-supertypes ,sub)))

(defun match-lhs (production)
  "Matches the lhs of a production."
  (let ((gain 0.0))
    (when *enable-rational-analysis*
      (setf gain (get-pg-c production))
      (when *exp-gain-noise*
        (incf gain (noise *exp-gain-noise*))))
    (if (and *utility-threshold* (< gain *utility-threshold*))
      (signal-output (or *exact-matching-trace* *partial-matching-trace*)
                     "Production ~S PG-C ~6,3F is below threshold: rejected."
                     production (production-pg-c production))
      (let ((i (production-size production))
            (lhs (production-lhs production)))
        (signal-output (or *exact-matching-trace* *partial-matching-trace*)
                       "Matching production ~S." production)
        (setf *instantiation* (production-instantiation production))
        (loop (decf i 1) (when (= i 0) (return))
              (setf (instantiation-variable *instantiation* i) nil))
;        (setf (instantiation-variable *instantiation* *instantiation-slots*)
;              *wmfocus*)
        (dolist (initialization (production-initializations production))
          (setf (instantiation-variable *instantiation* (car initialization))
                (symbol-value (cdr initialization))))
        (setf *extra-instantiation* (production-extra-instantiation production))
        ; FIX: call just the car of lhs if :era is enabled.
        (when *enable-rational-analysis*
          (setf (instantiation-gain *instantiation*) gain)
          (setf lhs (car lhs)))
        (simulate-call lhs)))
    *conflict-set*))

;;; Analogy

(defun get-wmes (tree &optional (constants nil))
  "Returns the list of wmes in tree.  If constants is t, they are preserved."
  (let ((wme-list nil))
    (dolist (leaf (if (consp tree) tree (when tree (list tree))) wme-list)
      (if (consp leaf)
        (setf wme-list (nconc wme-list (get-wmes leaf constants)))
        (let ((wme (if (wmep leaf) leaf
                       (or (get-wme leaf) (if constants leaf :no)))))
          (unless (eq wme :no) (push-last wme wme-list)))))))

(defun add-retrievals (retrievals variables constants singles
                                  &optional (differents nil))
  "Adds components of retrievals to either variables or singles."
  (dolist (retrieval retrievals (values variables constants singles))
    (when (and (atom retrieval) (not (wmep retrieval)))
      (setf retrieval (get-safe-wme retrieval)))
    (let ((wme (if (atom retrieval) retrieval (get-wme (first retrieval)))))
      (when (and wme (not (member wme variables :test #'equal)))
        (push wme variables)))
    (let ((wme-list nil))
      (cond ((wmep retrieval)
             (setf wme-list (rest (wme-slot-wmes retrieval)))
             (when differents
               (do ((rest-list wme-list (rest rest-list)))
                   ((null rest-list))
                 (setf wme-list (append (cdr (assoc (first rest-list) differents
                                                    :test #'equal))
                                        wme-list)))))
            (t
             (setf wme-list (get-wmes (rest retrieval)))))
      (dolist (wme wme-list)
        (unless (or (member wme variables :test #'equal)
                    (member wme constants :test #'equal))
          (cond ((member wme singles :test #'equal)
                 (push wme variables)
                 (setf singles (delete wme singles :test #'equal :count 1)))
                (t
                 (push wme singles))))))))

(defun get-name-variable (value variables &optional (role nil))
  "Gets the variable associated with value from the a-list variables.
   If no variable has been associated with value, then create one according to role.
   If no role is specified, then just variabilize the name value.
   If the name is taken, increment a counter until a free one is found."
  (let* ((name (if (wmep value) value (or (get-wme value) value)))
         (pair (assoc name variables :test #'equal)))
    (if pair
      (or (rest pair)
          (let* ((variable-name
                  (concatenate 'string "=" (coerce-string (or role name))))
                 (variable (intern variable-name))
                 (counter 0))
            (loop
              (if (and (not (eq role 'goal))
                       (rassoc variable variables))
                (setf variable (intern (concatenate 'string variable-name
                                                    (coerce-string (incf counter)))))
                (return)))
            (rplacd pair variable)
            variable))
      name)))

(defun wme-description (wme variables dont-cares
                            &key (differents nil) (isa-slot t) (name wme) (role nil))
  "Generates a pre-parsed production description of a wme, variabilizing variables.
   All slots are generated, except for dont-cares values.
   Slot negations are generated using differents.
   Also insert chunks and chunk types directly rather than as their names."
  (let* ((wmetype (wme-type wme))
         (wmename (var-var> (get-name-variable name variables
                                               (or role (wme-type-name wmetype)))))
         (slot-value nil)
         (slot-list nil)
         (different nil))
    (dolist (slot (wme-type-slots wmetype)
                  (when (or slot-list isa-slot)
                    (nconc (list wmename)
                           (when isa-slot (list (list 'isa wmetype)))
                           slot-list)))
      (setf slot-value (wme-slot wme (slot-index slot)))
      (when (and (or isa-slot
                     (not (eq slot-value (wme-slot name (slot-index slot)))))
                 (not (member slot-value dont-cares :test #'equal)))
        (push-last (list (slot-name slot) (get-name-variable slot-value variables
                                                             (slot-name slot)))
                   slot-list)
        (setf different (cdr (assoc slot-value differents :test #'equal)))
        (dolist (not different)
          (push-last (list '- (slot-name slot) (get-name-variable not variables))
                     slot-list))))))

(defun replace-variable-names (expression variables)
  "Replaces the values in expression using the variables alist."
  (if (listp expression)
    (let ((list nil))
      (dolist (value expression list)
        (push-last (replace-variable-names value variables) list)))
    (get-name-variable expression variables)))

(defun create-production (problem constraints solutions modified actions pop
                                  variables dont-cares differents)
  "Generates a production from analogy."
  ; production name based on goal type
  (let* ((type (wme-type-name (wme-type problem)))
         (name (safe-gentemp (coerce-string type)))
         (wme-variables nil)
         (wme nil)
         (lhs nil)
         (rhs nil))
    ; associate the variable wmes to their variablized name, initialized to NIL
    (dolist (variable variables)
      (push (list variable) wme-variables))
    ; problem and retrievals and lhs
    (push-last (wme-description problem wme-variables dont-cares
                                :differents differents :role 'goal)                                
               lhs)
    (dolist (constraint constraints)
      (cond ((atom constraint)
             (setf wme (get-safe-wme constraint))
             (when wme
               (push-last (wme-description wme wme-variables dont-cares :differents differents)
                          lhs)))
            ((commandp (first constraint))
             (push-last (replace-variable-names constraint wme-variables) lhs))
            (t
             (setf wme (replace-variable-names constraint wme-variables))
             (rplaca wme (var-var> (first wme)))
             (process-clause wme)
             (push-last wme lhs))))
    ; subgoals in rhs
    (dolist (solution solutions)
      (push-last (wme-description solution wme-variables dont-cares :role 'subgoal)
                 rhs))
    (setf solutions (nreverse solutions))
    (when (and modified (eq (wme-type problem) (wme-type modified)))
      (let ((main-goal (wme-description modified wme-variables dont-cares
                                        :isa-slot nil :name problem :role 'goal)))
        (when main-goal (push-last main-goal rhs))))
    (let ((subgoal (pop solutions)))
      (if subgoal
        (push-last (list (if pop '!focus-on! '!push!)
                         (get-name-variable subgoal wme-variables 'subgoal))
                   rhs)
        (when pop
          (push-last (list '!pop!) rhs))))
    (dolist (solution solutions)
      (push-last (list '!push! (get-name-variable solution wme-variables 'subgoal))
                 rhs))
    (dolist (action actions)
      (push-last (replace-variable-names action wme-variables) rhs))
    (values name type lhs rhs)))

(defun identical-productions (new old &optional (vars nil))
  "Checks whether the production bodies old and new are identical."
  (do ((rest-new new (rest rest-new))
       (rest-old old (rest rest-old)))
      ((or (null rest-new) (null rest-old))
       (when (and (null rest-new) (null rest-old)) vars))
    (let ((first-new (first rest-new))
          (first-old (first rest-old)))
      ;; Fix: make sure that it both ends with > AND starts with =
      (when (and (retrievalp first-new) (variablep first-new))
        (setf first-new (var>var first-new)))
      (when (and (retrievalp first-old) (variablep first-old))
        (setf first-old (var>var first-old)))
      (cond ((and (variablep first-new) (variablep first-old))
             (let ((first-new-assoc (cdr (assoc first-new vars :test #'eq)))
                   (first-old-assoc (car (rassoc first-old vars :test #'eq))))
               (if (or first-new-assoc first-old-assoc)
                 (unless (and (eq first-old first-new-assoc)
                              (eq first-new first-old-assoc))
                   (return nil))
                 (push (cons first-new first-old) vars))))
            ((and (listp first-new) (listp first-old))
             (unless (setf vars (identical-productions first-new first-old vars))
               (return nil)))
            ((equal first-new first-old))
            (t (return nil))))))

(defun reinforce-analogized-production (production)
  "Reinforces production by increasing the number of references."
  (when *strength-learning*
    (signal-output *production-compilation-trace* "Reinforcing production ~S." production)
    (add-reference (production-references production)))
  (when *parameters-learning*
    (signal-output *production-compilation-trace* "Updating parameters of production ~S."
                   production)
    (add-reference (production-successes production) (not (numberp *parameters-learning*)))
    (add-reference (production-efforts production) (not (numberp *parameters-learning*))
                   (production-effort production) (production-effort production))
    (recompute-production-parameters production)))

#|
Removed 10/30/03 by Dan because newer one below


(defun compile-or-select-production (name type lhs rhs success failure
                                          &optional (documentation nil))
  "Either create a new production or select an existing identical one.
   Return a second value indicating whether it is an existing production (t) or not (nil)."
  (dolist (production (wme-type-productions (get-safe-type type name))
                      (let ((new-production (compile-production name lhs rhs documentation)))
                        (signal-output *production-compilation-trace* "Compiling Production ~A." new-production)
                        (parameters-fct name (nconc (list :success (if success t nil)
                                                          :failure (if failure t nil))
                                                    *production-compilation-parameters*))
                        (values new-production nil)))
    (when (identical-productions (cons lhs rhs) (production-text production))
      (signal-output *production-compilation-trace* "Recreating Production ~A." production)
      (when *reinforce-analogized-production*
        (reinforce-analogized-production production))
      (return (values production t)))))
|#

(defun solve-by-analogy (dependency)
  "Main function to generate a production based on dependency.
   Returns the new analogized production."
  (signal-output *production-compilation-trace* "Analogizing to dependency ~S" dependency)
  (let ((stack-values (get-slot-value dependency 'stack))
        (stack-solutions nil)
        (stack-success nil)
        (stack-failure nil))
    (dolist (value (cond ((listp stack-values) stack-values)
                         ((wmep stack-values) (list (wme-name stack-values)))
                         (t nil)))
      (cond ((eq value 'success)
             (setf stack-success t))
            ((eq value 'failure)
             (setf stack-failure t))
            (t
             (push-last (if (wmep stack-values)
                          stack-values
                          (get-wme-or-constant value))
                        stack-solutions))))
    (let ((problem (first (get-wmes (get-slot-value dependency 'goal))))
          (solutions (nconc (get-wmes (get-slot-value dependency 'subgoals))
                            stack-solutions))
          (modified (first (get-wmes (get-slot-value dependency 'modified))))
          (constraints (get-slot-value dependency 'constraints))
          (actions (get-slot-value dependency 'actions))
          (success (or (get-slot-value dependency 'success) stack-success))
          (failure (or (get-slot-value dependency 'failure) stack-failure))
          (variables (get-wmes (get-slot-value dependency 'generals) t))
          (constants (get-wmes (get-slot-value dependency 'specifics) t))
          (dont-cares (get-wmes (get-slot-value dependency 'dont-cares) t))
          (differents (get-slot-value dependency 'differents))
          (pop nil)
          (singles nil))
      ;; Replace wme names in differents with the wmes themselves
      (when (and differents (not (listp (first differents))))
        (setf differents (list differents)))
      (dolist (different differents)
        (loop
          (unless different (return))
          (let ((wme (get-wme (first different))))
            (when wme (rplaca different wme)))
          (pop different)))
      ;; copy the constraints and action lists
      ;; checking for single conditions
      (setf constraints (if (or (not (listp constraints)) ; activation
                                (commandp (first constraints)) ; !eval!
                                (eq (second constraints) 'isa)) ; description
                          (list constraints)
                          (copy-list constraints)))
      (setf actions (if (not (listp actions)) ; !pop!
                      (list (list actions))
                      (if (commandp (first actions)) ; (!push! =goal)
                        (list actions)
                        (copy-list actions))))
      ;; check modified goal
      (when (and modified (not (eq (wme-type problem) (wme-type modified))))
        (push-last modified solutions)
        (setf modified nil)
        (setf pop t))
      (signal-output *production-compilation-trace* "The goal is ~S and the subgoals are~{ ~S~}"
                     problem (or solutions '(none)))
      ;; determine variables and constants
      (multiple-value-setq (variables constants singles)
        (add-retrievals (cons problem constraints)
                        variables constants singles differents))
      (multiple-value-setq (variables constants singles)
        (add-retrievals (append solutions (when modified (list modified)) actions)
                        variables constants singles))                                     
      ;; compile the production
      (multiple-value-bind (name type lhs rhs)
                           (create-production problem constraints solutions modified
                                              actions (or success failure pop)
                                              variables dont-cares differents)
        (compile-or-select-production name type lhs rhs success failure)))))

(defun whynot-dependency-fct (dependencies)
  "Tests production compilation with the dependencies."
  (let ((*production-compilation-trace* *command-trace*)
        (*exact-matching-trace* *command-trace*)
        (*partial-matching-trace* *command-trace*)        
        (*verbose* t)
        (*goal-depth* 1)
        (productions nil))
    (dolist (dependency dependencies productions)
      (let ((production (solve-by-analogy (get-safe-wme dependency))))
        (push-last (production-name production) productions)
        (pdisable-fct (list production))))))

(defun set-compilation-parameters-fct (params)
  "Sets the parameters for compiled productions."
  (setf *production-compilation-parameters* params))

;;; moved here before they are used by john's code to prevent warnings

(defmacro no-output (&rest forms)
  "Evaluates forms with *command-trace* turned off."
  `(let ((*command-trace* nil))
     ,@forms))

(defmacro sgp (&rest parameters)
  "Sets global parameters.  Checks values and performs housekeeping duties."
  `(sgp-fct ',parameters))




(defun get-set (ins) 
  (cond (ins
  (let ((list (get-instantiation ins)))
    (list (aref ins 0) (second (second (caar list)))
          (first list) (cdr list))))
        (t nil)))

(defun get-instantiation (&optional (instantiation *instantiation*)
                                       (trace *command-trace*)
                                       (format *production-trace*))
  "Pretty-prints an instantiation."
  (let ((production (instantiation-production instantiation)))
    (when (eq trace 'short) (setf trace t))
    (if (eq format 'short)
      (dolist (binding (production-bindings production))
        (signal-output trace "   ~A: ~A" (variable-name binding)
                       (if (integerp (variable-index binding))
                         (or (instantiation-variable instantiation (variable-index binding))
                             "VARIABLE STILL UNBOUND")
                         "RETURN VARIABLE STILL UNDETERMINED")))
      (let ((bindings nil)
            (text nil))
        (dolist (binding (production-bindings production))
          (when (integerp (variable-index binding))
            (let ((value (instantiation-variable instantiation (variable-index binding))))
              (when value
                (push (cons (variable-name binding) value) bindings)
                (push (cons (var-var> (variable-name binding))
                            (var-var> value))
                  bindings)))))
        (setf text (sublis bindings (production-text production)))
        text))))

;THIS PRODUCTION IS CALLED AFTER EACH CYCLE.  IT WILL CALL COMPOSE-PRODUCTION TO TRY TO COLLAPSE THE PREVIOUS
;AND CURRENT PRODUCTION UNLESS A PERIOD OF TIME HAS ELAPSED UNUSED BETWEEN THE TWO PRODUCTIONS -- IN WHICH
;CASE THE INFERENCE IS THAT WE WERE WAITING FOR SOME EXTERNAL EVENT.
(defun store-instantiation (i)
  (let (hold)
       (cond (*instantiation*
              (setf hold  (get-set i))
              (cond ((and (rest *previous-instantiation*)  (< (- *time* (first *previous-instantiation*)) *threshold-time*))
                     (compose-production (rest *previous-instantiation*) hold)))
              (setf *previous-instantiation* (cons *time* hold))))))


;;; lhs

(defmacro set-bindings (slots actions)
  "Sets the stack entries to the slots values."
  `(dolist (action ,actions t)
     (unless (setf (instantiation-variable *instantiation* (action-value action))
                   (slots-slot ,slots (action-slot action)))
       (signal-output *exact-matching-trace* "Variable in slot ~S cannot be bound to nil: binding fails."
                      (action-name action))
       (return nil))))

(defmacro interpret-value (action)
  "Interpret a slot value description in a number of ways."
  `(case (action-dispatch ,action)
     (:literal (action-value ,action))
     (:stack (instantiation-variable *instantiation* (action-value ,action)))
     (:eval (get-wme-or-constant (funcall (action-value ,action))))))

(defmacro test-slots-buffer (wme slots actions)
  "Tests that slots satisfies all the actions.  Specifically for buffers such as goal,
   retrieval and p/m buffers that do not require activation computations."
  `(progn
     (signal-output *exact-matching-trace* "Matching CHUNK ~S." ,wme)
     (dolist (action ,actions t)
       (let ((actual (slots-slot ,slots (action-slot action)))
             (desired (interpret-value action))
             (negation (action-negation action))
             (slot (action-name action)))
         (cond ((null negation)
                (unless (equal actual desired)
                  (signal-output *exact-matching-trace* "Value ~S is different from condition ~S in slot ~S: test fails."
                          actual desired slot)
                  (return nil)))
               ((eq negation '-)
                (when (equal actual desired)
                  (signal-output *exact-matching-trace* "Value ~S is equal to condition ~S in slot ~S: negation test fails."
                                 actual desired slot)
                  (return nil)))
               ((fboundp negation)
                (unless (and (numberp actual) (numberp desired)
                             (funcall negation actual desired))
                  (signal-output *exact-matching-trace* "Value ~S does not match test ~S with condition ~S in slot ~S: test fails."
                          actual negation desired slot)
                  (return nil)))
               (t
                (signal-output *exact-matching-trace* "Unknown test ~S in slot ~S: ignoring it."
                               negation slot)))))))

(defmacro test-slots (wme slots actions) ; &optional index)
  "Tests that slots satisfies all the actions.
   Also tests that activation is above threshold when applicable.
   Equal test necessary to handle the full potential range of values."
  `(progn
     (signal-output *exact-matching-trace* "Matching CHUNK ~S." ,wme)
     (dolist (action ,actions (if *retrieval-threshold* (activation ,wme) t))
       (let ((actual (slots-slot ,slots (action-slot action)))
             (desired (interpret-value action))
             (negation (action-negation action))
             (slot (action-name action)))
         (cond ((null negation)
                (unless (equal actual desired)
                  (signal-output *exact-matching-trace* "Value ~S is different from condition ~S in slot ~S: test fails."
                          actual desired slot)
                  (return nil)))
               ((eq negation '-)
                (when (equal actual desired)
                  (signal-output *exact-matching-trace* "Value ~S is equal to condition ~S in slot ~S: negation test fails."
                                 actual desired slot)
                  (return nil)))
               ((fboundp negation)
                (unless (and (numberp actual) (numberp desired)
                             (funcall negation actual desired))
                  (signal-output *exact-matching-trace* "Value ~S does not match test ~S with condition ~S in slot ~S: test fails."
                          actual negation desired slot)
                  (return nil)))
               (t
                (signal-output *exact-matching-trace* "Unknown test ~S in slot ~S: ignoring it."
                               negation slot)))))))

(defun add-instantiation-to-conflict-set (arguments)
  "Adds *instantiation* to conflict set, sorted by value or PG-C.
   No need to compute latencies."
  (declare (ignore arguments))
  (unless *enable-rational-analysis*
    (setf (instantiation-gain *instantiation*)
          (get-functional-parameter
           (production-value (instantiation-production *instantiation*))))
    (when *exp-gain-noise*
      (incf (instantiation-gain *instantiation*) (noise *exp-gain-noise*))))
  (let ((rest-cset *conflict-set*)
        (item nil)
        (index 0)
        (equals 0))
    (loop
      (setf item (pop rest-cset))
      (when (or (null item)
                (> (instantiation-gain *instantiation*) (instantiation-gain item)))
        (return))
      (if (= (instantiation-gain *instantiation*) (instantiation-gain item))
        (incf equals 1)
        (incf index 1)))
    (when (> equals 0)
      (incf index (if *enable-randomness* (random (+ equals 1)) equals)))
    (if (= index 0)
      (push *instantiation* *conflict-set*)
      (let ((previous (nthcdr (- index 1) *conflict-set*)))
        (push *instantiation* (cdr previous))))))

(defmacro add-latency (wme activation)
  `(let ((latency (activation-latency ,activation)))
     (signal-output *activation-trace* "CHUNK ~S Activation ~6,3F Latency ~6,3F"
                    ,wme ,activation latency)
     (incf *latency* latency)))

(defun buffer-test-and-bind (arguments)
  "Implements a direct match.  Tests for type and then slots, then bind
   variables.  Finally, call the function implementing the next clause.
   Arguments is a list of the wme's stack index, its type, a list of slots
   tests and bindings, and the next call if any.
   Returns t if a complete match is found, nil otherwise.
   SPECIFIC TO MODULAR BUFFERS."
  (let* ((wme-index (pop arguments))
         ;; FIX: get the current visual or sound object by calling the PM layer
         ;; If the variable is already bound, test that it is identical
         (external-call (pop arguments))
         (wme (apply (first external-call) (rest external-call)))
         (binding (instantiation-variable *instantiation* wme-index))
         (wmetype (pop arguments))
         (slots-tested (pop arguments))
         (slots-bound (pop arguments)))
    (cond ((and binding (not (eq wme binding)))
           (signal-output *exact-matching-trace* "Current visual object ~S does not match existing binding ~S."
                          wme binding))
          (t
           (unless binding
             (setf (instantiation-variable *instantiation* wme-index) wme))
           (if (and (wmep wme) (subtype (wme-type wme) wmetype))
             (let ((slots (wme-slots wme)))
               ;; Set bindings before testing slots
               (and (set-bindings slots slots-bound)
;                   ;; FIX: The retrieval threshold does not apply to PM calls
;                    (let ((*retrieval-threshold* nil))
;                      (test-slots wme slots slots-tested wme-index))
                    (test-slots-buffer wme slots slots-tested)
                    (simulate-call arguments)))
             (signal-output *exact-matching-trace* "~S is not a CHUNK of type ~S."
                            wme wmetype))))))
 
(defun direct-test-and-bind-buffer (arguments)
  "Implements a direct match.  Tests for type and then slots, then bind
   variables.  Finally, call the function implementing the next clause.
   Arguments is a list of the wme's stack index, its type, a list of slots
   tests and bindings, and the next call if any.
   Returns t if a complete match is found, nil otherwise.
   Specific to fixed buffers such as goal, retrieval, and p/m buffers."
  (let* ((wme-index (pop arguments))
         (wme (instantiation-variable *instantiation* wme-index))
         (wmetype (pop arguments))
         (slots-tested (pop arguments))
         (slots-bound (pop arguments)))
    (if (and (wmep wme) (subtype (wme-type wme) wmetype))
      (let ((slots (wme-slots wme)))
        ;; Set bindings before testing slots
        (and (set-bindings slots slots-bound)
             (test-slots-buffer wme slots slots-tested)
             (simulate-call arguments)))
      (signal-output *exact-matching-trace* "~S is not a CHUNK of type ~S."
                     wme wmetype))))

(defun direct-test-and-bind (arguments)
  "Implements a direct match.  Tests for type and then slots, then bind
   variables.  Finally, call the function implementing the next clause.
   Arguments is a list of the wme's stack index, its type, a list of slots
   tests and bindings, and the next call if any.
   Returns t if a complete match is found, nil otherwise."
  (let* ((wme-index (pop arguments))
         (wme (index-or-chunk wme-index))
         (wmetype (or (pop arguments) (when (wmep wme) (wme-type wme))))
         (slots-tested (pop arguments))
         (slots-bound (pop arguments)))
    (if (and (wmep wme) (subtype (wme-type wme) wmetype))
      (let ((slots (wme-slots wme)))
        ;; Set bindings before testing slots
        (and (set-bindings slots slots-bound)
             (test-slots wme slots slots-tested) ; wme-index)
             (simulate-call arguments)))
      (signal-output *exact-matching-trace* "~S is not a CHUNK of type ~S."
                     wme wmetype))))

(defun indirect-test-and-bind (arguments)
  "Implements an indirect match.  For all subtypes of the wme, try all the wmes
   and test their slots, then bind variables.  Finally, call the function
   implementing the next clause.
   Arguments is a list of the wme's stack index, its type, a list of slots
   tests and bindings, and the next call if any.
   Returns t if a complete match is found, nil otherwise."
  (let* ((wme-index (pop arguments))
         (wmetype (pop arguments))
         (slots nil)
         (slots-tested (pop arguments))
         (slots-bound (pop arguments))
         (retrieval-list nil))
    (signal-output *exact-matching-trace* "Matching CHUNKs of type ~S." wmetype)
    ;; Return the winning chunk or nil to handle-failure
    ;; If *enable-randomness* is on, then pick randomly otherwise pick the first
    (dolist (type (wme-type-subtypes wmetype)
                  (setf *retrieval-scheduler*
                        (if *enable-randomness*
                          (nth (random (length retrieval-list)) retrieval-list)
                          (first retrieval-list))))
      (dolist (wme (wme-type-wmes type))
        (setf slots (wme-slots wme))
        (if wme-index
          (setf (instantiation-variable *instantiation* wme-index) wme)
          (setf *retrieval-scheduler* wme))
        ;; Set bindings before testing slots
        (when (and (set-bindings slots slots-bound)
                   (test-slots wme slots slots-tested)
                   (simulate-call arguments))
          ;; In action retrieval mode, keep the list of retrievals
          (unless wme-index
            (push-last *retrieval-scheduler* retrieval-list))
          ; pick and initialize a new instantiation
          (let* ((production (instantiation-production *instantiation*))
                 (next-instantiation (make-instantiation production))
                 (index 1)
                 (size (production-size production)))
            (loop
              (when (= index size) (return))
              (setf (instantiation-variable next-instantiation index)
                    (instantiation-variable *instantiation* index))
              (incf index 1))
            (setf *instantiation* next-instantiation)))))))

(defun eval-test (arguments)
  "Evaluate the first of the arguments, then call the next clause if true."
  (let ((expr (pop arguments)))
    (if (funcall expr)
      (simulate-call arguments)
      (signal-output *exact-matching-trace* "Expression ~S evaluated to NIL." expr))))

(defun bind-lhs (arguments)
  "Binds the first argument to the evaluation of the second,
   then call the next clause if true."
  (let ((index (pop arguments))
        (expr (pop arguments)))
    (if (setf (instantiation-variable *instantiation* index)
              (get-wme-or-constant (funcall expr)))
      (simulate-call arguments)
      (signal-output *exact-matching-trace* "Expression ~S evaluated to NIL." expr))))

(defun bind-find-location (arguments)
  "Binds the first of the arguments to the value of find-location
   applied to the second."
  (let ((index (pop arguments))
        (expr (pop arguments)))
    (if (setf (instantiation-variable *instantiation* index)
              (get-wme-or-constant (apply #'find-location (funcall expr))))
      (simulate-call arguments)
      (signal-output *exact-matching-trace* "Find-location of ~S returned NIL." expr))))


;;; Partial Matching (LHS)

(defun similarity-fct (wmej wmei)
  "Retrieves the similarity between wmej and wmei, in that order."
  (let ((*verbose* t)
        (*goal-depth* 1)
        (similarity 0.0))
    (setf wmej (or (get-wme wmej) wmej))
    (setf wmei (or (get-wme wmei) wmei))
    (setf similarity (get-similarity wmej wmei))
    (signal-output *command-trace* "~6,3F" similarity)
    similarity))

(defun set-similarities-fct (triplets)
  "Sets the similarities between the list of triplets, both ways."
  (let ((similarities nil))
    (dolist (triplet triplets similarities)
      (let ((wmej (get-safe-wme (first triplet)))
            (wmei (get-safe-wme (second triplet)))
            (similarity (third triplet)))
        (when (and wmej wmei)
          (push-last similarity similarities)
          (set-similarity wmej wmei similarity)
          (set-similarity wmei wmej similarity))))))

(defmacro test-slots-pm (wme slots actions)
  "Tests that wme slots satisfy all the actions.  Equal test necessary to
   handle the full potential range of non-wme values.  Partial matching
   on wme values.  Returns the activation of wme if above threshold,
   nil otherwise."
  `(let ((activation (activation ,wme)))
     (signal-output *partial-matching-trace* "Partial matching chunk ~S with activation ~6,3F"
                    ,wme activation)
     (dolist (action ,actions
                     (progn
                       (signal-output *partial-matching-trace* "Matching score of chunk ~S is ~6,3F."
                                      ,wme activation)
                       activation))
       (let* ((value (slots-slot ,slots (action-slot action)))
              (target (interpret-value action))
              (similarity (get-similarity value target))
              (negation (action-negation action)))
         (signal-output *partial-matching-trace* "Similarity between chunks ~S and ~S is ~6,3F"
                        value target similarity)
         (cond ((null negation)
                (if (numberp similarity)
                  (let ((match (* similarity *mismatch-penalty*)))
                    (incf activation match)
                    (signal-output *partial-matching-trace* "Adjusting activation by ~6,3F to ~6,3F"
                                   match activation))
                  (unless similarity
                    (signal-output *partial-matching-trace* "Non-chunk mismatch between ~S and ~S: failure."
                                   value target)
                    (return nil))))
               ((eq negation '-)
                (if (numberp similarity)
                  (when (= similarity *max-sim*)
                    (let ((match (* *max-dif* *mismatch-penalty*)))
                      (incf activation match)
                      (signal-output *partial-matching-trace* "Adjusting activation by ~6,3F to ~6,3F"
                                     match activation)))
                  (when similarity
                    (signal-output *partial-matching-trace* "Non-chunk match between ~S and ~S for negation test: failure."
                                   value target)
                    (return nil))))
               ((fboundp negation)
                (unless (and (numberp value) (numberp target)
                             (funcall negation value target))
                  (signal-output *exact-matching-trace* "Value ~S does not match test ~S with condition ~S: test fails."
                                 value negation target)
                  (return nil)))
               (t
                (signal-output *exact-matching-trace* "Unknown test ~S: ignoring it."
                               negation)))))))

(defun direct-test-and-bind-pm (arguments)
  "Implements a direct match.  Tests for type and then slots, then bind
   variables.  Finally, call the function implementing the next clause.
   Arguments is a list of the wme's stack index, its type, a list of slots
   tests and bindings, and the next call if any.
   This is the rational analysis version, which handles partial matching too."
  (let* ((wme-index (pop arguments))
         (wme (index-or-chunk wme-index))
         (wmetype (or (pop arguments) (when (wmep wme) (wme-type wme))))
         (slots-tested (pop arguments))
         (slots-bound (pop arguments)))
    (cond ((and (wmep wme) (subtype (wme-type wme) wmetype))
           (signal-output *activation-trace* "Sources of activation are: ~S" *activation-sources*)
           (let* ((slots (wme-slots wme))
                  ;; Set bindings before testing slots
                  (activation (and (set-bindings slots slots-bound)
                                   (if *partial-matching*
                                     (test-slots-pm wme slots slots-tested)
                                     (test-slots wme slots slots-tested)))))
             (setf *sum-exp-act* (exp (/ *retrieval-threshold* *temperature*)))
             ;; In exact match, only counts the chunk activation if it matches?
             (when activation
               (incf *sum-exp-act* (exp (/ activation *temperature*))))
             ;; If activation is below threshold, then reject it
             (when (and activation (< activation *retrieval-threshold*))
               (signal-output (if *partial-matching* *partial-matching-trace* *exact-matching-trace*)
                              "CHUNK ~S match score ~6,3F is below threshold ~6,3F: failure."
                              wme activation *retrieval-threshold*)
               (setf activation nil))
             (add-latency wme (or activation *retrieval-threshold*))
             (when activation 
               (simulate-call arguments))))
          (t
           (setf *sum-exp-act* (exp (/ *retrieval-threshold* *temperature*)))
           (add-latency wme *retrieval-threshold*)
           (signal-output (if *partial-matching* *partial-matching-trace* *exact-matching-trace*)
                          "~S is not a CHUNK of type ~S."
                          wme wmetype)))))

(defmacro set-blending-bindings (chunk-probs actions total-prob)
  "Sets the stack entries to the slots values and returns the lowest
   activation of the matches, on the assumption that it determines retrieval."
  `(let ((worst-activation nil))
     ;; iterate separately for each value binding
     (dolist (action ,actions worst-activation)
       (let ((value-probs nil)
             (value nil)
             (probability nil)
             (value-type nil)
             (value-list nil)
             (best-value nil)
             (best-sum nil))
         ;; get the value and probability for each chunk-probability pair
         (dolist (chunk-prob ,chunk-probs)
           (setf value (wme-slot (car chunk-prob) (action-slot action)))
           (setf probability (/ (cdr chunk-prob)
                                ;; just in case total-prob is 0 (unlikely)
                                (if (zerop ,total-prob) 1.0 ,total-prob)))
           (push (cons value probability) value-probs)
           (signal-output *blending-trace* "Chunk ~S Value ~S Probability ~6,3F"
                          (car chunk-prob) value probability)
           ;; if the value is nil, then do nothing
           ;; if the value is a chunk, then note the type (t means conflict)
           ;; if the value is a number, then add it to the list and
           ;;    note whether it is integer or float
           ;; otherwise add it to the list if it isn't already numbers
           (cond ((null value))
                 ((wmep value)
                  (let ((type (wme-type value)))
                    (if (null value-type)
                      (setf value-type type)
                      (unless (equal value-type type)
                        (setf value-type t)))))
                 ((numberp value)
                  (let ((type (if (integerp value) 'integer 'float)))
                    (cond ((null value-list)
                           (setf value-list (list type)))
                          ((eq (first value-list) 'integer)
                           (when (eq type 'float)
                             (setf (first value-list) 'float)))
                          ((eq (first value-list) 'float))
                          (t
                           (setf value-list (list type))))
                    (push-last (cons value probability) value-list)))
                 (t
                  (unless (or (eq (first value-list) 'integer)
                              (eq (first value-list) 'float))
                    (push (cons value probability) value-list)))))
         ;; if all values are of the same chunk type, then look at all chunks
         ;; otherwise only consider the chunks from the given type
         ;; if some non-chunks values were present, then add them
         ;; unless they were numbers, which then take precedence
         (cond ((eq (first value-list) 'integer)
                (setf best-value
                      (round (funcall *blending-hook-fn* (rest value-list)))))
               ((eq (first value-list) 'float)
                (setf best-value
                      (funcall *blending-hook-fn* (rest value-list))))
               (t
                (if (eq value-type t)
                  (for-all-wmes wme (push wme value-list))
                  (unless (null value-type)
                    (dolist (wme (wme-type-wmes value-type))
                      (push wme value-list))))
                ;; compute the fit for all possible values and find the best
                (dolist (value value-list)
                  (let ((sum 0.0))
                    (dolist (value-prob value-probs)
                      (let ((penalty (- 1.0 (or (get-similarity (car value-prob)
                                                                value) 0.0))))
                        (incf sum (* (cdr value-prob) penalty penalty))))
                    (signal-output *blending-trace* "Value ~S Error ~6,3F"
                                   value sum)
                    (unless (and best-value (> sum best-sum))
                      (setf best-value value)
                      (setf best-sum sum))))))
         ;; compute the activation of the match given the best value
         (when best-value
           (let ((activation (funcall *blending-activation-fn*
                                      best-value value-probs ,total-prob)))
             (signal-output *blending-trace* "Best Value ~S Activation ~6,3F"
                            best-value activation)
             ;; if multiple values are bound, each process is independent
             ;; and the slowest one determines the latency
             (when (or (null worst-activation) (< activation worst-activation))
               (setf worst-activation activation))))
         ;; bind the best value if available or fail and return
         (unless (setf (instantiation-variable *instantiation* (action-value action))
                       best-value)
           (signal-output *exact-matching-trace* "Variable in slot ~S cannot be bound to nil: binding fails."
                          (action-name action))
           (return nil))))))

(defun indirect-test-and-bind-pm (arguments)
  "Implements an indirect match.  For all subtypes of the wme, try all the wmes
   and test their slots, then bind variables.  Finally, call the function
   implementing the next clause.
   Arguments is a list of the wme's stack index, its type, a list of slots
   tests and bindings, and the next call if any.
   This is the rational analysis version, which handles partial matching too.
   In addition, if blending is enabled then bind the consensus values."
  (let* ((wme-index (pop arguments))
         (wmetype (pop arguments))
         (slots-tested (pop arguments))
         (slots-bound (pop arguments))
         (best-activation (if *blending* most-negative-short-float
                              *retrieval-threshold*))
         (best-wme nil)
         (slots nil)
         (activation nil)
         (temperature (when *blending* (or *temperature* (* (sqrt 2.0) *activation-noise*))))
         (total-prob (when *blending* (exp (/ *retrieval-threshold* temperature))))
         (chunk-probs nil))
    (signal-output *activation-trace* "Sources of activation are: ~S" *activation-sources*)
    (signal-output (if *partial-matching* *partial-matching-trace* *exact-matching-trace*)
                   "Matching CHUNKs of type ~S." wmetype)
    (setf *sum-exp-act* (exp (/ *retrieval-threshold* *temperature*)))
    ;; If blending is on, then do not apply retrieval threshold to individual chunks
    (let ((*retrieval-threshold* best-activation))
      (dolist (type (wme-type-subtypes wmetype))
        (dolist (wme (wme-type-wmes type))
          (setf slots (wme-slots wme))
          (if wme-index
            (setf (instantiation-variable *instantiation* wme-index) wme)
            (setf *retrieval-scheduler* wme))
          ;; Set bindings before testing slots
          (setf activation (and (set-bindings slots slots-bound)
                                (if *partial-matching*
                                  (test-slots-pm wme slots slots-tested)
                                  (test-slots wme slots slots-tested))))
          ;; In exact match, only counts the chunk activation if it matches?
          (when activation
            (incf *sum-exp-act* (exp (/ activation *temperature*))))
          (when (and activation *blending*)
            (let ((prob (exp (/ activation temperature))))
              (push (cons wme prob) chunk-probs)
              (incf total-prob prob)))
          (when (and activation (>= activation best-activation))
            (signal-output (if *partial-matching* *partial-matching-trace* *exact-matching-trace*)
                           "Activation ~6,3F is larger than previous best ~6,3F: selecting ~S."
                           activation best-activation wme)
            (setf best-activation activation)
            (setf best-wme wme)))))
    (if (or (eq *blending* t)
            (and (eq *blending* 'rt)
                 (< best-activation *retrieval-threshold*)))
      (setf best-activation
            (set-blending-bindings chunk-probs slots-bound total-prob))
      (setf chunk-probs nil))
    (unless (or (eq *blending* 'rt)
                (>= best-activation *retrieval-threshold*))
      (setf best-activation *retrieval-threshold*)
      (setf best-wme nil))
    (add-latency (or best-wme 'failure) best-activation)
    (cond (best-wme
           (setf slots (wme-slots best-wme))
           (if wme-index
             (setf (instantiation-variable *instantiation* wme-index) best-wme)
             (setf *retrieval-scheduler* best-wme))
           (unless chunk-probs (set-bindings slots slots-bound))
           (simulate-call arguments))
          (t
           (signal-output (if *partial-matching* *partial-matching-trace* *exact-matching-trace*)
                          "No chunk reached activation threshold ~6,3F: matching fails."
                          *retrieval-threshold*)))))


;;; rhs

(defmacro set-wmfocus (&key (wme nil) (spread t) (activation-sources nil))
  "Sets the focus to wme, and update activation spread and goal sources, if necessary."
  `(progn
     (setf *wmfocus* ,wme)
     (signal-output *goal-trace* "Switching to goal ~S." *wmfocus*)
     (when *enable-rational-analysis*
       ,(when spread `(update-activation-spread)))
     (setf *goal-sources* (or ,activation-sources
                              (when ,wme
                                (copy-list (rest (wme-slot-wmes ,wme))))))
     *wmfocus*))

(defmacro reset-frame (stack-frame &key (spread nil))
  "Resets a frame of the goal stack."
  `(let ((frame ,stack-frame))
     (set-wmfocus :wme (goal-frame-focus frame) :spread ,spread
                  :activation-sources (goal-frame-sources frame))))

(defmacro return-values ()
  "Return values from the focus using the top goal frame."
  `(let ((wme nil)
         (slot nil)
         (value nil))
     (dolist (return (goal-frame-return-values (first *goal-stack*)))
       (setf value (wme-slot *wmfocus* (first return)))
       (dolist (destination (rest return))
         (setf wme (goal-frame-focus (nth (car destination) *goal-stack*)))
         (setf slot (cdr destination))
         (set-slot-value wme slot value)))))

(defun rehearse-chunk-fct (chunks &key (repeat 1) (force nil)
                                  (cycle nil) (time nil))
  "Rehearses chunks repeat times.  If a chunk is a list, then the first element
   of the list is the chunk to rehearse and the rest is the list of sources.
   When force is on, update the statistics whether or not learning is on.
   When cycle and/or time is set, increment the equivalent ACT-R counters
   by those amounts (1 by default)."
  (let ((structures nil)
        (level (* 1.0 repeat)))
    (unless (integerp repeat)
      (setf repeat (round repeat)))
    (dolist (spec chunks structures)
      (let ((chunk (get-safe-wme (if (listp spec) (first spec) spec))))
        (when chunk
          (push-last spec structures)
          (when (or force *base-level-learning*)
            ;; call add-reference because generalization needs to be implemented properly
            (add-reference (wme-references chunk) *optimized-learning* repeat *time*))
          (when (or force *associative-learning*)
            (let ((sources nil))
              (if (listp spec)
                (dolist (source (rest spec))
                  (setf source (get-safe-wme source))
                  (when source (push source sources)))
                ; add the chunk itself to the set of sources
                (setf sources (cons chunk (rest (wme-slot-wmes chunk)))))
              ;; Do not parcel out the sources.
              (incf (wme-needed chunk) level)
              (dolist (source sources)
                (incf (wme-contexts source) level)
                (incf (ia-fnicj (get-make-ia source chunk)) level))))
          (when cycle
            (incf *cycle* (* repeat (if (numberp cycle) cycle 1))))
          (when time
            (incf *time* (* level (if (numberp time) time 1.0)))))))))
   
(defun identical-wme (&optional (goal *wmfocus*))
  "If a wme identical to goal exists, then deletes goal and returns
   the identical wme after rehearsal.  Otherwise, returns goal."
  (when goal
    (let ((type (wme-type goal)))
      (setf goal
            (dolist (wme (wme-type-wmes type) goal)
              (when (and (not (eq goal wme))
                         (dolist (slot (wme-type-slots type) t)
                           (unless (equal (wme-slot goal (slot-index slot))
                                          (wme-slot wme (slot-index slot)))
                             (return nil))))
                ; delete goal and replace any remaining reference with wme
                (delete-wme goal wme)
                ; FIX: rehearse IAs as well as BLLs
                (rehearse-chunk-fct (list (cons wme *goal-sources*)))
                (return wme))))
      ;;; If of type dependency, then call solve-by-analogy
      ;;; FIX: only if it has not been popped in failure
      (when (and (eq (wme-type-name type) 'dependency)
                 *instantiation*
                 (not (get-functional-parameter
                       (production-failure
                        (instantiation-production *instantiation*)))))
        (solve-by-analogy goal))
      goal)))

(defun goal-focus-fct (&optional (wmes nil))
  "Sets the focus to the first wme, the list to focus-list,
   or print the current focus if none is selected."
  (let ((*verbose* t)
        (*goal-depth* 1)
        (wme nil))
    (cond (wmes
           (setf wme (get-safe-wme (first wmes)))
           (when wme
             (identical-wme)
             (set-wmfocus :wme wme :spread t))
           (setf *wmfocus-list* (list *wmfocus*))
           (dolist (wme-name (rest wmes))
             (setf wme (get-safe-wme wme-name))
             (when wme (push-last wme *wmfocus-list*))))
          (*wmfocus*
           (pprint-wme *wmfocus*))
          (t
           (signal-output *command-trace* "No focus currently selected.")))
    (when *wmfocus* (wme-name *wmfocus*))))

(defmacro focus-macro (goal &optional return-values)
  "Replaces the current focus with the new goal.  Return values from the old
   goal and install the return value specs for the new one in its place."
  `(progn
     (save-state-change
      :focus-on (make-goal-frame :focus *wmfocus*
                                 :return-values (when *goal-stack*
                                                  (goal-frame-return-values
                                                   (first *goal-stack*)))
                                 :sources *goal-sources*))
     (when *goal-stack*
       (return-values)
       (setf (goal-frame-return-values (first *goal-stack*)) ,return-values))
     (identical-wme)
     (set-wmfocus :wme ,goal)))

(defun focus-on-goal-fct (wme)
  "Replaces the current focus with the new goal.  Return values from the old
   goal and install the return value specs for the new one in its place."
  (let ((subgoal (get-safe-wme wme)))
    (when subgoal
      (focus-macro subgoal)
      (wme-name wme))))

(defun focus-fct (arguments)
  "Replaces the current focus with the new goal.  Return values from the old goal
   and install the return value specs for the new one in its place."
  (let* ((subgoal (pop arguments))
         (return-values (pop arguments)))
    (focus-macro (index-or-chunk subgoal) return-values)))

(defun buffers-fct (buffers)
  "Displays and sets the contents of buffers."
  (let ((buffer-values nil))
    (cond ((null buffers)
           (dolist (buffer *buffer-keywords*)
             (let ((name (first buffer))
                   (value (rest buffer)))
               (unless (variablep name)
                 (setf value (symbol-value value))
                 (push-last (when value (wme-name value)) buffer-values)
                 (format t "~S~C~S~%" name #\tab value)))))
          (t
           (loop
             (unless buffers (return))
             (let* ((buffer (pop buffers))
                    (name (assoc buffer *buffer-keywords*)))
               (cond (name
                      (unless (or (null buffers)
                                  (and (assoc (first buffers) *buffer-keywords*)
                                       (not (equal (first name) (first buffers)))))
                        (let ((value (when (first buffers) (get-safe-wme (first buffers)))))
                          (if (or (eq buffer 'goal) (eq buffer '=goal))
                            (focus-macro value)
                            (setf (symbol-value (rest name)) value)))
                        (pop buffers))
                      (let ((value (symbol-value (rest name))))
                        (push-last (when value (wme-name value)) buffer-values)))
                     (t
                      (signal-warn "UNKNOWN BUFFER ~S" buffer)))))))
    buffer-values))

(defun retrieval-fct (&optional (chunk nil))
  "Displays or changes (if argument is supplied) the value of
   the last chunk retrieved."
  (cond (chunk
         (setf chunk (get-safe-wme chunk))
         (setf *retrieval* chunk))
        (*retrieval*
         (pprint-wme *retrieval*))
        (t
         (signal-output *command-trace* "No retrieval currently present.")))
  (when *retrieval* (wme-name *retrieval*)))

(defmacro push-macro (goal &optional return-values)
  "Pushes the current goal on the stack and install goal as the new focus,
   with optional return values."
  `(progn
     (save-state-change :push-goal)
     (when *wmfocus*
       (push (make-goal-frame :focus *wmfocus*
                              :return-values ,return-values
                              :sources *goal-sources*)
             *goal-stack*)
       (incf *goal-depth* 3))     
     (set-wmfocus :wme ,goal)))

(defun push-goal-fct (wme)
  "Pushes the current goal on the stack and install wme as the new focus."
  (let ((subgoal (get-safe-wme wme)))
    (when subgoal
      (push-macro subgoal)
      (wme-name subgoal))))

(defun push-fct (arguments)
  "Pushes the current goal on the stack and install the first argument
   as the new focus."
  (let* ((subgoal (pop arguments))
         (return-values (pop arguments)))
    (push-macro (instantiation-variable *instantiation* subgoal) return-values)))

(defmacro pop-macro ()
  "Pops the top goal off the stack and into the focus and return values."
  `(progn
     (save-state-change :pop-goal
                        (make-goal-frame :focus *wmfocus*
                                         :return-values (when *goal-stack*
                                                          (goal-frame-return-values
                                                           (first *goal-stack*)))
                                         :sources *goal-sources*))
     (cond (*goal-stack*
            (return-values)
            (decf *goal-depth* 3)
            (identical-wme)
            (reset-frame (pop *goal-stack*)))
           (t
            (identical-wme)
            (set-wmfocus)))))

(defun pop-goal-fct ()
  "Pops the top goal off the stack and into the focus and return values."
  (pop-macro)
  (when *wmfocus* (wme-name *wmfocus*)))

(defun pop-fct (arguments)
  "Pops the top goal off the stack and into the focus and return values."
  (declare (ignore arguments))
  (pop-macro))

(defun clear-goal-stack-fct ()
  "Clears the goal stack by restoring the top goal."
  (loop
    (unless *goal-stack* (return))
    (pop-macro))
  (when *wmfocus* (wme-name *wmfocus*)))

(defun goal-stack-fct ()
  "Prints the list of wmes on the goal stack."
  (let ((*verbose* t)
        (*goal-depth* 1)
        (goals nil))
    (signal-output *command-trace* "Focus: ~S   ~6,3F" *wmfocus* *g*)
    (when *wmfocus* (push (wme-name *wmfocus*) goals))
    (dolist (frame *goal-stack*)
      (push-last (wme-name (goal-frame-focus frame)) goals)
      (signal-output *command-trace* "~S" (goal-frame-focus frame)))
    goals))

(defmacro set-slots (wme actions)
  "Sets wme slots according to actions."
  `(dolist (action ,actions)
     (set-slot-value ,wme (action-slot action) (interpret-value action))))

(defun modify-old-wme (arguments)
  "Set new slot values of an existing wme."
  (let* ((wme (instantiation-variable *instantiation* (pop arguments)))
         (type (pop arguments)))
    (signal-output *dm-trace* "Modifying CHUNK ~S." wme)
    (if (subtype (wme-type wme) type)
      (set-slots wme (pop arguments))
      (signal-warn "CHUNK ~S IS NOT OF TYPE ~S." wme type))
    (if (eq wme *wmfocus*)
        (update-activation-spread)
        (decf (wme-spread-stamp wme) 1))))

(defun create-new-wme (arguments)
  "Creates a new wme, sets its slot values and sets its stack binding."
  (let* ((wme-index (pop arguments))
         (wme-name (safe-gentemp (symbol-name (pop arguments))))
         (wme-type (pop arguments))
         (wme (create-wme wme-name wme-type)))
    (signal-output *dm-trace* "Creating CHUNK ~A." wme-name)
    (setf (instantiation-variable *instantiation* wme-index) wme)
    (set-slots wme (pop arguments))))

(defun output (arguments)
  "Prints the first argument, which is interpreted as a format command."
  (let* ((format-args (pop arguments))
         (format-string (first format-args))
         (format-list (list *goal-depth*)))
    (when (and *verbose* *output-trace*)
      (dolist (index (rest format-args))
        (push-last (if (functionp index)
                     (funcall index)
                     (instantiation-variable *instantiation* index))
                   format-list))
      (format *output-trace* "~?" format-string format-list))))

(defun eval-side (arguments)
  "Evaluate the first of the arguments (for the side-effects),
   then call the next clause."
  (funcall (pop arguments)))

(defun bind-rhs (arguments)
  "Binds the first argument to the evaluation of the second."
  (setf (instantiation-variable *instantiation* (pop arguments))
        (get-wme-or-constant (funcall (pop arguments)))))

(defun bind-move-attention (arguments)
  "Binds the first of the arguments to the value of move-location
   applied to the second."
  (setf (instantiation-variable *instantiation* (pop arguments))
        (get-wme-or-constant (apply #'move-attention (funcall (pop arguments))))))

(defun visual-action (arguments)
  "Calls the visual routine with proper evaluation of arguments."
  (apply (pop arguments) (funcall (pop arguments))))

(defun action-command (arguments)
  "Calls the send-command routine with the cycle time and rest of arguments.
   Was later modified to send-RPM-command."
  (apply 'send-rpm-command (- *time* *start-time*)
         (funcall (pop arguments))))

(defun delete-wme-variable (arguments)
  "Delete the wme indexed by the first argument."
  (let ((wme (svref *instantiation* (first arguments))))
    (signal-output *dm-trace* "Deleting CHUNK ~S." wme)
    (delete-wme wme)))

(defun copy-chunk-variable (arguments)
  "Binds the first argument to a list of the copies of the second."
  (setf (instantiation-variable *instantiation* (pop arguments))
        (copy-chunks (funcall (pop arguments)))))

(defun stop (arguments)
  "Stops the run after this cycle."
  (declare (ignore arguments))
  (setf *stop* t))

(defun restart-top-goal (arguments)
  "Pops all goals on the stack."
  (loop
    (unless *goal-stack* (return))
    (pop-fct arguments)))


;;; Run

(defmacro generate-all-instantiations ()
  "Do left-hand side matching of all productions on the current WM state."
  `(progn
     (setf *conflict-set* nil)
;     (if *wmfocus*
       (dolist (goal-type (cons (get-safe-type t 'generate-all-instantiations)
                                (when *wmfocus*
                                  (wme-type-supertypes (wme-type *wmfocus*))))
                          *conflict-set*)
         (dolist (production (wme-type-productions goal-type))
           (match-lhs production)))))
;       (signal-warn "NO GOAL SELECTED AS FOCUS."))))

(defun pmatches-fct ()
  "Generates and prints instantiations for the current WM state.
   Does NOT guarantee that Expected Gain Noise is going to be the same in run."
  (let ((*verbose* t)
        (*goal-depth* 1)
        (productions nil))
    (pprint-instantiations (generate-all-instantiations)
                           *command-trace* *matches-trace*)
    (dolist (instantiation *conflict-set* productions)
      (push-last (production-name (instantiation-production instantiation))
                 productions))))

(defun whynot-fct (productions)
  "Tries to match production(s) name with exact matching trace on.
   The whole lhs is matched, even when ERA is on."
  (let ((*exact-matching-trace* *command-trace*)
        (*partial-matching-trace* *command-trace*)
        (*verbose* t)
        (*goal-depth* 1))
    (when productions
      (setf *conflict-set* nil)
      (dolist (production productions)
        (setf production (get-safe-production production))
        (when production
          (when (and (match-lhs production) *enable-rational-analysis*)
            (let ((*instantiation* (production-instantiation production)))
              (unless (simulate-call (cdr (production-lhs production)))
                (setf *conflict-set*
                      (delete *instantiation* *conflict-set* :test #'eq))))))))
    (setf productions nil)
    (pprint-instantiations *conflict-set* *command-trace*)
    (dolist (instantiation *conflict-set* productions)
      (push-last (production-name (instantiation-production instantiation))
                 productions))))

(defun choose-instantiation ()
  "If rational analysis is enabled, then pick the best instantiation.
   Otherwise, pick the first one since it is the one with the highest value.
   Returns the winning instantiation, the latency of the matching process,
   and the latency of the productions which failed before the winning one."
  (let ((index 0)
        (failed-latencies 0.0))
    (setf *instantiation* nil)
    (setf *latency* 0.0)
    (cond ((and (null *conflict-set*) *retrieval-threshold*)
           (setf *latency* (activation-latency *retrieval-threshold*)))
          (*enable-rational-analysis*
;           (signal-output *activation-trace* "Sources of activation are: ~S" *activation-sources*)
           (dolist (instantiation *conflict-set* (setf *instantiation* nil))
             (let* ((production (instantiation-production instantiation))
                    (lhs (cdr (production-lhs production))))
                (signal-output (or *exact-matching-trace* *partial-matching-trace* *conflict-resolution-trace*)
                              "Matching production ~S" production)
;                (signal-output *activation-trace* "Adding latency of production ~A" production)
                (setf *instantiation* instantiation)
                (incf index 1)
                (when (simulate-call lhs)
                  (return))
                ;;; if matching fails, learn the failure with the latency of the failed matching
                (when *parameters-learning* (learn-parameters nil nil
                                                              :latency (- *latency* failed-latencies)))
                (signal-output *conflict-resolution-trace* "Failed production ~A"
                               production)
                (signal-output *conflict-resolution-trace* "Matching Latency: ~6,3F"
                               (- *latency* failed-latencies))
                (setf failed-latencies *latency*)))
           (signal-output *conflict-set-trace* "~D productions out of ~D considered; expected gain of chosen is: ~6,3F"
                          index (length *conflict-set*)
                          (when *instantiation* (instantiation-gain *instantiation*))))
          (t
           (signal-output *conflict-set-trace* "~D instantiations in the conflict set" (length *conflict-set*))
           (setf *instantiation* (first *conflict-set*))))
    (when (and *instantiation* *production-trace*)
      (pprint-instantiation *instantiation* *production-trace*))
    (values *instantiation* *latency* failed-latencies)))

(defun matching-cycle (&optional (cycles nil) (retrieval-threshold *retrieval-threshold*) 
                                 (utility-threshold *utility-threshold*))
  "Implements the production matching (lhs) cycle called by run-fct and the PM scheduler.
   Cycles is the time limit for the matching cycle if applicable.
   Returns the latency of the action phase if any, nil otherwise."
  (let ((matching-latency 0.0)
        (failed-latencies 0.0)
        (action nil))
    (when (and *retrieval-threshold* *activation-noise* *threshold-noise*)
      (let ((noise (noise *activation-noise*)))
        (setf *retrieval-threshold* (+ retrieval-threshold noise))
        (signal-output *activation-trace* "   Adding noise ~6,3F to retrieval threshold for a total of ~6,3F"
                       noise *retrieval-threshold*)))
    (when (and *utility-threshold* *exp-gain-noise* *threshold-noise*)
      (let ((noise (noise *exp-gain-noise*)))
        (setf *utility-threshold* (+ utility-threshold noise))
        (signal-output (or *exact-matching-trace* *partial-matching-trace*)
                       "   Adding noise ~6,3F to utility threshold for a total of ~6,3F"
                       noise *utility-threshold*)))
    (generate-all-instantiations)
    (when *matches-trace*
      (pprint-instantiations *conflict-set* *matches-trace* *matches-trace*))
    (setf *instantiation* nil)
    (when *conflict-set-hook-fn*
      (multiple-value-bind (instantiations latency)
                           (funcall *conflict-set-hook-fn* *conflict-set*)
        (declare (ignore latency))
        (when instantiations ; if returns nil, then continue unchanged
          (cond ((listp instantiations) ; if returns a list, then interpret as new cset
                 (setf *conflict-set* instantiations))
                (t ; otherwise, restrict the conflict set to selected instantiation
                 (setf *conflict-set* (list instantiations)))))))
    (unless *instantiation*        
      (multiple-value-setq (*instantiation* matching-latency failed-latencies)
        (choose-instantiation)))
    ;;; the following used to be part of the action cycle but really belongs in matching
    (cond (*instantiation*
           (let ((production (instantiation-production *instantiation*)))
             (cond ((and *abort-instantiation* *enable-rational-analysis*
                         (floatp cycles)
                         (> (+ *time* matching-latency)
                            cycles))
                    (signal-output *latency-trace* "Time ~6,3F: ~A Aborted" *time* production))
                   (t
                    (when (or *base-level-learning* *associative-learning*
                              *strength-learning*)
                      (learn-matching))
                    (when *enable-rational-analysis*
                      (incf *time* matching-latency)
                      (when (plusp matching-latency)
                        (signal-output *latency-trace* "Latency ~6,3F: ~A Matching"
                                       matching-latency production)))
                    (signal-output *cycle-trace* "Time ~6,3F: ~A Selected" *time* production)
                    (setf (instantiation-latency *instantiation*)
                          (- matching-latency failed-latencies))
                    (setf action (production-action-cost production))))))
          ((and *enable-rational-analysis* *pop-upon-failure* (null *retrieval-scheduler*))
           (incf *time* matching-latency)
           (when (plusp matching-latency)
             (signal-output *latency-trace* "Latency ~6,3F: Failure Matching"
                            matching-latency))
           (setf action *default-action-time*))
          (t
           (signal-output *latency-trace* "Time ~6,3F: No Instantiation Found." *time*)))
    action))

(defun action-cycle (action-time)
  "Implements the production action (rhs) cycle called by run-fct and the PM scheduler.
   The argument action-time holds the latency of the action cycle"
  ;; Reset *latency* for the action retrievals
  (setf *latency* 0.0)
  (cond (*instantiation*
         (let* ((production (instantiation-production *instantiation*))
                (success (< (random 1.0)
                            (production-action-probability production))))
           (when (member (production-name production) *break-productions*
                         :test #'eq)
             (pprint-instantiation)
             (break "Production ~S is about to fire.~%" production))
           ;; increment time by action latency before production firing
           (incf *time* action-time)
           (signal-output *latency-trace* "Latency ~6,3F: ~A Action"
                          action-time production)
           (cond (success
                  (when *enable-production-learning* (store-instantiation *instantiation*))
                  (when *firing-hook-fn*
                    (funcall *firing-hook-fn* *instantiation*))
                  (dolist (rhs-call (production-rhs production))
                    (simulate-call rhs-call))
                  (signal-output *cycle-trace* "Time ~6,3F: ~A Fired" *time* production)
                  (when *enable-rational-analysis*
                    (update-activation-spread)))
                 (t
                  (signal-output *cycle-trace* "Time ~6,3F: ~A Fails" *time* production)))
           (when *parameters-learning*
             (learn-parameters (get-functional-parameter (production-success production))
                               (get-functional-parameter (production-failure production))
                               :latency (+ (instantiation-latency *instantiation*)
                                           action-time)))))
        (t
         (when *goal-stack*
           (let ((failure (get-wme 'failure)))
             (unless failure ; if failure isn't yet defined then add it
               (add-dm-fct '((failure isa error condition failure)) :reset-ia nil)
               (setf failure (get-wme 'failure)))
             (dolist (return (goal-frame-return-values (first *goal-stack*)))
               (set-slot-value *wmfocus* (first return) failure))))
         (incf *time* action-time)
         (signal-output *latency-trace* "Latency ~6,3F: Failure Action"
                        action-time)
         (signal-output *cycle-trace* "Time ~6,3F: ~A Popped in Failure" *wmfocus* *time*)
         (pop-fct "failure")
         (when *parameters-learning*
           (learn-parameters nil t :instantiation nil)))))

(defun retrieval-event (&optional (time *time*))
  "Sets the retrieval if the current time is past the retrieval time.  If the time passed
   as argument is after the current time, this implements some lookahead capability."
  (when (and *retrieval-scheduler* (>= time (car *retrieval-scheduler*)))
    (signal-output *cycle-trace* "Time ~6,3F: ~A Retrieved"
                   (car *retrieval-scheduler*) (cdr *retrieval-scheduler*))
    (setf *retrieval* (cdr *retrieval-scheduler*))
    (setf *retrieval-scheduler* nil))
  )

(defun run-fct (&optional (cycles -1))
  (let ((start-time *time*)
        (start-cycle *cycle*)
        (retrieval-threshold *retrieval-threshold*)
        (utility-threshold *utility-threshold*))
    (cond ((integerp cycles) (incf cycles *cycle*))
          ((floatp cycles) (incf cycles *time*))
          (t (signal-warn "ARGUMENT TO RUN COMMAND MUST BE A NUMBER.")))
    (loop
      ;;; sets the time of the start of the production cycle
      (setf *start-time* *time*)
      (retrieval-event)
      (cond (*stop*
             (signal-output *latency-trace* "Time ~6,3F: Stop Requested" *time*)
             (setf *stop* nil)
             (return))
;            ((null *wmfocus*)
;             (signal-output *latency-trace* "Time ~6,3F: No Goal" *time*)
;             (return))
            ((and (integerp cycles) (= *cycle* cycles))
             (signal-output *latency-trace* "Time ~6,3F: Stopped by Run Cycle Limit" *time*)
             (return))
            ((and (floatp cycles) (>= *time* cycles))
             (signal-output *latency-trace* "Time ~6,3F: Stopped by Run Time Limit" *time*)
             (return))
            (t))
      (save-state-change :run *time* *cycle* *spread-stamp* (make-random-state))
      ;;; production cycle
      (let ((action-time (matching-cycle cycles retrieval-threshold utility-threshold)))
        (cond (action-time ;;; if production left-hand side match
               (retrieval-event (+ *time* action-time))  ;;; for retrievals during action
               (action-cycle action-time))  ;;; then execute the right-hand side action
              (*retrieval-scheduler*  ;;; if retrieval event pending
               (setf *time* (if (floatp cycles)  ;; schedule if before end of run quantum
                              (min cycles (car *retrieval-scheduler*))
                              (car *retrieval-scheduler*))))
              (t (return))))        ;;; else quit
      (incf *cycle* 1)
      (when *cycle-hook-fn* (funcall *cycle-hook-fn* *instantiation*))
      (when *web-hook-fn* (funcall *web-hook-fn* *instantiation*))
      )
    ;; Reset at the end because of aborted cycles
    (setf *retrieval-threshold* retrieval-threshold)
    (setf *utility-threshold* utility-threshold)
    (when *end-run-hook-fn* (funcall *end-run-hook-fn* (- *time* start-time)))
    (signal-output *latency-trace* "Run Latency: ~6,3F" (- *time* start-time))
    (values (* 0.001 (round (- *time* start-time) 0.001)) (- *cycle* start-cycle))))

(defun step-fct (conflict-set)
  "Asks the user to choose between step, stop, run, or select."
  (loop
    (signal-output *command-trace* "Another step?  [Y] step, [N] stop, [R] run, [#] select: ")
    (let ((input (read)))
      (cond ((eq input 'y)
             (return conflict-set))
            ((eq input 'n)
             (setf *stop* t)
             (return conflict-set))
            ((eq input 'r)
             (setf *conflict-set-hook-fn* nil)
             (setf *matches-trace* nil)
             (return conflict-set))
            ((and (integerp input)
                  (> input 0) (<= input (length conflict-set)))
             (return (nth (- input 1) conflict-set)))
            (t
             (signal-output *command-trace* "Unknown entry ~S"
                            input))))))

(defun pstep-fct (&optional (cycles -1))
  "Runs for a specified number of cycles (if integer) or time (if real)
   or forever if no value specified.  At each cycle, displays the instantiations
   with matches trace, then allow the user to decide what to do next."
  (let ((*matches-trace* (or *matches-trace* t))
        (*verbose* t)
        (*conflict-set-hook-fn* *step-fn*))
    (run-fct cycles)))

(defun run-many-fct (&optional (n 1))
  "Runs the whole wmfocus list n times."
  (dotimes (i n)
    (dolist (goal *wmfocus-list*)
      (set-wmfocus :wme goal :spread t)
      (run-fct))))


;;; Setting of global parameters

(defmacro assign-stream (parameter value)
  "Handles the (de)assigning of streams to traces."
  `(progn
     (when (streamp ,parameter)
       (close ,parameter))
     (when (or (stringp ,value) (pathnamep ,value))
       (setf ,value (open ,value :direction :output
                          :if-exists :append :if-does-not-exist :create)))))

(defmacro sigp (parameter par test warning &rest housekeeping)
  "Sets individual global parameter.  Tests that value (if not no-set)
   satisfies test before setting parameter and doing housekeeping,
   otherwise issue warning."
  `(cond (,test
          (setf ,parameter value)
          ,@housekeeping
          value)
         (t
          (signal-warn "GLOBAL PARAMETER ~A CANNOT TAKE VALUE ~S BECAUSE IT MUST BE ~A."
                       ,par value ,warning)
          :error)))

(defmacro variance-to-s (variance)
  "Given a variance, returns the corresponding value."
  `(when ,variance (/ (sqrt (* 3.0 ,variance)) 3.1416)))

(defmacro s-to-variance (s)
  "Given s, returns the corresponding variance."
  `(when ,s
     (let ((pis (* 3.1416 ,s)))
       (/ (* pis pis) 3.0))))

(defun global-parameters-fct (&optional parameters)
  "Return the value of the global parameters, or print them all if none specified."
  (let* ((verbose *verbose*)
         (*verbose* t)
         (*goal-depth* 1)
         (value nil)
         (values nil))
    (cond (parameters
           (dolist (parameter parameters values)
             (setf value
                   (case parameter
                     ((:esc :era) *enable-rational-analysis*)
                     (:g *g*)
                     (:egn (s-to-variance *exp-gain-noise*))
                     (:egs *exp-gain-noise*)
                     (:er *enable-randomness*)
                     (:ut *utility-threshold*)
                     (:ga *goal-activation*)
                     (:blc *base-level-constant*)
                     (:an (s-to-variance *activation-noise*))
                     (:ans *activation-noise*)
                     (:pan (s-to-variance *permanent-activation-noise*))
                     (:pas *permanent-activation-noise*)
                     (:lf *latency-factor*)
                     (:le *latency-exponent*)
                     (:dat *default-action-time*)
                     (:pm *partial-matching*)
                     (:mp *mismatch-penalty*)
                     (:ms *max-sim*)
                     (:md *max-dif*)
                     (:mas *maximum-associative-strength*)
                     (:rt *retrieval-threshold*)
                     (:tmp *temperature*)
                     (:bln *blending*)
                     (:cp *cost-penalty*)
                     (:ie *initial-experience*)
                     (:tt *threshold-time*)
                     (:epl *enable-production-learning*)
                     (:ol *optimized-learning*)
                     (:bll *base-level-learning*)
                     (:al *associative-learning*)
                     (:sl *strength-learning*)
                     (:pl *parameters-learning*)
                     (:emt *exact-matching-trace*)
                     (:pmt *partial-matching-trace*)
                     (:pct *production-compilation-trace*)
                     (:act *activation-trace*)
                     (:blt *blending-trace*)
                     (:crt *conflict-resolution-trace*)
                     (:cst *conflict-set-trace*)
                     (:mt *matches-trace*)
                     (:pt *production-trace*)
                     (:ct *cycle-trace*)
                     (:lt *latency-trace*)
                     (:ot *output-trace*)
                     (:dmt *dm-trace*)
                     (:gt *goal-trace*)
                     (:v verbose)
                     (t (signal-warn "NO GLOBAL PARAMETER ~A DEFINED" parameter)
                        :error)))
             (push-last value values)
             (signal-output *command-trace* "~S ~6,3F" parameter value)))
          (t
           (setf values
                 (list *enable-rational-analysis* *g* *exp-gain-noise*
                       *enable-randomness* *utility-threshold*
                       *goal-activation* *base-level-constant*
                       *activation-noise* *permanent-activation-noise*
                       *latency-factor* *latency-exponent* *default-action-time*
                       *partial-matching* *mismatch-penalty* *max-sim* *max-dif*
                       *maximum-associative-strength* *retrieval-threshold* *temperature* *blending*
                       *optimized-learning* *base-level-learning* *associative-learning*
                       *strength-learning* *parameters-learning*
                       *cost-penalty* *initial-experience* *threshold-time* *enable-production-learning*
                       *exact-matching-trace* *partial-matching-trace* *production-compilation-trace*
                       *activation-trace* *blending-trace*
                       *conflict-resolution-trace* *conflict-set-trace*
                       *matches-trace* *production-trace* *cycle-trace* *latency-trace*
                       *output-trace* *dm-trace* *goal-trace* verbose))
           (signal-output *command-trace* "~?"
                          "Enable Subsymbolic Computations (esc): ~S~% G (g): ~S~%~
                           ~1TExpected Gain S (egs): ~S~% Enable Randomness (er): ~S~%~
                           ~1TUtility Threshold (ut): ~S~%~
                           ~1T--------------------~%~
                           ~1TGoal Activation (ga): ~S~% Base Level Constant (blc): ~S~%~
                           ~1TActivation Noise S (ans): ~S~% Permanent Activation S (pas): ~S~%~
                           ~1T--------------------~%~
                           ~1TLatency Factor (lf): ~S~%~
                           ~1TLatency Exponent (le): ~S~% Default Action Time (dat): ~S~%~
                           ~1T--------------------~%~
                           ~1TPartial Matching (pm): ~S~% Mismatch Penalty (mp): ~S~%~
                           ~1TMaximum Similarity (ms): ~S~% Maximum Difference (md): ~S~%~
                           ~1TMaximum Associative Strength (mas): ~S~% Retrieval Threshold (rt): ~S~%~
                           ~1TTemperature (tmp): ~S~% Blending (bln): ~S~%~
                           ~1T--------------------~%~
                           ~1TOptimized Learning (ol): ~S~% Base Level Learning (bll): ~S~%~
                           ~1TAssociative Learning (al): ~S~% Strength Learning (sl): ~S~%~
                           ~1TParameters Learning (pl): ~S~% Cost Penalty (cp): ~S~%~
                           ~1TInitial Experience (ie): ~S~% Threshold Time (tt): ~S~% Enable Production Learning (epl): ~S~%~
                           ~1T--------------------~%~
                           ~1TExact Matching Trace (emt): ~S~% Partial Matching Trace (pmt): ~S~%~
                           ~1TProduction Compilation Trace (pct): ~S~%~
                           ~1TActivation Trace (act): ~S~% Blending Trace (blt): ~S~%~
                           ~1TConflict Resolution Trace (crt): ~S~% Conflict Set Trace (cst): ~S~%~
                           ~1TMatches Trace (mt): ~S~% Production Trace (pt): ~S~% Cycle Trace (ct): ~S~%~
                           ~1TLatency Trace (lt): ~S~% Output Trace (ot): ~S~%~
                           ~1TDeclarative Memory Trace (dmt): ~S~% Goal Trace (gt): ~S~% Verbose (v): ~S~%"
                          values)
           nil))))

(defun set-global-parameters-fct (parameters)
  "Sets global parameters."
  (let ((values nil)
        (parameter nil)
        (value nil))
    (loop
      (unless parameters (return values))
      (setf parameter (pop parameters))
      (setf value (pop parameters))
      (when (and (listp value) (eq (first value) 'quote))
        (setf value (second value)))  ;; for compatibility with evaluating versions
      (push-last
       (case parameter
         (:dat (sigp *default-action-time* :dat (and (numberp value) (>= value 0.0)) "a non-negative number"))
         (:ga  (sigp *goal-activation* :ga (and (numberp value) (>= value 0.0)) "a non-negative number"
                     (update-activation-spread)))
         (:g (sigp *g* :g (and (numberp value) (>= value 0.0)) "a non-negative number"
                   (all-pg-c)))
         (:egn (sigp *exp-gain-noise* :egn (or (null value) (and (numberp value) (>= value 0.0))) "either NIL or a non-negative number"
                     (setf *exp-gain-noise* (variance-to-s *exp-gain-noise*))))               
         (:egs (sigp *exp-gain-noise* :egs (or (null value) (and (numberp value) (>= value 0.0))) "either NIL or a non-negative number"))
         (:ut (sigp *utility-threshold* :ut (or (null value) (numberp value)) "NIL or a number"))
         (:lf (sigp *latency-factor* :lf (and (numberp value) (>= value 0.0)) "a non-negative number"))
         (:le (sigp *latency-exponent* :le (and (numberp value) (>= value 0.0)) "a non-negative number"))
         (:blc (let ((old-blc *base-level-constant*))
                 (sigp *base-level-constant* :blc (numberp value) "a number"
                       (let ((base-level-inc (- value old-blc)))
                         (for-all-wmes wme
                                       (incf (wme-base-level wme) base-level-inc)
                                       (incf (wme-activation wme) base-level-inc))))))
         (:an (sigp *activation-noise* :an (or (null value) (and (numberp value) (>= value 0.0))) "either NIL or a non-negative number"
                    (progn
                      (setf *activation-noise* (variance-to-s *activation-noise*))
                      (for-all-wmes wme (decf (wme-time-stamp wme) 1.0)))))
         (:ans (sigp *activation-noise* :as (or (null value) (and (numberp value) (>= value 0.0))) "either NIL or a non-negative number"
                    (for-all-wmes wme (decf (wme-time-stamp wme) 1.0))))
         (:pan (sigp *permanent-activation-noise* :pan (or (null value) (and (numberp value) (>= value 0.0))) "either NIL or a non-negative number"
                     (progn
                       (setf *permanent-activation-noise*
                             (variance-to-s *permanent-activation-noise*))                       
                       (for-all-wmes wme
                                     (let ((noise (if value (noise value) 0.0)))
                                       (incf (wme-activation wme)
                                             (- noise (wme-permanent-noise wme)))
                                       (setf (wme-permanent-noise wme) noise))))))
         (:pas (sigp *permanent-activation-noise* :pas (or (null value) (and (numberp value) (>= value 0.0))) "either NIL or a non-negative number"
                     (for-all-wmes wme
                                   (let ((noise (if value (noise value) 0.0)))
                                     (incf (wme-activation wme)
                                           (- noise (wme-permanent-noise wme)))
                                     (setf (wme-permanent-noise wme) noise)))))
         (:mas (sigp *maximum-associative-strength* :mas (or (null value) (numberp value)) "either NIL or a number"
                     (reset-ia-fct)))
         (:mp (sigp *mismatch-penalty* :mp (and (numberp value) (>= value 0.0)) "a non-negative number"))
         (:ms (sigp *max-sim* :ms (numberp value) "a number"
                    (for-all-wmes wme (set-similarity wme wme *max-sim*))))
         (:md (sigp *max-dif* :md (numberp value) "a number"))
         (:tmp (sigp *temperature* :tmp (or (null value) (and (numberp value) (> value 0.0))) "NIL or a positive number"))
         (:bln (sigp *blending* :bln (or (null value) (eq value t) (eq value 'rt)) "NIL, T or RT"))
         ; FIX: switch representation when :era is activated.
         ; Make sure to not re-apply for a noop.
         ((:esc :era)
          (unless (eq *enable-rational-analysis* value)
            ;;; No guarantee of goal or ordering but use of special function for buffers:
            ;;; use nsublis for all of the lhs AND the rhs!!
            (sigp *enable-rational-analysis* :era (or (null value) (eq value t)) "T or NIL"
                  (if *enable-rational-analysis*
                    (let ((substs '((direct-test-and-bind . direct-test-and-bind-pm)
                                    (indirect-test-and-bind . indirect-test-and-bind-pm))))
                      (dolist (production *procedural-memory*)
                        (let* ((lhs (production-lhs (cdr production)))
                               (direct-length (first-retrieval-index lhs (production-initializations (cdr production)))))
                          (setf (production-lhs (cdr production))
                                (cons (nconc (subseq lhs 0 direct-length) (last lhs))
                                      (nconc (butlast (subseq lhs direct-length))
                                             (list 'not))))
                          (setf (production-lhs (cdr production))
                                (nsublis substs (production-lhs (cdr production)) :test #'eq))
                          (setf (production-rhs (cdr production))
                                (nsublis substs (production-rhs (cdr production)) :test #'eq))
                          ))
                      (update-activation-spread)
                      (unless *retrieval-threshold*
                        (sgp-fct '(:rt 0.0))))
                    (let ((substs '((direct-test-and-bind-pm . direct-test-and-bind)
                                    (indirect-test-and-bind-pm . indirect-test-and-bind))))
                      (dolist (production *procedural-memory*)
                        (let ((lhs (production-lhs (cdr production))))
                          (setf (production-lhs (cdr production))
                                (nconc (butlast (first lhs)) (butlast (rest lhs))
                                       (last (first lhs)))))
                          (setf (production-lhs (cdr production))
                                (nsublis substs (production-lhs (cdr production)) :test #'eq))
                          (setf (production-rhs (cdr production))
                                (nsublis substs (production-rhs (cdr production)) :test #'eq))
                        )
                      (sgp-fct '(:rt nil)))))))
         ; FIX: call sgp-fct recursively instead of setting :era directly
         (:rt (sigp *retrieval-threshold* :rt (or (null value) (numberp value)) "NIL or a number"
                    (if *retrieval-threshold*
                      (unless *enable-rational-analysis*
                        (sgp-fct '(:era t)))
                      (when *enable-rational-analysis*
                        (sgp-fct '(:era nil))))))
         ; FIX: now switch representation on :era rather than :pm
         (:pm (sigp *partial-matching* :pm (or (null value) (eq value t)) "T or NIL"))
         (:er (sigp *enable-randomness* :er (or (null value) (eq value t)) "T or NIL"))
         (:ol (sigp *optimized-learning* :ol (or (null value) (eq value t) (and (integerp value) (plusp value))) "T, NIL or a positive integer"
                    (for-all-wmes wme
                                  (adapt-references (wme-references wme)
                                                    (wme-creation-time wme))
                                  (decf (wme-time-stamp wme) 1.0))
                    (dolist (production *procedural-memory*)
                      (adapt-references (production-references (cdr production))
                                        (production-creation-time (cdr production)))
                      (decf (production-time-stamp (cdr production)) 1.0))))
         (:bll (let ((old-bll *base-level-learning*))
                 (sigp *base-level-learning* :bll (or (null value) (and (numberp value) (>= value 0.0))) "either NIL or a non-negative number"
                       (if (and old-bll (null value))
                         ;;; when turning bll off, update all base levels
                         (let ((*base-level-learning* old-bll))
                           (for-all-wmes wme (compute-base-level-activation wme)))
                         (for-all-wmes wme (decf (wme-time-stamp wme) 1.0))))))
         (:al (let ((old-al *associative-learning*))
                (sigp *associative-learning* :al (or (null value) (and (numberp value) (>= value 0.0))) "either NIL or a non-negative number"
                      (when (and old-al (null value))
                        ;;; when turning al off, update all IAs
                        (let ((*associative-learning* old-al))
                          (for-all-wmes wme
                                        (dolist (wme-ia (wme-ias wme))
                                          (ia-value (cdr wme-ia) (car wme-ia) wme)))))
                      (incf *spread-stamp* 1))))
         (:sl (let ((old-sl *strength-learning*))
                (sigp *strength-learning* :sl (or (null value) (and (numberp value) (>= value 0.0))) "either NIL or a non-negative number"
                      (when (and old-sl (null value))
                        ;;; when turning sl off, update all production strengths
                        (let ((*strength-learning* old-sl))
                          (dolist (production *procedural-memory*)
                            (strength (cdr production)))))
                      (dolist (production *procedural-memory*)
                        (decf (production-time-stamp (cdr production)) 1.0)))))
         (:pl (sigp *parameters-learning* :pl (or (null value) (eq value t) (numberp value)) "T, NIL or a NUMBER"
                    (when *parameters-learning*
                      (dolist (production *procedural-memory*)
                        (setf production (cdr production))
                        (adapt-references (production-successes production) (production-creation-time production)
                                          (not (numberp *parameters-learning*)))
                        (adapt-references (production-failures production) (production-creation-time production)
                                          (not (numberp *parameters-learning*)))
                        (when (numberp *parameters-learning*)
                          (let ((successes (round (first (production-successes production))))
                                (efforts (first (production-efforts production))))
                            (setf (production-efforts production)
                                  (cons efforts (make-list successes :initial-element (/ efforts successes))))))
                        (recompute-production-parameters production)))))
         (:cp (sigp *cost-penalty* :cp (numberp value) "a number"))
         (:ie (sigp *initial-experience* :ie (and (numberp value) (>= value 0.0)) "a non-negative number"))
         (:tt (sigp *threshold-time* :tt (and (numberp value) (>= value 0.0)) "a non-negative number")) 
         (:epl (sigp *enable-production-learning* :epl (or (null value) (eq value t)) "T or NIL"))
         (:ot (assign-stream *output-trace* value)
              (sigp *output-trace* :ot (or (null value) (eq value t) (streamp value)) "T, NIL or a valid PATHNAME"))
         (:ct (assign-stream *cycle-trace* value)
              (sigp *cycle-trace* :ct (or (null value) (eq value t) (streamp value)) "T, NIL or a valid PATHNAME"))
         (:lt (assign-stream *latency-trace* value)
              (sigp *latency-trace* :lt (or (null value) (eq value t) (streamp value)) "T, NIL or a valid PATHNAME"))
         (:pmt (assign-stream *partial-matching-trace* value)
               (sigp *partial-matching-trace* :pmt  (or (null value) (eq value t) (streamp value)) "T, NIL or a valid PATHNAME"))
         (:pct (assign-stream *production-compilation-trace* value)
              (sigp *production-compilation-trace* :at (or (null value) (eq value t) (streamp value)) "T, NIL or a valid PATHNAME"))
         (:act (assign-stream *activation-trace* value)
               (sigp *activation-trace* :act (or (null value) (eq value t) (streamp value)) "T, NIL or a valid PATHNAME"
                     (when *activation-trace*
                       (sgp-fct '(:era t)))))
         (:blt (assign-stream *blending-trace* value)
               (sigp *blending-trace* :blt  (or (null value) (eq value t) (streamp value)) "T, NIL or a valid PATHNAME"))
         (:crt (assign-stream *conflict-resolution-trace* value)
               (sigp *conflict-resolution-trace* :crt (or (null value) (eq value t) (streamp value)) "T, NIL or a valid PATHNAME"))
         (:cst (assign-stream *conflict-set-trace* value)
               (sigp *conflict-set-trace* :cst (or (null value) (eq value t) (streamp value)) "T, NIL or a valid PATHNAME"))
         (:gt (assign-stream *goal-trace* value)
              (sigp *goal-trace* :gt (or (null value) (eq value t) (streamp value)) "T, NIL or a valid PATHNAME"))
         (:dmt (assign-stream *dm-trace* value)
               (sigp *dm-trace* :dmt (or (null value) (eq value t) (streamp value)) "T, NIL or a valid PATHNAME"))
         (:pt (assign-stream *production-trace* value)
              (sigp *production-trace* :pt (or (null value) (eq value t) (eq value 'short) (streamp value))
                    "T, SHORT, NIL or a valid PATHNAME"))
         (:mt (assign-stream *matches-trace* value)
              (sigp *matches-trace* :mt (or (null value) (eq value t) (eq value 'short) (streamp value))
                    "T, SHORT, NIL or a valid PATHNAME"))
         (:emt (assign-stream *exact-matching-trace* value)
               (sigp *exact-matching-trace* :emt (or (null value) (eq value t) (streamp value)) "T, NIL or a valid PATHNAME"))
         (:v (sigp *verbose* :v (or (null value) (eq value t)) "T or NIL"))
         (t (signal-warn "NO GLOBAL PARAMETER ~A DEFINED" parameter)
            :error))
       values))))

(defun sgp-fct (&optional parameters)
  "Inspects and sets global parameters."
  (if (and (rest parameters)
           (not (keywordp (second parameters))))
    (set-global-parameters-fct parameters)
    (global-parameters-fct parameters)))

(defun pset-fct (sets)
  "Prints, defines and activates sets of parameters."
  (let ((*verbose* t)
        (*goal-depth* 1))
    (if (null sets)
      ;; if no argument is given, then displays the current sets
      (dolist (set *parameter-sets*)
        (signal-output *command-trace* "Parameter set ~S:" (first set))
        (dolist (parameter (rest set))
          (signal-output *command-trace* "~S" parameter))
        (signal-output *command-trace* ""))
      (if (listp sets) ; standard format through pset command
        (if (and (atom (first sets)) (null (rest sets)))
          ;; if a single name is given, then activate that set
          (let ((set (assoc (first sets) *parameter-sets* :test #'equal)))
            (if set
              (dolist (parameter (rest set))
                (cond ((fboundp (first parameter))
                       (eval parameter))
                      ((get-production (first parameter))
                       ;; interpreted as parameters command by default
                       (parameters-fct (first parameter) (rest parameter)))
                      (t
                       (signal-warn "UNKNOWN PARAMETER COMMAND ~S IN SET ~S"
                                    parameter (first sets)))))
              (signal-warn "UNKNOWN PARAMETER SET ~S" (first sets))))
          ; if a list or lists are given, then create the new set(s)
          (dolist (set (if (atom (first sets)) (list sets) sets))
            (let ((old-set (assoc (first set) *parameter-sets* :test #'equal)))
              (cond (old-set
                     (signal-warn "REDEFINING PARAMETER SET ~S" (first set))
                     (rplacd old-set (rest set)))
                    (t
                     (push-last set *parameter-sets*))))))
        (signal-warn "UNKNOWN ARGUMENT FORMAT ~S FOR PSET COMMAND" sets)))))

(defun set-g-fct (g &key threshold)
  "Sets G and perhaps its threshold.  Provided for compatibility only.  Use sgp."
  (sgp-fct (list :g g))
  (when threshold (sgp-fct (list :gth threshold))))

(defun output-stream-fct (file &key echo)
  "Switches output trace to file."
  (if (or (stringp file) (pathnamep file))
    (let ((stream (open file :direction :output
                        :if-exists :append :if-does-not-exist :create)))
      (when echo
        (setf stream (make-broadcast-stream stream *standard-output*)))
      (sgp-fct (list :ot stream))
      stream)
    (signal-warn "ARGUMENT ~S TO OUTPUT-STREAM SHOULD BE A VALID FILENAME."
                 file)))

(defun close-output-fct ()
  "Closes the output trace stream."
  (sgp-fct (list :ot t)))

(defun trace-stream-fct (file &key echo)
  "Switches all trace outputs to file."
  (if (or (stringp file) (pathnamep file))
    (let ((stream (open file :direction :output
                        :if-exists :append :if-does-not-exist :create)))
      (when echo
        (setf stream (make-broadcast-stream stream *standard-output*)))
      (sgp-fct (list :ct (and *cycle-trace* stream)
                     :lt (and *latency-trace* stream)
                     :pmt (and *partial-matching-trace* stream)
                     :pct (and *production-compilation-trace* stream)
                     :act (and *activation-trace* stream)
                     :blt (and *blending-trace* stream)
                     :crt (and *conflict-resolution-trace* stream)
                     :cst (and *conflict-set-trace* stream)
                     :gt (and *goal-trace* stream)
                     :dmt (and *dm-trace* stream)
                     :pt (and *production-trace* stream)
                     :mt (and *matches-trace* stream)
                     :emt (and *exact-matching-trace* stream)))
      stream)
    (signal-warn "ARGUMENT ~S TO TRACE-STREAM SHOULD BE A VALID FILENAME."
                 file)))

(defun close-trace-fct ()
  "Closes the trace stream."
  (let ((stream t))
    (sgp-fct (list :ct (and *cycle-trace* stream)
                   :lt (and *latency-trace* stream)
                   :pmt (and *partial-matching-trace* stream)
                   :pct (and *production-compilation-trace* stream)
                   :act (and *activation-trace* stream)
                   :blt (and *blending-trace* stream)
                   :crt (and *conflict-resolution-trace* stream)
                   :cst (and *conflict-set-trace* stream)
                   :gt (and *goal-trace* stream)
                   :dmt (and *dm-trace* stream)
                   :pt (and *production-trace* stream)
                   :mt (and *matches-trace* stream)
                   :emt (and *exact-matching-trace* stream)))))

(defun construct-pathname (file directory)
  "Constructs a complete pathname from file and directory."
  (make-pathname :host (pathname-host directory)
                 :device (pathname-device directory)
                 :directory (pathname-directory directory)
                 :name (pathname-name file)
                 :type (pathname-type file)))

(defun load-model-list (file &optional (save nil))
  (with-open-file (model file)
    (loop
      (let ((expr (read model nil :end)))
        (when (eq expr :end) (return))
        (when (and (not save) (listp expr)
                   (member (first expr)
                           '(clearall clearall-fct
                             clear-all clear-all-fct)))
          (setf save t))
        (when save
          (cond ((eq (first expr) 'load)
                 (load-model-list (second expr) t))
                ((eq (first expr) 'load-model)
                 (load-model-list
                  (construct-pathname (second expr)
                                      (or (third expr) file))
                  t))
                (t
                 (push-last expr *model*))))))))

(defun seed (&optional (seed t))
  "Randomized the random number generator by running it seed times."
  (when (eq seed t)
    (setf seed (decode-universal-time (get-universal-time))))
  (cond ((integerp seed)
         (dotimes (i seed)
           (random 1.0)))
        ((random-state-p seed)
         (setf *random-state* (make-random-state seed)))
        (seed
         (signal-warn "UNKNOWN ARGUMENT ~S TO SEED FUNCTION." seed))
        (t)))

(seed)

(defun reset-fct (&optional (seed t))
  "Resets the current model by reevaluating all the commands."
  (cond ((rest *model*)
         (seed seed)
         (dolist (command (rest *model*))
           (eval command)))
        (t
         (reload-fct seed))))

(defun load-model-fct (file &optional (directory *load-pathname*))
  "Loads model file at directory, which defaults to the local folder.
   Sets *load-pathname* for those Lisps where it is not done."
  (let ((*load-pathname* (construct-pathname file directory)))
    (when (load *load-pathname* :verbose *verbose*)
      *load-pathname*)))

(defun reload-fct (&optional (seed t))
  "Updates the current model by reloading the file."
  (cond ((first *model*)
         (seed seed)
         (load (first *model*) :verbose *verbose*))
        (t
         (signal-warn "NO MODEL STORED.  LOAD THE MODEL FILE BEFORE USING RESET OR RELOAD.~%~
                       DO NOT LOAD MODEL BY EVALUATING THE BUFFER."))))

(defun import-model-fct (file)
  "Compresses a bunch of environment files into file, which has to be a string
   of the form <directory>:<file> where <directory> is where the files are located
   and <file> is their name minus the extensions (Chunk Types, Chunks, Productions)."
  (let ((directory (directory-namestring file))
        (name (file-namestring file))
        (lines nil))
    (with-open-file (common file :direction :output :if-exists :supersede
                            :if-does-not-exist :create)
      ; first the code
      (with-open-file (misc (make-pathname :directory directory
                                           :name (concatenate  'string name " Misc"))
                            :direction :input :if-does-not-exist nil)
        (when misc
          (let ((code nil))
            (loop
              (let ((line (read-line misc nil :end)))
                (cond ((eq line :end) (return))
                      ((equal line "") (setf code nil))
                      ((equal (subseq line 0 4) "(def") (setf code t)))
                (if code
                  (format common "~A~%" line)
                  (push-last line lines)))))))
      (format common "~%(clear-all)~2%")
      (clear-all-fct)
      (with-open-file (global (make-pathname :directory directory
                                             :name "Global-Parameters")
                              :direction :input :if-does-not-exist nil)
        (when global
          (let ((keyword-values nil))
            (loop
              (let ((exp (read global nil :end)))
                (cond ((eq exp :end)
                       (return))
                      ((and (listp exp) (eq (first exp) 'sgp))
                       (pop exp)
                       (loop
                         (unless exp (return))
                         (let ((keyword (pop exp))
                               (value (pop exp)))
                           (unless (equal value
                                          (let ((*command-trace* nil))
                                            (first (sgp-fct (list keyword)))))
                             (push value keyword-values)
                             (push keyword keyword-values)))))
                      (t
                       (signal-warn "UNKNOWN COMMAND ~S IN GLOBAL-PARAMETERS FILE"
                                    exp)))))
            (when keyword-values
              (format common "(sgp~{ ~(~S~)~})~2%" keyword-values)))))
        (dolist (extension '("Chunk Types" "Chunks" "Productions"))
          (with-open-file (part (make-pathname :directory directory
                                               :name (concatenate 'string name " "
                                                                  extension))
                                :direction :input :if-does-not-exist nil)
            (when part
              (loop
                (let ((line (read-line part nil :end)))
                  (if (eq line :end)
                    (return)
                    (format common "~A~%" line))))
              (terpri common))))
        (dolist (line lines)
          (format common "~A~%" line)))))

(clear-all-fct nil)

(defun pundo-fct (&optional arg)
  "Undoes production firings.  If arg is nil, toggles the Save State Change mode.
   If arg is a number, undoes that number of production firings.  If arg is t,
   undo all production firings saved."
  (cond
   ((null arg)
    (setf *save-state-changes* (if *save-state-changes* nil (list :on))))
   ((or (integerp arg) (eq arg t))
    (let* ((cycles (if (eq arg t) -1 arg))
           (command nil)
           (keyword nil)
           (state-changes *save-state-changes*))
      (setf *save-state-changes* nil)
      (if state-changes
        (loop
          (when (or (zerop cycles) (eq (first state-changes) :on))
            (return arg))
          (decf cycles 1.0)
          (loop
            (setf command (pop state-changes))
            (setf keyword (pop command))
            (case keyword
              (:create-wme
               (delete-wme (first command)))
              (:set-slot-value
               (set-slot-value (first command) (second command) (third command)))
              (:delete-wme
               (let ((wme (first command)))
                 (incf *wme-number* 1)
                 (setf (get-wme (wme-name wme)) wme)
                 (push-last wme (wme-type-wmes (wme-type wme)))))
              (:delete-production
               (let ((production (first command)))
                 (push-last (cons (production-name production) production)
                            *procedural-memory*)
                 (push-last production (wme-type-productions
                                        (production-goal-type production)))))
              (:add-reference
               (let ((references (first command)))
                 (decf (first references) 1.0)
                 (pop (rest references))))
              (:compile-production
               (delete-production (first command)))
              (:penable-fct
               (pdisable-fct (first command)))
              (:pdisable-fct
               (penable-fct (first command)))
              (:focus-on
               (let ((frame (first command)))
                 (focus-macro (goal-frame-focus frame)
                              (goal-frame-return-values frame))))
              (:pop-goal
               (let ((frame (first command)))
                 (push-macro (goal-frame-focus frame) (goal-frame-return-values frame))))
              (:push-goal
               (pop-macro))
              (:run
               (when *enable-rational-analysis* (update-activation-spread))
               (setf *time* (first command))
               (setf *cycle* (second command))
               (setf *spread-stamp* (third command))
               (setf *random-state* (make-random-state (fourth command)))
               (return))
              (otherwise
               (signal-error "UNKNOWN PUNDO OPERATION ~S" keyword)))))
        (signal-warn "UNDOING MUST FIRST BE TURNED ON WITH (PUNDO)."))
      (setf *save-state-changes* state-changes))
    (for-all-wmes wme
                  (setf (wme-time-stamp wme) (- *time* 1.0))
                  (setf (wme-spread-stamp wme) (- *spread-stamp* 1)))
    (dolist (production *procedural-memory*)
      (setf (production-time-stamp (cdr production)) (- *time* 1.0)))
    *cycle*)
   (t
    (signal-warn "UNKNOWN ARGUMENT TO PUNDO: ~S" arg))))

(defun help-fct (&optional (commands nil))
  "Outputs a short description of a list of ACT-R commands,
   or the full list of commands if none is supplied."
  (let ((*verbose* t)
        (*goal-depth* 1))
    (if commands
      (dolist (command commands)
        (signal-output *command-trace* "~A" command)
        (signal-output *command-trace* "~A"
                       (documentation command 'function)))
      (dolist (command-pair *command-mappings*)
        (signal-output *command-trace* "~A"
                       (first command-pair))))))

;;;
;;; USER-LEVEL MACROS
;;;

(defmacro clear-all (&optional (save-model t))
  "Clears the whole system."
  `(clear-all-fct ',save-model))

(defmacro clear-dm ()
  "Clears working memory elements."
  `(clear-dm-fct))

(defmacro clear-productions ()
  "Clears all productions."
  `(clear-productions-fct))

(defmacro actr-time (&optional inc)
  "Returns the current act-r time, or adds inc to it if specified."
  `(actr-time-fct ',inc))

(defmacro chunk-type (&rest arguments)
  "The user-level command to define a new wme type."
  `(chunk-type-fct ',arguments))

(defmacro new-name (&optional (name "CHUNK"))
  "Returns a unique symbol as generated by gentemp based on name,
   which can be either a string, a symbol, or else defaults to CHUNK."
  `(new-name-fct ',name))

(defmacro add-dm (&rest wmes)
  "Adds the following wmes to working memory."
  `(add-dm-fct ',wmes))

(defmacro set-dm (&rest wmes)
  "Adds the following wmes to working memory."
  `(set-dm-fct ',wmes))

(defmacro copy-chunk (&rest wmes)
  "Makes copies of the wmes."
  `(copy-chunk-fct ',wmes))

(defmacro delete-chunk (&rest wmes)
  "Deletes the following wmes from working memory."
  `(delete-chunk-fct ',wmes))

(defmacro get-base-level (&rest wmes)
  "Returns the base level of wme."
  `(get-base-level-fct ',wmes))

(defmacro set-all-base-levels (references &optional (creation-time nil))
  "Sets all base level activations."
  `(set-all-base-levels-fct ',references ',creation-time))

(defmacro set-base-levels (&rest settings)
  "Sets base level activations."
  `(set-base-levels-fct ',settings))

(defmacro set-general-base-levels (&rest settings)
  "Sets base level activations.  Same as set-base-levels."
  `(set-general-base-levels-fct ',settings))

(defmacro add-ia (&rest settings)
  "Sets individual ias."
  `(add-ia-fct ',settings))

(defmacro set-ia (&rest settings)
  "Sets individual ias."
  `(set-ia-fct ',settings))

(defmacro activation-sources ()
  "Displays activation sources."
  `(activation-sources-fct))

(defmacro update-activation ()
  "Updates the activation of all wmes by recomputing it."
  `(update-activation-fct))

(defmacro chunk-slot-value (wme slot)
  "Returns the slot value of wme."
  `(chunk-slot-value-fct ',wme ',slot))

(defmacro mod-chunk (wme &rest slot-values)
  "Sets slots of wme to values."
  `(mod-chunk-fct ',wme ',slot-values))

(defmacro mod-focus (&rest slot-values)
  "Sets slots of wm focus to values."
  `(mod-focus-fct ',slot-values))

(defmacro get-name (&rest wmes-or-productions)
  "Returns the names of a list of wmes or productions."
  `(get-name-fct ',wmes-or-productions))

(defmacro dm (&rest wmes)
  "Prints the following wmes.  If none specified, print them all."
  `(dm-fct ',wmes))

(defmacro sdm (&rest slot-values)
  "Prints wmes which have values in slots."
  `(sdm-fct ',slot-values))

(defmacro pmatches ()
  "Generates and prints the instantiations which match the current wm state."
  `(pmatches-fct))

(defmacro whynot (&rest productions)
  "Tries to match production(s) with exact matching trace on."
  `(whynot-fct ',productions))

(defmacro whynot-dependency (&rest wmes)
  "Tries analogy with wme(s) as examples."
  `(whynot-dependency-fct ',wmes))

(defmacro reset-ia ()
  "Resets all the ia values, preserving user-specified and learned values."
  `(reset-ia-fct))

(defmacro ia (wmej wmei)
  "Returns the ia value between wmej and wmei, in that order."
  `(ia-fct ',wmej ',wmei))

(defmacro similarity (wmej wmei)
  "Returns the similarity between wmej and wmei, in that order."
  `(similarity-fct ',wmej ',wmei))

(defmacro set-similarities (&rest triples)
  "Sets the similarities, i.e a list of triplets containg wmej wmei and value."
  `(set-similarities-fct ',triples))

(defmacro sdp (&rest wme-parameters)
  "Inspects and sets wme parameters."
  `(sdp-fct ',wme-parameters))

(defmacro define-buffer (buffer-name variable-name &key
                                     (equal-lhs 'get-buffer-content)
                                     (plus-rhs 'create-buffer-chunk)
                                     (equal-rhs 'modify-buffer-chunk)
                                     (minus-rhs 'clear-buffer))
  `(define-buffer-fct ',buffer-name ',variable-name
     :equal-lhs ',equal-lhs :plus-rhs ',plus-rhs 
     :equal-rhs ',equal-rhs :minus-rhs ',minus-rhs))

(defmacro p (&rest definition)
  "Production definition."
  `(p-fct ',definition))

(defmacro penable (&rest productions)
  "Enables disabled productions."
  `(penable-fct ',productions))

(defmacro pdisable (&rest productions)
  "Disables productions."
  `(pdisable-fct ',productions))

(defmacro pbreak (&rest productions)
  "Sets break points for productions."
  `(pbreak-fct ',productions))

(defmacro punbreak (&rest productions)
  "Removes break points for productions."
  `(punbreak-fct ',productions))

(defmacro pp (&rest productions)
  "Prints the following productions.  If none specified, print all active ones."
  `(pp-fct ',productions))

(defmacro production-parameter (production &rest parameters)
  "Returns value of production parameters, or print them all if none is specified."
  `(production-parameter-fct ',production ',parameters))

(defmacro parameters (production &rest parameters)
  "Sets a production parameters using keyword arguments."
  `(parameters-fct ',production ',parameters))

(defmacro set-compilation-parameters (&rest params)
  "Sets the parameters for analogized productions"
  `(set-compilation-parameters-fct ',params))

(defmacro spp (&rest production-parameters)
  "Inspects and sets production parameters."
  `(spp-fct ',production-parameters))

(defmacro pset (&rest sets)
  "Prints, defines and activates sets of parameters."
  `(pset-fct ',sets))

(defmacro set-g (g &key threshold)
  "Sets G and perhaps its threshold.  Provided for compatibility only.  Use sgp."
  `(set-g-fct ',g :threshold ',threshold))

(defmacro output-stream (file &key echo)
  "Switches output trace to file."
  `(output-stream-fct ',file :echo ',echo))

(defmacro close-output ()
  "Closes the output trace stream."
  `(close-output-fct))

(defmacro trace-stream (file &key echo)
  "Switches all trace outputs to file."
  `(trace-stream-fct ',file :echo ',echo))

(defmacro close-trace ()
  "Closes the trace stream."
  `(close-trace-fct))

(defmacro rehearse-chunk (&rest chunks)
  "Rehearses chunks.  If a chunk is a list, then the first element of the list
   is the chunk to rehearse and the rest is the list of sources."
  `(rehearse-chunk-fct ',chunks))

(defmacro buffers (&rest buffers)
  "Displays and sets the contents of buffers."
  `(buffers-fct ',buffers))

(defmacro retrieval (&optional (chunk nil))
  "Displays or changes (if argument is supplied) the value of
   the last chunk retrieved."
  `(retrieval-fct ',chunk))

(defmacro goal-focus (&rest wmes)
  "Sets the focus to wme or wmes, or prints the current one if none specified."
  `(goal-focus-fct ',wmes))

(defmacro goal-stack ()
  "Prints the current list of goals on the goal stack."
  `(goal-stack-fct))

(defmacro push-goal (wme)
  "Pushes wme on top of the stack."
  `(push-goal-fct ',wme))

(defmacro pop-goal ()
  "Pops the top goal."
  `(pop-goal-fct))

(defmacro focus-on-goal (wme)
  "Pops the top goal then focuses on wme."
  `(focus-on-goal-fct ',wme))

(defmacro clear-goal-stack ()
  "Clears the goal stack by restoring the top goal."
  `(clear-goal-stack-fct))

(defmacro run (&optional (cycles -1))
  "Runs for a specified number of cycles (if integer) or time (if real)
   or forever if no value specified."
  `(run-fct ',cycles))

(defmacro pstep (&optional (cycles -1))
  "Runs for a specified number of cycles (if integer) or time (if real)
   or forever if no value specified.  At each cycle, displays the instantiations
   with matches trace, then allow the user to decide what to do next."
  `(pstep-fct ',cycles))

(defmacro run-many (&optional (n 1))
  "Runs the whole wmfocus list n times."
  `(run-many-fct ',n))

(defmacro pundo (&optional arg)
  "Undoes production firings."
  `(pundo-fct ',arg))

(defmacro reset (&optional (seed t))
  "Resets the model by reevaluating it as stored in *model*.
   Seed can be provided to control reinitialization of random number generator."
  `(reset-fct ',seed))

(defmacro load-model (file &optional (directory *load-pathname*))
  "Loads model file in directory (local by default)."
  `(load-model-fct ',file ',directory))

(defmacro reload (&optional (seed t))
  "Reloads the model by reloading the *model* file.
   Seed can be provided to control reinitialization of random number generator."
  `(reload-fct ',seed))

(defmacro import-model (file)
  "Compresses a bunch of environment files into file, which has to be a string
   of the form <directory>:<file> where <directory> is where the files are located
   and <file> is their name minus the extensions (WMETypes, Productions, etc)."
  `(import-model-fct ',file))

(defmacro help (&rest commands)
  "Outputs a short description of one or more ACT-R command(s),
   or the full list of commands if none is supplied."
  `(help-fct ',commands))

;;;
;;; OLD USER-LEVEL MACROS AND FCT FOR COMPATIBILITY PURPOSES ONLY
;;;

(defmacro clearall (&optional (save-model t))
  "See CLEAR-ALL."
  `(clear-all-fct ',save-model))

(defun clearall-fct (&optional (save-model t))
  "See CLEAR-ALL-FCT."
  (clear-all-fct save-model))

(defmacro clearwm ()
  "See CLEAR-DM."
  `(clear-dm-fct))

(defun clearwm-fct ()
  "See CLEAR-DM-FCT."
  (clear-dm-fct))

(defmacro clearproductions ()
  "See CLEAR-PRODUCTIONS."
  `(clear-productions-fct))

(defun clearproductions-fct ()
  "See CLEAR-PRODUCTIONS-FCT."
  (clear-productions-fct))

(defmacro actrtime (&optional inc)
  "See ACTR-TIME."
  `(actr-time-fct ',inc))

(defun actrtime-fct (&optional inc)
  "See ACTR-TIME-FCT."
  (actr-time-fct inc))

(defmacro wmetype (&rest arguments)
  "See CHUNK-TYPE."
  `(chunk-type-fct ',arguments))

(defun wmetype-fct (arguments)
  "See CHUNK-TYPE-FCT."
  (chunk-type-fct arguments))

(defmacro addwm (&rest wmes)
  "See ADD-DM."
  `(add-dm-fct ',wmes))

(defun addwm-fct (wmes &key (reset-ia t))
  "See ADD-DM-FCT."
  (add-dm-fct wmes :reset-ia reset-ia))

(defmacro setwm (&rest wmes)
  "See SET-DM."
  `(set-dm-fct ',wmes))

(defun setwm-fct (wmes)
  "See SET-DM-FCT."
  (set-dm-fct wmes))

(defmacro copywme (&rest wmes)
  "See COPY-CHUNK."
  `(copy-chunk-fct ',wmes))

(defun copywme-fct (wmes)
  "See COPY-CHUNK-FCT."
  (copy-chunk-fct wmes))

(defmacro deletewm (&rest wmes)
  "See DELETE-CHUNK."
  `(delete-chunk-fct ',wmes))

(defun deletewm-fct (wmes)
  "See DELETE-CHUNK-FCT."
  (delete-chunk-fct wmes))

(defmacro getbaselevel (&rest wmes)
  "See GET-BASE-LEVEL."
  `(get-base-level-fct ',wmes))

(defun getbaselevel-fct (wmes)
  "See GET-BASE-LEVEL-FCT."
  (get-base-level-fct wmes))

(defmacro setallbaselevels (references &optional (creation-time nil))
  "See SET-ALL-BASE-LEVELS."
  `(set-all-base-levels-fct ',references ',creation-time))

(defun setallbaselevels-fct (references &optional (creation-time nil))
  "See SET-ALL-BASE-LEVELS-FCT."
  (set-all-base-levels-fct references creation-time))

(defmacro setbaselevels (&rest settings)
  "See SET-BASE-LEVELS."
  `(set-base-levels-fct ',settings))

(defun setbaselevels-fct (settings)
  "See SET-BASE-LEVELS-FCT."
  (set-base-levels-fct settings))

(defmacro setgeneralbaselevels (&rest settings)
  "See SET-GENERAL-BASE-LEVELS."
  `(set-general-base-levels-fct ',settings))

(defun setgeneralbaselevels-fct (settings)
  "See SET-GENERAL-BASE-LEVELS-FCT."
  (set-general-base-levels-fct settings))

(defmacro addia (&rest settings)
  "See ADD-IA."
  `(add-ia-fct ',settings))

(defun addia-fct (settings)
  "See ADD-IA-FCT."
  (add-ia-fct settings))

(defmacro setia (&rest settings)
  "See SET-IA."
  `(set-ia-fct ',settings))

(defun setia-fct (settings)
  "See SET-IA-FCT."
  (set-ia-fct settings))

(defmacro activationsources ()
  "See ACTIVATION-SOURCES."
  `(activation-sources-fct))

(defun activationsources-fct ()
  "See ACTIVATION-SOURCES-FCT."
  (activation-sources-fct))

(defmacro wmeslotvalue (wme slot)
  "See CHUNK-SLOT-VALUE."
  `(chunk-slot-value-fct ',wme ',slot))

(defun wmeslotvalue-fct (wme slot)
  "See CHUNK-SLOT-VALUE-FCT."
  (chunk-slot-value-fct wme slot))

(defmacro modwme (wme &rest slot-values)
  "See MOD-CHUNK."
  `(mod-chunk-fct ',wme ',slot-values))

(defun modwme-fct (wme slot-values)
  "See MOD-CHUNK-FCT."
  (mod-chunk-fct wme slot-values))

(defmacro modfocus (&rest slot-values)
  "See MOD-FOCUS."
  `(mod-focus-fct ',slot-values))

(defun modfocus-fct (slot-values)
  "See MOD-FOCUS-FCT."
  (mod-focus-fct slot-values))

(defmacro wm (&rest wmes)
  "See DM."
  `(dm-fct ',wmes))

(defun wm-fct (wmes)
  "See DM-FCT."
  (dm-fct wmes))

(defmacro swm (&rest slot-values)
  "See SDM."
  `(sdm-fct ',slot-values))

(defun swm-fct (slot-values)
  "See SDM-FCT."
  (sdm-fct slot-values))

(defmacro resetia ()
  "See RESET-IA."
  `(reset-ia-fct))

(defun resetia-fct ()
  "See RESET-IA-FCT."
  (reset-ia-fct))

(defmacro setsimilarities (&rest triples)
  "See SET-SIMILARITIES."
  `(set-similarities-fct ',triples))

(defun setsimilarities-fct (triples)
  "See SET-SIMILARITIES-FCT."
  (set-similarities-fct triples))

(defmacro swp (&rest wme-parameters)
  "See SDP."
  `(sdp-fct ',wme-parameters))

(defun swp-fct (wme-parameters)
  "See SDP-FCT."
  (sdp-fct wme-parameters))

(defmacro setanalogizedparameters (&rest params)
  "See SET-ANALOGIZED-PARAMETERS."
  `(set-analogized-parameters-fct ',params))

(defmacro setg (g &key threshold)
  "See SET-G."
  `(set-g-fct ',g :threshold ',threshold))

(defun setg-fct (g &key threshold)
  "See SET-G-FCT."
  (set-g-fct g :threshold threshold))

(defmacro outputstream (file &key echo)
  "See OUTPUT-STREAM."
  `(output-stream-fct ',file :echo ',echo))

(defun outputstream-fct (file &key echo)
  "See OUTPUT-STREAM-FCT."
  (output-stream-fct file :echo echo))

(defmacro closeoutput ()
  "See CLOSE-OUTPUT."
  `(closeoutput-fct))

(defun closeoutput-fct ()
  "See CLOSE-OUTPUT-FCT."
  (close-output-fct))

(defmacro tracestream (file &key echo)
  "See TRACE-STREAM."
  `(trace-stream-fct ',file :echo ',echo))

(defun tracestream-fct (file &key echo)
  "See TRACE-STREAM-FCT."
  (trace-stream-fct file :echo echo))

(defmacro closetrace ()
  "See CLOSE-TRACE."
  `(close-trace-fct))

(defun closetrace-fct ()
  "See CLOSE-TRACE-FCT."
  (close-trace-fct))

(defmacro wmfocus (&rest wmes)
  "See GOAL-FOCUS."
  `(goal-focus-fct ',wmes))

(defun wmfocus-fct (&optional (wmes nil))
  "See GOAL-FOCUS-FCT."
  (goal-focus-fct wmes))

(defmacro goalstack ()
  "See GOAL-STACK."
  `(goal-stack-fct))

(defun goalstack-fct ()
  "See GOAL-STACK-FCT."
  (goal-stack-fct))

(defmacro push-wme (wme)
  "See PUSH-GOAL."
  `(push-goal-fct ',wme))

(defun push-wme-fct (wme)
  "See PUSH-GOAL-FCT."
  (push-goal-fct wme))

(defmacro pop-wme ()
  "See POP-GOAL."
  `(pop-goal-fct))

(defun pop-wme-fct ()
  "See POP-GOAL-FCT."
  (pop-goal-fct))

(defmacro focus-on-wme (wme)
  "See FOCUS-ON-GOAL."
  `(focus-on-goal-fct ',wme))

(defun focus-on-wme-fct (wme)
  "See FOCUS-ON-GOAL-FCT."
  (focus-on-goal-fct wme))

(defmacro cleargoalstack ()
  "See CLEAR-GOAL-STACK."
  `(clear-goal-stack-fct))

(defun cleargoalstack-fct ()
  "See CLEAR-GOAL-STACK-FCT."
  (clear-goal-stack-fct))

(defmacro whynot-analogy (&rest wmes)
  "Tries analogy with wme(s) as examples."
  `(whynot-dependency-fct ',wmes))

(defmacro set-analogized-parameters (&rest params)
  "Sets the parameters for analogized productions"
  `(set-compilation-parameters-fct ',params))


;;;
;;; This code comes from the prod-comp-update.lisp
;;; file.
;;; 
;;; Added by Dan 10/31/03 

(defun to-variable (term)
"maps a to =a"
    (let* ((string  (string-sym term))
           (bound (length string))
           (ans (make-string (+ 1 bound))))
      (setf (aref ans 0) #\=)
      (do ((count 0 (1+ count)))
          ((equal count bound) (intern ans))
        (setf (aref ans (+ count 1)) (aref string count)))))

(defun =buffer (term)
"maps term to =term>"
    (let* ((string  (string-sym term))
           (bound (length string))
           (ans (make-string (+ 2 bound))))
      (setf (aref ans 0) #\=)
      (setf (aref ans (1+ bound)) #\>)
      (do ((count 0 (1+ count)))
          ((equal count bound) (intern ans))
        (setf (aref ans (+ count 1)) (aref string count)))))

(defun +buffer (term)
"maps term to +term>"
    (let* ((string  (string-sym term))
           (bound (length string))
           (ans (make-string (+ 2 bound))))
      (setf (aref ans 0) #\+)
      (setf (aref ans (1+ bound)) #\>)
      (do ((count 0 (1+ count)))
          ((equal count bound) (intern ans))
        (setf (aref ans (+ count 1)) (aref string count)))))

(defun to-header (term)
   "maps term to term>"
    (let* ((string  (string-sym term))
           (bound (length string))
           (ans (make-string (+ 1 bound))))
      (setf (aref ans bound) #\>)
      (do ((count 0 (1+ count)))
          ((equal count bound) (intern ans))
        (setf (aref ans count) (aref string count)))))

(defun -buffer (term)
"maps term to -term>"
    (let* ((string  (string-sym term))
           (bound (length string))
           (ans (make-string (+ 2 bound))))
      (setf (aref ans 0) #\-)
      (setf (aref ans (1+ bound)) #\>)
      (do ((count 0 (1+ count)))
          ((equal count bound) (intern ans))
        (setf (aref ans (+ count 1)) (aref string count)))))


(defun string-sym (item)
    (cond ((symbolp item) (string1 item))
          (t (string1 (car (no-output (eval `(wm ,item))))))))

(defun string1 (item)
    (cond ((numberp item) (prin1-to-string item))
          (t (string item))))

(defvar *pc-speed* 0.05 ; NT: Added parameter in production compilation
  "Parameter that controls the speed of learning new productions.")

(defparameter *buffer-lis* '(goal retrieval visual-location visual visual-state manual manual-state aural aural-state
                                  aural-location vocal vocal-state
                                   intentional)
    "List of buffer names")

(defparameter *=buffer-lis* (mapcar '=buffer *buffer-lis*)
    "List of =buffer> names")

(defparameter *-buffer-lis* (mapcar '-buffer *buffer-lis*)
    "List of -buffer> names")

(defparameter *+buffer-lis* (mapcar '+buffer *buffer-lis*)
    "List of +buffer> names")

(defparameter *variable-lis* (mapcar 'to-variable *buffer-lis*)
    "List of =buffer names")

(defparameter *header* (append *=buffer-lis* *-buffer-lis* *+buffer-lis*))



(defun compute-p (p)
  "Computes the p parameter according to ACT Parameters Learning Equation."
  (let ((m (+ (first (production-successes p))(first (production-failures p)))))
    (if (< (production-creation-time p) 0.01) ; rule was there from beginning of model run
      (setf (production-p p)
            (compute-probabilities (production-successes p)
                                   (production-failures p)))
      (setf (production-p p)
            (/ (+ (* *initial-experience* (production-priorP p)) 
                  (* m
                     (compute-probabilities (production-successes p)
                                            (production-failures p))))
               (+ *initial-experience* m))))))

(defun compute-c (p)
  "Computes the c parameter according to ACT Parameters Learning Equation."
  (let ((m (+ (first (production-successes p))(first (production-failures p)))))
    (if (< (production-creation-time p) 0.01) ; rule was there from beginning of model run
      (setf (production-c p)
            (compute-costs (production-successes p) (production-failures p)
                           (production-efforts p)))
      (setf (production-c p)
            (/ (+ (* *initial-experience* (if (production-priorC p) (production-priorC p) *g*)) 
                  (* m
                     (compute-costs (production-successes p) (production-failures p)
                                    (production-efforts p))))
               (+ *initial-experience* m))))))



(defun finish-up (production identical old1 old2)

  (cond ((and (not identical) *production-compilation-trace* *verbose*) (eval `(pp ,production))))
  (no-output
  (let*
    ((initial-experience *initial-experience*)
     (effort1 (caar (eval `(spp ,old1 :effort))))
     (effort2 (caar (eval `(spp ,old2 :effort))))
     (effort (+ effort1 effort2 (- *default-action-time*))) ;we save one default action time by having one production less
     (c2 (caar (eval `(spp ,old2 :c))))
     (p2 (caar (eval `(spp ,old2 :p))))
     (c1 (caar (eval `(spp ,old1 :c))))
     (p1 (caar (eval `(spp ,old1 :p))))
     (s1 (caar (eval `(spp ,old1 :success))))
     (s2 (caar (eval `(spp ,old2 :success))))
     (c c1)
;     (c  (max 0 (+ c1 *cost-penalty*))) ;this is the pessimism built into the productions
;avoid negative costs
     (p p1) ;since this new production must compete with the first it makes sense to base its parameters on the first.
     (successes (* p initial-experience))
     (failures (- initial-experience successes))
     (efforts (* c initial-experience))
     (s (or s1 s2))
     (pl (first (no-output (sgp :pl)))))
    (declare (ignore-if-unused p2 c2))
    (if (and identical pl)
      (let*
        ((prod (get-safe-production production))
         (priorP (production-priorP prod))
         (priorC (production-priorC prod)))
        (setf (production-priorP prod) (+ priorP (* *pc-speed* (- p priorP)))
              (production-priorC prod) (+ priorC (* *pc-speed* (- c priorC))))
        (recompute-production-parameters prod)
        (when (and *production-compilation-trace* *verbose*) 
          (format t "~%Priors of ~S: P: ~6,2F C: ~6,3F" production (production-priorP prod)(production-priorC prod))))
              
      (cond ((eq pl t)
           (no-output
            (eval `(spp ,production
                        :effort ,effort
                        :efforts .05
                        :success ,s
                        :successes 1
                        :failures 0)))
           (setf (production-priorP (get-safe-production production)) 0.0
                 (production-priorC (get-safe-production production)) *g*))
          ((null pl)
           (no-output
            (eval `(spp ,production
                        :effort ,effort
                        :c ,c
                        :p ,p))))
          (t
           (no-output
            (eval `(spp ,production
                        :effort ,effort
                        :efforts ,efforts
                        :success ,s
                        :successes ,successes
                        :failures ,failures))))))))
           
    (when (and *production-compilation-trace* *verbose*) (eval `(spp ,production)))
  )

(defun legal-condition (condition)
    (do ((temp condition (cdr temp)))
        ((null temp) t)
      (cond ((not (member (caar temp) (cons '!eval! *header*))) (return nil)))))

(defun legal-action (action)
    (do ((temp action (cdr temp)))
        ((null temp) t)
      (cond ((not (member (caar temp) (cons '!bind! *header*))) (return nil)))))

(defun goal-test (act1p act2p)
    (not (and (assoc '+goal> act1p) (assoc '+goal> act2p)
              (> (length (second (assoc '+goal> act1p))) 1)
              (> (length (second (assoc '+goal> act2p))) 1))))


(defun legal-pair (p1 p2) (and (legal-condition (car p1)) (legal-action (cdr p1))
                               (legal-condition (car p2)) (legal-action (cdr p2))
                               (goal-test (cdr p1) (cdr p2))
                               (not (equal 'error (second (second (assoc '=retrieval> (car p2))))))))

(defun find-binds (lis)
    (do ((temp lis (cdr temp))
         (result nil (cond ((equal (caar temp) '!bind!) (cons (car temp) result))
                           (t result))))
        ((null temp) (reverse result))))


(defun compose-production (x y)
    "Takes two production instantiations, x and y, and attempt to calculate a composition"
    (let* ((prod1 (symbol-filter (production-text (get-safe-production (car x)))))
           (prod2 (symbol-filter (production-text (get-safe-production (car y))))))
    (if (legal-pair prod1 prod2)
      (let* ((reti-act1 (get-term (cdr prod1) (symbol-filter (fourth x)) '+retrieval>))
           (reti-cond2 (get-term (car prod2) (symbol-filter (third y)) '=retrieval>))
          (retp-act1 (test-switch (assoc '+retrieval>  (cdr prod1) )))
          (retp-cond2 (assoc '=retrieval> (car prod2) ))
          (retp-act2 (assoc '=retrieval> (cdr prod2)))
          (prod1 (instantiate-first prod1 retp-act1 retp-cond2 reti-act1))
          (prod2 (instantiate-second prod2 retp-act1 retp-cond2 reti-cond2))
          (set (buffer-move prod1 prod2))
          (prod1 (first set))
          (prod2 (second set))
          (switches (third set))
          (renamings (renamings prod1 prod2))
          (prod2 (substitute-vars prod2 renamings))
          (buffer-list (reverse (do ((temp *buffer-lis* (cdr temp))
                            (result nil (cons (extract-buffers (car temp) prod1 prod2) result)))
                           ((null temp) result))))
          (rt (buffer-test retp-act1 retp-cond2 (car prod1) (symbol-filter (third x))))
          (substitutions (mapcan 'substitutions (merge-bindings (mapcan 'unification buffer-list)))))
      (cond ((and (not (buffer-jam buffer-list)) (not (member 'fail substitutions))
                  (not (and retp-act1 retp-cond2 retp-act2))
                 (not (and (assoc '+goal> (cdr prod1)) (not (assoc 'goal switches))
                            (member 'goal (mapcar 'second switches)))))
             (setf buffer-list (mapcar 'merge-buffer buffer-list))
             (let* ((eval1 (substitute-vars (assoc '!eval! (car prod1)) substitutions))
                    (eval2 (substitute-vars (assoc '!eval! (car prod2)) substitutions))
                    (evals (eval-merge  rt eval1 eval2))
                    (bind1 (substitute-vars (find-binds (cdr prod1)) substitutions))
                    (bind2 (substitute-vars (find-binds (cdr prod2)) substitutions))
                    (buffer-list  (substitute-vars buffer-list substitutions))(new-condition (remove-nils (append (mapcar 'first buffer-list) (list evals))))
                   (new-action (remove-nils (append bind1 bind2 (mapcar 'second buffer-list)
                                                    (mapcar 'third buffer-list))))
                   (production-name (new-name-fct "PRODUCTION"))
                   (com-result (multiple-value-list (compile-or-select-production production-name
  (second (second (caar prod1)))
                                            new-condition new-action nil nil)))
                    (production (first com-result))
                    (identical (second com-result)))
               (when  production (finish-up production identical (car x) (car y))))))))))




;;; Proceduralization code

(defun test-switch (act)
"Ignores +buffer actions where this is just to switch buffer contents"
    (cond ((and (equal (length (second act)) 1)
                    (member (strip-var (car (second act)))  *buffer-lis*)) nil)
          (t act)))


(defun instantiate-first (prod actp condp acti)
"Calculates the instantiation of the first production produced by production composition"
    (cond ((and actp condp (not (equal 'error (second (second condp)))))
           (substitute-vars (cons (car prod) (remove actp (cdr prod)))
                            (extract-retrieval-map actp acti)))
          (t prod)))

(defun instantiate-second (prod actp condp condi)
"Calculates the instantiation of the second production produced by production composition"
    (cond ((and actp condp (not (equal 'error (second (second condp)))))
           (substitute-vars (cons (remove condp (car prod)) (cdr prod))
                            (cons (list '=retrieval (headere (car condi)))
                                   (extract-retrieval-map condp condi))))
          (t prod)))


(defun extract-retrieval-map (pat ins)
"Calculates retrieval mapping when pat is instantiated by ins"
    (cond ((equal (length (second pat)) 1)
           (cond ((and (variablep (car (second pat)))
                       (not (member  (strip-var (car (second pat))) *buffer-lis*)))
                       (list (list (car (second pat)) (car (second ins)))))
                 (t nil)))
          (t
    (do ((temp1 (cddr pat) (cdr temp1))
         (temp2 (cddr ins) (cdr temp2))
         (result nil (cond ((variablep (first (last (car temp1))))
                            (cons (list (first (last (car temp1))) (first (last (car temp2)))) result))
                           (t result))))
        ((null temp1) result)))))

;;; Renaming variables to avoid conflict

(defun renamings (prod1 prod2)
"Calculates new names so variables in second production will not have same names as variables in first"
    (let ((variables1 (extract-variables prod1))
          (variables2 (extract-variables prod2)))
      (do ((temp variables2 (cdr temp))
           (result nil (cond ((member (car temp) variables1)
                              (cons (list (car temp) (safe-gentemp (string (car temp)))) result))
                             (t result))))
          ((null temp) result))))

(defun extract-variables (production)
"Finds the non-buffer variables in lis"
    (do ((temp (apply 'append (mapcar 'cdr (append (car production) (cdr production)))) (cdr temp))
         (result nil (cond ((and (variablep (car temp))
                                 (not (member (car temp) result)))
                            (cons (car temp) result))
                            ((and (variablep (car (last (car temp))))
                                 (not (member  (strip-var (car (last (car temp)))) *buffer-lis*))
                                 (not (member (car (last (car temp))) result)))
                            (cons (car (last (car temp))) result))
                           (t result))))
        ((null temp) result)))

;;; Buffer code

(defun buffer-move (prod1 prod2)
"When the productions are composed it may be necessary to rename buffers in the second to correspond to buffer switches"
    (let ((temp (cdr prod1))
          result hold
          newprod1)
      (loop
        (cond ((null temp) (return (setf prod1 (cons (car prod1) (reverse newprod1)))))
              ((and (listp (second (car temp)))(equal (length (second (car temp))) 1)
                    (member (strip-var (car (second (car temp)))) *buffer-lis*))
               (setf result (cons (list (strip-pre-post (caar temp))
                                           (strip-var (car (second (car temp))))) result))
               (cond ((not (assoc (caar temp) prod2)) (setf hold (cons (car temp) hold)))))
              (t (setf newprod1 (cons (car temp) newprod1))))
        (setf temp (cdr temp)))
      (do ((temp result (cdr temp))
           (bindings nil (append bindings (list (list (=buffer (first (car temp))) (mark (=buffer (second (car temp)))))
                                            (list (to-variable (first (car temp))) (mark (to-variable (second (car temp)))))))))
          ((null temp) (setf prod2 (substitute-vars prod2 bindings))))
      (do ((temp result (cdr temp))
           (bindings nil (append bindings (list (list (mark (=buffer (second (car temp)))) (=buffer (second (car temp))))
                                            (list (mark (to-variable (second (car temp))))  (to-variable (second (car temp))))))))
          ((null temp) (list prod1 (append (substitute-vars prod2 bindings) hold) result
                            )))))



(defun extract-buffers (buffer prod1 prod2)
"For each buffer finds condition, =action, and +action and makes a list of all 6 for each production"
    (let ((=buffer (=buffer buffer))
          (+buffer (+buffer buffer))
          (-buffer (-buffer buffer)))
      (list (assoc =buffer (car prod1)) (assoc =buffer (cdr prod1))
            (or (assoc +buffer (cdr prod1))(assoc -buffer (cdr prod1)))
            (assoc =buffer (car prod2)) (assoc =buffer (cdr prod2))
            (or (assoc +buffer (cdr prod2))(assoc -buffer (cdr prod2))))))

(defun buffer-test (actp condp pat ins)
"If necessary builds a test of the identity of the chunk retrieved which will be deleted from buffer"
    (cond ((and actp condp)
           (let ((possibles (cddr actp)))
             (do ((temp (mapcar 'last-elem possibles) (cdr temp)))
                 ((null temp) nil)
               (cond ((member (strip-var (car temp)) *buffer-lis*)
                      (return `(!EVAL! (EQUAL ,(car temp) (quote
                                    ,(headere (car (get-term pat ins (to-header (car temp))))))))))))))))
(defun buffer-jam (buffers)
"Tests if action from one production will jam with test or action from the second"
    (cond ((or (and (third (car buffers)) (sixth (car buffers)))
               (equal (length (second (third (car buffers)))) 1)) t)
          (t (do ((temp (cddr buffers) (cdr temp)))
                 ((null temp) nil)
               (cond ((and (third (car temp))
                      (or (fourth (car temp)) (fifth (car temp)) (sixth (car temp)))) (return t)))))))

(defun merge-buffer (lis)
"Merge the buffers to form one production"
    (let ((=cond1 (first lis)) (=act1 (second lis)) (+act1 (third lis))
          (=cond2 (fourth lis)) (=act2 (fifth lis)) (+act2 (sixth lis)))
    (cond ((or (not +act1) (and (equal (car +act1) '+retrieval>) +act2))
           (list (chunk-merge-condition =cond1 (chunk-dif =cond2 =act1))
           (chunk-merge-action =act2 =act1) +act2))
          (+act2 (list =cond1 =act1 +act2))
          ((and (equal (car +act1) '+retrieval>) (not =cond2) (not +act2))
           (list =cond1 =act1 +act1))
          (t (list =cond1 =act1
                   (remove-nils (append (list (first +act1) (second +act1))
                           (cdr (chunk-merge-action =act2
                                       (cons (car +act1) (cddr +act1)))))))))))

(defun chunk-merge-condition (chunk1 chunk2)
"Merge two condition chunks"
    (cond ((not chunk1) chunk2)
          ((not chunk2) chunk1)
          (t
    (let ((pair))
    (do ((temp1 (cdr chunk1) (cdr temp1))
         (result (list (car chunk1))
                 (cond ((equal (caar temp1) '-)
                        (do ((temp2 (cdr chunk2) (cdr temp2)))
                            ((null temp2) (append result (list (car temp1))))
                          (cond ((and (equal (first (car temp2)) '-)
                                      (equal (second (car temp1)) (second (car temp2)))
                                      (equal (third (car temp1)) (third (car temp2))))
                                 (setf chunk2 (remove (car temp2) chunk2))
                                 (return (append result (list (car temp1))))))))
                       ((setf pair (assoc (caar temp1) (cdr chunk2)))
                        (setf chunk2 (remove pair chunk2))
                        (append result (list (max-specific (car temp1) pair))))
                       (t (append result (list (car temp1)))))))
        ((null temp1) (append result (cdr chunk2))))))))



(defun chunk-merge-action (chunk1 chunk2)
"Merge two action chunks"
    (cond ((not chunk1) chunk2) (t
    (let ((pair))
    (do ((temp1 (cdr chunk1) (cdr temp1))
         (result (list (car chunk1))
                 (cond ((equal (caar temp1) '-)
                        (do ((temp2 (cdr chunk2) (cdr temp2)))
                            ((null temp2) (append result (list (car temp1))))
                          (cond ((and (equal (first (car temp2)) '-)
                                      (equal (second (car temp1)) (second (car temp2)))
                                      (equal (third (car temp1)) (third (car temp2))))
                                 (setf chunk2 (remove (car temp2) chunk2))
                                 (return (append result (list (car temp1))))))))
                       ((setf pair (assoc (caar temp1) (cdr chunk2)))
                        (setf chunk2 (remove pair chunk2))
                        (append result (list (car temp1))))
                       (t (append result (list (car temp1)))))))
        ((null temp1) (append result (cdr chunk2))))))))

(defun max-specific (pair1 pair2)
"Determines which slot-value pair is more specific"
    (cond ((variablep (second pair2)) pair1)
          ((variablep (second pair1)) pair2)
          ((and (equal (car pair1) 'isa)
                (< (length (wme-type-supertypes (get-safe-type (second pair1))))
                   (length (wme-type-supertypes (get-safe-type (second pair2))))))
          pair2)
          (t pair1)))


(defun chunk-dif (condition action)
"Finds what in condition was not produced by action"
    (cond ((not condition) nil) (t
    (setf action (cdr action))
    (do ((temp2 (cddr condition) (cdr temp2))
         (result2 (list (car condition) (cadr condition))
                  (cond ((assoc (caar temp2) action) result2)
                        ((and (equal 3 (length (car temp2))) (assoc (second (car temp2)) action)) result2)
                        (t (append result2 (list (car temp2)))))))
        ((null temp2) result2)))))

       (defun last-elem (lis) (car (last lis)))


;;; Unification code

(defun unification (lis)
"Calculates unification for a buffer"
    (let ((=cond1 (first lis)) (=act1 (second lis)) (+act1 (third lis))
          (=cond2 (fourth lis)) )
      (cond ((not +act1) (unify-clauses (merge-two (cdr =act1) (cddr =cond1)) (cddr =cond2)))
            (t (unify-clauses (cddr +act1) (cddr =cond2))))))

(defun merge-two (act cond)
"Creates chunk combination of =buffer in condition and action"
    (do ((temp cond (cdr temp))
         (result act (cond ((assoc (caar temp) act) result)
                            ((equal (caar temp) '-) result)
                            (t (cons (car temp) result)))))
        ((null temp) result)))

(defun unify-clauses (lis1 lis2)
"Calculates unification of lis1 from production 1 and lis2 from production2"
    (let (pair)
    (do ((temp lis1 (cdr temp))
         (result nil (cond ((setf pair (assoc (caar temp) lis2))
                            (append result (list (list (second (car temp)) (second pair)))))
                           (t result))))
        ((null temp) result))))

(defun merge-bindings (lis)
"Combines constraints from different buffers -- one constraint per buffer"
    (cond ((null lis) nil)
          (t (let ((result (merge-bindings (cdr lis))) old1 old2)
                (setf old1 (and (variablep (first (car lis))) (findit (first (car lis)) result)))
                (setf old2 (and (variablep (second (car lis))) (findit (second (car lis)) result)))
                (cons (combine old1 old2 (car lis))
                      (remove old1 (remove old2 result :test 'equal) :test 'equal))))))

(defun findit (item lis)
"Determines if item has already been bound in lis"
    (do ((temp lis (cdr temp)))
        ((null temp) nil)
      (cond ((member item (car temp)) (return (car temp))))))

(defun combine (a b c)
"All the terms in a, b, and c must have the same referencec"
    (union a (union b c)))

(defun substitutions (lis)
"Collapses constraints produced by unification"
    (let (variable constant result)
      (do ((temp lis (cdr temp)))
          ((null temp) result)
        (cond ((variablep (car temp))
               (cond (constant (setf result (cons (list (car temp) constant) result)))
                     (variable (setf result (cons (list (car temp) variable) result)))
                     (t (setf variable (car temp)))))
              (t (cond ((and constant (not (equal (car temp) constant))) (return (list 'fail)))
                       (constant nil)
                       (variable (setf result (cons (list variable (car temp))
                                       (substitute-vars result (list (list variable (car temp))))))
                                 (setf constant (car temp)))
                       (t (setf constant (car temp)))))))))


;;; Calculation of evals

(defun eval-merge (lis1 lis2 lis3)
"Merges all the evals"
   (quote-constants (let ((result (remove-dups (remove-nils (mapcar 'eval-filter (append (extract-eval lis1) (extract-eval lis2) (extract-eval lis3)))))))
      (cond ((null result) nil)
            ((equal (length result) 1) (cons '!eval! result))
            (t (list '!eval! (cons 'and result)))))))

(defun quote-constants (lis)
    (cond ((atom lis)
           (cond ((variablep lis) lis)
                 ((null lis) nil)
                 ((stringp lis) lis)
                 (t (list 'quote lis))))
          ((equal (car lis) 'quote) lis)
          (t (cons (car lis) (mapcar 'quote-constants (cdr lis))))))

(defun remove-dups (lis)
    (do ((temp lis (cdr temp))
         (result nil (cond ((member (car temp) (cdr temp) :test 'equal) result)
                           (t (cons (car temp) result)))))
        ((null temp) (reverse result))))

(defun extract-eval (lis)
    (cond ((null lis) nil)
          ((equal (car (second lis)) 'and) (cdr (second lis)))
          (t (cdr lis))))

(defun contains-variable (lis)
  (cond ((atom lis) (variablep lis))
        (t (or (contains-variable (car lis)) (contains-variable (cdr lis))))))

(defun eval-filter (lis)
    (cond ((contains-variable lis) lis)
          (t nil)))


(defun copy-all (x)
  (cond ((atom x) 
         (cond ((wmep x) (wme-name x)) (t x)))
        (t (cons (copy-all (car x)) (copy-all (cdr x))))))


;;; Changed compiler


(defun compile-or-select-production (name type lhs rhs success failure
                                            &optional (documentation nil))
    "Either create a new production or select an existing identical one.
     Return a second value indicating whether it is an existing production (t) or not (nil)."
    (if (bad-prod-test lhs rhs)
;filter cases of a constant assignment
    (dolist (production (wme-type-productions (get-safe-type type name))
                        (let ((new-production (compile-production name (copy-all lhs)(copy-all rhs) documentation)))
                          (signal-output *production-compilation-trace* "Compiling Production ~A." new-production)
                          (parameters-fct name (nconc (list :success (if success t nil)
                                                            :failure (if failure t nil))
  *production-compilation-parameters*))
                          (values new-production nil)))
      (when (identical-productions (copy-all (cons lhs rhs)) (symbol-filter (production-text production)) )
;more of my attempt to deal with my problem with wmes and symbols
        (signal-output *production-compilation-trace* "Recreating Production ~A." production)
;this change enables the change in finish-up
        (return (values production t)))))) ; NT: added the (values ... t) to indicate a new production was created.

(defun bad-prod-test (lhs rhs)
"Test for bad production"
    (let ((newgoal (assoc '+goal> rhs))
          (retrieval (assoc '+retrieval> rhs)))
       (not (or (and (assoc '=retrieval> rhs) (not (assoc '=retrieval> lhs)))
                (and newgoal (not (equal (car (second newgoal)) 'isa)) (not (variablep (car (second newgoal)))))
                (and retrieval (not (equal (car (second retrieval)) 'isa)) (not (variablep (car (second retrieval)))))))))




;;; General Utilities

(defun get-term (pat ins index)
"Finds in the instantiation ins the same term that begins with index in pattern pat"
    (let ((element (assoc index pat)))
      (cond (element (nth (position element pat) ins)))))


(defun symbol-filter (x)
"Converts everything into symbols"
    (cond ((null x) nil)
          ((atom x) (cond ((wme-p x) (wme-name x))
                          ((wme-type-p x) (wme-type-name x))
                          ((symbolp x) x)
                          ((numberp x) x)
                          ((stringp x) x)
                          (t  (print x))))
          (t (cons (symbol-filter (car x)) (symbol-filter (cdr x))))))


(defun substitute-vars (sexp mapping)
"Calculates the substitute in sexp defined by mapping"
    (cond ((null sexp) nil)
          ((atom sexp)
           (cond ((assoc sexp mapping) (cadr (assoc sexp mapping)))
                 (t sexp)))
          (t (cons (substitute-vars (car sexp) mapping)
                   (substitute-vars (cdr sexp) mapping)))))

(defun strip-var (term)
"maps =var to var"
    (cond ((variablep term)
    (let* ((string  (string-sym term))
           (bound (1- (length string)))
           (ans (make-string bound)))
      (do ((count 0 (1+ count)))
          ((equal count bound) (intern ans))
        (setf (aref ans count) (aref string (1+ count))))))))

(defun strip-pre-post (term)
"maps pvar> to var"
    (let* ((string  (string-sym term))
           (bound (1- (length string)))
           (ans (make-string (1- bound))))
      (do ((count 1 (1+ count)))
          ((equal count bound) (intern ans))
        (setf (aref ans (1- count)) (aref string count)))))


(defun headere (term)
"maps x> to x"
    (let* ((string  (string-sym term))
           (bound (1- (length string)))
           (ans (make-string bound)))
      (do ((count 0 (1+ count)))
          ((equal count bound) (intern ans))
        (setf (aref ans count) (aref string count)))))

(defun mark (term)
"Maps a to *a"
    (let* ((string  (string-sym term))
           (bound (length string))
           (ans (make-string (+ 1 bound))))
      (setf (aref ans 0) #\*)
      (do ((count 0 (1+ count)))
          ((equal count bound) (intern ans))
        (setf (aref ans (+ count 1)) (aref string count)))))


(defun remove-nils (lis)
    (do ((temp lis (cdr temp))
         (result nil (cond ((null (car temp)) result)
                           (t (cons (car temp) result)))))
        ((null temp) (reverse result))))


#|
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
|#