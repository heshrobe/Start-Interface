;;; -*- Mode: Common-Lisp; Package: start; Readtable: Joshua -*-

(in-package :start)

(defun tokenize (string) (substitute #\_ #\space (string string)))

(defun generate-main (generator subject verb object &rest stuff &key &allow-other-keys)
  (multiple-value-bind (clause generator) (apply #'generate-subordinate-clause generator subject verb object stuff)
    (make-clause-main generator clause)
    generator))

(defun make-clause-main (generator clause)
  (intern-and-add-texp generator (list clause
				       (intern-constant generator '|is_main|)
				       (intern-constant generator '|yes|))))

(defun generate-subordinate-clause (generator subject verb object &key negative tense progressive modality voice perfective)
  (let* ((generator (or generator (make-instance 'question-generator)))
	 (subject (if-raw subject (intern-entity generator subject)))
	 (object (if-raw object (intern-entity generator object)))
	 (verb (if-raw verb (intern-verb generator (string-downcase (string verb))
					 :negative negative
					 :tense  tense
					 :progressive progressive
                                         :perfective perfective
					 :modality modality
					 :voice  voice)))
	 (clause (intern-and-add-texp generator (list subject verb object))))
    (values clause generator)))

(defun generate-confidence-clause (generator confidence-level)
  (let* ((I (intern-constant generator "I"))
         (confidence (intern-entity generator "confidence" :definite? 'none :owner I :modifier confidence-level))
         (have (intern-verb generator "have"))
         ;; (is (intern-verb generator "is"))
         )
    (generate-main generator I have confidence)
    ))

(Defun generate-think-clause (generator subordinate-clause &key negative (verb "think") (make-main? t) modifier tense)
  (let* ((generator (or generator (make-instance 'question-generator)))
	 (subject (intern-constant generator 'I))
	 (verb (if-raw verb (intern-verb generator verb
					 :negative negative
					 )))
	 (clause (intern-and-add-texp generator (list subject verb subordinate-clause)))
         )
    (when tense
      (intern-and-add-texp generator (list clause (intern-constant generator "has_tense") (intern-constant generator tense))))
    (when modifier
      (let ((has-modifier (intern-entity generator "has_modifier" :definite? 'none :singular? 'none))
            (modifier (intern-constant generator modifier)))
        (intern-and-add-texp generator (list has-modifier (intern-constant generator "has_position") (intern-constant generator "mid_verbal")))
        (intern-and-add-texp generator (list verb has-modifier modifier))))
    (when make-main?
      (make-clause-main generator clause))
    (values clause generator)))

(defun generate-causative (generator result cause &key (make-main? t) (relation "because"))
  (let* ((generator (or generator (make-instance 'question-generator)))
	 (relation (if-raw relation (intern-entity generator relation)))
	 (clause (intern-and-add-texp generator (list result relation cause))))
    (when make-main?
      (make-clause-main generator clause))
    (values clause generator)))


(defun generate-instrument-clause (generator instrument instrument-relation core-clause)
  (when instrument
    (setq instrument (if-raw instrument (intern-entity generator instrument)))
    (setq instrument-relation (if-raw instrument-relation (intern-entity generator (tokenize instrument-relation) :singular? 'none :definite? 'none)))
    (intern-and-add-texp generator (list core-clause instrument-relation instrument))))

(defun generate-motion-clause (generator subject verb &key source source-relation
                                                           destination destination-relation
                                                           instrument instrument-relation
                                                           (make-main? t)
                                                           negative tense progressive modality voice)
  (let* ((generator (or generator (make-instance 'question-generator)))
	 (verb (if-raw verb (intern-verb generator (string-downcase (string verb))
					 :negative negative
					 :tense  tense
					 :progressive  progressive
					 :modality  modality
					 :voice  voice)))
	 (subject (if-raw subject (intern-entity generator subject)))
	 (null (intern-constant generator "null"))
	 (core-clause nil)
         (source-relation-clause nil)
         (destination-relation-clause nil))
    ;; there's a funny case, for example: the boy leaves the room where there's a source but no source relation
    ;; and if there's no source at all, then the main clause has null as the object
    (cond
     ((and (null source) (null destination))
      (setq core-clause (intern-and-add-texp generator (list subject verb null))))
     ((and source (null destination))
      (setq source (if-raw source (intern-entity generator source)))
      (cond ((null source-relation)
             (setq core-clause (intern-and-add-texp generator (list subject verb source))))
            (t
             (setq core-clause (intern-and-add-texp generator (list subject verb null)))
             (setq source-relation (if-raw source-relation (intern-entity generator (tokenize source-relation) :singular? 'none :definite? 'none)))
             (setq source-relation-clause (intern-and-add-texp generator (list core-clause source-relation source))))))
     ((and (null source) destination)
      ;; similarly there could be a destination without a relation if there's no source
      (setq destination (if-raw destination (intern-entity generator destination)))
      (cond
       ((null destination-relation)
        (setq core-clause (intern-and-add-texp generator (list subject verb destination))))
       (t
        (setq core-clause (intern-and-add-texp generator (list subject verb null)))
        (setq destination-relation (if-raw destination-relation (intern-entity generator (tokenize destination-relation) :singular? 'none :definite? 'none)))
        (setq destination-relation-clause (intern-and-add-texp generator (list core-clause destination-relation destination))))))
     (t
      (setq source (if-raw source (intern-entity generator source)))
      (setq core-clause (intern-and-add-texp generator (list subject verb null)))
      (setq source-relation (if-raw source-relation (intern-entity generator (tokenize source-relation) :singular? 'none :definite? 'none)))
      (setq source-relation-clause (intern-and-add-texp generator (list core-clause source-relation source)))
      (setq destination (if-raw destination (intern-entity generator destination)))`
      (setq destination-relation (if-raw destination-relation (intern-entity generator (tokenize destination-relation) :singular? 'none :definite? 'none)))
      (setq destination-relation-clause (intern-and-add-texp generator (list core-clause destination-relation destination)))))
    (generate-instrument-clause generator instrument instrument-relation core-clause)
    ;; apparently I don't need to include this, but it is what the parser would have done
    (intern-and-add-texp generator (list null (intern-constant generator "has_category") (intern-constant generator "nil")))
    (when source-relation (intern-and-add-texp generator (list source-relation-clause (intern-constant generator "has_position") (intern-constant generator "trailing"))))
    (when destination-relation (intern-and-add-texp generator (list destination-relation-clause (intern-constant generator "has_position") (intern-constant generator "trailing"))))
    (when make-main?
      (make-clause-main generator core-clause))
    (values core-clause generator)))

(defun generate-justification-clause (generator premise justification &key (make-main? t) (connective 'because))
  (let* ((generator (or generator (make-instance 'question-generator)))
	 (connective (if-raw connective (intern-entity generator connective :definite? 'none :singular? 'none)))
	 (justification-clause (intern-and-add-texp generator (list premise connective justification))))
    (when make-main?
      (make-clause-main generator  justification-clause))
    (values justification-clause generator)))

(defun add-temporal-bound (generator clause upper-bound &optional (units 'minutes))
  (let* ((within (intern-entity generator 'within :definite? 'none :singular? 'none))
         (units (intern-entity generator units :definite? t :singular? 'plural))
         ;;(has-property (intern-entity generator "has_property" :definite? 'none :singular? 'none))
         (has-modifier (intern-entity generator "has_modifier" :definite? 'none :singular? 'none))
         (has-quantity (intern-entity generator "has_quantity" :definite? 'none :singular? 'none)))
    (intern-and-add-texp generator (list clause within units))
    (intern-and-add-texp generator (list units has-quantity (intern-constant generator upper-bound)))
    (intern-and-add-texp generator (list (intern-constant generator upper-bound) has-modifier (intern-constant generator "next")))))

(defun test-gen ()
  (let* ((generator (make-instance 'question-generator))
	 (player (intern-entity generator 'player :definite? t :singular? t))
	 (door (intern-entity generator 'door :definite? t :singular? t))
	 (open-clause (generate-subordinate-clause generator player 'open door  :modality '|may|))
	 (move-clause (generate-motion-clause generator player 'walk :destination door :destination-relation "towards"
                                              :make-main? nil :tense 'past :progressive t))
	 (think-clause (generate-think-clause generator open-clause :verb "believe" :modifier "weakly" :make-main? nil))
	 (because (intern-entity generator 'because :definite? 'none :singular? 'none)))
    (add-temporal-bound generator open-clause 5 'minute)
    (generate-justification-clause generator think-clause move-clause :connective because :make-main? t)
    generator))
