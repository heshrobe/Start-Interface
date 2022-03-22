;;; -*- Syntax: Ansi-common-lisp; Package: cl-USER; Base: 10; Mode: LISP -*-

(in-package :cl-user)

;;; I'm trying to remove Allegro dependencies
;;; So aserve -> drakma
;;; and sax -> cxml
;;; these have similar interfaces to the Allegro components

#+ignore
(eval-when (:compile-toplevel :load-toplevel)
  (require :aserve)
  (require :sax)
  )

(defpackage start
  (:use common-lisp sax joshua #+ignore net.aserve.client #+ignore net.xml.sax)
  (:shadow type-of)
  (:export
   ;; Symbols that the XML parser needs to have when parsing from another package
   my-start-element my-end-element
   ;; basic functions
   generate-text get-lexical-entry parse-text
   flush-knowledge-base
   string-for text-for-generator vocalize-generator
   ;; functions for building sentences
   add-clause-modifier add-temporal-bound add-verb-modifier
   create-about-phrase create-main-clause create-modality create-using-clause
   generate-causative generate-confidence-clause generate-instrument-clause generate-justification-clause
   generate-main generate-motion-clause generate-relative-clause generate-subordinate-clause generate-think-clause
   make-clause-main make-verb-negative
   tokenize
   intern-and-add-texp intern-constant intern-connective intern-entity intern-verb
   core-parser-mixin
   ;; joshua linkage
   is-appropriate-response
   texp-and-names texp-match
   as-relation as-subject as-object
   relation-of subject-of object-of
   relation subject object
   get-main get-mains get-mains-and-verb name full-name
   find-singular-of-noun find-infinitive-of-verb
   ;; key connectives and constants in START
   ;; (or these only is_main yes and no may be necessary since all the others are probably
   ;; only used in building sentences and those functions are in the start package
   ;;
   ;; Some connectives are binary in which case they take yes, no and their name is is_xxx
   "is_main" "is_perfective" "is_progressive"
   "is_proper"
   ;; Other connectives take a variety of values and these are named has_xxx
   "has_category" "has_det" "has_modal" "has_modifier" "has_number" "has_person" "has_position"
   "has_property" "has_quantifier" "has_rel_clause" "has_tense" "has_voice"
   ;;
   ;; constants for the binary valued things
   "yes" "no"  "null" "nil"
   ;; Determiners
   "indefinite" "definite" "none"
   ;; Nouns
   "singular" "plural"
   ;; Vebs
   "present" "past" "will"
   "trailing" "mid_verbal"
   ;; Persons that are built in
   "somebody" "I"
   ))
