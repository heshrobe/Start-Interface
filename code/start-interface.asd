;;; -*- Syntax: Ansi-common-lisp; Package: cl-USER; Base: 10; Mode: LISP -*- 

(in-package :cl-user)

(load "~/quicklisp/setup.lisp")
(ql:quickload '(:drakma :cxml))

(eval-when (:execute :load-toplevel)
  (let* ((loading-file *load-truename*)
	 (host (pathname-host loading-file))
	 (device (pathname-device loading-file))
	 (code-dir (pathname-directory loading-file))
	 (home-dir (butlast code-dir))
	 (wild-dir (append home-dir (list :wild-inferiors))))
    (let ((home-directory (make-pathname :directory home-dir
					 :host host
					 :device device))
	  (code-directory (make-pathname :directory code-dir
					 :host host
					 :device device))
	  (wild-directory (make-pathname :directory wild-dir
					 :host host 
					 :device device
					 :type :wild
					 :name :wild
					 :version :unspecific)))
      (setf (logical-pathname-translations "start")
	`(("code;*.*" ,code-directory)
	  ("home;*.*" ,home-directory)
	  ("**;*.*"   ,wild-directory)
	  ))
      #+allegro
      (with-open-file (F #P"start:home;my-logical-pathnames.lisp" :direction :output :if-exists :supersede)
	(format f "~%;;; start")
	(format f "~2%~s" "start")
	(loop for (a b) in (logical-pathname-translations "start")
	  do (format f "~%'(~s ~s)" (namestring a) (namestring b)))
	(terpri f)
	)
      #+allegro
      (pushnew (namestring (truename #P"start:home;my-logical-pathnames.lisp"))
	       (logical-pathname-translations-database-pathnames)
	       :test #'string-equal)
      )))

(asdf:defsystem start-interface
  :pathname "."
  :name "Start-Interface"
  :description "Interface to START"
  :maintainer "Howie Shrobe"
  :serial t
  :components ((:file "package-definition")
               (:file "http-interface")
               (:joshua-file "parse-interface")
               (:file "generators")))

