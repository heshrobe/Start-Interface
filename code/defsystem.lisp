;;; -*- Syntax: Ansi-common-lisp; Package: cl-USER; Base: 10; Mode: LISP -*- 

(in-package :cl-user)

(load "~/quicklisp/setup.lisp")
(ql:quickload :drakma)
(ql:quickload :cxml)

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
      (with-open-file (F #P"start:home;my-logical-pathnames.lisp" :direction :output :if-exists :supersede)
	(format f "~%;;; start")
	(format f "~2%~s" "start")
	(loop for (a b) in (logical-pathname-translations "start")
	  do (format f "~%'(~s ~s)" (namestring a) (namestring b)))
	(terpri f)
	)      
      (pushnew (namestring (truename #P"start:home;my-logical-pathnames.lisp"))
	       (logical-pathname-translations-database-pathnames)
	       :test #'string-equal)
      )))

(defsystem start-interface
    (:default-pathname "start:code;"
	:default-module-class separate-destination-module)
  (:serial
   ("package-definition" (:module-class separate-destination-module)) 
   ("http-interface" (:module-class separate-destination-module)) 
   ("parse-interface" (:module-class separate-destination-module))
   ("generators" (:module-class separate-destination-module))
   ))

