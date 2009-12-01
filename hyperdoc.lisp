;;;; -*- Mode: Lisp -*-
;;;;
;;;; Copyright (c) 2003, 2004 Nikodemus Siivola
;;;; Copyright (c) 2009 Tobias C. Rittweiler
;;;;
;;;; Permission is hereby granted, free of charge, to any person obtaining
;;;; a copy of this software and associated documentation files (the
;;;; "Software"), to deal in the Software without restriction, including
;;;; without limitation the rights to use, copy, modify, merge, publish,
;;;; distribute, sublicense, and/or sell copies of the Software, and to
;;;; permit persons to whom the Software is furnished to do so, subject to
;;;; the following conditions:
;;;;
;;;; The above copyright notice and this permission notice shall be
;;;; included in all copies or substantial portions of the Software.
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;;;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;;;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;;;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(defpackage :hyperdoc
  (:use :cl)
  (:export
   #:register-documentation
   #:lookup
   #:generate-index
   #:*index-directory*
   #:*documentation-types*
   ))

(in-package :hyperdoc)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :hyperdoc *features*))

;;;; Utility functions

(defun find-value (name package)
  "Returns the symbol-value of the symbol NAME in PACKAGE, and T as a
secondary value if the symbol is bound. Returns NIL, NIL if the symbol
doesn't exist or is not bound."
  (let ((symbol (find-symbol name package)))
    (if (and symbol (boundp symbol))
	(values (symbol-value symbol) t)
	(values nil nil))))

(defun find-function (name package)
  "Returns the symbol-funciton of the symbol NAME in PACKAGE, and T as
a secondary value if the symbol is fbound. Returns NIL, NIL if the
symbol doesn't exist or is not fbound."
  (let ((symbol (find-symbol name package)))
    (if (and symbol (fboundp symbol))
	(symbol-function symbol)
	nil)))

(defun merge-uris (base relative)
  "Merges RELATIVE to BASE."
  ;; Yuck. This is so WRONG.
  (concatenate 'string base relative))

(defun package-string (package)
  "Returns the name of the designated package."
  (etypecase package
    (string package)
    (symbol (symbol-name package))
    (package (package-name package))))

(defun gethash-multiple-keys (keys hashtable)
  (loop for key in keys do (multiple-value-bind (value foundp)
			       (gethash key hashtable)
			     (when foundp
			       (return (values value foundp))))))

(defun hash-alist (hash)
  "Returns an alist corresponding to the HASH."
  (let (alist)
    (maphash (lambda (key value)
	       (push (cons key value) alist))
	     hash)
    alist))

(defun alist-hash (alist &key (test #'eql))
  "Returns a hash corresponding to the ALIST."
  (let ((hash (make-hash-table :test test)))
    (dolist (x alist)
      (setf (gethash (car x) hash) (cdr x)))
    hash))

(defun ensure-list (thing)
  (if (listp thing) thing (list thing)))

;; from swank.lisp
(defun package-names (package)
  "Return the name and all nicknames of PACKAGE in a fresh list."
  (cons (package-name package) (copy-list (package-nicknames package))))

;; adapted from swank.lisp
(defun classify-symbol (symbol)
  "Returns a list of classifiers that classify SYMBOL according to its
underneath objects (e.g. :BOUNDP if SYMBOL constitutes a special
variable.) The list may contain the following classification
keywords: :VARIABLE, :FUNCTION, :CONSTANT, :GENERIC-FUNCTION,
:TYPE, :CLASS, :MACRO, :SPECIAL-OPERATOR, and/or :PACKAGE"
  (check-type symbol symbol)
  (flet ((type-specifier-p (s) (documentation s 'type)))
    (let (result)
      (when (boundp symbol)             (push (if (constantp symbol) 
						  :constant 
						  :variable) 
					      result))
      (when (fboundp symbol)            (push :function result))
      (when (type-specifier-p symbol)   (push :type result))
      (when (find-class symbol nil)     (push :class result))
      (when (macro-function symbol)     (push :macro result))
      (when (special-operator-p symbol) (push :special-operator result))
      (when (find-package symbol)       (push :package result))
      (when (typep (ignore-errors (fdefinition symbol))
                   'generic-function)
        (push :generic-function result))

      result)))

;;;; Variables

(defvar *index* nil
  "In memory index. FIXME: This should be loaded when hyperdoc is
loaded -- or at least lazily on first call to X.")

(defvar *index-directory* (merge-pathnames ".hyperdoc/" (user-homedir-pathname))
  "The directory where Hyperdoc keeps the pregenerated indices.")

(defvar *name-index-version* "Hyperdoc Name Index -- Version 1"
  "Magic version indentifier used in the name index files.")

(defvar *package-index-version* "Hyperdoc Package Index -- Version 1"
  "Magic version indentifier used in the package index files.")

(defvar *index-herald* 
  ";;;; This is an automatically generated index -- do not edit by hand!
;;;; See http://www.common-lisp.net/project/hyperdoc/ for more information."
  "Herald printed in the beginning of the index file to discourage tampering.")

(defparameter *documentation-types* 
  (list t 
	:symbol-macro :macro :condition :method
	:generic-function :class :type :function :compiler-macro :setf
	:method-combination :type :structure :package
	:variable :constant)
  "Documentation types used by Hyperdoc. These correspond to what
DOCUMENTATION uses with a few additions.")


;;;; DOCUMENTATION REGISTRATION

(defvar *documentation-registry* 
  (make-hash-table 
   ;; We don't use packages themselves as key so people can register
   ;; documentation for stuff that may be loaded on-demand. (E.g.
   ;; implementation's contribs.)
   :test 'equal)
  "Mapping from package name (possibly nickname) to documentation data.")

(defun parse-register-documentation-keys (keys)
  (flet ((being (type value)
	   (assert (typep value type) 
		   (value)
		   "~S is not of type ~S." value type)
	   value))
    (loop for (key value) on keys by #'cddr
	  if (eq key :base-uri) 
	    collect (being 'string value) into base-uris
	  else if (eq key :relative-uri-function) 
            collect (being '(or symbol function) value) into relative-uri-functions
	  else if (eq key :extra-types-function)
	    collect (being '(or null symbol function) value) into extra-types-functions
	  else 
	    do (error "Unknown REGISTER-DOCUMENTATION key ~S." key)
	  finally (assert (= (length relative-uri-functions) (length base-uris)) ()
			  ":RELATIVE-URI-FUNCTION and :BASE-URI must come in pair.")
		  (return (values base-uris
				  relative-uri-functions
				  extra-types-functions)))))

(defun register-documentation (packages &rest keys &key base-uri
			                                relative-uri-function
                                                        extra-types-function
			       &allow-other-keys)
"packages      ::= package-designator | (package-designators+)
 keys          ::= { :BASE-URI string
                     :RELATIVE-URI-FUNCTION fn
                   [ :EXTRA-TYPES-FUNCTION fn ] }+
"
  (declare (ignore base-uri relative-uri-function extra-types-function))
  (check-type packages (or string symbol package cons))
  (check-type keys (not null))
  (multiple-value-bind (base-uris relative-uri-functions extra-types-functions)
      (parse-register-documentation-keys keys)
    (let ((package-names (mapcar #'package-string (ensure-list packages)))
	  (docdata 
	   (loop for b = (pop base-uris)
		 for r = (pop relative-uri-functions)
		 for e = (pop extra-types-functions)
		 while b collect (list :base-uri b
				       :relative-uri-function r
				       :extra-types-function e))))
      (loop for package-name in package-names do
	(let ((entry (gethash package-name *documentation-registry*)))
	  
	  (when entry
	    (warn "Overwriting hyperdoc documentation entry for package ~S:~%~
                   Old entry: ~S~%New entry: ~S~%" package-name entry keys))
	  (setf (gethash package-name *documentation-registry*) docdata))))))


;;;; DOCUMENTATION LOOKUP

(defun lookup (package-designator symbol-name &optional doc-type)
  "Look up documentation URI-string for the named symbol of doc-type
in the designated package. If the package is not loaded pregenerated
indices are used.

If package doesn't support Hyperdoc, or no documentation for the named
symbol is found, a call to hyperspec:lookup with the symbol-name is
attempted.

If the package supports Hyperdoc, but no doc-type is given and there
are multiple matches, a list of applicable (doc-type . uri-string)
pairs is returned -- if only single doc-type matches just the URI is
returned."
  (let* ((doc-types (ensure-list doc-type))
	 (package (find-package package-designator))
	 (uris
	  (if package
	      (introspective-lookup package symbol-name doc-types)
              (static-lookup (package-string package-designator) 
			     symbol-name doc-types))))
    (if (and (listp uris) (null (cdr uris)))
	(cdr (first uris))
	uris)))

(defun introspective-lookup (package symbol-name doc-types)
  ;; N.B. package locks.
  (let ((symbol (ignore-errors (intern symbol-name package))))
    (when symbol 
      (lookup-symbol-using-package symbol package doc-types))))

(defun lookup-symbol-using-package (symbol package &optional doc-types)
  "Looks up hyperdocumentation for SYMBOL in PACKAGE in the image."
  (setq doc-types (or doc-types *documentation-types*))
  (flet ((lookup (base-uri relative-uri-function extra-types-function)
	   (loop for type in (union (intersection doc-types (classify-symbol symbol))
				    (and extra-types-function
					 (funcall extra-types-function symbol)))
		 for relative-uri = (funcall relative-uri-function symbol type)
		 when relative-uri
		   collect (cons type (merge-uris base-uri relative-uri)))))
    (loop for entry in (documentation-entries-for-package package)
	  thereis (destructuring-bind (&key base-uri 
				       relative-uri-function 
				       extra-types-function) entry
		    (remove-duplicates (lookup base-uri relative-uri-function
					       extra-types-function)
				       :test #'string= :key #'cdr)))))

;;; Don't we all love Allegro's modern mode?
(defparameter +hyperdoc-base-uri+
  (string '#:*hyperdoc-base-uri*))
(defparameter +hyperdoc-lookup+
  (string '#:hyperdoc-lookup))
(defparameter +hyperdoc-documentation-types+
  (string '#:*hyperdoc-documentation-types*))

(defun documentation-entries-for-package (package)
  (flet ((retrieve-from-interning (package)
	   ;; for backwards-compatibility
           (let ((base-uri (find-value +hyperdoc-base-uri+ package))
                 (lookup   (find-function +hyperdoc-documentation-types+ package))
                 ;; (types    (find-value +hyperdoc-documentation-types+ package))
		 )
             (when (and base-uri lookup)
	       (list (list :base-uri base-uri :relative-uri-function lookup)))))
         (retrieve-from-registry (package)
           (gethash-multiple-keys (package-names package) *documentation-registry*)))
    (append (retrieve-from-interning package)
	    (retrieve-from-registry package))))

;;;; Static indexes

(defun static-lookup (package-name symbol-name doc-type)
  "Looks up hyperdocumentation for the symbol in the pregenerated indices."
  (unless *index*
    (multiple-value-bind (index not-empty-p) (read-index)
      (if not-empty-p
	  (setf *index* index)
	  (error "Static index file ~A is empty or does not exist. ~
                 You probably forgot to run HYPERDOC:GENERATE-INDEX."
		 (name-index-pathname)))))
  (let* ((name (gethash package-name (name-table *index*)))
	 (base-uri (gethash name (base-uri-table *index*))))
    (mapcar (lambda (pair)
	      (cons (car pair) (merge-uris (cdr pair) base-uri)))
	    (let ((uris	(gethash symbol-name 
				 (gethash name (package-table *index*)))))
	      (if doc-type
		  (assoc doc-type uris)
		  uris)))))

(defclass index ()
  ((names :accessor name-table
	  :initform (make-hash-table :test #'equal))
   (base-uris :accessor base-uri-table
	      :initform (make-hash-table :test #'equal))
   (package-tables :accessor package-table
		   :initform (make-hash-table :test #'equal))))


(defun name-index-pathname ()
  (merge-pathnames "names.sexp" *index-directory*))

(defun package-index-pathname (package)
  (merge-pathnames (make-pathname :name (package-string package) 
				  :type "sexp" )
		   (merge-pathnames "packages/" *index-directory*)))

(defun generate-index (package-designator)
  "Generate Hyperdoc index for the designated package."
  (unless *index*
    (setf *index* (read-index)))
  (let* ((package (or (find-package package-designator)
		     (error "No such package: ~S." package-designator)))
	 (name (package-name package))
	 (all-names (cons name (package-nicknames package)))
	 (base-uri (base-uri package))
	 (name-table (name-table *index*))
	 (base-uri-table (base-uri-table *index*))
	 (package-table (package-table *index*)))

    ;; Clear old entries
    (let (old-name)
      (maphash (lambda (key value)
		 (when (member key all-names :test #'string=)
		   (setf old-name value)
		   (remhash key name-table)))
	       name-table)
      (remhash old-name base-uri-table)
      (remhash old-name package-table)
      ;; Handle case where the canonical name has changed
      (when (gethash old-name name-table)
	(remhash old-name name-table)))

    ;; New entries
    (dolist (n all-names)
      (setf (gethash n name-table) name))
    (setf (gethash name base-uri-table) base-uri)
    (let ((symbol-table (make-hash-table :test #'equal)))
      (do-external-symbols (sym package)
	(let ((docs (%lookup sym)))
	  (when docs
	    (setf (gethash (symbol-name sym) symbol-table) docs))))
      (setf (gethash name package-table) symbol-table))

    ;; Save
    (ensure-directories-exist *index-directory*)
    (with-standard-io-syntax 
      (with-open-file (f (name-index-pathname)
			 :direction :output
			 :if-exists :rename)
	(write-line *index-herald* f)
	(prin1 *name-index-version* f)
	(terpri f)
	(prin1 (hash-alist name-table) f)
	(terpri f))
      (let ((package-index (package-index-pathname name)))
	(ensure-directories-exist package-index)
	(with-open-file (f package-index
			   :direction :output
			   :if-exists :rename)
	  (write-line *index-herald* f)
	  (prin1 *package-index-version* f)
	  (terpri f)
	  (prin1 `(("BASE-URI" . ,base-uri)
		   ("SYMBOLS" . ,(hash-alist (gethash name package-table))))
		 f)
	  (terpri f))))))

(defun read-index ()
  (let* ((index (make-instance 'index))
	 (pathname (name-index-pathname))
	 (names (when (probe-file pathname)
		  (with-open-file (f pathname)
		    (unless (equal *name-index-version* (read f))
		      (error "Name index version mismatch. Oh dear."))
		    (read f)))))
    (when names
      (dolist (n names)
	(setf (gethash (car n) (name-table index)) (cdr n)))
      (maphash (lambda (nick name)
		 (declare (ignore nick))
		 (with-open-file (f (package-index-pathname name))
		   (unless (equal *package-index-version* (read f))
		     (error "Package index version mismatch. Opps."))
		   (let ((package-index (read f)))
		     (setf (gethash name (package-table index))
			   (alist-hash (cdr (assoc "SYMBOLS" package-index
						   :test #'string=))
				       :test #'equal))
		     (setf (gethash name (base-uri-table index))
			   (cdr (assoc "BASE-URI" package-index :test #'string=))))))
	       (name-table index)))
    (if names
	(values index t)
	(values index nil))))


