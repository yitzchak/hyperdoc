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

(in-package :hyperdoc)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :hyperdoc *features*))


;;; DOCUMENTATION TYPES

(defparameter *documentation-types*
  (list :symbol-macro :macro :condition :method
	:generic-function :class :type :function :compiler-macro :setf
	:method-combination :type :structure :package
	:variable :constant)
  "Documentation types used by Hyperdoc. These correspond to what
DOCUMENTATION uses with a few additions.")

;; adapted from swank.lisp
(defun classify-symbol (symbol)
  (check-type symbol symbol)
  (macrolet ((awhen (cond &body body)
	       `(let ((it ,cond))
		  (when it ,@body))))
    (let (result)
      (flet ((seen? (item &rest more-items)
	       (intersection result (cons item more-items) :test #'eq))
	     (note (item)
	       (push item result)))
        (awhen (find-class symbol nil)
	  (note (cond ((subtypep it (find-class 'condition))
		       :condition)
		      ((subtypep (class-of it) 'structure-class)
		       :structure)
		      (t
		       :class))))
        (when (and (not (seen? :class :structure :condition))
		   (ignore-errors (nth-value 1 (subtypep symbol 'nil))))
	  (note :type))
        (when (macro-function symbol)
	  (note :macro))
        (when (nth-value 1 (macroexpand symbol nil))
	  (note :symbol-macro))
        (when (special-operator-p symbol)
	  (note :special-operator))
        (when (find-package symbol)
	  (note :package))
        (when (boundp symbol)
	  (note (if (constantp symbol)
		    :constant
		    :variable)))
        (when (and (fboundp symbol)
		   (not (seen? :macro :special-operator)))
	  (note (if (typep (fdefinition symbol) 'generic-function)
		    :generic-function
		    :function)))
        (when (and (or (seen? :function :generic-function))
		   (compiler-macro-function symbol))
	  (note :compiler-macro)))
      result)))


;;;; DOCUMENTATION REGISTRATION

(defvar *documentation-registry*
  (make-hash-table
   ;; We don't use packages themselves as key so people can register
   ;; documentation for stuff that may be loaded on-demand. (E.g.
   ;; implementation's contribs.)
   :test 'equal)
  "Mapping from package name (possibly nickname) to documentation data.")


;;; The split into %DD is to ensure that a DD will always be
;;; initialized with all necessary information...

(defstruct (%documentation-descriptor
	     (:conc-name %dd-))
  (base-uri                 (required) :type string)
  (relative-uri-function    (required) :type function)
  (extra-types-function     nil        :type (or null function))
  (normalize-types-function nil        :type (or null function))
  (all-symbols-p            nil        :type boolean))

(defstruct (documentation-descriptor
	     (:include %documentation-descriptor)
	     (:conc-name dd-)
	     ;; ...perhaps I have to go to hell for this, still it was
	     ;; worth it. :-)
	     (:constructor make-documentation-descriptor
                (package-name %dd
		 &aux (base-uri                 (%dd-base-uri %dd))
		      (relative-uri-function    (%dd-relative-uri-function %dd))
		      (extra-types-function     (%dd-extra-types-function %dd))
		      (normalize-types-function (%dd-normalize-types-function %dd))
		      (all-symbols-p            (%dd-all-symbols-p %dd)))))
  (package-name (required) :type string))

(declaim (ftype (function ((or symbol package cons)
                           &rest t
                           &key (:base-uri
                                 string)
                                (:relative-uri-function
                                 (and (not null) (or symbol function)))
                                (:extra-types-function
                                 (or null symbol function))
                                (:normalize-types-function
                                 (or null symbol function))
                                (:all-symbols
                                 boolean)
                           &allow-other-keys)
                          (values (eql t) &optional))
                register-documentation))
(defun register-documentation (packages &rest keys &key base-uri
			                                relative-uri-function
                                                        extra-types-function
                                                        normalize-types-function
                                                        all-symbols
			       &allow-other-keys)
"
 > packages      ::= package-designator | (package-designators+)
 > keys          ::= { :BASE-URI                 string
 >                     :RELATIVE-URI-FUNCTION    function-designator
 >                   [ :EXTRA-TYPES-FUNCTION     function-designator ]
 >                   [ :NORMALIZE-TYPES-FUNCTION function-designator ]
 >                   [ :ALL-SYMBOLS              boolean             ] }+
"
  (declare (ignore base-uri relative-uri-function extra-types-function
		   normalize-types-function all-symbols))
  (assert keys () "You didn't pass any arguments to REGISTER-DOCUMENTATION.")
  (let* ((package-names (mapcar #'package-string (ensure-list packages)))
	 (%dds (parse-register-documentation-keys keys)))
    (dolist (package-name package-names)
      (let ((old-entries (gethash package-name *documentation-registry*))
	    (new-entries (loop for %dd in %dds collect
			       (make-documentation-descriptor package-name %dd))))
        (when (and old-entries (not (equalp new-entries old-entries)))
	  ;; (EQUALP in absence of alternative.)
	  (warn "Overwriting hyperdoc documentation entry for package ~S:~%~
		 Old entries: ~S~%New entries: ~S~%"
                package-name old-entries new-entries))
        (setf (gethash package-name *documentation-registry*) new-entries)))
    t))

(defun parse-register-documentation-keys (keys)
  (labels ((sectionize (keys)
	     (cond
	       ((null keys) nil)
	       ((not (eq (first keys) :base-uri))
                (error ":BASE-URI expected; ~S found." (first keys)))
	       ((length= keys 2) keys)
	       (t
                (let* ((next-section-start (position :base-uri keys :start 2))
		       (current-section    (subseq keys 0 next-section-start))
		       (remaining-keys     (when next-section-start
					     (subseq keys next-section-start))))
		  (list* current-section (sectionize remaining-keys)))))))
    (assert (proper-plist-p keys) () "~S is not a proper plist." keys)
    (loop for section in (sectionize keys) collect
	  (destructuring-bind (&key ((:base-uri base-uri))
				    ((:relative-uri-function rufn))
				    ((:extra-types-function eftn))
				    ((:normalize-types-function ntfn))
				    ((:all-symbols all-symbols)))
	      section
	    (make-%documentation-descriptor
	     :base-uri base-uri
	     :relative-uri-function    (ensure-function rufn)
	     :extra-types-function     (and eftn (ensure-function eftn))
	     :normalize-types-function (and ntfn (ensure-function ntfn))
	     :all-symbols-p all-symbols)))))


;;;; DOCUMENTATION LOOKUP

(declaim (ftype (function
                 ((or package symbol string) string &optional (or symbol cons))
                 (values list &optional))
                lookup))
(defun lookup (package-designator symbol-name &optional doc-types)
  "Looks up documentation for symbol named by `symbol-name' in the
package designated by `package-designator', or any package in its
PACKAGE-USE-LIST.

`doc-types' may either be a symbol, or a list of symbols. If
`doc-types' are given, the lookup is restricted to documentation of
the entities associated with the given `doc-types'.

`doc-types' default to *DOCUMENTATION-TYPES*.

LOOKUP returns a list of applicable (DOC-TYPE . URI-STRING) pairs.

If the designated package does not exist, or if no the designated
symbol does not exist, or if no documentation for the designated
symbol is found, NIL is returned."
  (let ((package (find-package package-designator)))
    (when package
      (introspective-lookup package symbol-name
                            (cond
                              ((null doc-types)
                               *documentation-types*)
                              ((length= doc-types 1)
                               (list doc-types))
                              ((consp doc-types)
                               doc-types))))))

(defun introspective-lookup (package symbol-name doc-types)
  (multiple-value-bind (symbol foundp)
      (find-symbol symbol-name package)
    (when foundp
      (lookup-symbol-using-package symbol package doc-types))))

(defun lookup-symbol-using-package (symbol package doc-types)
  "Looks up hyperdocumentation for SYMBOL in PACKAGE."
  (flet ((try (dd)
	   (when (or (dd-all-symbols-p dd)
		     (exported-symbol-p symbol (dd-package dd)))
	     (loop for type in (union (intersection doc-types (classify-symbol symbol))
				      (extra-types dd symbol))
		   for type*        = (normalize-type dd type)
		   for relative-uri = (and type* (relative-uri dd symbol type*))
		   when relative-uri
		     collect (cons type (merge-uris (dd-base-uri dd) relative-uri))))))
    (mappend #'try (mappend #'find-documentation-descriptors
                            (cons package (package-use-list package))))))

(defun dd-package (entry)
  (being (of-type 'package) (find-package (dd-package-name entry))))

(defun relative-uri (dd symbol type)
  (with-output-to-string (stream)
    (funcall (dd-relative-uri-function dd) stream symbol type)
    (when (zerop (file-position stream))
      (return-from relative-uri nil))))

(defun extra-types (dd symbol)
  (let ((fn (dd-extra-types-function dd)))
    (when fn (funcall fn symbol))))

(defun normalize-type (dd type)
  (let ((fn (dd-normalize-types-function dd)))
    (if fn
        (funcall fn type)
        type)))

;;; Don't we all love Allegro's modern mode?
(defparameter +hyperdoc-base-uri+
  (string '#:*hyperdoc-base-uri*))
(defparameter +hyperdoc-lookup+
  (string '#:hyperdoc-lookup))
(defparameter +hyperdoc-documentation-types+
  (string '#:*hyperdoc-documentation-types*))

(defun find-documentation-descriptors (package)
  (gethash-multiple-keys (package-names package) *documentation-registry*))




;;;;;; These are commented out because I haven't had time to adapt this
;;;;;; code to my changes. (TCR.)

;;;; Static indexes

;; (defvar *index* nil
;;   "In memory index. FIXME: This should be loaded when hyperdoc is
;; loaded -- or at least lazily on first call to X.")

;; (defvar *index-directory* (merge-pathnames ".hyperdoc/" (user-homedir-pathname))
;;   "The directory where Hyperdoc keeps the pregenerated indices.")

;; (defvar *name-index-version* "Hyperdoc Name Index -- Version 1"
;;   "Magic version indentifier used in the name index files.")

;; (defvar *package-index-version* "Hyperdoc Package Index -- Version 1"
;;   "Magic version indentifier used in the package index files.")

;; (defvar *index-herald*
;;   ";;;; This is an automatically generated index -- do not edit by hand!
;; ;;;; See http://www.common-lisp.net/project/hyperdoc/ for more information."
;;   "Herald printed in the beginning of the index file to discourage tampering.")

;; (defun static-lookup (package-name symbol-name doc-type)
;;   "Looks up hyperdocumentation for the symbol in the pregenerated indices."
;;   (unless *index*
;;     (multiple-value-bind (index not-empty-p) (read-index)
;;       (if not-empty-p
;; 	  (setf *index* index)
;; 	  (error "Static index file ~A is empty or does not exist. ~
;;                  You probably forgot to run HYPERDOC:GENERATE-INDEX."
;; 		 (name-index-pathname)))))
;;   (let* ((name (gethash package-name (name-table *index*)))
;; 	 (base-uri (gethash name (base-uri-table *index*))))
;;     (mapcar (lambda (pair)
;; 	      (cons (car pair) (merge-uris (cdr pair) base-uri)))
;; 	    (let ((uris	(gethash symbol-name
;; 				 (gethash name (package-table *index*)))))
;; 	      (if doc-type
;; 		  (assoc doc-type uris)
;; 		  uris)))))

;; (defclass index ()
;;   ((names :accessor name-table
;; 	  :initform (make-hash-table :test #'equal))
;;    (base-uris :accessor base-uri-table
;; 	      :initform (make-hash-table :test #'equal))
;;    (package-tables :accessor package-table
;; 		   :initform (make-hash-table :test #'equal))))


;; (defun name-index-pathname ()
;;   (merge-pathnames "names.sexp" *index-directory*))

;; (defun package-index-pathname (package)
;;   (merge-pathnames (make-pathname :name (package-string package)
;; 				  :type "sexp" )
;; 		   (merge-pathnames "packages/" *index-directory*)))

;; (defun generate-index (package-designator)
;;   "Generate Hyperdoc index for the designated package."
;;   (unless *index*
;;     (setf *index* (read-index)))
;;   (let* ((package (or (find-package package-designator)
;; 		     (error "No such package: ~S." package-designator)))
;; 	 (name (package-name package))
;; 	 (all-names (cons name (package-nicknames package)))
;; 	 (base-uri (base-uri package))
;; 	 (name-table (name-table *index*))
;; 	 (base-uri-table (base-uri-table *index*))
;; 	 (package-table (package-table *index*)))

;;     ;; Clear old entries
;;     (let (old-name)
;;       (maphash (lambda (key value)
;; 		 (when (member key all-names :test #'string=)
;; 		   (setf old-name value)
;; 		   (remhash key name-table)))
;; 	       name-table)
;;       (remhash old-name base-uri-table)
;;       (remhash old-name package-table)
;;       ;; Handle case where the canonical name has changed
;;       (when (gethash old-name name-table)
;; 	(remhash old-name name-table)))

;;     ;; New entries
;;     (dolist (n all-names)
;;       (setf (gethash n name-table) name))
;;     (setf (gethash name base-uri-table) base-uri)
;;     (let ((symbol-table (make-hash-table :test #'equal)))
;;       (do-external-symbols (sym package)
;; 	(let ((docs (%lookup sym)))
;; 	  (when docs
;; 	    (setf (gethash (symbol-name sym) symbol-table) docs))))
;;       (setf (gethash name package-table) symbol-table))

;;     ;; Save
;;     (ensure-directories-exist *index-directory*)
;;     (with-standard-io-syntax
;;       (with-open-file (f (name-index-pathname)
;; 			 :direction :output
;; 			 :if-exists :rename)
;; 	(write-line *index-herald* f)
;; 	(prin1 *name-index-version* f)
;; 	(terpri f)
;; 	(prin1 (hash-alist name-table) f)
;; 	(terpri f))
;;       (let ((package-index (package-index-pathname name)))
;; 	(ensure-directories-exist package-index)
;; 	(with-open-file (f package-index
;; 			   :direction :output
;; 			   :if-exists :rename)
;; 	  (write-line *index-herald* f)
;; 	  (prin1 *package-index-version* f)
;; 	  (terpri f)
;; 	  (prin1 `(("BASE-URI" . ,base-uri)
;; 		   ("SYMBOLS" . ,(hash-alist (gethash name package-table))))
;; 		 f)
;; 	  (terpri f))))))

;; (defun read-index ()
;;   (let* ((index (make-instance 'index))
;; 	 (pathname (name-index-pathname))
;; 	 (names (when (probe-file pathname)
;; 		  (with-open-file (f pathname)
;; 		    (unless (equal *name-index-version* (read f))
;; 		      (error "Name index version mismatch. Oh dear."))
;; 		    (read f)))))
;;     (when names
;;       (dolist (n names)
;; 	(setf (gethash (car n) (name-table index)) (cdr n)))
;;       (maphash (lambda (nick name)
;; 		 (declare (ignore nick))
;; 		 (with-open-file (f (package-index-pathname name))
;; 		   (unless (equal *package-index-version* (read f))
;; 		     (error "Package index version mismatch. Opps."))
;; 		   (let ((package-index (read f)))
;; 		     (setf (gethash name (package-table index))
;; 			   (alist-hash (cdr (assoc "SYMBOLS" package-index
;; 						   :test #'string=))
;; 				       :test #'equal))
;; 		     (setf (gethash name (base-uri-table index))
;; 			   (cdr (assoc "BASE-URI" package-index :test #'string=))))))
;; 	       (name-table index)))
;;     (if names
;; 	(values index t)
;; 	(values index nil))))


