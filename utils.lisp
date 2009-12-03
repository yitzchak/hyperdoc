(in-package :hyperdoc)

;;;; Utility functions

(defun being (predicate value)
  (assert (funcall predicate value) (value) "~S failed ~S." value predicate)
  value)

(defun proper-plist-p (list)
  ;; Kludge: Does not work on dotted lists.
  (let ((length (list-length list)))
    (and length (evenp length))))

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
  "Like GETHASH but takes multiple keys."
  (loop for key in keys do (multiple-value-bind (value foundp)
			       (gethash key hashtable)
			     (when foundp
			       (return (values value foundp))))))

(defun exported-symbol-p (symbol &optional (package (symbol-package symbol)))
  (eq (nth-value 1 (find-symbol (symbol-name symbol) package))
      :external))


;; from Alexandria..

(declaim (inline ensure-function))	; to propagate return type.

(declaim (ftype (function (t) (values function &optional))
                ensure-function))
(defun ensure-function (function-designator)
  "Returns the function designated by FUNCTION-DESIGNATOR:
if FUNCTION-DESIGNATOR is a function, it is returned, otherwise
it must be a function name and its FDEFINITION is returned."
  (if (functionp function-designator)
      function-designator
      (fdefinition function-designator)))

(defun required (&optional name)
  "Signals an error for a missing argument of NAME. Intended for
use as an initialization form for structure and class-slots, and
a default value for required keyword arguments."
  (error "Required argument ~@[~S ~]missing." name))

(defun of-type (type)
  "Returns a function of one argument, which returns true when its
argument is of TYPE."
  (lambda (thing) (typep thing type)))

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

(defun mappend (function &rest lists)
  "Applies FUNCTION to respective element(s) of each LIST, appending all the
all the result list to a single list. FUNCTION must return a list."
  (loop for results in (apply #'mapcar function lists)
        append results))


;; from Swank..

(defun package-names (package)
  "Return the name and all nicknames of PACKAGE in a fresh list."
  (cons (package-name package) (copy-list (package-nicknames package))))


(defun length= (seq n)
  "Test for whether SEQ contains N number of elements. I.e. it's equivalent
 to (= (LENGTH SEQ) N), but besides being more concise, it may also be more
 efficiently implemented."
  (etypecase seq
    (list (do ((i n (1- i))
               (list seq (cdr list)))
              ((or (<= i 0) (null list))
               (and (zerop i) (null list)))))
    (sequence (= (length seq) n))))