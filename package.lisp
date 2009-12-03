
(in-package :common-lisp-user)

(defpackage :hyperdoc
  (:use :cl)
  (:export #:*documentation-types* #:register-documentation #:lookup)
  (:documentation
   "
* What is Hyperdoc?

  Hyperdoc is a hypertext documentation support system for Common Lisp,
  licensed under a MIT-style license. Basically, it takes a symbol and
  outputs a URL to that symbol's documentation.

  It's supposed to be used a) by Common Lisp development environments to
  provide arbitrary documentation look up on key press, and b) by
  library authors to make their library's documentation conveniently
  available to users of their library.

* Notes on the API

  Although we have made Hyperdoc available to the public, We do not
  consider it to be released yet. The effective difference between these
  two states is that you won't get any guarantee from us regarding
  backwards-incompatible changes -- although we'll try to avoid them,
  such changes may turn out to be necessary to overcome current
  limitations of Hyperdoc.

* Limitations

  Hyperdoc currently only supports introspective lookup of
  documentation -- this means that the symbols you want to inquire
  documentation for must be loaded into the running Lisp image (along
  their respective packages, of course.)

  In particular, generating a static indexing for all symbols of a
  package to avoid the necessity of introspection is not currently
  supported. Likewise, downloading documentation to the hard disk for
  offline usage is not supported either.

  The reason behind these restrictions is that personally, we can live
  with an introspective, online facility for now. Patches are welcome,
  of course. (In fact, it may be a nice down-to-the-earth project to use
  Drakma and Montezuma for this purpose.)

* Usage (IDE User)

  You only have to download Hyperdoc, and link its .asd file to ASDF's
  central-registry; your development environment should actually care
  for the rest. (If it doesn't, pleaser refer the respective authors to
  this document and nicely ask them to add support for Hyperdoc.)

  In case you're using Slime, you have to enable the slime-hyperdoc
  contrib (or the slime-fancy meta contrib), and the key combination
  \"C-c C-d h\" will inquire Hyperdoc about the symbol at your cursor's
  position.

* Usage (IDE Hacker)

  All you have to do is call out to LOOKUP on key press. Notice that it
  may return multiple URLs, you have to decide how to handle them. You
  should probably call LOOKUP with the current buffer package as first
  argument -- except if the symbol is explicitly qualified.

* Usage (Library Hacker)

  Registering your project's documentation to Hyperdoc is very
  easy. Depending on how complicated the linking scheme of your
  documentation generation tool is, the efforts range from being
  straightforward to being a bit more involved.

  You have to perform the following steps:

    * Your ASDF system should depend on Hyperdoc weakly by means of
      the :WEAKLY-DEPENDS-ON option of DEFSYSTEM. A weak dependency will
      only be loaded if the depended system is actually installed at the
      user's site.

    * You should include a call to REGISTER-DOCUMENTATION
      (reader-conditionalized on the :HYPERDOC feature) somewhere in
      your sources:

      > #+hyperdoc (hyperdoc:register-documentation :foo ...)

    * In the call to REGISTER-DOCUMENTATION, you have to specify the
      base (or root) URL that all your documentation entries share, and
      a function which, given a symbol, computes the remaining part to
      complement the URL.

  For example, registering documentation that has been created via Edi
  Weitz' Documentation-Tool is trivial:

  > (defvar *ediware*
  >   '(:capi-overview
  >     :chunga
  >     :cl-dongle
  >     :cl-fad
  >     ;; ...
  >     ))
  >
  > (dolist (package *ediware*)
  >   (register-documentation package
  >     :base-uri (format nil \"http://weitz.de/~(~A~)/\" package)
  >     :relative-uri-function (formatter \"#~(~A~)\")))

  Notice how first an /output/ /string stream/, then the symbol being
  looked up, and then the desired documentation type is passed to
  `relative-uri-function' -- as you can see, this makes it viable to
  concisely specify everything that is needed by using FORMATTER in
  trivial cases.

  REGISTER-DOCUMENTATION provides additional means to customize the
  lookup process. The next two sections will discuss them.

* Advanced Usage: Documentation Type Normalization

  As symbols can denote multiple entities (Lisp-N ftw!), some
  documentation tools generate html anchors with a prefix to
  distcriminate between different entities. For example, let's assume
  your documentation tool generates anchors like
  \"#function:symbol-name\", or \"#variable:symbol-name\". For that
  purpose, the `relative-uri-function' is called with a documentation
  type as third argument that describes what entity is currently
  involved.

  However, the documentation type that is passed is very specific. For
  example, Hyperdoc distinguishes between :FUNCTION and
  :GENERIC-FUNCTION even though your documentation tool may coalesce
  these two. (See *DOCUMENTATION-TYPES*)

  In that case you'd have to write

  > (register-documentation :foo
  >   :base-uri \"http://foo.com/\"
  >   :relative-uri-function
  >   #'(lambda (stream symbol type)
  >       (format stream \"#~(~A~):~(~A~)\"
  >               (case type
  >                 ((:function :generic-function) :function)
  >                 ;; ...
  >                 )
  >               symbol)))

  As that case may be quite common, you can actually write

  > (register-documentation :foo
  >   :base-uri \"http://foo.com/\"
  >   :relative-uri-function (formatter \"#~(~*~A~):~(~@*~A~)\")
  >   :normalize-types-function
  >   #'(lambda (type)
  >       (case type
  >         ((:function :generic-function) :function)
  >         ;; ...
  >        )))
  
  Of course, the elegance of the FORMAT noise is in the eye of the
  beholder.

* Advanced Usage: Documentation Type Filtering

  When a `normalize-types-function' returns NIL for a type, the
  `relative-uri-function' won't be invoked for that type.

* Advanced Usage: Extra Documentation Types

  The LOOKUP function determines all valid documentation types for the
  passed symbol before it invokes `relative-uri-function' for each
  determined type. (See *DOCUMENTATION-TYPES*)

  You may want to extend the determination process.

  For example, let's say you have a package ASSEM which includes symbols
  that denote instructions. The instructions are just symbols, in
  particular they're not represented by macros, functions, or anything
  else (for instance they may only be hashes into some internal
  hash-table.) In that case, you'd do the following:

  > (register-documentation :assem
  >   :base-uri \"http://asmreference.invalid/\"
  >   :relative-uri-function (formatter \"instructions.php&op=~A\")
  >   :extra-types-function
  >   #'(lambda (symbol) (and (member symbol *ops*) '(:instruction)))
  >   :normalize-types-function
  >   #'(lambda (type) (and (eq type :instruction) :instruction))) ; filter

  If the instruction symbols are not exported from ASSEM, you'd have to
  add an :ALL-SYMBOLS T.
"))


