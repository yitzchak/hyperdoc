;;;; -*- Mode: Lisp -*-
;;;;
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

;;;; Pre-registered documentations

(in-package :hyperdoc)

(defun write-string-for-texinfo (string stream)
  (with-standard-io-syntax
    (let ((*print-case* :downcase))
      (loop for char across string do
            (if (alphanumericp char) 
                (format stream "~(~C~)" char)
                (format stream "~(_~4,'0X~)" (char-code char)))))))


;;;; CFFI

;;; FIXME: This should be incorporated into cffi

(register-documentation :cffi
  :base-uri "http://common-lisp.net/project/cffi/manual/html_node/"
  :relative-uri-function
  #'(lambda (stream symbol type)
      (declare (ignore type))
      (let ((symbol-name (symbol-name symbol)))
        (write-string-for-texinfo symbol-name stream)
        (write-char #\# stream)
        ;; Why, oh why, does texinfo's html cross-reference
        ;; scheme seem to be so irregularly weird?
        (when (char= (char symbol-name 0) #\*)
          (write-string "g_t" stream))
        (write-string-for-texinfo symbol-name stream))))


;;;; Ediware

;;; We pre-register all these to convince the world that Hyperdoc is
;;; useful, and thus to gain traction in the Lisp community.

(defvar *ediware*
  '(:capi-overview
    :chunga
    :cl-dongle
    :cl-fad
    :cl-gd
    :cl-interpol
    :cl-ppcre
    :cl-unicode
    :cl-wbxml
    :cl-who
    :documentation-template
    :drakma
    :flexi-streams
    :fm-plugin-tools
    :html-extract
    :html-template
    :hunchentoot
    :lw-add-ons
    :lw-doc
    :lw-win
    :midgets
    :odd-streams
    :rdnzl
    :regex-plugin
    :starter-pack
    :tbnl
    :url-rewrite
    ))

(dolist (package *ediware*)
  (register-documentation package
    :base-uri (format nil "http://weitz.de/~(~A~)/" package)
    :relative-uri-function (formatter "#~(~A~)")))


;;;; ASDF

(register-documentation :asdf
  :base-uri "http://common-lisp.net/project/asdf/manual.html"
  :relative-uri-function
  #'(lambda (stream symbol type)
      (format stream "#~(~A~)." type)
      (loop for char across (symbol-name symbol) do
            (if (alphanumericp char) 
                (format stream "~(~C~)" char)
                (format stream ":~(~2,'0X~)" (char-code char)))))
  :normalize-types-function
  #'(lambda (type)
      (case type
        ((:function :generic-function) :function)
        ((:variable)                   :variable)
        (t nil))))

;;;; MOP

(defvar *mop-packages*
  (list #+sbcl      :sb-mop
        #+lispworks :clos
        #+openmcl   :openmcl-mop
        #+cmucl     :pcl
        #+ecl       :clos
        #+abcl      :mop
        #+scl       :clos
        #+clisp     :clos
        ;; Allegro has its own definition below.
        ))

(when *mop-packages*
  (register-documentation *mop-packages*
    :base-uri hs:*mop-root*
    :relative-uri-function
    #'(lambda (stream symbol type)
        (declare (ignore type))
        (let ((uri (gethash (symbol-name symbol) hs::*mop-table*)))
          (when uri (princ uri stream))))))

;;;; CL

(register-documentation :common-lisp
  :base-uri hs:*hyperspec-root*
  :relative-uri-function
  #'(lambda (stream symbol type)
      (declare (ignore type))
      (let ((uri (gethash (symbol-name symbol) hs::*hyperspec-table*)))
        (when uri (princ uri stream)))))

;;;; Franz documentation

#+allegro
(progn

 (defvar *allegro-doc-packages*
  '(:common-graphics :compiler :composer
    :dbi :defsys
    :excl
    :ff
    :javatools.jil :javatools.jlinker
    :mp
    :net.post-office :net.rpc :net.uri
    :prof
    :socket :system
    :top-level
    :util.test
    :xref))

(defun translate-asterisks (symbol-name)
  (excl:replace-regexp (excl:replace-regexp symbol-name "^\\*" "s_")
		       "\\*$" "_s"))

(defun relative-uri-for-franz-doc (stream symbol type)
  (let ((kind (case type
                ((:function :generic-function :macro) "operators")
                ((:variable :constant)                "variables")
                ((:class)                             "classes")
                (t (return-from relative-uri-for-franz-doc nil))))
        (package 
         (loop for pkg-name in (package-names (symbol-package symbol))
               thereis (find pkg-name *allegro-doc-packages*
                             :test #'string-equal)))
        (symbol (translate-asterisks (symbol-name symbol))))
    (format stream "~A/~A/~A.htm" kind package symbol)))

(register-documentation *allegro-doc-packages*
  :base-uri "http://franz.com/support/documentation/current/doc/"
  :relative-uri-function 'relative-uri-for-franz-doc)

(register-documentation :mop
  :base-uri "http://franz.com/support/documentation/current/doc/mop/dictionary.html"
  :relative-uri-function (formatter "#~A"))

;;; For some strange reason EXCL.OSI does not fall under Franz'
;;; regular documentation scheme.

(defun relative-uri-for-franz-osi (stream symbol type)
  (multiple-value-bind (file kind)
      (case type
        ((:function :generic-function :macro) (values "os-interface" "op"))
        ((:variable :constant)                (values "osi-constants" "var"))
        ((:class)                             (values "os-interface" "class"))
        (t (return-from relative-uri-for-franz-osi nil)))
    (format stream "~A.htm#~A-~A-bookmarkxx" 
            file (translate-asterisks (symbol-name symbol)) kind)))

(register-documentation :excl.osi
  :base-uri "http://www.franz.com/support/documentation/current/doc/"
  :relative-uri-function 'relative-uri-for-franz-osi)

) ; #+allegro (progn ...

;;;; Clozure CL documentation

;; FIXME: untested

#+ccl
(register-documentation :ccl
  :base-uri "http://ccl.clozure.com/ccl-documentation.html"
  :relative-uri-function
  #'(lambda (stream symbol type)
      (block lambda
        (format stream "#~A_~A"
                (case type
                  ((:function :generic-function) "f")
                  ((:macro)                      "m")
                  ((:variable)                   "v")
                  ((:class)                      "c")
                  (t (return-from lambda nil)))
                (remove-if #'(lambda (c) (char= c #\*)) 
                           (symbol-name symbol))))))

;;;; SBCL manual

;;; FIXME: It doesn't seem to be possible to write a
;;; relative-uri-function for SBCL's manual at the moment.


#+sbcl
(progn

(defun sbcl-package-p (package)
  (let ((name (package-name package)))
    (eql (mismatch "SB-" name) 3)))

(defvar *sbcl-packages* 
  (remove-if-not #'sbcl-package-p (list-all-packages)))

(flet ((find-op-idx (symbol)
         (position symbol *ops*
                   :test #'(lambda (s ops)
                             (member s ops :test #'string-equal))
                   :key #'(lambda (entry)
                            (if (symbolp entry)
                                (list entry)
                                entry)))))
  (register-documentation :sb-vm
    :base-uri "http://siyobik.info/"
    :relative-uri-function
    #'(lambda (stream symbol type)
        (declare (ignore type))
        (let ((idx (find-op-idx symbol)))
          (when idx
            (format stream "index.php?module=x86&id=~D" idx))))
    :extra-types-function
    #'(lambda (s) (and (find-op-idx s) '(:instruction)))
    :normalize-types-function
    #'(lambda (type) (and (eq type :instruction) :instruction))
    :all-symbols t))

(defvar *ops*
  '(<dummy>                             ; &id= start at 1

    aaa aad aas adc add addpd addps addsd addss addsubpd addsubps and
    andpd andps andnpd andnps arpl

    bound bsf bsr bswap bt btc btr bts

    call (cbw cwde) clc cld clflush cli clts cmc cmovcc cmp cmppd cmpps
    (cmps cmpsb cmpsw cmpsd) cmpsd cmpss cmpxchg cmpxchg8b comisd comiss
    cpuid cvtdq2pd cvtdq2ps cvtpd2dq cvtpd2pi cvtpd2ps cvtpi2pd
    cvtpi2ps cvtps2dq cvtps2pd cvtps2pi cvtsd2si cvtsd2ss cvtsi2sd
    cvtsi2ss cvtss2sd cvtss2si cvttpd2pi cvttpd2dq cvttps2dq cvttps2pi
    cvttsd2si cvttss2si (cwd cdq)

    daa das dec div divpd divps divsd divss emms enter
    
    f2xm1 fabs (fadd faddp fiadd) fbld fbstp fchs (fclex fnclex) fcmovcc
    (fcom fcomp fcompp) (fcomi fcomip fucomi fucomip) fcos fdecstp
    (fdiv fdivp fidiv) (fdivr fdivrp fidivr) ffree (ficom ficomp) fild
    fincstp (finit fninit) (fist fistp) fisttp fld
    (fld1 fldl2t fldl2e fldpi fldlg2 fldln2 fldz) fldcw fldenv
    (fmul fmulp fimul) fnop fpatan fprem fprem1 fptan frndint frstor
    (fsave fnsave) fscale fsin fsincos fsqrt (fst fstp) (fstcw fnstcw)
    (fstenv fnstenv) (fstsw fnstsw) (fsub fsubp fisub) (fsubr fsubrp fisubr)
    ftst (fucom fucomp fucompp) fxam fxch fxrstor fxsave fxtract fyl2x
    fyl2xp1


    haddpd haddps hlt hsubpd hsubps

    idiv imul
    in inc (ins insb insw insd) int invd invlpg (iret iretd)

    jcc jmp

    lahf lar lddqu ldmxcsr (lds les lfs lgs lss) lea leave lfence
    (lgdt lidt) lldt lmsw lock (lods lodsb lodsw lodsd) (loop loopcc) lsl
    ltr

    maskmovdqu maskmovq maxpd maxps maxsd maxss mfence minpd minps
    minsd minss monitor mov mov mov movapd movaps movd movddup movdqa
    movdqu movdq2q movhlps movhpd movhps movlhps movlpd movlps
    movmskpd movmskps movntdq movnti movntpd movntps movntq movshdup
    movsldup movq movq2dq (movs movsb movsw movsd) movsd movss movsx
    movupd movups movzx mul mulpd mulps mulsd mulss mwait

    neg nop not

    or orpd orps out (outs outsb outsw outsd)

    (packsswb packssdw) packuswb (paddb paddw paddd) paddq (paddsb paddsw)
    (paddusb paddusw) pand pandn pause (pavgb pavgw)
    (pcmpeqb pcmpeqw pcmpeqd) (pcmpgtb pcmpgtw pcmpgtd) pextrw pinsrw
    pmaddwd pmaxsw pmaxub pminsw pminub pmovmskb pmulhuw pmulhw pmullw
    pmuludq pop (popa popad) (popf popfd) por prefetchh psadbw pshufd
    pshufhw pshuflw pshufw pslldq (psllw pslld psllq) (psraw psrad) psrldq
    (psrlw psrld psrlq) (psubb psubw psubd) psubq (psubsb psubsw)
    (psubusb psubusw) (punpckhbw punpckhwd punpckhdq punpckhqdq)
    (punpcklbw punpcklwd punpckldq punpcklqdq) push (pusha pushad)
    (pushf pushfd) pxor

    (rcl rcr rol ror) rcpps rcpss rdmsr rdpmc rdtsc
    (rep repe repz repne repnz) ret rsm rsqrtps rsqrtss

    sahf (sal sar shl shr) sbb (scas scasb scasw scasd) setcc sfence sgdt
    shld shrd shufpd shufps sidt sldt smsw sqrtpd sqrtps sqrtsd sqrtss
    stc std sti stmxcsr (stos stosb stosw stosd) str sub subpd subps
    subsd subss sysenter sysexit

    test

    ucomisd ucomiss ud2 unpckhpd unpckhps unpcklpd unpcklps
    
    (verr verw)

    (wait fwait) wbinvd wrmsr

    xadd xchg (xlat xlatb) xor xorpd xorps))

) ; #+sbcl (progn ...
