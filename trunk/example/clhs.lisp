;;;; $Id$
;;;; $Source$

;;;; clhs.lisp - an example IRC bot for cl-irc

;;; clhs is an example IRC bot for cl-irc. It runs on
;;; irc.freenode.net in the channels #lisp and #clhs (preferred for
;;; testing). It responds to queries of the form "clhs symbol" for
;;; symbols in the spec, "clhs 3.1.2.1.2.1" for sections, and "clhs
;;; format:A" for format control args. You will want to edit
;;; *hyperspec-pathname* to point to where you have the HyperSpec
;;; unpacked. You should also check out Mop_Sym.txt and put it in the
;;; directory where you will be running the bot from.

;;; To use it, load the cl-irc system, load clhs.lisp, and
;;; invoke (clhs::start-clhs-bot "desirednickname" "desiredserver"
;;; "#channel1" "#channel2" "#channel3" ...)

(defpackage :clhs (:use :common-lisp :irc) (:export :start-clhs-bot))
(in-package :clhs)

;;; CLHS. This will be the default lookup.
(defparameter *hyperspec-pathname* #p"/home/bmastenbrook/HyperSpec/")

(defparameter *hyperspec-map-file* (merge-pathnames "Data/Map_Sym.txt" *hyperspec-pathname*))

(defparameter *hyperspec-root* "http://www.lispworks.com/reference/HyperSpec/")

;;; AMOP.
(defparameter *mop-map-file* #p"Mop_Sym.txt")

(defparameter *mop-root* "http://www.alu.org/mop/")

(defvar *table* (make-hash-table :test 'equalp))
                                                   
(defun add-clhs-section-to-table (&rest numbers)
  (let ((key (format nil "~{~d~^.~}" numbers))
        (target (concatenate 'string *hyperspec-root* (format nil "Body/~2,'0d_~(~{~36r~}~).htm" (car numbers) (mapcar #'(lambda (x) (+ x 9)) (cdr numbers))))))
    (setf (gethash key *table*) target)))

(defun valid-target (&rest numbers)
  (probe-file (format nil "Body/~2,'0d_~(~{~36r~}~).htm" (car numbers) (mapcar #'(lambda (x) (+ x 9)) (cdr numbers)))))

(defun populate-table ()
  ;; Hyperspec
  (with-open-file (s *hyperspec-map-file*)
    ;; populate the table with the symbols from the Map file
    ;; this bit is easy and portable.
    (do ((symbol-name (read-line s nil s) (read-line s nil s))
         (url (read-line s nil s) (read-line s nil s)))
        ((eq url s) 'done)
      (setf (gethash symbol-name *table*) (concatenate 'string *hyperspec-root* (subseq url 3))))
    ;; add in section references.
    (let ((*default-pathname-defaults* *hyperspec-pathname*))
      ;; Yuk. I know. Fixes welcome.
      (loop for section from 0 to 27
            do (add-clhs-section-to-table section)
            do (loop named s for s1 from 1 to 26
                     unless (valid-target section s1)
                       do (return-from s nil)
                     do (add-clhs-section-to-table section s1)
                       do (loop named ss for s2 from 1 to 26
                                unless (valid-target section s1 s2)
                                  do (return-from ss nil)
                                do (add-clhs-section-to-table section s1 s2)
                                do (loop named sss for s3 from 1 to 26
                                         unless (valid-target section s1 s2 s3)
                                           do (return-from sss nil)
                                         do (add-clhs-section-to-table section s1 s2 s3)
                                         do (loop named ssss for s4 from 1 to 26
                                                  unless (valid-target section s1 s2 s3 s4)
                                                    do (return-from ssss nil)
                                                  do (add-clhs-section-to-table section s1 s2 s3 s4)
                                                  do (loop named sssss for s5 from 1 to 26
                                                           unless (valid-target section s1 s2 s3 s4 s5)
                                                             do (return-from sssss nil)
                                                           do (add-clhs-section-to-table section s1 s2 s3 s4 s5))))))))
    ;; format directives
    (loop for code from 32 to 127
          do (setf (gethash (format nil "format:~A" (code-char code)) *table*)
                   (concatenate 'string
                    *hyperspec-root*
                    (case (code-char code)
                      ((#\c #\C) "Body/22_caa.htm")
                      ((#\%) "Body/22_cab.htm")
                      ((#\&) "Body/22_cac.htm")
                      ((#\|) "Body/22_cad.htm")
                      ((#\~) "Body/22_cae.htm")
                      ((#\r #\R) "Body/22_cba.htm")
                      ((#\d #\D) "Body/22_cbb.htm")
                      ((#\b #\B) "Body/22_cbc.htm")
                      ((#\o #\O) "Body/22_cbd.htm")
                      ((#\x #\X) "Body/22_cbe.htm")
                      ((#\f #\F) "Body/22_cca.htm")
                      ((#\e #\E) "Body/22_ccb.htm")
                      ((#\g #\G) "Body/22_ccc.htm")
                      ((#\$) "Body/22_ccd.htm")
                      ((#\a #\A) "Body/22_cda.htm")
                      ((#\s #\S) "Body/22_cdb.htm")
                      ((#\w #\W) "Body/22_cdc.htm")
                      ((#\_) "Body/22_cea.htm")
                      ((#\<) "Body/22_ceb.htm")
                      ((#\i #\I) "Body/22_cec.htm")
                      ((#\/) "Body/22_ced.htm")
                      ((#\t #\T) "Body/22_cfa.htm")
                      ;; FIXME
                      ((#\<) "Body/22_cfb.htm")
                      ((#\>) "Body/22_cfc.htm")
                      ((#\*) "Body/22_cga.htm")
                      ((#\[) "Body/22_cgb.htm")
                      ((#\]) "Body/22_cgc.htm")
                      ((#\{) "Body/22_cgd.htm")
                      ((#\}) "Body/22_cge.htm")
                      ((#\?) "Body/22_cgf.htm")
                      ((#\() "Body/22_cha.htm")
                      ((#\)) "Body/22_chb.htm")
                      ((#\p #\P) "Body/22_chc.htm")
                      ((#\;) "Body/22_cia.htm")
                      ((#\^) "Body/22_cib.htm")
                      ((#\Newline) "Body/22_cic.htm")
                      (t "Body/22_c.htm")))))
    ;; glossary.
    )
  ;; MOP
  (with-open-file (s *mop-map-file*)
    (do ((symbol-name (read-line s nil s) (read-line s nil s))
         (url (read-line s nil s) (read-line s nil s)))
        ((eq url s) 'done)
      (setf (gethash (concatenate 'string "MOP:" symbol-name) *table*) (concatenate 'string *mop-root* url)))))

(defvar *clhs-connection*)
(defvar *clhs-nickname*)

(defmacro aif (test conseq &optional (else nil))
  `(let ((it ,test))
     (if it ,conseq
       (symbol-macrolet ((it ,test))
         ,else))))

(defun spec-lookup (str)
  (aif (gethash str *table*)
       it
       (format nil "Nothing was found for: ~A" str)))

(defparameter *clhs-attention-prefixes* '("clhs " "clhs: "))

(defun valid-clhs-message-1 (message prefix)
  (if (eql (search prefix (trailing-argument message) :test #'char-equal) 0)
      (and (not (find #\space (trailing-argument message) :start (length prefix)))
           (length prefix))
      nil))

(defun valid-clhs-message (message)
  (some #'(lambda (e) (valid-clhs-message-1 message e)) *clhs-attention-prefixes*))

(defun msg-hook (message)
  (if (string-equal (first (arguments message)) *clhs-nickname*)
      (aif (valid-clhs-message message)
          (privmsg *clhs-connection* (source message) (spec-lookup (subseq (trailing-argument message) it)))
        (privmsg *clhs-connection* (source message) (spec-lookup (trailing-argument message))))
    (aif (valid-clhs-message message)
        (privmsg *clhs-connection* (first (arguments message)) (spec-lookup (subseq (trailing-argument message) it))))))

(defun start-clhs-bot (nick server &rest channels)
  (populate-table)
  (setf *clhs-nickname* nick)
  (setf *clhs-connection* (connect :nickname *clhs-nickname* :server server))
  (mapcar #'(lambda (channel) (join *clhs-connection* channel)) channels)
  (add-hook *clhs-connection* 'irc::irc-privmsg-message 'msg-hook)
  #+sbcl (start-background-message-handler *clhs-connection*)
  #-sbcl (read-message-loop *clhs-connection*))

(defun shuffle-hooks ()
  (irc::remove-hooks *clhs-connection* 'irc::irc-privmsg-message)
  (add-hook *clhs-connection* 'irc::irc-privmsg-message 'msg-hook))
