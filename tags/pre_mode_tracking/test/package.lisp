;;;; $Id$
;;;; $Source$

;;;; See the LICENSE file for licensing information.

(in-package :cl-user)

(eval-when (:execute :load-toplevel :compile-toplevel)
  (defpackage :cl-irc-test
      (:use :cl :rt)
    (:nicknames :cl-irc-test)
    (:export :do-tests)))
