;;;; $Id$
;;;; $Source$

;;;; See the LICENSE file for licensing information.

(in-package :net-nittin-irc-test)

(defvar *nick1* "kire")
(defvar *nick2* "k|[]re")
(defvar *nick3* "k^{]re")
(defvar *chan1* "#liSP")

(deftest normalize-nickname.1 (irc:normalize-nickname *nick1*) "kire")
(deftest normalize-nickname.2 (irc:normalize-nickname *nick2*) "k\\[]re")
(deftest normalize-nickname.3 (irc:normalize-nickname *nick3*) "k~[]re")

(deftest normalize-channel-name.1 (irc:normalize-channel-name *chan1*) "#lisp")
