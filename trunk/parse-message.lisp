;;;; $Id$
;;;; $Source$

;;;; See the LICENSE file for licensing information.

(in-package :irc)

(defun find-reply-name (reply-number &key (reply-names *reply-names*))
  "Numeric replies in the IRC RFCs have more meaningful names.  Given
a numeric reply (`reply-number') this function will either return the
symbol representing the reply or raise a continuable error
(`no-such-reply') which gives you the opportunity to ignore the
situation."
  (let ((name (assoc reply-number reply-names)))
    (if name
        (cadr name)
        (progn
          (cerror "Ignore unknown reply."
                  'no-such-reply :reply-number reply-number)
          :unknown-reply))))

(defun return-source (string &key (start 0))
  "Assuming `string' is a valid IRC message this function returns the
source part of the message.  Returns nil if the source part is not
present."
  (cut-between string #\: '(#\! #\Space) :start start))

(defun return-user (string &key (start 0))
  "Assuming `string' is a valid IRC message this function returns the
user part of the message.  Returns nil if the user part is not
present."
  (cut-between string #\! '(#\@ #\Space) :start start))

(defun return-host (string &key (start 0))
  "Assuming `string' is a valid IRC message this function returns the
host part of the message.  Returns nil if the host part is not
present."
  (cut-between string #\@ '(#\Space) :start start))

(defun return-command (string &key (start 0))
  "Assuming `string' is a valid IRC message this function returns the
command part of the message.  Returns nil if the command part is not
present."
  (if (eql (char string start) #\Space)
      (cut-between string #\Space '(#\Space) :start start)
      (cut-between string nil '(#\Space) :start start :cut-extra nil)))

(defun return-arguments (string &key (start 0))
  "Assuming `string' is a valid IRC message this function returns the
arguments part of the message as a list.  Returns nil if the arguments
part is not present."
  (multiple-value-bind (end-position return-argument)
      (cut-between string nil '(#\: #\Return) :start start)
    (values end-position (tokenize-string return-argument
                                          :delimiters '(#\Space)))))

(defun return-trailing-argument (string &key (start 0))
  "Assuming `string' is a valid IRC message this function returns the
trailing-argument part of the message.  Returns nil if the
trailing-argument part is not present."
  (cut-between string #\: '(#\Return) :start start))

(defun parse-raw-message (string &key (start 0))
  "Assuming `string' is a valid IRC message, parse the message and
return the values in the following order:

  - source
  - user
  - host
  - command
  - arguments
  - trailing-argument

Any values not present will be represented as nil."
  (let ((index start)
        (returns nil))
    (dolist (function '(return-source
                        return-user
                        return-host
                        return-command
                        return-arguments
                        return-trailing-argument))
      (multiple-value-bind (return-index return-string)
          (funcall function string :start index)
        (setf index return-index)
        (push return-string returns)))
    (apply #'values (reverse returns))))

(defun irc-error-reply-p (string)
  "Returns t if `string' is a string-representation of an IRC error
reply message, nil otherwise."
  (unless (zerop (length string))
    (if (and (every #'digit-char-p string)
             (member (char string 0) '(#\4 #\5)))
        t
        nil)))

(defun numeric-reply-p (string)
  "Returns t if `string' is a string-representation of an IRC number
reply, nil otherwise."
  (every #'digit-char-p string))

(defun ctcp-type-p (string type)
  "Is the `string' actually a representation of the CTCP `type'?"
  (if (string-equal (subseq string 1 (min (length string) 
                                          (1+ (length (symbol-name type))))) 
                    type)
      type
      nil))
                                                       
(defun dcc-type-p (string type)
  "Is the `string' actually a representation of the DCC `type'?"
  (case type
    (:dcc-chat-request
     (when (string-equal (char string 5) #\C)
       :dcc-chat-request))
    (:dcc-send-request
     (when (string-equal (char string 5) #\S)
       :dcc-send-request))
    (otherwise nil)))

(defun ctcp-message-type (string)
  "If `string' is a CTCP message, return the type of the message or
nil if this is a) not a CTCP message or b) a CTCP message we don't
know about."
  (if (or (not (stringp string))
          (zerop (length string))
          (not (eql (char string 0) +soh+)))
      nil
      (case (char string 1)
        (#\A (ctcp-type-p string :action))
        (#\C (ctcp-type-p string :clientinfo))
        (#\D
         (or (dcc-type-p string :dcc-chat-request)
             (dcc-type-p string :dcc-send-request)))
        (#\F (ctcp-type-p string :finger))
        (#\P (ctcp-type-p string :ping))
        (#\S (ctcp-type-p string :source))
        (#\T (ctcp-type-p string :time))
        (#\U (ctcp-type-p string :userinfo))
        (#\V (ctcp-type-p string :version))
        (otherwise nil))))

(defun create-irc-message (string)
  "If `string' is a valid IRC message parse it and return an object of
the correct type with its slots prefilled according to the information
in the message."
  (multiple-value-bind (source user host command arguments trailing-argument)
      (parse-raw-message string)
    (let ((class 'irc-message)
          (ctcp (ctcp-message-type trailing-argument)))
      (when command
        (cond
          ((irc-error-reply-p command)
           (progn
             (setf command (find-reply-name (parse-integer command)))
             (setf class 'irc-error-reply)))
          ((numeric-reply-p command)
           (progn
             (setf command (find-reply-name (parse-integer command)))
             (setf class (find-irc-message-class command))))
          (t
           (progn
             (setf command (intern (string-upcase command)
                                   (find-package :keyword)))
             (setf class (find-irc-message-class command))))))
      (when ctcp
        (setf class (find-ctcp-message-class ctcp)))
      (let ((instance (make-instance class
                                     :source (or source "")
                                     :user (or user "")
                                     :host (or host "")
                                     :command (if command
                                                  (string command)
                                                  "")
                                     :arguments arguments
                                     :connection nil
                                     :trailing-argument (or trailing-argument "")
                                     :received-time (get-universal-time)
                                     :raw-message-string (or string ""))))
        (when ctcp
          (setf (ctcp-command instance) ctcp))
        instance))))
