;;;; $Id$
;;;; $Source$

;;;; See the LICENSE file for licensing information.

(in-package :irc)

(defun get-day-name (day-number)
  "Given a number, such as 1, return the appropriate day name,
abbrevated, such as \"Tue\".  Index 0 is Monday."
  (case day-number
    (0 "Mon")
    (1 "Tue")
    (2 "Wed")
    (3 "Thu")
    (4 "Fri")
    (5 "Sat")
    (6 "Sun")
    (otherwise
     (error "Unknown day ~A." day-number))))

(defun get-month-name (month-number)
  "Index 1 is January."
  (case month-number
    (1 "Jan")
    (2 "Feb")
    (3 "Mar")
    (4 "Apr")
    (5 "May")
    (6 "Jun")
    (7 "Jul")
    (8 "Aug")
    (9 "Sep")
    (10 "Oct")
    (11 "Nov")
    (12 "Dec")
    (otherwise
     (error "Unknown month ~A." month-number))))

(defun make-time-message (second minute hour date month year day)
  "Returns a string composed of the input parameters so that it
represents a time message as by the IRC protocol."
  (format nil "~A ~A ~2D ~2,'0D:~2,'0D:~2,'0D ~D"
          (get-day-name day)
          (get-month-name month)
          date
          hour
          minute
          second
          year))

(defun make-irc-message (command &key (arguments nil)
                                 (trailing-argument nil))
  "Return a valid IRC message, as a string, composed of the input
parameters."
  (let ((*print-circle* nil))
    (format nil "~A~{ ~A~}~A~A~A~A" command arguments
	    (if trailing-argument
		" :"
              "")
	    (or trailing-argument "")
	    #\Return
	    #\Linefeed)))

(defun make-ctcp-message (string)
  "Return a valid IRC CTCP message, as a string, composed by
`string'."
  (format nil "~A~A~A" +soh+ string +soh+))

(defun tokenize-string (string &key
                               (delimiters '(#\Space #\Return #\Linefeed #\Newline)))
  "Split string into a list, splitting on `delimiters' and removing any
empty subsequences."
  (split-sequence:split-sequence-if #'(lambda (character)
                                        (member character delimiters))
                                    string :remove-empty-subseqs t))

(defun list-of-strings-to-integers (list)
  "Take a list of strings and return a new list of integers (from
parse-integer) on each of the string elements."
  (let ((new-list nil))
    (dolist (element (reverse list))
      (push (parse-integer element) new-list))
    new-list))

(defun host-byte-order (string)
  "Convert a string, such as 192.168.1.1, to host-byte-order, such as
3232235777."
  (let ((list (list-of-strings-to-integers (split-sequence:split-sequence #\. string))))
    (+ (* (first list) 256 256 256) (* (second list) 256 256)
       (* (third list) 256) (fourth list))))

(defun hbo-to-dotted-quad (integer)
  "Host-byte-order integer to dotted-quad string conversion utility."
  (let ((first (ldb (byte 8 24) integer))
        (second (ldb (byte 8 16) integer))
        (third (ldb (byte 8 8) integer))
        (fourth (ldb (byte 8 0) integer)))
    (format nil "~A.~A.~A.~A" first second third fourth)))

(defun hbo-to-vector-quad (integer)
  "Host-byte-order integer to dotted-quad string conversion utility."
  (let ((first (ldb (byte 8 24) integer))
        (second (ldb (byte 8 16) integer))
        (third (ldb (byte 8 8) integer))
        (fourth (ldb (byte 8 0) integer)))
    (vector first second third fourth)))

(defun cut-between (string start-char end-chars &key (start 0) (cut-extra t))
  "If `start-char' is not nil, cut string between `start-char' and any
of the `end-chars', from `start'.  If `start-char' is nil, cut from
`start' until any of the `end-chars'.

If `cut-extra' is t, we will cut from start + 1 instead of just
`start'.

When there is no string matching the input parameters `start' and nil
will be returned, otherwise `end-position' and the string are
returned."
  (let ((end-position (position-if #'(lambda (char)
                                       (member char end-chars))
                                   string :start (1+ start)))
        (cut-from (if cut-extra
                      (1+ start)
                      start)))
    (if (and end-position start-char)
        (if (eql (char string start) start-char)
            (values end-position
                    (subseq string cut-from end-position))
            (values start nil))
        (if end-position
            (values end-position
                    (subseq string cut-from end-position))
            (values start nil)))))

(defun cut-before (string substring end-chars &key (start 0) (cut-extra t))
  "Cut `string' before `substring' or any of the `end-chars', from `start'.

If `cut-extra' is t, we will cut from start + 1 instead of just
`start'.

When there is no string matching the input parameters `start' and nil
will be returned, otherwise `end-position' and the string are
returned."
  (let ((end-position (or (search substring string :start2 (1+ start))
                          (position-if #'(lambda (x)
                                           (member x end-chars))
                                       string :start (1+ start))))
        (cut-from (if cut-extra (1+ start) start)))
    (if end-position
        (values end-position
                (subseq string cut-from end-position))
      (values start nil))))
