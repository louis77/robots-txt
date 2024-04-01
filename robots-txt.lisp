;;; This package provides a ROBOTS.TXT parser. Its main function is 
;;; to match a given clause against a ROBOTS.TXT file and tell if it
;;; matches or not.
;;;
;;; It adheres to the Gemini robots.txt community spec described here:
;;; gemini://geminiprotocol.net/docs/companion/robots.gmi
;;; 
;;; Usage:
;;; 
;;; Create a DISALLOWED-TABLE with MAKE-DISALLOWED-TABLE providing the ROBOTS.TXT content as a string.
;;; Query DISALLOWED-P with the DISALLOWED-TABLE and a User-Agent and Path.

(defpackage "ROBOTS-TXT"
  (:use :cl)
  (:export #:make-disallowed-table #:disallowed-p))

(in-package :robots-txt)

;; API

(defun disallowed-p (disallowed-table user-agent path)
  "Test if PATH is disallowed for USER-AGENT."
  (check-type user-agent string)
  (check-type path string)
  (check-type disallowed-table hash-table)
  (let ((prefixes (append (gethash user-agent disallowed-table)
                          (gethash "*" disallowed-table))))
    (some #'(lambda (prefix) 
              (uiop:string-prefix-p prefix path))
          prefixes)))
    
    

;; Parser

(defparameter +whitespace+ '(#\Space #\Tab #\Newline))

(defun make-disallowed-table (robots-txt)
  "Parses a robots.txt string and returns a disallowed-table which is later used by DISALLOWED-P."
  (flet ((parse-line (line)
           (let ((line (string-trim +whitespace+ line)))
             (when (and (not (uiop:string-prefix-p "#" line))
                        (position #\: line))
               (destructuring-bind (key value)
                   (mutility:string-split line :char-bag (list #\:) :count 2)
                 (list (string-trim +whitespace+ key) 
                       (string-trim +whitespace+ value)))))))                  
      (with-input-from-string (stream robots-txt)
        (loop with parser-table = (make-hash-table :test #'equalp)
              and disallow-state = NIL
              and current-agents = NIL
              for line = (read-line stream nil :eof)
              until (eql line :eof)
              for parsed-line = (parse-line line)
              when parsed-line
                when (string-equal (car parsed-line) "User-Agent")
                  do (when disallow-state
                       (setf current-agents nil)
                       (setf disallow-state nil))
                     (push (cadr parsed-line) current-agents)
                  end
                and when (and (string-equal (car parsed-line) "Disallow")
                              current-agents)
                      do
                        (setf disallow-state T)
                        (dolist (ua current-agents)
                          (setf (gethash ua parser-table) (push (cadr parsed-line)
                                                                (gethash ua parser-table))))
              finally (return parser-table)))))
