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
;;;
;;; Special considerations:
;;;
;;; - According to the spec, values for Disallow are path prefixes, not regexes or globs.
;;; - An empty Disallow value is considered "match all"


(defpackage "ROBOTS-TXT"
  (:use :cl)
  (:export #:make-disallowed-table #:disallowed-p))

(in-package :robots-txt)

;; API

(defun disallowed-p (disallowed-table user-agent path)
  "Test if PATH is disallowed for USER-AGENT."
  (unless path
    (setf path "/"))
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

(defun make-line-iterator (data)
  "Returns an iterator FN which yields the contents of DATA line-by-line. DATA can be a one of:
- STRING
- a LIST of STRINGs
- a STREAM.
When there are no more lines, returns the symbol :EOF."
  (when (typep data 'string)
    (setf data (make-string-input-stream data)))
    #'(lambda ()
        (etypecase data
          (stream (if (open-stream-p data)
                      (let ((line (read-line data nil :eof)))
                        (if (eq line :eof)
                            (progn
                              (close data)
                              :eof)
                          line))
                    :eof))
          (list (prog1
                  (if (not (or (car data)
                               (cdr data)))
                      :eof
                    (car data))
                  (setf data (cdr data))))))))
          

(defun make-disallowed-table (robots-txt)
  "Parses a robots.txt and returns a disallowed-table which is later used by DISALLOWED-P. ROBOTS-TXT can either be a STRING (which is considered to be the full robots.txt content), or a LIST (which is considered to be a list of lines) or a STREAM."
  (flet ((parse-line (line)
           (let ((line (string-trim +whitespace+ line)))
             (when (and (not (uiop:string-prefix-p "#" line))
                        (position #\: line))
               (destructuring-bind (key &optional (value "/"))
                   (mutility:string-split line :char-bag (list #\:) :count 2)
                 (list (string-trim +whitespace+ key) 
                       (string-trim +whitespace+ value)))))))                  
    (loop with parser-table = (make-hash-table :test #'equalp)
          and line-iterator = (make-line-iterator robots-txt)
          and disallow-state = NIL
          and current-agents = NIL
          for line = (funcall line-iterator)
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
          finally (return parser-table))))
