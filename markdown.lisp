;;;; Copyright (c) 2016, Rice University  All rights reserved.
;;;;
;;;;   Redistribution and use in source and binary forms, with or
;;;;   without modification, are permitted provided that the following
;;;;   conditions are met:
;;;;
;;;;   * Redistributions of source code must retain the above
;;;;     copyright notice, this list of conditions and the following
;;;;     disclaimer.
;;;;   * Redistributions in binary form must reproduce the above
;;;;     copyright notice, this list of conditions and the following
;;;;     disclaimer in the documentation and/or other materials
;;;;     provided with the distribution.
;;;;   * Neither the name of copyright holder the names of its
;;;;     contributors may be used to endorse or promote products
;;;;     derived from this software without specific prior written
;;;;     permission.
;;;;
;;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
;;;;   CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
;;;;   INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;;;   MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;;;   DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
;;;;   CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;;;   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
;;;;   USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
;;;;   AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;;;   LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
;;;;   ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;;;   POSSIBILITY OF SUCH DAMAGE.

(in-package :ntdoc)

(defvar *output*)
(defvar *md-key*)
(defvar *md-package*)

(defvar *md-docstring-hash*)

(defun md-docstring-hash (symbol type)
  (gethash (cons symbol type) *md-docstring-hash*))

(defun (setf md-docstring-hash) (docstring symbol type)
  (setf (gethash (cons symbol type) *md-docstring-hash*)
        docstring))

(defun md-format (control-string &rest args)
  (apply #'format *output* control-string args))

(defun md-write-char (char)
  (write-char char *output*))

(defun md-write-string (thing)
  (write-string thing *output*))

(defun md-header (level text &optional key)
  (let ((text (rope-string text))
        (anchor (if key
                    (format nil " {#~A}" (rope-string key))
                    "")))
    (if (>= level 3)
        (md-format "~&~A ~A~A~%~%"
                   (make-string level :initial-element #\#)
                   text
                   anchor)
        (md-format "~&~A ~A~%~A~%~%"
                   text anchor
                   (make-string (length (string text))
                                :initial-element (ecase level
                                                   (1 #\=)
                                                   (2 #\-)))))))


(defun md-lambda-list* (lambda-list &optional specializers)
  "The function which does all the work for WRITE-LAMBDA-LIST and
calls itself recursive if needed."
  (let (body-seen after-required-args-p (firstp t))
    (dolist (part lambda-list)
      (cond (body-seen (setq body-seen nil))
            (t (when (and (consp part) after-required-args-p)
                 (setq part (first part)))
               (unless firstp
                 (md-write-char #\Space))
               (setq firstp nil)
               (cond ((consp part)
                      ;; a destructuring lambda list - recurse
                      (md-write-char #\()
                      (md-lambda-list* part)
                      (md-write-char #\)))
                     ((member part '(&key &optional &rest &allow-other-keys &aux &environment &whole))
                      ;; marks these between <tt> and </tt>
                      (setq after-required-args-p t)
                      (md-format "~A" (string-downcase part)))
                     ((eq part '&body)
                      ;; we don't really write '&BODY', we write it
                      ;; like in the CLHS
                      (setq body-seen t
                            after-required-args-p t)
                      (md-write-string "declaration* statement*"))
                     (t
                      (let ((specializer (pop specializers)))
                        (cond ((and specializer (not (eq specializer t)))
                               ;; add specializers if there are any left
                               (md-write-string (escape-string
                                              (string-downcase
                                               (format nil "(~A ~A)" part specializer)))))
                              (t (md-write-string (escape-string (string-downcase part)))))))))))))

(defun md-lambda-list (lambda-list &key (resultp t) specializers)
  "Writes the lambda list LAMBDA-LIST, optionally with the
specializers SPECIALIZERS.  Adds something like `=> result' at
the end if RESULTP is true."
  (md-write-string "(")
  (md-lambda-list* lambda-list specializers)
  (md-write-string ")")
  (when resultp
    (md-format " => result~%~%")))

(defun md-docstring (docstring)
  (when docstring
    (md-format "~&<pre>~A</pre>~%~%" docstring)))


(defgeneric markdown-entry (symbol type arguments docstring &optional types other))

(defun md-entry-key (symbol type)
  (with-output-to-string (out)
    (let ((str (rope-string (rope *md-key* "_" *md-package* "_" type "_" symbol))))
      (loop for c across str
         do
           (case c
             (#\* (write-string "_STAR_" out))
             (#\+ (write-string "_PLUS_" out))
             (#\- (write-string "_MINUS_" out))
             (#\> (write-string "_GT_" out))
             (#\< (write-string "_LT_" out))
             (#\/ (write-string "_DIV_" out))
             (#\( (write-string "_LP_" out))
             (#\) (write-string "_RP_" out))
             (#\% (write-string "_PCT_" out))
             (#\Space (write-string "_SP_" out))
             (#\_ (write-string "__" out))
             (otherwise (write-char c out)))))))


(defun md-entry-header (symbol type)
  (let ((symbol (format nil "~A" symbol)))
    (md-header 2 symbol (md-entry-key symbol type))
    (md-format "~&<i>[~A]</i>~%~%" (string-downcase type))))


(defun check-docstring (symbol type docstring)
  (unless docstring
    (format *error-output* "~&~A ~A is not documented.~%"
            type symbol)))

(defun index-markdown-entry (symbol type arguments docstring &optional types other)
  (declare (ignore arguments types other))
  (setf (md-docstring-hash symbol type) docstring))

(defmethod markdown-entry (symbol (type (eql :constant)) arguments docstring &optional types other)
  (check-docstring symbol type docstring)
  (md-entry-header symbol type)
  (md-docstring docstring))

(defmethod markdown-entry (symbol (type (eql :special-var)) arguments docstring &optional types other)
  (check-docstring symbol type docstring)
  (md-entry-header symbol type)
  (md-docstring docstring))

(defmethod markdown-entry (symbol (type (eql :macro)) arguments docstring &optional types other)
  (check-docstring symbol type docstring)
  (md-entry-header symbol type)
  (md-docstring docstring))

(defmethod markdown-entry (symbol (type (eql :function)) arguments docstring &optional types other)
  (check-docstring symbol type docstring)
  (md-entry-header symbol type)
  (md-format "    ~A: " symbol)
  (md-lambda-list arguments)
  (md-docstring docstring))

(defmethod markdown-entry (symbol (type (eql :generic-function)) arguments docstring &optional types other)
  (check-docstring symbol type docstring)
  (md-entry-header symbol type)
  (md-format "    ~A: " symbol)
  (md-lambda-list arguments)
  (md-docstring docstring))

(defmethod markdown-entry (symbol (type (eql :method)) arguments docstring &optional types other)
  (md-entry-header symbol type)
  (md-format "    ~A: " symbol)
  (md-lambda-list arguments :specializers types)
  (md-docstring (or docstring
                    (md-docstring-hash symbol :generic-function))))

(defmethod markdown-entry (symbol (type (eql :class)) arguments docstring &optional types other)
  (md-entry-header symbol (if (subtypep symbol 'condition)
                              "Condition type" "Standard class"))
  (md-docstring docstring))


(defun markdown-thing (thing)
  (print thing))

(defun markdown (packages &key
                           target
                           system
                           title
                           (toc t)
                           key
                           ;;author-homepage
                           ;; (subtitle (asdf:system-description system))
                           ;; (license (asdf:system-license system))
                           ;; (author (asdf:system-author system))
                           ;; (repository (asdf:system-source-control system))
                           ;; (description (asdf:system-long-description system))
                           ((:maybe-skip-methods-p *maybe-skip-methods-p*)
                            *maybe-skip-methods-p*)
                           (if-exists :supersede)
                           (if-does-not-exist :create))
  "Writes an Markdown page with documentation entries and an
index for all exported symbols of the package PACKAGE to the file
TARGET.  If MAYBE-SKIP-METHODS-P is true, documentation entries for
inidividual methods are skipped if the corresponding generic function
has a documentation string."
  (let* (;*symbols*
         (packages (ensure-list packages))
         (title (or title (car packages)))
         (system (etypecase system
                   (null (asdf:find-system (car packages)))
                   ((or string symbol)
                    (asdf:find-system system))))
         (key (or key (car packages)))
         (*md-docstring-hash* (make-hash-table :test #'equal))
         (*md-key* key)
         (homepage (asdf:system-homepage system))
         (repo (asdf:system-source-control system))
         (subtitle (asdf:system-description system))
         (description (asdf:system-long-description system))
         (license (asdf:system-license system))
         (authors (alexandria:ensure-list(asdf:system-author  system))))

    (labels ((helper ()
               ;; header
               (md-header 1 title key)
               (when subtitle
                 (md-format "~&*~A*~%~%" subtitle))
               (when homepage
                 (md-format "~&* **Homepage:** [~A](~A)~%" homepage homepage))
               (when repo
                 (md-format "~&* **Source Repository:** [~A](~A)~%" repo repo))
               (when license
                 (multiple-value-bind (name url) (license-info license)
                   (if url
                       (md-format "~&* **License:** [~A](~A)" name url)
                       (md-format "~&* **License:** ~A" name ))))
               (when description
                 (md-format "~&~%~A~%~%" description))
               (when toc
                 (md-format "~&[TOC]~%~%"))
               ;; body
               (map nil #'output-package packages)
               ;; Footer
               (when authors
                 (md-header 1 "Authors" "authors")
                 (dolist (a authors)
                   (md-format "~&* ~A~%" a)))
               )
             (map-entries (function entries)
               (map nil (lambda (entry) (apply function entry)) entries))
             (output-package (package)
               (let ((entries (collect-all-doc-entries package))
                     (*md-package* package))
                 (md-header 1 (rope "The " package " dictionary")
                            (rope key "_" package))

                 (map-entries #'index-markdown-entry entries)
                 (map-entries #'markdown-entry entries))))
      (etypecase target
        (null
         (with-output-to-string (*output*)
           (helper)))
        (stream
         (let ((*output* target))
           (helper)))
        ((or string pathname)
         (with-open-file (*output* target
                                   :direction :output
                                   :if-exists if-exists
                                   :if-does-not-exist if-does-not-exist)
           (helper)))))))
