;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-
;;; $Header: /usr/local/cvsrep/documentation-template/documentation-template.asd,v 1.15 2014-11-23 12:12:59 edi Exp $

;;; Copyright (c) 2006-2014, Dr. Edmund Weitz.  All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cl-user)

(defpackage :ntdoc-asd
  (:use :cl :asdf))

(in-package :ntdoc-asd)

#+:sbcl
(require :sb-introspect)

(asdf:defsystem :ntdoc
  :serial t
  :license :bsd-3
  :description "A Documentation Generator"
  :source-control "https://github.com/ndantam/cl-ntdoc"
  :homepage "http://ndantam.github.io/cl-ntdoc"
  :author ("Edi Weitz"
           "Michael Weber (SBCL patches)"
           "Willem Broekema (AllegroCL patches)"
           "Andreas Thiele (CCL patches)"
           "Neil T. Dantam (ASDF Metadata Extraction)"
           )
  :version "0.4.4"
  :components ((:file "packages")
               (:file "specials")
               (:file "util")
               (:file "output")
               (:file "markdown"))
  :depends-on (:cl-who :alexandria :sycamore)
  :long-description
  "NTDOC generates documentation for a Common Lisp package via
introspection, extracting metadata from the corresponding ASDF system.
It is based on DOCUMENTATION-TEMPLATE by Edi Weitz."
  )
