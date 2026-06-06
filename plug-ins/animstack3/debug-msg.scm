#!/usr/bin/env gimp-script-fu-interpreter-3.0
;;; -*- coding: utf-8 -*-
;;;!# Close comment started on first line. Needed by gettext.

;;; GIMP 3.2.4 Script-fu script
;;; +--------------------------------+
;;; |  MODIFICATION HISTORY          |
;;; +--------------------------------+
;;; revised for GIMP 3.0.4 by Ssiun Enuy on July 16,2025.

;;(load (string-append gimp-directory "\\" "plug-ins\\animstack3\\ssiun-utils3-for-animstack3.scm"))

(define DEBUG_MODE #t)
(define (debug_vars . args )
  (if DEBUG_MODE
      (apply ssiun-errmsgln-vars* args)))

(define (debug_msg . args )
  (if DEBUG_MODE
      (apply ssiun-errmsgln* args)))

;;(load (string-append gimp-directory "\\" "plug-ins\\animstack3\\animstack3.scm"))
