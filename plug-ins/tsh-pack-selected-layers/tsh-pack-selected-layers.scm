#!/usr/bin/env gimp-script-fu-interpreter-3.0
;;; -*- coding: utf-8 -*-
;;;!# Close comment started on first line. Needed by gettext.

;;; GIMP 3.2.4 Script-fu script

;;; GIMP Animation Tools
;;; by Timofei Shatrov
;;; v. 0.64

;;; +-----------------------------+
;;; | MODIFICATION HISTORY        |
;;; +-----------------------------+
;;; 2026-05-18: Changed pack-linked-layers to tsh-pack-selected-layers
;;;             as layer 'linked' state is gone in Gimp 3 -- Sensu Iun
;;;             Made tsh-pack-selected-layers a separate plugin.

;;; (load (string-append gimp-directory "\\" "plug-ins\\tsh-pack-selected-layers\\ssiun-utils-v2v3.scm"))

(define *tsh-debug* #t)

(define *tsh-v3* #f)

(define (tsh_set_v3 enable)
  (if (or (eqv? enable #t) (eqv? enable TRUE))
      (begin
        (script-fu-use-v3)
        (set! *ssiun-v3* #t)
        (set! *tsh-v3* #t))
      (begin
        (script-fu-use-v2)
        (set! *ssiun-v3* #f)
        (set! *tsh-v3* #f))))

(define (tsh-debugvars . args )
  (if *tsh-debug*
      (apply ssiun-errmsgln-vars* args)))

(define (tsh-debugmsg . args )
  (if *tsh-debug*
      (apply ssiun-errmsgln* args)))

;; Layer group helpers (release as a separate script maybe?)
(define (tsh-vector-for-each fn vec)
  "Run fn on each element of vec"
  (do ((len (vector-length vec))
       (i 0 (+ i 1)))
      ((= i len))
    (fn (vector-ref vec i))))

(define (tsh-vector-for-each-i fn vec)
  "Run fn on each element of vec"
  (do ((len (vector-length vec))
       (i 0 (+ i 1)))
      ((= i len))
    (fn (vector-ref vec i) i)))


(define (tsh-is-true? fn item)
  ;; does fn return '(TRUE) ?
  (tsh_set_v3 #t)
  (fn item))


(define (tsh-walk-layers-recursive img test fn)
  (tsh_set_v3 #t)
  (let loop ((layers (gimp-image-get-layers img)))
    (tsh-vector-for-each
     (lambda (layer)
       (cond ((test layer)
              (fn layer))
             ((tsh-is-true? gimp-item-is-group layer)
              (loop (gimp-item-get-children layer)))))
     layers)))

(define (get-visible-layers img)
  (tsh_set_v3 #t)
  (let* ((visible-layers #()))
    (define (_save-visible-layers img item)
      (cond
       ((eqv? img item)
        (let* ((image-layers (gimp-image-get-layers img)))
          (for-each
           (lambda (layer)
             (_save-visible-layers img layer))
           (vector->list image-layers))))
       (else
        (let* ((item-visible #f))
          (set! item-visible (gimp-item-get-visible item))
          (if item-visible
              (set! visible-layers (ssiun-vector-append visible-layers (vector item))))
          (if (gimp-item-is-group item)
              (let* ((children (gimp-item-get-children item)))
                (for-each
                 (lambda (child)
                   (_save-visible-layers img child))
                 (vector->list children))))))))
    (_save-visible-layers img img)
    visible-layers))

(define (make-only-selected-layers-visible img item selected-layers)
  (tsh_set_v3 #t)
  (cond
   ((eqv? img item)
    (let* ((image-layers #()))
      (set! image-layers (gimp-image-get-layers img))
      (for-each
       (lambda (layer)
         (make-only-selected-layers-visible img layer selected-layers))
       (vector->list image-layers))))
   (else
    (let* ((item-selected #f))
      (if (member item (vector->list selected-layers))
          (set! item-selected #t))
      (gimp-item-set-visible item item-selected)
      (if (gimp-item-is-group item)
          (let* ((children #()))
            (set! children (gimp-item-get-children item))
            (for-each
             (lambda (child)
               (make-only-selected-layers-visible img child selected-layers))
             (vector->list children))))))))

(define (all-has-same-parent selected-layers)
  (if (<= (vector-length selected-layers) 0)
      #f
      (do ((first-parent (gimp-item-get-parent (vector-ref selected-layers 0)))
           (parent -1)
           (all-same-parent #t)
           (i 0 (+ i 1))
           (layer -1))
          ((or (>= i (vector-length selected-layers))
               (eqv? all-same-parent #f))
           all-same-parent)
        (set! layer (vector-ref selected-layers i))
        (set! parent (gimp-item-get-parent layer))
        (if (not (eqv? parent first-parent))
            (set! all-same-parent #f)))))

(define (script-fu-tsh-pack-selected-layers img InDrawables)
  (tsh_set_v3 #t)
  (let* ((group (gimp-group-layer-new img))
         (pos 0)
         (selected-layers #()))
    (set! selected-layers (gimp-image-get-selected-layers img))
    (if (<= (vector-length selected-layers) 0)
        (begin
          (gimp-message _"No layers selected"))
        (let* ((visible-layers #()))
          (gimp-image-undo-group-start img)
          (set! visible-layers (get-visible-layers img))
          (make-only-selected-layers-visible img img selected-layers)
          (gimp-image-freeze-layers img)
          (if (all-has-same-parent selected-layers)
              (let* ((layer (vector-ref selected-layers 0))
                     (pos (gimp-image-get-item-position img layer))
                     (parent (gimp-item-get-parent layer)))
                (if (eqv? parent -1)
                    (gimp-image-insert-layer img group 0 pos)
                    (gimp-image-insert-layer img group parent pos)))
              (gimp-image-insert-layer img group 0 0))
          (gimp-item-set-visible group #f)
          (tsh-walk-layers-recursive
           img
           (lambda (layer) (tsh-is-true? gimp-item-get-visible layer))
           (lambda (layer)
             (catch #f ;;prevent crash on reordering layer group into one of its children
               (begin
                 (gimp-image-reorder-item img layer group pos)
                 (set! pos (+ pos 1))
                 (gimp-item-set-visible layer #f)))))
          (gimp-image-thaw-layers img)
          (gimp-image-set-selected-layers img selected-layers)
          (for-each
           (lambda (layer)
             (gimp-item-set-visible layer #t))
           (vector->list visible-layers))
          (gimp-item-set-visible group #t)
          (gimp-image-undo-group-end img)
          (gimp-displays-flush)))))

(define (script-fu-tsh-pack-selected-layers-main-menu img InDrawables)
  (script-fu-tsh-pack-selected-layers img InDrawables))

(script-fu-register-filter
 "script-fu-tsh-pack-selected-layers"
 _"Pack Selected Layers"
 _"Put all selected layers in a new layer group"
 "Timofei Shatrov"
 "Copyright 2012"
 "June 27, 2012"
 "RGB RGBA GRAY GRAYA"
 SF-ONE-OR-MORE-DRAWABLE)

(script-fu-register-filter
 "script-fu-tsh-pack-selected-layers-main-menu"
 _"Pack Selected Layers"
 _"Put all selected layers in a new layer group"
 "Timofei Shatrov"
 "Copyright 2012"
 "June 27, 2012"
 "RGB RGBA GRAY GRAYA"
 SF-ONE-OR-MORE-DRAWABLE)

(script-fu-menu-register "script-fu-tsh-pack-selected-layers-main-menu"
                         ;; FOR TRANSLATORS: Don't translate '<Image>/Layer/'
                         _"<Image>/Layer/Group")
(script-fu-register-i18n "script-fu-tsh-pack-selected-layers-main-menu" "Standard")

(script-fu-menu-register "script-fu-tsh-pack-selected-layers"
                         ;; FOR TRANSLATORS: Don't translate '<Layers>/Layers Menu/'
                         _"<Layers>/Layers Menu/Group")
(script-fu-register-i18n "script-fu-tsh-pack-selected-layers" "Standard")

