#!/usr/bin/env gimp-script-fu-interpreter-3.0
;;;  -*- coding: utf-8 -*- ;!# Comment Closer Needed by gettext.

;;; Export layers plus: export layers as separate images
;;; v. 0.1


;; You can find original export-layers-plus.scm at https://github.com/tshatrov/scriptfu/tree/gimp-2.10
;;              -- Sensu Iun

;; This location of the Document file should be:
;; {gimp-directory}/plug-ins/export-layers-plus/Doc/export-layers-plus.html
;; or You should change elp-help-document value at around the bottom of this scm plugin.
;;             -- Sensu Iun

;; +--------------------------+
;; | MODIFICATION HISTORY     |
;; +--------------------------+
;; 2026-05-04: Removed Gimp 2.10 codes. -- Sensu Iun.
;;             (script-fu-use-v3 #t) is used. script-fu-register-filter is used.
;; 2026-05-04: Bug Fix: -- Ssiun
;;             changed (layer-name (gimp-item-get-name layer))
;;                  to (layer-name (car (gimp-item-get-name layer)))
;; 2026-05-04: Bug Fix: Because gimp-layer-copy doesn't have add-alpha argument in GIMP 3,
;;                      made copy_gimp_layer function. -- Ssiun
;; 2026-04-23: Fixed elp-walk-layers error. (unbound variable 'layers') -- Ssiun
;; 2026-04-23: fixed ssiun-space-pad error in ssiun-utils-v2v3.scm -- Ssiun
;; 2026-04-23: script-fu-register-i18n is used -- Ssiun
;; 2025-12-27: bug fix: changed 'scripts/...html' to 'plug-ins/...html'  -- Sensu Iun
;;
;; 2025-11-16: Sensu Iun: Renamed to 'Export layers Plus...'
;;             Because I have other 'export layers' plugins written by other authors
;; 2025-11-16: Sensu Iun: Added 'Export layers (by Timo Shatrov) Help' menu item.
;;             This menu item opens f"{Gimp.directory()}/scripts/export-layers-plus/Doc/export-layers-plus.html"

(define DEBUG_MODE #f)

(define (DEBUGMSG . args)
  (script-fu-use-v3)
  (set! *ssiun-v3* #t)
  (if DEBUG_MODE (apply ssiun-errmsgln* args)))

(define (DEBUGVARS . args)
  (script-fu-use-v3)
  (set! *ssiun-v3* #t)
  (if DEBUG_MODE (apply ssiun-errmsgln-vars* args)))

(define *elp-default-frame-rate* 100)

(define (elp-sanitize-string s)
  "Remove characters illegal in Windows filenames"
  (let* ((bad-characters (string->list "\\/:*?\"<>|"))
         (reslist (let loop ((slist (string->list s)))
                    (cond ((null? slist) (list))
                          ((memv (car slist) bad-characters)
                           (loop (cdr slist)))
                          (else (cons (car slist) (loop (cdr slist))))))))
    (list->string reslist)))

(define (re-re-match re string buffer)
  "Workaround GIMP 2.10 bug https://gitlab.gnome.org/GNOME/gimp/issues/2965"
  (and (re-match re string)
       (re-match re string buffer)))

(define (elp-replace-once _string tokens)
  (if (null? tokens) (cons "%" _string)
      (let* ((token-re (string-append "^" (caar tokens)))
             (token-val-fn (cdar tokens))
             (buffer (make-vector 1)))
        (if (re-re-match token-re _string buffer)
            (let* ((boundaries (vector-ref buffer 0))
                   (token (substring _string (car boundaries) (cdr boundaries)))
                   (rest (substring _string (cdr boundaries) (string-length _string))))
              (cons (token-val-fn token) rest))
            (elp-replace-once _string (cdr tokens))))))

(define (elp-replace-all string tokens)
  (let loop ((slist (string->list string))
             (result ""))
    (cond ((null? slist) result)
          ((char=? (car slist) #\%)
           (let ((res (elp-replace-once (list->string (cdr slist)) tokens)))
             (loop (string->list (cdr res)) (string-append result (car res)))))
          (else
           (loop (cdr slist) (string-append result (make-string 1 (car slist))))))))

(define (elp-generic-val-fn value)
  (lambda (token) (elp-sanitize-string value)))

(define (elp-format-percent-i value)
  (lambda (token)
    (let* ((token-len (string-length token))
           (len (and (> token-len 1)
                     (string->number (substring token 0 (- token-len 1)))))
           (ns (number->string value))
           (lns (string-length ns)))
      (cond
       ((not len) ns)
       ((> lns len) (substring ns (- lns len) lns))
       (else (string-append (make-string (- len lns) #\0) ns)))
      )))

(define (elp-vector-for-each fn vec)
  "Run fn on each element of vec"
  (do ((len (vector-length vec))
       (i 0 (+ i 1)))
      ((= i len))
    (fn (vector-ref vec i))))

(define (copy_gimp_layer layer add-alpha)
  (script-fu-use-v3)
  (set! *ssiun-v3* #t)
  (let* ((copied_layer (gimp-layer-copy layer)))
    (if (py-True? add-alpha)
        (gimp-layer-add-alpha layer))
    copied_layer) )

(define (elp-walk-layers walk-direction img test fn)
  (script-fu-use-v3)
  (set! *ssiun-v3* #t)
  (DEBUGMSG "elp-walk-layers")
  (let ((layers (vector->list (gimp-image-get-layers img)))
        (count 0))
    (if (= walk-direction 0) (set! layers (reverse layers)))
    (for-each
     (lambda (layer)
       (if (or (not test) (test layer)) (fn count layer))
       (set! count (+ count 1)))
     layers)))

(define (elp-image-copy img)
  (DEBUGMSG "elp-image-copy:")
  (script-fu-use-v3)
  (set! *ssiun-v3* #t)
  (let ((newimg (gimp-image-new (gimp-image-get-width img)
                                (gimp-image-get-height img)
                                RGB))
        (layers (reverse (vector->list (gimp-image-get-layers img)))))
    (for-each
     (lambda (layer)
       (let* ((layer-name (gimp-item-get-name layer))
              (layer-copy (gimp-layer-new-from-drawable layer newimg)))
         (gimp-item-set-name layer-copy (gimp-item-get-name layer))
         (gimp-image-insert-layer newimg layer-copy 0 0)))
     layers)
    newimg))

(define (elp_is_in item list_or_vector)
  (cond
   ((vector? list_or_vector)
    (if (member item (vector->list list_or_vector)) #t #f))
   ((list? list_or_vector)
    (if (member item list_or_vector) #t #f))
   (else #f)
   ))

(define (elp-export-layer img img-name path filename-template count layer progress-text)
  (script-fu-use-v3)
  (set! *ssiun-v3* #t)
  (let* ((tokens `(("%" . ,(lambda (token) "%"))
                   ("n" . ,(elp-generic-val-fn img-name))
                   ("l" . ,(elp-generic-val-fn (gimp-item-get-name layer)))
                   ("\\d*i" . ,(elp-format-percent-i count))))
         (name (elp-replace-all filename-template tokens))
         (outpath (string-append path "/" name))
         )
    (set! outpath (pregexp-replace* "\\\\" outpath "/"))

    (let* (
           (old-image img)
           (old-layer layer)
           (old-layer-position -1)
           (new-image 0)
           (new-layers '())
           )
      (set! old-layer-position (gimp-image-get-item-position old-image old-layer))
      (set! new-image (gimp-image-duplicate old-image))
      (set! new-layers (gimp-image-get-layers new-image))
      (do
          ( (i 0 (+ i 1)) )
          ((= i (vector-length new-layers)) #t)
        (if (= i old-layer-position)
            (begin (gimp-item-set-visible (vector-ref new-layers i) #t))
            (gimp-item-set-visible (vector-ref new-layers i) #f)
            )
        )
      ;;GIMP 3: (gimp-file-save run-mode image file options)

      (if (not (equal? progress-text ""))
          (gimp-progress-set-text progress-text))
      (gimp-file-save RUN-NONINTERACTIVE new-image outpath)
      (if (not (equal? progress-text ""))
          (gimp-progress-set-text progress-text))
      (gimp-progress-pulse)
      (gimp-image-delete new-image)
      (gimp-progress-pulse)
      )))

(define (elp-walk-layers-filtered walk-direction img layer-filter fn)
  (script-fu-use-v3)
  (set! *ssiun-v3* #t)
  (define (GIMP_ITEM_GET_SELECTED layer)
    (not (equal? #f (member layer (vector->list (gimp-image-get-layers img))))))
  (elp-walk-layers
   walk-direction img
   (cond ((= layer-filter 0) #f)
         ((= layer-filter 1) (lambda (layer) (gimp-item-get-visible layer)))
         ((= layer-filter 2) (lambda (layer) (GIMP_ITEM_GET_SELECTED layer))))
   fn))

(define (elp-simple-export img img-name path filename-template walk-direction count-offset layer-filter)
  ;;; DOESNT APPLY LAYER MASKS!
  (script-fu-use-v3)
  (set! *ssiun-v3* #t)
  (elp-walk-layers-filtered
   walk-direction img layer-filter
   (lambda (count layer)
     (elp-export-layer img img-name path filename-template (+ count count-offset) layer ""))))

(define (elp-index-export timg img-name path filename-template count-offset index)
  "index is just a list of layers that need to be exported"
  (script-fu-use-v3)
  (set! *ssiun-v3* #t)
  (let* ((count 0))
    (for-each
     (lambda (layer)
       (let* ( (outstring ""))
         (set! outstring (string-append "Export layers plus ... "
                                        (number->string count)
                                        "/"
                                        (number->string (length index))))
         (gimp-progress-set-text outstring)
         (elp-export-layer timg img-name path filename-template (+ count count-offset) layer outstring)
         (set! count (+ count 1))))
     index)))

(define (elp-has-layer-masks img)
  (script-fu-use-v3)
  (set! *ssiun-v3* #t)
  (DEBUGMSG "elp-has-layer-masks")
  (let loop ( (layers (vector->list (gimp-image-get-layers img))))
    (if (null? layers) #f
        (let ((mask (gimp-layer-get-mask (car layers))))
          (if (not (= mask -1)) #t
              (loop (cdr layers)))))))

(define (elp-apply-masks img)
  (script-fu-use-v3)
  (set! *ssiun-v3* #t)
  (elp-vector-for-each
   (lambda (layer)
     (let* ((mask (gimp-layer-get-mask layer)))
       (if (not (= mask -1)) (gimp-layer-remove-mask layer MASK-APPLY))))
   (gimp-image-get-layers img)))


(define (elp-get-delay layer)
  (script-fu-use-v3)
  (set! *ssiun-v3* #t)
  (let* ((buffer (make-vector 2))
         (layer-name (gimp-item-get-name layer)))
    (if (re-re-match "\\((\\d+)ms\\)" layer-name buffer)
        (let* ((boundaries (vector-ref buffer 1))
               (ldelay (string->number (substring layer-name (car boundaries) (cdr boundaries)))))
          (if (= ldelay 0) *elp-default-frame-rate* ldelay))
        *elp-default-frame-rate*)))

(define (elp-get-timeline img walk-direction layer-filter)
  (let ((timeline '()))
    (elp-walk-layers-filtered
     walk-direction img layer-filter
     (lambda (count layer)
       (set! timeline (cons (cons layer (elp-get-delay layer)) timeline))))
    (reverse timeline)))


(define (elp-process-timeline timeline frame-rate)
  (let* ((make-empty-frame
          (lambda () (list frame-rate)))
         (fill-frame
          (lambda (frame tll)
            "return #f if frame is not filled, #t if filled"
            (let ((remaining (car frame))
                  (available (cdr tll)))
              (cond ((= remaining 0) #t)
                    ((>= available remaining)
                     (set-car! frame 0)
                     (set-cdr! frame (cons (cons (car tll) remaining) (cdr frame)))
                     (set-cdr! tll (- available remaining))
                     #t)
                    (else
                     (set-car! frame (- remaining available))
                     (set-cdr! frame (cons (cons (car tll) available) (cdr frame)))
                     (set-cdr! tll 0)
                     #f)))))
         (reverse-frame
          (lambda (frame)
            (set-cdr! frame (reverse (cdr frame)))))
         )
    (let loop ((tl timeline)
               (ptl '())
               (curframe (make-empty-frame)))
      (if (pair? tl)
          (if (fill-frame curframe (car tl))
              (loop (if (= (cdar tl) 0) (cdr tl) tl)
                    (cons (reverse-frame curframe) ptl)
                    (make-empty-frame))
              (loop (cdr tl) ptl curframe))
          (reverse
           (if (pair? (cdr curframe))
               (cons curframe ptl)
               ptl))))))

(define (elp-select-best-layer frame)
  (let loop ((choices (cdr frame))
             (max-delay 0)
             (best '()))
    (if (pair? choices)
        (let ((cur-delay (cdar choices)))
          (cond ((> cur-delay max-delay)
                 (loop (cdr choices) cur-delay (list (car choices))))
                ((= cur-delay max-delay)
                 (loop (cdr choices) max-delay (cons (car choices) best)))
                (else
                 (loop (cdr choices) max-delay best))))
        (car (list-ref best (quotient (length best) 2))))))


(define (elp-interpolate-layers img layer1 layer2 w1 w2)
  "Uses a more precise algorithm than bgmask average layers to deal with transparency

If pair (x, a) represents a pixel with color x and opacity a, the correct interpolation would be:
 (x, a) ~ (y, b) = ((ax+by)/(a+b), (a+b)/2)

However this algorithm (and bgmask's algorithm) do NOT perform correct interpolation because it seems
too hard to emulate the necessary arithmetic via layer modes.

bgmask's is pretty much only correct when a=b=1

This algorithm is correct when a=b and when a=0, b=1 and a=1, b=0. It always calculates opacity correctly,
and the colors are approximately correct, aside from the abovementioned cases when it's exactly correct.

The formula for this algorithm is:

 (x, a) ~ (y, b) = ( (a+(1-b))/2 * x + (b+(1-a))/2 * y, (a+b)/2)

Weights w1 and w2 are used to calculate wfactor = w1/(w1+w2) which is used instead of 50% opacity
to calculate averages.
"
  (script-fu-use-v3)
  (set! *ssiun-v3* #t)
  (gimp-item-set-visible layer1 #t)
  (gimp-item-set-visible layer2 #t)
  (let ((wfactor (if (= w1 w2) 50
                     (* (/ w1 (+ w1 w2)) 100)))
        (l1copy (gimp-layer-copy layer1 #t))
        (l2copy (gimp-layer-copy layer2 #t))
        (new-from-alpha
         (lambda (layer)
           (let ((newmask (gimp-layer-create-mask layer ADD-MASK-ALPHA-TRANSFER))
                 (masklayer (copy_gimp_layer layer #t)))
             (gimp-layer-add-mask layer newmask)
             (gimp-image-select-item img 2 newmask)
             (gimp-selection-invert img)
             (gimp-drawable-fill masklayer FILL-WHITE)
             (gimp-image-insert-layer img masklayer 0 0)
             (gimp-context-push)
             (gimp-context-set-foreground '(0 0 0))
             ;; gimp-selection-bounds:
             ;;   @param image: GimpImage
             ;;   @returns non-empty:bool, x1:gint, y1:gint, x2:gint, y2:gint
             (if (eqv? #t (car (gimp-selection-bounds img)))
                 (gimp-drawable-edit-fill masklayer 0))
             (gimp-context-pop)
             (gimp-selection-none img)
             (gimp-layer-remove-mask layer MASK-DISCARD)
             masklayer))))

    (gimp-image-insert-layer img l2copy 0 0)
    (gimp-image-insert-layer img l1copy 0 0)
    (let* ((ml2 (new-from-alpha l2copy))
           (ml1 (new-from-alpha l1copy))
           (ml2c (copy_gimp_layer ml2 #t))
           (ml1c (copy_gimp_layer ml1 #t)))
      ;; ml1 > ml2 > l1 > l2
      (gimp-image-insert-layer img ml2c 0 0)
      (gimp-image-insert-layer img ml1c 0 0)
      ;; ml1c > ml2c > ml1 > ml2 > l1 > l2
      (gimp-drawable-invert ml2c)
      (gimp-layer-set-opacity ml1c wfactor)
      (gimp-layer-set-opacity ml1 wfactor)
      (let* ((colorlayer (gimp-image-merge-down img ml1c 0))
             (colormask (gimp-layer-create-mask colorlayer ADD-MASK-COPY))
             (opacitylayer (gimp-image-merge-down img ml1 0))
             (opacitymask (gimp-layer-create-mask opacitylayer ADD-MASK-COPY)))
        (gimp-layer-add-mask colorlayer colormask)
        (gimp-layer-add-mask opacitylayer opacitymask)
        (gimp-image-select-item img 2 colormask)
        (let ((mask (gimp-layer-create-mask l1copy ADD-MASK-SELECTION)))
          (gimp-layer-add-mask l1copy mask))
        (gimp-image-select-item img 2 opacitymask)
        (let* ((final (gimp-image-merge-down img l1copy 0))
               (finalmask (gimp-layer-create-mask final ADD-MASK-SELECTION)))
          (gimp-layer-add-mask final finalmask)
          (gimp-layer-remove-mask final MASK-APPLY)
          (gimp-image-remove-layer img colorlayer)
          (gimp-image-remove-layer img opacitylayer)
          (gimp-selection-none img)
          final)))))


(define (elp-resample-frame img frame resample-threshold)
  ;; only resample when 2 layers
  (if (or (not (= (length (cdr frame)) 2))
          (< (cdadr frame) resample-threshold)
          (< (cdaddr frame) resample-threshold))
      (elp-select-best-layer frame)
      (elp-interpolate-layers img (caadr frame) (caaddr frame) (cdadr frame) (cdaddr frame))))

(define (elp-resampling-index img timeline resample-mode frame-rate resample-threshold)
  (let ((ptl (elp-process-timeline timeline frame-rate)))
    (cond ((= resample-mode 1)
           (map elp-select-best-layer ptl))
          ((= resample-mode 2)
           (map (lambda (frame) (elp-resample-frame img frame resample-threshold)) ptl)))))

(define (script-fu-export-layers-plus InImage Indrawables path filename-template
                                      walk-direction count-offset
                                      layer-filter
                                      resample-mode frame-rate
                                      resample-threshold
                                      )

  (script-fu-use-v3)
  (set! *ssiun-v3* #t)
  (let* ((img InImage)
         (img-name (gimp-image-get-name img))
         (do-simple-export
          (lambda (img)
            (elp-simple-export img img-name path filename-template walk-direction count-offset layer-filter)))
         (simple (not (or (not (= resample-mode 0)) (elp-has-layer-masks img))))
         )
    (if simple
        (begin
          (do-simple-export img)
          )
        (let* ((timg (elp-image-copy img))
               (tempimgs (list timg)))
          (elp-apply-masks timg)
          (if (= resample-mode 0)
              (begin
                (do-simple-export timg))
              (let* ((timeline (elp-get-timeline timg walk-direction layer-filter))
                     (index (elp-resampling-index timg timeline resample-mode frame-rate resample-threshold)))
                (elp-index-export timg img-name path filename-template count-offset index)
                ))
          (for-each (lambda (_image)
                      (gimp-image-delete _image))
                    tempimgs)))))

(script-fu-register-filter
 "script-fu-export-layers-plus"
 _"Export Layers Plus (scm)..."
 _"Export layers as separate images (by Timofei Shatrov)"
 "Timofei Shatrov"
 "2013"
 "2013"
 "*"
 SF-ONE-OR-MORE-DRAWABLE
 SF-DIRNAME     _"Output _directory"  "."
 SF-STRING      _"File_name format:
%n - image name
%l - layer name
%i - number of layer
%6i - padded to 6 digits
%% = %" "%n_%6i.png"
SF-OPTION _"_Walk direction" '(_"Bottom to top" _"Top to bottom")
SF-ADJUSTMENT _"Co_unt offset" '(0 0 999999 1 10 0 1)
SF-OPTION _"_Filter" '(_"All layers" _"Visible layers" _"Selected layers")
SF-OPTION _"Resample _mode" '(_"Off" _"No interpolation" _"Use interpolation")
SF-ADJUSTMENT _"Frame ra_te (ms)" '(40 1 999999 1 10 0 1)
SF-ADJUSTMENT _"_Interpolation threshold (ms)" '(0 0 999999 1 10 0 1)
)

(script-fu-menu-register "script-fu-export-layers-plus"
                                        ; FOR TRANSLATORS: Don't translate '<Image>/File'
                         _"<Image>/File/Export")

(script-fu-register-i18n "script-fu-export-layers-plus" "Standard" )

(define (script-fu-export-layers-plus-help InImage Indrawables)
  (script-fu-use-v3)
  (set! *ssiun-v3* #t)
  (let* ((elp-help-document (string-append gimp-directory
                                           "/"
                                           "plug-ins/export-layers-plus/Doc/export-layers-plus-en.html" ) ))
    (define (elp-filepath-to-url filepath)
      (set! filepath (pregexp-replace* "\\\\" filepath "/"))
      (set! filepath (pregexp-replace* "/{2,}" filepath "/"))
      (set! filepath (string-append "file:///" filepath))
      filepath
      )
    (plug-in-web-browser (elp-filepath-to-url elp-help-document))))

(script-fu-register-filter
 "script-fu-export-layers-plus-help"
 _"Help for 'Export Layers Plus' (scm)"
 _"Opens the html document for 'Export Layers Plus'"
 "Sesu Iun"
 "2025"
 "2025"
 "*"
 SF-ONE-OR-MORE-DRAWABLE
 )

(script-fu-menu-register
 "script-fu-export-layers-plus-help"
 _"<Image>/File/Export")

(script-fu-register-i18n "script-fu-export-layers-plus-help" "Standard" )
