#!/usr/bin/env gimp-script-fu-interpreter-3.0
;;; -*- coding: utf-8 -*-
;;;!# Close comment started on first line. Needed by gettext.

;;; GIMP 3.2.4 Script-fu script

;;; GIMP Animation Tools
;;; by Timofei Shatrov
;;; v. 0.64
;;(load (string-append gimp-directory "\\" "plug-ins\\animstack3\\ssiun-utils-v2v3.scm"))
(define *tsh-debug* #t)
(define *tsh-v3* #f)
(define (tsh_get_v3) *tsh-v3*)
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

;; revised for GIMP 3.0.4 by Ssiun Enuy on July 16,2025.
(define (tsh-display-to-string value)
  "Prints anything to string using display function"
  (let* ((port (open-output-string)))
    (display value port)
    ;; Fun fact: get-output-string is not mentioned once in tinyscheme docs...
    (get-output-string port)))


(define (tsh-gimp-message* . args)
  (let* ((port (open-output-string)))
    (for-each
     (lambda (arg)
       (display arg port)
       (display " " port))
     args)
    (gimp-message (get-output-string port))))

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
  (if (not *tsh-v3*)
      (or (eqv? TRUE (car (fn item))) (eqv? #t (car (fn item))))
      (or (eqv? TRUE (fn item)) (eqv? #t (fn item))) ))

(define (tsh-int-round x)
  (inexact->exact (round x)))

(define *tsh-default-animstack-hash-size* 200)

(define (tsh-animstack-hash size hashfn)
  ;;based on http://www.math.grin.edu/~stone/events/scheme-workshop/hash-tables.html
  (let* ((table (make-vector size '()))
         (add (lambda (key . value)
                (let* ((index (hashfn key)))
                  (vector-set! table index
                               (cons (cons key value) (vector-ref table index))))))
         (init (lambda (assoc-list)
                 (for-each (lambda (pair) (apply add pair)) assoc-list)))
         (get-assoc (lambda (key)
                      (let* ((index (hashfn key)))
                        (assoc key (vector-ref table index))))))
    (lambda (op . args)
      (case op
        ((add) (apply add args))
        ((init) (apply init args))
        ((assoc) (apply get-assoc args))
        ((contents) table)
        ((stat)
         (let* ((collisions 0)
                (filled 0))
           (tsh-vector-for-each
            (lambda (cell)
              (if (pair? cell)
                  (begin
                    (set! filled (+ filled 1))
                    (if (> (length cell) 1) (set! collisions (+ collisions 1))))))
            table)
           `(("Size:" ,size) ("Filled:" ,filled) ("Collisions:" ,collisions))))
        (else (error (string-append "Unsupported hash operation" (symbol->string op))))))))

(define (tsh-animstack-hashfn size)
  (let* ((seed (random size))
         (sqsize (tsh-int-round (sqrt size)))
         (hashfn
          (lambda (obj)
            "It's terrible, but quick..."
            (cond
             ((integer? obj) obj)
             ((char? obj) (char->integer obj))
             ((string? obj)
              (let* ((sl (string-length obj)))
                (+ sl
                   (* sqsize (if (> sl 0) (hashfn (string-ref obj 0)) seed))
                   (if (> sl 1) (hashfn (string-ref obj 1)) seed)
                   )))
             ((symbol? obj)
              (hashfn (symbol->string obj)))
             ((else (hashfn (tsh-display-to-string obj))))))))
    (lambda (obj) (modulo (hashfn obj) size))))

(define (tsh-make-animstack-hash assoc-list)
  (let* ((size *tsh-default-animstack-hash-size*)
         (ah (tsh-animstack-hash size (tsh-animstack-hashfn size))))
    (ah 'init assoc-list)
    ah))



(define (tsh-edit-paste-into-one-drawable drawable paste-into)
  (let* ((new-drawables (car (gimp-edit-paste drawable paste-into))))
    (list (vector-ref new-drawables 0))))

(define (tsh-floating-sel-check-and-anchor floating-sel)
  (let* ((is-floating (car (gimp-layer-is-floating-sel floating-sel))))
    (if (or (eqv? is-floating TRUE) (eqv? is-floating #t))
        (gimp-floating-sel-anchor floating-sel))))



(define (tsh-flatten-layer-group img layer)
  "Flatten a single layer group"
  (let* ((retvar -1) (old_v3 (tsh_get_v3)))
    (tsh_set_v3 #t)
    (set! retvar
          (if (gimp-item-is-group layer)
              (gimp-group-layer-merge layer)
              layer))
    (tsh_set_v3 old_v3)
    retvar))

(define (tsh-flatten-layer-groups img)
  "Flatten all layer groups in an image"
  (let* ((retvar -1) (old-v3 (tsh_get_v3)))
    (tsh_set_v3 #t)
    (set! retvar
          (let* ((layers (gimp-image-get-layers img)))
            (gimp-image-undo-group-start img)
            (gimp-image-freeze-layers img)
            ;; flatten each layer group
            (tsh-vector-for-each
             (lambda (layer)
               (if (gimp-item-is-group layer)
                   (begin
                     (tsh-flatten-layer-group img layer)
                     (gimp-progress-pulse))))
             layers)
            (gimp-image-thaw-layers img)
            (gimp-image-undo-group-end img)
            (gimp-progress-end)
            (gimp-displays-flush)))
    (tsh_set_v3 old-v3)
    retvar))

(define (script-fu-tsh-flatten-layer-groups-filter InImage InDrawables)
  (tsh-flatten-layer-groups InImage))


(script-fu-register-filter
 "script-fu-tsh-flatten-layer-groups-filter"
 _"Flatten Layer Groups (T. Shatrov)"
 _"Flattens all layer groups in an image"
 "Timofei Shatrov"
 "Copyright 2012"
 "June 15, 2012"
 "RGB RGBA GRAY GRAYA"
 SF-ONE-OR-MORE-DRAWABLE
 )

(script-fu-menu-register "script-fu-tsh-flatten-layer-groups-filter"
                         ;; FOR TRANSLATORS: Don't translate '<Image>/Image/'
                         _"<Image>/Image/Timofei Shatrov"
                         )
(script-fu-register-i18n "script-fu-tsh-flatten-layer-groups-filter" "Standard")

(define (tsh-reorder-or-insert-layer image item parent position)
  "because you never know..."
  (let* ((args (list image item parent position)))
    (catch ;; actually GIMP still displays the error. it doesn't die, but might scare the user
      (catch #f (apply gimp-image-insert-layer args))
      (apply gimp-image-reorder-item args))))

(define (tsh-groupify-layer img layer)
  "If layer is not a layer group, make a layer group containing only layer"
  (if (not (tsh-is-true? gimp-item-is-group layer))
      (let* ((group (car (gimp-group-layer-new img)))
             (layer-name (car (gimp-item-get-name layer)))
             (default-name-function ;; potentially customizable?
               (lambda (str) (string-append "+ " str))))

        (gimp-image-set-selected-layers img (vector layer)) ; set active layer

        (gimp-item-set-name group (default-name-function layer-name))
        (gimp-image-insert-layer img group 0 -1)
        (gimp-image-reorder-item img layer group 0)
        (tsh-animstack-copy-layer-labels layer group)
        group)
      layer))

(define (tsh-put-layer-in-group img layer parent position . rest)
  "Position might be a positive (counted from the top) or negative (from the bottom)"
  (let* ((group (tsh-groupify-layer img parent))
         (group-length (vector-length (car (gimp-item-get-children group))))
         (pos (if (< position 0)
                  (max 0 (+ group-length position 1))
                  (min group-length position)))
         (fn (if (null? rest) tsh-reorder-or-insert-layer (car rest)))
                                        ; if rest... is fn is rest[0],
                                        ; otherwise fn is tsh-reorder-or-insert-layer
         )
    (fn img layer group pos)
    group))

(define (tsh-get-layer-in-group group position)
  (if (tsh-is-true? gimp-item-is-group group)
      (let* ((group-children (car (gimp-item-get-children group)))
             (group-length (vector-length group-children))
             (pos (if (< position 0)
                      (max 0 (+ group-length position))
                      (min (- group-length 1) position))))
        (vector-ref group-children pos))
      group))

(define (tsh-string2number str . opt)
  "Replacement for string->number, which throws an uncatchable
exception as of GIMP 2.8. Returns #f if not a number."
  (let* ((s2a (string->atom str))
         (fn (if (pair? opt) (car opt) number?)))
    (and (fn s2a) s2a)))

(define tsh-animstack-save-selection #f)
(define tsh-animstack-restore-selection #f)

(let* ((sel #f))
  (set! tsh-animstack-save-selection
        (lambda (img)
          (set! sel #f)
          (if (tsh-is-true? gimp-selection-bounds img)
              (set! sel (car (gimp-selection-save img))))))
  (set! tsh-animstack-restore-selection
        (lambda (img)
          (if sel
              (begin
                (gimp-image-select-item img CHANNEL-OP-REPLACE sel);
                (gimp-image-remove-channel img sel))
              (gimp-selection-none img)))))

;; Tag syntax is [tag] or [tag:parameter] or [tag:param1:param2:...]
;; All parameters should be integers

(define (tsh-string-split string char)
  (let* ((res (list)))
    (do ((i (string-length string) (- i 1))
         (chunk (list))
         (new (lambda (chunk) (cons (list->string chunk) res))))
        ((<= i 0) (new chunk))
      (let* ((chr (string-ref string (- i 1))))
        (if (char=? chr char)
            (begin (set! res (new chunk)) (set! chunk (list)))
            (set! chunk (cons chr chunk)))))))

(define (tsh-parse-tag-param-simple str)
  (let ((valid-param (lambda (x) (or (number? x) (symbol? x)))))
    (tsh-string2number str valid-param)))

(define (tsh-parse-tag-param* str)
  (let ((valid-param (lambda (x) (or (number? x) (symbol? x)))))
    (if (= (string-length str) 0)
        #f
        (or (tsh-string2number str valid-param) 'err))))

(define (tsh-parse-generator-defn defn)
  (let* ((mul-split (tsh-string-split defn #\*))
         (add-split (tsh-string-split (car mul-split) #\+))
         (mul-found (= (length mul-split) 2))
         (add-found (= (length add-split) 2))
         (mul-str (if mul-found
                      (if add-found (cadr add-split) (car mul-split))
                      #f))
         (add-str (if add-found (car add-split) #f))
         (rest-str (if mul-found (cadr mul-split)
                       (if add-found (cadr add-split)
                           defn))))
    (list (or (and add-str (tsh-string2number add-str)) 0)
          (or (and mul-str (tsh-string2number mul-str)) 1)
          rest-str)))


(define (tsh-parse-tag-param str)
  (let* ((ari (tsh-parse-generator-defn str))
         (rest (tsh-parse-tag-param* (caddr ari))))
    (cond ((or (eqv? rest 'err)
               (and (= (car ari) 0) (= (cadr ari) 1))) rest)
          (else (list (car ari) (cadr ari) rest)))))

(define (tsh-parse-animstack-tag string)
  "Parse string beginning with [tag]. Returns a pair ( tag . index )
where tag might be #f"
  (let* ((split1 (tsh-string-split string #\]))
         (tagstr (car split1)))
    (if (> (length split1) 1)
        ;;this might be legitimate tag
        (let* ((split2 (tsh-string-split tagstr #\:))
               (tagname (substring (car split2) 1 (string-length (car split2))))
               (params (map (if (= (string-length tagname) 0)
                                tsh-parse-tag-param-simple ;;don't do ari stuff on label tags
                                tsh-parse-tag-param)
                            (cdr split2)))
               (pos (+ (string-length tagstr) 1)))
          (if (memv 'err params)
              (cons #f pos)
              (cons (cons tagname params) pos)))
        ;;cant find closing bracket...
        (cons #f (string-length string)))))

(define (tsh-extract-animstack-tags layer . params)
  "Returns list of animstack tags in the form (tag . parameters)"
  (let loop ((layer-name-list (string->list (car (gimp-item-get-name layer)))))
    (let ((tagtail (memv #\[ layer-name-list)))
      (if tagtail
          (let* ((parsed (tsh-parse-animstack-tag (list->string tagtail)))
                 (tag (car parsed))
                 (next (list-tail tagtail (cdr parsed))))
            (if (and tag (or (null? params) ((car params) tag)))
                (cons tag (loop next))
                (loop next)))
          (list)))))

(define (tsh-strip-tags string . params)
  "Removes all tags from string"
  (list->string
   (let loop ((str-list (string->list string)))
     (cond ((null? str-list) (list))
           ((char=? (car str-list) #\[)
            (let ((rest (memv #\] str-list)))
              (if (and rest
                       (let ((tag (car (tsh-parse-animstack-tag (list->string str-list)))))
                         (and tag (or (null? params) ((car params) tag)))))
                  (loop (cdr rest))
                  (cons (car str-list) (loop (cdr str-list)))
                  )))
           (else (cons (car str-list) (loop (cdr str-list))))))))


(define (tsh-is-untagged? layer)
  (null? (tsh-extract-animstack-tags layer)))

(define (tsh-pop-params n params)
  (let ((pv (make-vector n #f)))
    (do ((i 0 (+ i 1))
         (pl params (cdr pl)))
        ((or (null? pl) (>= i n)) pv)
      (vector-set! pv i (car pl)))))


;; it seems this was deprecated
(macro (tsh-prog1 form)
  (let ((res (gensym)))
    `(let ((,res ,(cadr form)))
       ,@(cddr form)
       ,res)))

(define (tsh-process-param-list param-list)
  (let ((count 0))
    (map
     (lambda (param-def)
       (tsh-prog1
        (if (pair? param-def)
            `(,(car param-def) (or (vector-ref pv ,count) (begin ,@(cdr param-def))))
            `(,param-def (vector-ref pv ,count)))
        (set! count (+ count 1))))
     param-list)))

;; (tsh-with-params (x y z) ....)
(macro (tsh-with-params form)
  (let* ((param-list (cadr form))
         (body (cddr form)))
    `(let* ((pv (tsh-pop-params ,(length param-list) params))
            ,@(tsh-process-param-list param-list))
       ,@body)))

;; fun fact: this is the single most important function in AnimStack
(define (tsh-layer-walk get-layer start next test action limit ignore-first terminate-label)
  (let loop ((i start)
             (left limit)
             (terminate #f))
    (let ((layer (get-layer i)))
      (cond
       ((or (not layer) terminate (and left (<= left 0))) #f)
       (else
        (if (and terminate-label (tsh-animstack-layer-has-label layer terminate-label))
            (set! terminate #t))
        (cond ((and ignore-first (= i start)))
              ((not (test layer)))
              (else
               (action layer)
               (if left (set! left (- left 1)))))
        (loop (next i) left terminate))))))

(define (tsh-layer-getter layers)
  (let ((maxlen (vector-length layers)))
    (lambda (pos)
      (and (< -1 pos maxlen) (vector-ref layers pos)))))



(define (tsh-default-copy-name item . prefixargs)
  (let ((prefix (if (pair? prefixargs) (car prefixargs) "* ")))
    (string-append prefix (tsh-strip-tags (car (gimp-item-get-name item))))))

(define (tsh-get-bindings bindings)
  (map (lambda (g) (list (car g) ((cadr g)))) bindings))

(define (tsh-apply-effects-simple img effects . rest)
  (lambda (layer target)
    (for-each (lambda (effect) (apply effect img layer target rest)) effects)))

(define (tsh-copy-action img source pos opts)
  "Copy source layer and put it into target group at position pos"
  (let ((copy-name (tsh-default-copy-name source)))
    (lambda (target)
      (let* ((bindings (tsh-get-bindings (cadr opts)))
             (tsh-apply-effects (tsh-apply-effects-simple img (list-ref opts 3)
                                                          bindings #f)))
        (if (caar opts) (tsh-apply-effects source target))

        (let ((new (car (gimp-layer-copy source))))
          (gimp-item-set-name new copy-name)
          (set! target (tsh-put-layer-in-group img new target pos gimp-image-insert-layer))
          (tsh-process-dup-options* opts img bindings target (tsh-dup-getter img new))
          (if (not (caar opts)) (tsh-apply-effects new target)))))))

(define (tsh-animstack-next-fn delta opts)
  (let* ((reverse_ (cadar opts))
         (delta (if reverse_ (- delta) delta)))
    (lambda (i) (+ i delta))))


(define (tsh-animstack-common delta pos actionfn)
  (lambda (img layer opts . params)
    (let* ((limit (if (null? params) #f (car params)))
           (start (tsh-get-start-position img layer opts))
           (layers (car (gimp-image-get-layers img)))
           (getter (tsh-layer-getter layers))
           (next (tsh-animstack-next-fn delta opts))
           (action (actionfn img layer pos opts))
           (tl (list-ref (car opts) 2)))
      (if (and limit (<= limit 0)) (set! limit #f))
      (for-each (lambda (effect) (effect img layer #f (list) #f)) (list-ref opts 2))
      (tsh-layer-walk getter start next tsh-is-untagged? action limit #t tl))
    (gimp-image-remove-layer img layer)
    ))

(define tsh-animstack-bg (tsh-animstack-common -1 -1 tsh-copy-action))

(define tsh-animstack-fg (tsh-animstack-common 1 0 tsh-copy-action))

(define (tsh-animstack-copy img layer opts . params)
  (tsh-with-params
   ((pos -1) limit)
   (let* ((fn (tsh-animstack-common -1 pos tsh-copy-action)))
     (fn img layer opts limit))))

(define (tsh-noop-action img source pos opts)
  "do nothing with source layer, but do execute all the actions"
  (lambda (target)
    (let* ((bindings (tsh-get-bindings (cadr opts)))
           (tsh-apply-effects (tsh-apply-effects-simple img (list-ref opts 3)
                                                        bindings #f)))
      (tsh-apply-effects source target)
      (tsh-process-dup-options* opts img bindings target (lambda (tgt) source)))))


(define tsh-animstack-noop (tsh-animstack-common -1 -1 tsh-noop-action))

(define (tsh-get-roll-layer-list layer)
  (if (tsh-is-true? gimp-item-is-group layer)
      (vector->list (car (gimp-item-get-children layer)))
      (list layer)))


(define tsh-animstack-reset-count #f)
(define tsh-animstack-init-count #f)
(define tsh-animstack-inc-count #f)
(define tsh-animstack-get-count #f)
(define tsh-animstack-set-repeat-value #f)

(let* ((count 0)
       (repeat-count 0)
       (repeat-value 1))
  (set! tsh-animstack-reset-count
        (lambda () (set! repeat-value 1) (set! repeat-count 0)))
  (set! tsh-animstack-init-count
        (lambda (n) (set! count (tsh-int-round n)) (set! repeat-count 0)))
  (set! tsh-animstack-set-repeat-value
        (lambda (n) (if (= repeat-count 0) (set! repeat-value (tsh-int-round n)))))
  (set! tsh-animstack-inc-count
        (lambda ()
          (cond ((> repeat-value 0)
                 (set! repeat-count (+ repeat-count 1))
                 (if (>= repeat-count repeat-value)
                     (begin (tsh-animstack-reset-count) (set! count (+ count 1)))))
                ((= repeat-value 0) (tsh-animstack-reset-count))
                ((< repeat-value 0) (set! count (+ count repeat-value)) (tsh-animstack-reset-count)))
          count))
  (set! tsh-animstack-get-count (lambda () count)))

(define (tsh-roll-action img source pos opts roll-offset)
  (let* ((roll-list (tsh-get-roll-layer-list source))
         (roll-length (length roll-list))
         (count roll-offset))
    (if (= roll-length 0) (error "Empty roll"))
    (if (< count 0) (set! count (random roll-length)))
    (tsh-animstack-init-count count)
    (lambda (target)
      (let* ((pick (- roll-length 1 (modulo (tsh-animstack-get-count) roll-length)))
             (layer (list-ref roll-list pick))
             (bindings (tsh-get-bindings (cadr opts)))
             (tsh-apply-effects (lambda (layer src)
                                  (for-each (lambda (effect)
                                              (effect img layer target bindings src))
                                            (list-ref opts 3)))))
        (if (caar opts) (tsh-apply-effects layer source))
        (let ((new (car (gimp-layer-copy layer))))
          (gimp-item-set-name new (tsh-default-copy-name new))
          (set! target (tsh-put-layer-in-group img new target pos gimp-image-insert-layer))
          (tsh-process-dup-options* opts img bindings target (tsh-dup-getter img new))
          (if (not (caar opts)) (tsh-apply-effects new #f))
          (tsh-animstack-inc-count))))))


(define (tsh-animstack-roll img layer opts . params)
  "Rolls a layer stack or a single layer up the layer list in a given position"
  (tsh-with-params
   ((pos -1) limit (roll-offset 0))
   (let* ((start (tsh-get-start-position img layer opts))
          (layers (car (gimp-image-get-layers img)))
          (getter (tsh-layer-getter layers))
          (next (tsh-animstack-next-fn -1 opts))
          (action (tsh-roll-action img layer pos opts roll-offset))
          (tl (list-ref (car opts) 2)))
     (if (and limit (<= limit 0)) (set! limit #f))
     (tsh-animstack-reset-count)
     (for-each (lambda (effect) (effect img layer #f (list) layer)) (list-ref opts 2))
     (tsh-layer-walk getter start next tsh-is-untagged? action limit #t tl)))
  (gimp-image-remove-layer img layer))

(define (tsh-animstack-splice img layer opts . params)
  (tsh-with-params
   ((pos -1) (roll-offset 0))
   (let* ((limit (length (tsh-get-roll-layer-list layer))))
     (tsh-animstack-roll img layer opts pos limit roll-offset))))

(define (tsh-check-all lst test)
  (let loop ((l lst))
    (cond ((null? l) #t)
          ((not (test (car l))) #f)
          (else (loop (cdr l))))))


;; matte action

(define (tsh-layer-matte-cutout img layer bg-layer threshold)
  (tsh-animstack-save-selection img)
  (if (tsh-is-true? gimp-selection-bounds img)
      (set! sel (car (gimp-selection-save img))))
  (gimp-item-set-visible bg-layer FALSE)
  (gimp-image-select-item img CHANNEL-OP-REPLACE layer)
  (let ((chl (car (gimp-selection-save img))))
    (gimp-selection-none img)
    (gimp-drawable-threshold chl HISTOGRAM-VALUE 0 (/ threshold 255))
    (gimp-image-select-item img CHANNEL-OP-REPLACE chl)
    (gimp-image-remove-channel img chl))
  (gimp-layer-add-alpha bg-layer)
  (gimp-drawable-edit-clear bg-layer)
  (gimp-item-set-visible bg-layer TRUE)
  (tsh-animstack-restore-selection img))


;;TODO: fix terrible copy pasting of tsh-copy-action
(define (tsh-matte-action threshold img source pos opts)
  (let ((copy-name (tsh-default-copy-name source)))
    (lambda (target)
      (let* ((bindings (tsh-get-bindings (cadr opts)))
             (tsh-apply-effects (tsh-apply-effects-simple img (list-ref opts 3)
                                                          bindings #f)))
        (if (caar opts) (tsh-apply-effects source target))
        (let ((new (car (gimp-layer-copy source))))
          (gimp-item-set-name new copy-name)
          (set! target (tsh-put-layer-in-group img new target pos gimp-image-insert-layer))
          (tsh-process-dup-options* opts img bindings target (tsh-dup-getter img new))
          (if (not (caar opts)) (tsh-apply-effects new target))
          (tsh-layer-matte-cutout img target new threshold))))))

(define (tsh-animstack-matte img layer opts . params)
  (tsh-with-params
   ((threshold 1) limit)
   (let* ((threshold (max threshold 0))
          (fn (tsh-animstack-common -1 -1
                                    (lambda args (apply tsh-matte-action threshold args)))))
     (fn img layer opts limit))))

;; delete
(define (tsh-delete-action-factory step width)
  (lambda (img source pos opts)
    (let ((count 0))
      (lambda (target)
        (let* ((bindings (tsh-get-bindings (cadr opts)))
               (tsh-apply-effects (tsh-apply-effects-simple img (list-ref opts 3)
                                                            bindings #f)))
          (tsh-apply-effects source target)
          (tsh-process-dup-options* opts img bindings target (lambda (tgt) source))
          (if (< (modulo count step) width)
              (gimp-image-remove-layer img target))
          (set! count (+ count 1)))))))

(define (tsh-animstack-delete img layer opts . params)
  (let* ((pv (tsh-pop-params 3 params))
         (step (max (or (vector-ref pv 0) 2) 1))
         (width (max (or (vector-ref pv 1) (- step 1)) 0))
         (limit (vector-ref pv 2))
         (action (tsh-delete-action-factory step width))
         (fn (tsh-animstack-common -1 -1 action)))
    (fn img layer opts limit)))


(define (tsh-render-layer-group img group interval)
  "Like flatten-layer-group, but returns a new layer instead of merging"
  (let ((name (tsh-default-copy-name group "R "))
        (layers (car (gimp-image-get-layers img)))
        (result #f)
        (changed '()))
    (tsh-vector-for-each
     (lambda (lr) (gimp-item-set-visible lr (if (= lr group) TRUE FALSE)))
     layers)
    (if interval
        (tsh-vector-for-each-i
         (lambda (lr i)
           (if (not (<= (car interval) i (cdr interval)))
               (begin
                 (set! changed (cons (cons lr (car (gimp-item-get-visible lr))) changed))
                 (gimp-item-set-visible lr FALSE))))
         (car (gimp-item-get-children group))))
    (set! result (car (gimp-layer-new-from-visible img img name)))
    (if (pair? changed)
        (for-each (lambda (lv) (gimp-item-set-visible (car lv) (cdr lv))) changed))
    (tsh-vector-for-each
     (lambda (lr) (gimp-item-set-visible lr TRUE))
     layers)
    result))

(define (tsh-animstack-get-interval group only interval)
  (if (tsh-is-true? gimp-item-is-group group)
      (let* ((nlayers (vector-length (car (gimp-item-get-children group))))
             (onlypos (if (< only 0)
                          (max 0 (+ nlayers only))
                          (min (- nlayers 1) only))))
        (if (< interval 0)
            (cons (max 0 (+ onlypos interval)) onlypos)
            (cons onlypos (min (- nlayers 1) (+ onlypos interval)))))
      #f))

(define (tsh-render-action replace under only interval img source pos opts)
  (lambda (target)
    (let* ((bindings (tsh-get-bindings (cadr opts)))
           (tsh-apply-effects (tsh-apply-effects-simple img (list-ref opts 3)
                                                        bindings #f))
           (interval (if only (tsh-animstack-get-interval target only interval) #f))
           (realpos (if interval (car interval) 0))
           (shift (if under 0 1))
           (new (tsh-render-layer-group img target interval)))
      (if under
          (if interval
              (set! realpos (+ (cdr interval) 1))
              (set! realpos -1)))
      (set! target (tsh-put-layer-in-group img new target realpos gimp-image-insert-layer))
      (tsh-process-dup-options* opts img bindings target (tsh-dup-getter img new))
      (if replace
          (let ((test (if interval
                          (lambda (i) (<= (+ (car interval) shift) i (+ (cdr interval) shift)))
                          (lambda (i) (not (= i 0))))))
            (tsh-vector-for-each-i
             (lambda (layer i) (if (test i) (gimp-image-remove-layer img layer)))
             (car (gimp-item-get-children target)))))
      (tsh-apply-effects new target) ;; render is always non-cumulative
      )))

;; [render:limit:replace:only:interval]
(define (tsh-animstack-render img layer opts . params)
  (tsh-with-params
   (limit replace only (interval 0))
   (let* ((rep (and replace (> replace 0)))
          (under (and replace (< replace 0)))
          (action (lambda args (apply tsh-render-action rep under only interval args)))
          (fn (tsh-animstack-common -1 0 action)))
     (fn img layer opts limit))))

(define (tsh-animstack-sample-to-target img source target x y width height)
  (tsh-animstack-save-selection img)
  (gimp-selection-none img)
  (gimp-drawable-edit-clear target)
  (gimp-image-select-rectangle img CHANNEL-OP-REPLACE x y width height)
  (if (tsh-is-true? gimp-edit-copy (vector source))
      (let ((fl (car (tsh-edit-paste-into-one-drawable target FALSE)))
            (offsets (gimp-drawable-get-offsets target))
            (new-width (car (gimp-drawable-get-width target)))
            (new-height (car (gimp-drawable-get-height target))))
        ;;(gimp-context-set-interpolation INTERPOLATION-CUBIC)
        (gimp-layer-scale fl new-width new-height TRUE)
        (gimp-layer-set-offsets fl (car offsets) (cadr offsets)) ;;;; REVIEWED UP TO HERE
        (tsh-floating-sel-check-and-anchor fl)))
  (tsh-animstack-restore-selection img))

(define (tsh-animstack-linear-transition count coords1 coords2)
  (if (> count 0)
      (let* ((avg (lambda (i n)
                    (let ((c1 (list-ref coords1 i))
                          (c2 (list-ref coords2 i)))
                      (+ c1 (* (/ n count) (- c2 c1))))))
             (di (lambda (i)
                   (let ((c1 (list-ref coords1 i))
                         (c2 (list-ref coords2 i)))
                     (/ (- c2 c1) count))))
             (dx (di 0))
             (dy (di 1))
             (dzx (di 2))
             (dzy (di 3)))
        (lambda (n)
          (let* ((newcoords (if (= n count)
                                coords2
                                (let loop ((i 0))
                                  (if (< i 4) (cons (avg i n) (loop (+ i 1)))))))
                 (motion (if (= n count)
                             (list 0 0 0 0 #t)
                             (let ((cw (list-ref newcoords 2))
                                   (ch (list-ref newcoords 3)))
                               (list (/ (+ dx (/ dzx 2)) cw)
                                     (/ (+ dy (/ dzy 2)) ch)
                                     (/ dzx (- cw dzx))
                                     (/ dzy (- ch dzy))
                                     #f)))))
            (list newcoords motion))))))


(define (tsh-animstack-linear-path count nodes)
  (let* ((segment-map (make-vector count))
         (m (length nodes))
         (node-map (make-vector m))
         (first-val (list (car nodes) (list 0 0 0 0 #t))))
    (cond
     ((< count m) (error (string-append "Too many nodes: "
                                        (number->string m)
                                        ", no more than count="
                                        (number->string count)
                                        " allowed.")))
     ((or (= m 1) (= count 1)) (lambda (n) first-val))
     (else
      (do ((i 0 (+ i 1))) ((>= i (- m 1)))
        (vector-set! node-map i (round (* (/ (+ i 1) (- m 1)) (- count 1)))))
      (vector-set! segment-map 0 first-val)
      (let ((prev 0))
        (tsh-vector-for-each-i
         (lambda (next i)
           (if (< i (- m 1))
               (let* ((curcoords (list-ref nodes i))
                      (nextcoords (list-ref nodes (+ i 1)))
                      (fn (tsh-animstack-linear-transition (- next prev) curcoords nextcoords)))
                 (do ((j (+ prev 1) (+ j 1))) ((> j next))
                   (vector-set! segment-map j (fn (- j prev))))))
           (set! prev next))
         node-map))
      (lambda (n) (vector-ref segment-map n))))))


(define (tsh-get-layer-coords layer)
  (let ((offsets (gimp-drawable-get-offsets layer))
        (width (car (gimp-drawable-get-width layer)))
        (height (car (gimp-drawable-get-height layer))))
    (list (car offsets) (cadr offsets) width height)))

(define (tsh-get-toplevel-parent layer)
  (let ((parent (car (gimp-item-get-parent layer))))
    (if (= parent -1)
        layer
        (tsh-get-toplevel-parent parent))))


(define (tsh-sampler-count-frames-above img layer opts)
  (let* ((reverse_ (cadar opts))
         (tl (list-ref (car opts) 2))
         (count 0)
         (toplevel (tsh-get-toplevel-parent layer))
         (pos (car (gimp-image-get-item-position img toplevel)))
         (layers (car (gimp-image-get-layers img)))
         (last_ (- (vector-length layers) 1))
         (terminate #f))
    (do ((i pos (+ i (if reverse_ 1 -1))))
        ((or terminate (< i 0) (> i last_)) count)
      (let* ((layer (vector-ref layers i)))
        (if (tsh-is-untagged? layer)
            (set! count (+ count 1)))
        (if (and tl (tsh-animstack-layer-has-label layer tl))
            (set! terminate #t))))))

(define (tsh-make-temp-sampler-layer img group width height)
  (let ((layer (car (gimp-layer-new img width height RGBA-IMAGE
                                    "Sample layer"
                                    100 NORMAL-MODE))))
    (gimp-image-insert-layer img layer group 0)
    (gimp-layer-set-offsets layer 0 0)
    layer))


(define tsh-animstack-get-motion #f)
(define tsh-animstack-reset-motion #f)
(define tsh-animstack-set-motion #f)
(let ((motion (list 0 0 0 0 #f)))
  (set! tsh-animstack-get-motion (lambda () motion))
  (set! tsh-animstack-reset-motion (lambda () (set! motion (list 0 0 0 0 #f))))
  (set! tsh-animstack-set-motion (lambda (new-motion) (set! motion new-motion))))


(define (tsh-sampler-action img temp-layer source path
                            pos opts roll-mode)
  (let* ((roll-list (if (>= roll-mode 0)
                        (tsh-get-roll-layer-list source)
                        (list source)))
         (roll-length (length roll-list))
         (roll-count (if (>= roll-mode 0) roll-mode 0))
         (sampler-count 0))
    (if (= roll-length 0) (error "Empty roll"))
    (tsh-animstack-init-count roll-count)
    (lambda (target)
      (let* ((pick (- roll-length 1 (modulo (tsh-animstack-get-count) roll-length)))
             (src (list-ref roll-list pick))
             (pt (path sampler-count))
             (bindings (tsh-get-bindings (cadr opts)))
             (tsh-apply-effects (tsh-apply-effects-simple img (list-ref opts 3)
                                                          bindings #f)))
        (apply tsh-animstack-sample-to-target img src temp-layer (car pt))
        (tsh-animstack-set-motion (cadr pt))
        (if (caar opts) (tsh-apply-effects temp-layer target))
        (let ((new (car (gimp-layer-copy temp-layer))))
          (gimp-item-set-name new (tsh-default-copy-name new))
          (set! target (tsh-put-layer-in-group img new target pos gimp-image-insert-layer))
          (tsh-process-dup-options* opts img bindings target (tsh-dup-getter img new))
          (if (not (caar opts)) (tsh-apply-effects new target))
          (tsh-animstack-inc-count)
          (set! sampler-count (+ sampler-count 1)))))))

(define tsh-animstack-remember-image-size #f)
(define tsh-animstack-restore-image-size #f)


(let ((width #f)
      (height #f))
  (set! tsh-animstack-remember-image-size
        (lambda (img)
          (set! width (car (gimp-image-get-width img)))
          (set! height (car (gimp-image-get-height img)))))
  (set! tsh-animstack-restore-image-size
        (lambda (img)
          (gimp-image-resize img width height 0 0))))

;; [sampler:pos:count:limit:roll-mode:width:height]
(define (tsh-animstack-sampler img layer opts . params)
  (tsh-with-params
   ((pos -1) count limit (roll-mode -1)
    (width (car (gimp-image-get-width img)))
    (height (car (gimp-image-get-height img))))
   (let* ((start (tsh-get-start-position img layer opts))
          (layers (car (gimp-image-get-layers img)))
          (getter (tsh-layer-getter layers))
          (next (tsh-animstack-next-fn -1 opts))
          )
     (if (or (not count) (= count 0)) (set! count (max (tsh-sampler-count-frames-above img layer opts) 1)))
     (let* ((group (tsh-groupify-layer img layer))
            (temp-layer #f)
            (srcs (car (gimp-item-get-children group)))
            (src-count (vector-length srcs))
            (source #f)
            (node-layers #f))
       (if (= src-count 0) (error "Empty sampler"))
       (set! source (vector-ref srcs (- src-count 1)))

       (if (< count 0) (set! count (length (tsh-get-roll-layer-list source))))
       (if (or (not limit) (<= limit 0) (> limit count)) (set! limit count))

       (tsh-animstack-remember-image-size img)
       ;; need to resize the image so that it includes the whole source
       (let ((source-offsets (gimp-drawable-get-offsets source))
             (source-width (car (gimp-drawable-get-width source)))
             (source-height (car (gimp-drawable-get-height source))))
         (gimp-image-resize img (+ (car source-offsets) source-width)
                            (+ (cadr source-offsets) source-height) 0 0))
       (set! node-layers
             (if (= src-count 1) (list source)
                 (cdr (reverse (vector->list srcs)))))
       (set! temp-layer (tsh-make-temp-sampler-layer img group width height))
       (tsh-animstack-reset-count)
       ;; before effects are applied to temp layer
       (for-each
        (lambda (effect) (effect img temp-layer #f (list) #f))
        (list-ref opts 2))
       (let* ((nodes (map tsh-get-layer-coords node-layers))
              (path (tsh-animstack-linear-path count nodes))
              (action (tsh-sampler-action img temp-layer source path
                                          pos opts roll-mode))
              (tl (list-ref (car opts) 2)))
         (tsh-layer-walk getter start next tsh-is-untagged? action limit #t tl))
       (tsh-animstack-restore-image-size img)
       (tsh-animstack-reset-motion)
       (gimp-image-remove-layer img group)
       ))))

;; Duplicate trees

(define (tsh-dup-getter img layer)
  (let ((pos (car (gimp-image-get-item-position img layer))))
    (lambda (target) (tsh-get-layer-in-group target pos))))

(define (tsh-duplicate-target-frame img target dir ipos)
  (let* ((new (car (gimp-layer-copy target)))
         (pos (or ipos (car (gimp-image-get-item-position img target)))))
    (if (and (not ipos) (<= dir 0)) (set! pos (+ pos 1)))
    (gimp-image-insert-layer img new 0 pos)
    new))


(define (tsh-process-dup-options dup-options img bindings target getter)
  (let* ((opts1 (car dup-options))
         (cumulative (car opts1))
         (dir (list-ref opts1 3))
         ;; range set to always true because it gets confusing otherwise
         (tsh-apply-effects (lambda (layer tgt effects)
                              ((tsh-apply-effects-simple img effects bindings #f (lambda (n) #t)) layer tgt)))
         (pos (car (gimp-image-get-item-position img target)))
         )
    (letrec ((branch-out
              (lambda (from opts old-be old-de)
                (let* ((before-effects (append old-be (caar opts)))
                       (during-effects (append old-de (cadar opts)))
                       (branch-from from)
                       (cur-source from))
                  (for-each
                   (lambda (el)
                     (if (car el)
                         (branch-out branch-from el
                                     (if (= branch-from from) before-effects '())
                                     during-effects)
                         (let* ((incpos (if (<= dir 0) (set! pos (+ pos 1))))
                                (new (tsh-duplicate-target-frame img cur-source dir pos))
                                (layer (getter new)))
                           (tsh-apply-effects layer new (cadr el)) ;; before-effects local
                           (if (= cur-source from) ;; before-effects branch
                               (tsh-apply-effects layer new before-effects))
                           (tsh-apply-effects layer new during-effects) ;; runtime-effects branch
                           (tsh-apply-effects layer new (cddr el)) ;; runtime-effects local
                           (set! branch-from new)
                           (if cumulative (set! cur-source new)))))
                   (cdr opts))))))
      (branch-out target (cdr dup-options) '() '()))))

(define (tsh-process-dup-options* opts . rest)
  (let ((dup-options (tsh-get-dup-opts opts)))
    (if dup-options (apply tsh-process-dup-options dup-options rest))))

(define (tsh-get-dup-opts opts)
  (and (> (length opts) 4) (list-ref opts 4)))

(define (tsh-get-start-position img layer opts)
  (let ((dup-opts (tsh-get-dup-opts opts)))
    (if dup-opts
        (cadar dup-opts)
        (car (gimp-image-get-item-position img layer)))))

(define (tsh-build-dup-branch layers)
  (map (lambda (layer)
         (let* ((tags (tsh-sort-animstack-tags (tsh-extract-animstack-tags layer)))
                (before-effects (tsh-process-effect-tags (list-ref tags 2) #f))
                (during-effects (tsh-process-effect-tags (list-ref tags 3) #f)))
           (if (tsh-is-true? gimp-item-is-group layer)
               (cons (list before-effects during-effects)
                     (tsh-build-dup-branch (vector->list (car (gimp-item-get-children layer)))))
               (cons #f (cons before-effects during-effects)))))
       layers))

(define (tsh-build-dup-tree dupes opts position dir)
  (let ((cumulative (caar opts))
        (generator-alist (cadr opts))
        (before-effects (list-ref opts 2))
        (during-effects (list-ref opts 3)))
    (cons (list cumulative position generator-alist dir)
          (cons (list before-effects during-effects)
                (tsh-build-dup-branch dupes)))))


;; [dt:direction]
(define (tsh-animstack-dup-tree img layer opts . params)
  (tsh-with-params
   ((dir 1))
   (let* ((group (tsh-groupify-layer img layer))
          (contents (vector->list (car (gimp-item-get-children group))))
          (primary #f)
          (dupes #f)
          (position (car (gimp-image-get-item-position img group))))

     (if (= (length contents) 0)
         (begin
           (tsh-gimp-message* "Empty duplicate tree !")
           (error "Empty duplicate tree")))
     (if (> dir 0) (set! contents (reverse contents)))
     (set! primary (car contents))
     (set! dupes (cdr contents))
     ;; nested dts don't do anything (it just hurts my brain thinking about it)
     (if (or (<= (length opts) 4) (not (list-ref opts 5)))
         (tsh-animstack-process-layer img primary (tsh-build-dup-tree dupes opts position dir)))
     (gimp-image-remove-layer img group))))

;; Action tag processing

(define (tsh-check-tag-params params test)
  (tsh-check-all params (lambda (x) (or (not x) (test x)))))

(define (tsh-animstack-parse-tagname str)
  ;; (name . (cumulative reverse terminate-label))
  (let* ((cumulative #t)
         (reverse_ #f)
         (terminate-label #f)
         (bake
          (lambda (str)
            (let* ((split (tsh-string-split str #\>)))
              (if (= (length split) 2)
                  (begin
                    (set! terminate-label (tsh-parse-tag-param-simple (cadr split)))
                    (set! str (car split)))))
            (list str cumulative reverse_ terminate-label))))
    (let loop ((str str))
      (cond ((= (string-length str) 0) (bake str))
            ((char=? (string-ref str 0) #\.)
             (set! cumulative #f)
             (loop (substring str 1 (string-length str))))
            ((char=? (string-ref str 0) #\~)
             (set! reverse_ #t)
             (loop (substring str 1 (string-length str))))
            (else (bake str))))))

(define *tsh-animstack-action-tag-assocs*
  (tsh-make-animstack-hash
   `(("bg" ,tsh-animstack-bg)
     ("fg" ,tsh-animstack-fg)
     ("copy" ,tsh-animstack-copy)
     ("roll" ,tsh-animstack-roll)
     ("splice" ,tsh-animstack-splice)
     ("noop" ,tsh-animstack-noop)
     ("no" ,tsh-animstack-noop)
     ("matte" ,tsh-animstack-matte)
     ("delete" ,tsh-animstack-delete)
     ("render" ,tsh-animstack-render)
     ("sampler" ,tsh-animstack-sampler)
     ("dt" ,tsh-animstack-dup-tree)
     )))

(define (animstack-process-tag img layer tag generator-alist before-effects during-effects extra-opts)
  (let* ((tagpair '())
         (tagname '())
         (opts '())
         (tag-assoc '())
         (ret-vals '()))
    (set! tagpair (tsh-animstack-parse-tagname (car tag))) ;; (tagname . other opts)
    (set! tagname (car tagpair))
    (set! opts (apply list (cdr tagpair) generator-alist before-effects during-effects extra-opts))
    (set! tag-assoc (*tsh-animstack-action-tag-assocs* 'assoc tagname))
    (set! ret-vals (and tag-assoc
                        (or (tsh-check-tag-params (cdr tag) integer?)
                            (error "Action tag parameters must be integer"))
                        (apply (cadr tag-assoc) img layer opts (cdr tag))))
    ret-vals))

;; generators
;;
;; a generator processor must return a function with zero parameters
;; that returns numeric values when called repeatedly. It's called
;; once per frame to generate values for variables

(macro (tsh-generator form)
  (let ((res (gensym)))
    `(let ((x 0))
       (lambda ()
         (let ((,res (begin ,@(cdr form))))
           (set! x (+ x 1))
           ,res)))))

(define (tsh-animstack-const params)
  "const:value"
  (tsh-with-params ((value 0)) (lambda () value)))

(define (tsh-random-float range)
  (* (/ (random-next) 2147483647) range))


(define (tsh-animstack-rng params)
  "rng:range"
  (tsh-with-params ((range 10)) (lambda () (tsh-random-float range))))


(define (tsh-animstack-irng params)
  "irng:range"
  (tsh-with-params ((range 10)) (lambda () (random range))))

(define (tsh-float-remainder x period)
  (let ((div (floor (/ x (abs period)))))
    (- x (* (abs period) div))))

(define (tsh-sin-normalized x)
  "sine function with period 1"
  (sin (* (- x (floor x)) 2 *pi*)))


(define (tsh-animstack-osc params)
  "osc:amplitude:period:phase"
  (tsh-with-params
   ((amplitude 10) (period 10) (phase (tsh-random-float 1)))
   (if (= period 0) (error "Oscillator period cannot be 0"))
   (tsh-generator
    (* amplitude (tsh-sin-normalized (+ (/ x period) phase)))
    )))

(define (tsh-animstack-dosc params)
  "dosc:amplitude:period:phase"
  (tsh-with-params
   ((amplitude 10) (period 10) (phase (tsh-random-float 1)))
   (if (= period 0) (error "Oscillator period cannot be 0"))
   (tsh-generator
    (* amplitude (/ (* 2 *pi*) period) (tsh-sin-normalized (+ (/ x period) phase 0.25)))
    )))

(define (tsh-animstack-inc params)
  "inc:step:init"
  (tsh-with-params
   ((step 1) (init 0)) (tsh-generator (+ init (* x step)))))

(define (tsh-animstack-cycle params)
  (if (null? params) (set! params (list 0)))
  (let ((len (length params)))
    (tsh-generator (or (list-ref params (modulo x len)) 0))))

(define (tsh-animstack-poly params)
  (if (null? params) (set! params (list 1)))
  (set! params (reverse params))
  (if (= (or (car params) 0) 0) (set-car! params 1))
  (tsh-generator
   (let ((result 0))
     (for-each
      (lambda (k) (set! result (+ (* result x) (or k 0))))
      params)
     (* result x))))

(define (tsh-animstack-dpoly params)
  (if (null? params) (set! params (list 1)))
  (set! params (reverse params))
  (if (= (or (car params) 0) 0) (set-car! params 1))
  (tsh-generator
   (let ((result 0)
         (m (length params)))
     (for-each
      (lambda (k)
        (set! result (+ (* result x) (* m (or k 0))))
        (set! m (- m 1)))
      params)
     result)))

(define (tsh-adjust-generator-fn coeffs fn)
  (let ((add (car coeffs))
        (mul (cadr coeffs)))
    (lambda () (+ add (* mul (fn))))))

(define *tsh-animstack-generator-tag-assocs*
  (tsh-make-animstack-hash
   `(("rng" ,tsh-animstack-rng)
     ("irng" ,tsh-animstack-irng)
     ("osc" ,tsh-animstack-osc)
     ("dosc" ,tsh-animstack-dosc)
     ("inc" ,tsh-animstack-inc)
     ("const" ,tsh-animstack-const)
     ("cycle" ,tsh-animstack-cycle)
     ("poly" ,tsh-animstack-poly)
     ("dpoly" ,tsh-animstack-dpoly)
     )))

(define (tsh-init-generators generator-tags)
  (let ((alist (list)))
    (do ((rest generator-tags (cdr rest)))
        ((null? rest) alist)
      (let* ((curtag (car rest))
             (namesplit (tsh-string-split (car curtag) #\=))
             (var (string->atom (car namesplit))))
        (if (not (and (= (length namesplit) 2) (symbol? var)))
            (error (string-append "syntax error: " (car curtag)))
            (let* ((defnlist (tsh-parse-generator-defn (cadr namesplit)))
                   (gen-assoc (*tsh-animstack-generator-tag-assocs* 'assoc (caddr defnlist))))
              (and gen-assoc
                   (or (tsh-check-tag-params (cdr curtag) number?)
                       (error "Generator tag parameters must be numbers"))
                   (set! alist
                         (cons (list var (tsh-adjust-generator-fn defnlist ((cadr gen-assoc) (cdr curtag))))
                               alist)))))))))

;; Effect tags (before and during)
;;
;; before [!<tagname>:p1:p2]
;; during affects 1 layer [-<range>;<tagname>:p1:p2]
;; during affects whole group [=<range>;<tagname>:p1:p2]
;;
;; range syntax comma separated list of range designators
;; n - executed on nth step
;; n- executed after nth step (inclusive)
;; n-m - executed from nth to mth step (inclusive)
;; /n - executed every nth step
;; m/n - executed every nth step with offset m
;; rn - executed randomly 1 out of n times
;; mrn - executed randomly m out of n times
(define (tsh-get-range-fn range)
  (let* ((range-lst (string->list range))
         (paramtest (lambda (x) (and (integer? x) (>= x 0))))
         (pos-paramtest (lambda (x) (and (integer? x) (> x 0))))
         (res (cond ((null? range-lst) (lambda (n) #t))
                    ((memv #\- range-lst) ;; simple range
                     (let ((srange (tsh-string-split range #\-)))
                       (and (= (length srange) 2)
                            (let ((from (tsh-string2number (car srange) paramtest))
                                  (to (tsh-string2number (cadr srange) paramtest)))
                              (if (= (string-length (cadr srange)) 0)
                                  (and from (<= 0 from)
                                       (lambda (n) (<= from n)))
                                  (and from to (<= 0 from) (<= from to)
                                       (lambda (n) (<= from n to))))))))
                    ((memv #\/ range-lst) ;; period range
                     (let ((prange (tsh-string-split range #\/)))
                       (and (= (length prange) 2)
                            (let ((offset (tsh-string2number (car prange) paramtest))
                                  (period (tsh-string2number (cadr prange) paramtest)))
                              (if (= (string-length (car prange)) 0)
                                  (set! offset 0))
                              (and offset period (< 0 period)
                                   (lambda (n) (and (<= offset n)
                                                    (= (modulo (- n offset) period) 0))))))))
                    ((memv #\r range-lst) ;; random range
                     (let ((rrange (tsh-string-split range #\r)))
                       (and (= (length rrange) 2)
                            (let ((tries (tsh-string2number (car rrange) paramtest))
                                  (total (tsh-string2number (cadr rrange) pos-paramtest)))
                              (if (= (string-length (car rrange)) 0)
                                  (begin
                                    (set! tries 1)
                                    (if (= (string-length (cadr rrange)) 0)
                                        (set! total 2))))
                              (and tries total
                                   (lambda (n) (< (tsh-random-float 1) (/ tries total))))))))
                    (else ;; atom
                     (let ((nr (tsh-string2number range integer?)))
                       (and nr (lambda (n) (= n nr))))))))
    (if res res (error (string-append "Invalid range: " range)))))

(define (tsh-parse-range str)
  "Returns a function that given a nonnegative argument returns if it is in range"
  (if (= (string-length str) 0)
      (lambda (n) #t)
      (let ((collection (map tsh-get-range-fn (tsh-string-split str #\,))))
        (lambda (n)
          (let loop ((rlist collection))
            (cond ((null? rlist) #f)
                  (((car rlist) n) #t)
                  (else (loop (cdr rlist)))))))))

;; Actual effects procedures here

(define (tsh-animstack-move img params)
  (tsh-with-params
   ((x 0) (y 0))
   (cons
    (lambda (layer target)
      (let ((offsets (gimp-drawable-get-offsets layer)))
        (gimp-layer-set-offsets layer (+ (car offsets) x) (+ (cadr offsets) y))))
    #t)))

(define (tsh-animstack-offset img params)
  "if x or y param is omitted, offsets randomly by that param"
  ;; we need to support stackability for random case so that every
  ;; layer in a roll has the same random offset
  (tsh-with-params
   (x y wrap)
   (if (not (and wrap (= wrap 0)))
       (set! wrap TRUE))
   (cons (lambda (layer target)
           (let ((xx x)
                 (yy y)
                 (layers (tsh-get-roll-layer-list layer)))
             (if (not x)
                 (let ((width (car (gimp-drawable-get-width (car layers)))))
                   (set! xx (tsh-random-float width))))
             (if (not y)
                 (let ((height (car (gimp-drawable-get-height (car layers)))))
                   (set! yy (tsh-random-float height))))
             (for-each (lambda (layer)
                         (gimp-drawable-offset layer wrap 1 xx yy))
                       layers)))
         #t)))

(define (tsh-animstack-resize img params)
  "sets layer to image size"
  (cons (lambda (layer target)
          (gimp-layer-resize-to-image-size layer))
        #f))

;; (define (tsh-last lst)
;;   (if (null? lst)
;;     '()
;;     (if (null? (cdr lst))
;;        (car lst)
;;        (tsh-last (cdr lst)))))

;; \(last function is not bult-in in GIMP 3
(define (tsh-last lst)
  (if (null? lst)
      '()
      (list-ref lst (- (length lst) 1))))

(define (tsh-animstack-scatter img params)
  "scatter:mode - mode can be
  <= 0 (default) - moves the layer randomly so it doesn't go outside the borders of the image
   > 0 - moves layer randomly so that part of it is still within image

   If there's a non-empty selection, use selection bounds instead of borders.
   "
  (tsh-with-params
   ((mode 0))
   (let* ((image-width (car (gimp-image-get-width img)))
          (image-height (car (gimp-image-get-height img)))
          (ox 0) (oy 0))
     (let* ((selection-bounds (gimp-selection-bounds img)))
       (if (or (eqv? (car selection-bounds) TRUE) (eqv? (car selection-bounds) #t))
           (begin
             (set! ox (cadr selection-bounds))
             (set! oy (caddr selection-bounds))
             (set! image-width (- (cadddr selection-bounds) ox))
             ;; \(last function is not bult-in in GIMP 3
             (set! image-height (- (car (tsh-last selection-bounds)) oy)))))

     (cons (lambda (layer target)
             (let ((layer-width (car (gimp-drawable-get-width layer)))
                   (layer-height (car (gimp-drawable-get-height layer)))
                   (minx #f)
                   (maxx #f)
                   (miny #f)
                   (maxy #f))
               (cond ((<= mode 0)
                      (set! minx 0) (set! miny 0)
                      (set! maxx (- image-width layer-width))
                      (set! maxy (- image-height layer-height)))
                     (else
                      (set! minx (- layer-width))
                      (set! miny (- layer-height))
                      (set! maxx image-width)
                      (set! maxy image-height)))
               (set! minx (+ minx ox))
               (set! maxx (+ maxx ox))
               (set! miny (+ miny oy))
               (set! maxy (+ maxy oy))
               (if (and (< minx maxx) (< miny maxy))
                   (let ((ox (+ minx (tsh-random-float (- maxx minx))))
                         (oy (+ miny (tsh-random-float (- maxy miny)))))
                     (gimp-layer-set-offsets layer ox oy)))))
           #t))))


;; this function is bugged because re-match doesn't support unicode

(define (animstack-get-disposal-mode str)
  "mode can be (replace) or (combine) at the end of the string"
  (let ((buffer (make-vector 2)))
    (and (re-match "\\((combine|replace)\\)\\s*$" str buffer)
         (let ((boundaries (vector-ref buffer 1)))
           (substring str (car boundaries) (cdr boundaries))))))
(define (tsh-add-combine-replace str mode)
  "mode can be \"keep\" to keep existing mode"
  (let ((keep-mode (equal? mode "keep"))
        (old-mode #f))
    (set! str (list->string
               (let loop ((sl (string->list str)))
                 (cond ((null? sl) (list))
                       ((char=? (car sl) #\()
                        (let* ((split (tsh-string-split (list->string (cdr sl)) #\)))
                               (inside (car split)))
                          (if (and (> (length split) 1)
                                   (or (equal? inside "combine")
                                       (equal? inside "replace")))
                              (begin
                                (if keep-mode
                                    (set! old-mode inside))
                                (loop (cdr (memv #\) sl))))
                              (cons (car sl) (loop (cdr sl))))))
                       (else (cons (car sl) (loop (cdr sl))))))))
    (let ((new-mode (if keep-mode old-mode mode)))
      (if new-mode (string-append str " (" new-mode ")") str))))

(define (tsh-add-frame-delay str delay)
  ;; we must find all substrings of the form (<number>ms) and remove them
  (set! str (list->string
             (let loop ((sl (string->list str)))
               (cond ((null? sl) (list))
                     ((char=? (car sl) #\()
                      (let* ((split (tsh-string-split (list->string (cdr sl)) #\)))
                             (inside (car split))
                             (len (string-length inside)))
                        (if (and (> (length split) 1)
                                 (> len 2)
                                 (equal? (substring inside (- len 2) len) "ms")
                                 (tsh-string2number (substring inside 0 (- len 2)) integer?))
                            (loop (cdr (memv #\) sl)))
                            (cons (car sl) (loop (cdr sl))))))
                     (else (cons (car sl) (loop (cdr sl))))))))
  (tsh-add-combine-replace
   (if delay (string-append str " (" (number->string delay) "ms)") str)
   "keep"))

(define (tsh-animstack-delay img params)
  "Sets frame delay for target"
  (tsh-with-params
   ((frame-delay 40) (corner-delay frame-delay))
   (if (<= frame-delay 0) (set! frame-delay #f))
   (cons (lambda (layer target)
           (if target
               (let* ((motion (tsh-animstack-get-motion))
                      (corner? (list-ref motion 4))
                      (oldname (car (gimp-item-get-name target)))
                      (newname (tsh-add-frame-delay oldname
                                                    (if corner? corner-delay frame-delay))))
                 (gimp-item-set-name target newname))))
         #t)))

(define (tsh-animstack-replace img params)
  (tsh-with-params
   ((mode 1))
   (set! mode (cond ((< mode 0) "combine")
                    ((> mode 0) "replace")
                    (else #f)))
   (cons (lambda (layer target)
           (if target
               (let* ((oldname (car (gimp-item-get-name target)))
                      (newname (tsh-add-combine-replace oldname mode)))
                 (gimp-item-set-name target newname))))
         #t)))

(define (tsh-animstack-erase img params)
  "erase:n:direction - Cuts n letters from text layer, either from
   end (mode<=0) or from beginning (mode>0)

  Doesn't work for formatted text unfortunately (use shrink)"
  (tsh-with-params
   ((n 1) (mode 0))
   (if (< n 0) (set! n 0))
   (if (not (integer? n)) (set! n (tsh-int-round n)))
   (cons (lambda (layer target)
           (if (tsh-is-true? gimp-item-id-is-text-layer layer)
               (let* ((text (car (gimp-text-layer-get-text layer)))
                      (tlen (string-length text)))
                 (set! text (if (> tlen n)
                                (if (> mode 0)
                                    (substring text n tlen)
                                    (substring text 0 (- tlen n)))
                                ""))
                 (gimp-text-layer-set-text layer text))))
         #f)))

(define (tsh-animstack-shrink img params)
  (tsh-with-params
   ((dright 0) (dbottom 0) (dleft 0) (dtop 0))
   (cons (lambda (layer target)
           (let* ((width (car (gimp-drawable-get-width layer)))
                  (height (car (gimp-drawable-get-height layer)))
                  (offsets (gimp-drawable-get-offsets layer))
                  (ox (car offsets))
                  (oy (cadr offsets))
                  (new-ox ox)
                  (new-oy oy)
                  (new-width width)
                  (new-height height))
             ;; (if (= width 1) (begin (set! width 0) (gimp-item-set-visible layer TRUE)))
             ;; (if (= height 1) (begin (set! height 0) (gimp-item-set-visible layer TRUE)))
             (set! new-width (- new-width dright))
             (set! new-height (- new-height dbottom))
             (if (< dleft new-width)
                 (begin (set! new-ox (+ new-ox dleft))
                        (set! new-width (- new-width dleft)))
                 (begin (set! new-ox (+ new-width))
                        (set! new-width 0)))
             (if (< dtop new-height)
                 (begin (set! new-height (- new-height dtop))
                        (set! new-oy (+ new-oy dtop)))
                 (begin (set! new-oy (+ new-oy new-height))
                        (set! new-height 0)))
             ;; because GIMP won't allow 0 width/height layers for some reason...
             (if (= new-width 0)
                 (begin (set! new-width 1)
                        (gimp-item-set-visible layer FALSE)))
             (if (= new-height 0)
                 (begin (set! new-height 1)
                        (gimp-item-set-visible layer FALSE)))
             (gimp-layer-resize layer new-width new-height
                                (- ox new-ox) (- oy new-oy))))
         #f)))

(define (tsh-animstack-scale img params)
  (tsh-with-params
   ((hscale 1) (vscale hscale))
   (cons (lambda (layer target)
           ;;(gimp-context-set-interpolation INTERPOLATION-CUBIC)
           (let* ((width (car (gimp-drawable-get-width layer)))
                  (height (car (gimp-drawable-get-height layer)))
                  (new-width (max 1 (* width hscale)))
                  (new-height (max 1 (* height vscale))))
             (gimp-layer-scale layer new-width new-height TRUE)))
         #f)))

(define (tsh-animstack-stretch img params)
  (tsh-with-params
   ((dwidth 0) (dheight 0))
   (cons (lambda (layer target)
           ;;(gimp-context-set-interpolation INTERPOLATION-CUBIC)
           (let* ((width (car (gimp-drawable-get-width layer)))
                  (height (car (gimp-drawable-get-height layer)))
                  (new-width (max 1 (+ width dwidth)))
                  (new-height (max 1 (+ height dheight))))
             (gimp-layer-scale layer new-width new-height TRUE)))
         #f)))

(define (tsh-animstack-add-margin layer margin)
  (cond ((tsh-is-true? gimp-item-is-group layer)
         (tsh-vector-for-each
          (lambda (child) (tsh-animstack-add-margin child margin))
          (car (gimp-item-get-children layer))))
        (else
         (let ((width (car (gimp-drawable-get-width layer)))
               (height (car (gimp-drawable-get-height layer)))
               (m2 (* 2 margin)))
           (gimp-layer-resize layer (+ m2 width) (+ m2 height) margin margin)))))

(define (tsh-autocrop-layer-transparent img layer margin)
  "Only remove transparent borders from layer"

  (gimp-image-set-selected-layers img (vector layer)) ; set active layer

  (if (not (tsh-is-true? gimp-item-is-group layer))
      (gimp-layer-add-alpha layer))
  ;;add 1px transparent border
  (tsh-animstack-add-margin layer 1)
  (gimp-image-autocrop-selected-layers img layer)

  (if (and margin (> margin 0))
      (tsh-animstack-add-margin layer margin)))

(define (tsh-animstack-rotate img params)
  (tsh-with-params
   ((angle 90) nocrop)
   (let* ((fn (lambda (rotator)
                (lambda (layer)
                  (tsh-animstack-save-selection img)
                  (gimp-selection-none img)
                  (gimp-context-set-transform-resize 0)
                  (rotator layer)
                  (tsh-animstack-restore-selection img)))))
     (set! angle (tsh-float-remainder angle 360))
     (cond ((= angle 0) (set! fn (lambda (layer))))
           ((or (= angle 90)
                (= angle 180)
                (= angle 270))
            (set! fn
                  (fn (lambda (layer)
                        (gimp-item-transform-rotate-simple
                         layer (- (/ angle 90) 1) TRUE 0 0)))))
           (else
            (set! angle (/ (* *pi* angle) 180))
            (set! fn
                  (fn (lambda (layer)
                        ;;(gimp-context-set-interpolation INTERPOLATION-CUBIC)
                        (gimp-context-set-transform-direction 0)
                        (gimp-item-transform-rotate layer angle TRUE 0 0))))))
     (cons (lambda (layer target)
             (if (not nocrop) (tsh-autocrop-layer-transparent img layer 0))
             (fn layer)) #t))))

(define (tsh-animstack-drotate img params)
  (tsh-with-params
   ((x 0) (y -1) (ix 0) (iy -1) nocrop)
   (let* ((angle (if (or (= ix iy 0) (= x y 0)) 0
                     (/ (* (- (atan y x) (atan iy ix)) 180) *pi*))))
     (tsh-animstack-rotate img (list angle nocrop)))))

(define (tsh-animstack-crop img params)
  (tsh-with-params
   ((margin 0))
   (cons (lambda (layer target)
           (tsh-autocrop-layer-transparent img layer margin))
         #t)))

(define (tsh-animstack-dup img params)
  "Duplicates the target frame. The result is very different in cumulative and non-cumulative modes.
   If parameter is <=0, put the duplicate before the target frame, otherwise put after."
  (tsh-with-params
   ((dir 0))
   (cons (lambda (layer target)
           (if target (tsh-duplicate-target-frame img target dir #f)))
         #t)))

(define (tsh-animstack-mask img params)
  "[mask:from] Adds mask from selection (or replaces existing one). If from is specified,
   applies a mask based on alpha of target layer at position *from*."
  (tsh-with-params
   (from)
   (let ((mask-from
          (lambda (layer target from)
            (tsh-animstack-save-selection img)
            (let ((source (tsh-get-layer-in-group target (tsh-int-round from))))
              (gimp-image-select-item img CHANNEL-OP-REPLACE source)))))
     (cons (lambda (layer target)
             (cond ((tsh-is-true? gimp-item-is-group layer)) ;; Layer groups do not support masks
                   (else (if (>= (car (gimp-layer-get-mask layer)) 0)
                             (gimp-layer-remove-mask layer MASK-DISCARD)) ;; option to MASK-APPLY?
                         (if from (mask-from layer target from))
                         (let ((mask (car (gimp-layer-create-mask layer ADD-MASK-SELECTION))))
                           (gimp-layer-add-mask layer mask))
                         (if from (tsh-animstack-restore-selection img)))))
           #f))))

(define (tsh-animstack-opacity img params)
  (tsh-with-params
   ((opacity 100))
   (if (< opacity 0) (set! opacity 0))
   (if (> opacity 100) (set! opacity 100))
   (cons (lambda (layer target)
           (gimp-layer-set-opacity layer opacity))
         #f)))

(define (tsh-angle-constraint angle)
  (- angle (* (floor (/ angle 360)) 360)))

(define (tsh-descartes-to-blur-params dx dy)
  (cons (sqrt (+ (* dx dx) (* dy dy)))
        (tsh-angle-constraint
         (if (= dx dy 0) 0 (/ (* 180 (atan (- dy) (- dx))) *pi*)))))

(define (linear_motion_blur layer len angle)
  "
   @param layer: input drawable
   @param len: length in pixels
   @param angle: Angle (0 <= angle <= 360)
  "
  (define (fmodulo n d)
    "modulo of n / d (where n and d are float numbers)"
    (- n (* (truncate (/ n d)) d)))
  (define (clampTo180 angle)
    "
    clamp angle to Angle(0..360) to Angle(-180..180)
    "
    (let* ((c_angle (fmodulo angle 360)))
      (if (> c_angle 180)
          (+ -180 (fmodulo c_angle 180))
          c_angle)))
  ;;gegl:motion-blur-linear
  ;;           length: default: 10.00, minimum: 0.00, maximum: 1000.00
  ;;           angle: default: 0.00, minimum: -180.00, maximum: 180.00
  (gimp-drawable-merge-new-filter layer "gegl:motion-blur-linear" 0 LAYER-MODE-REPLACE 1.0
                                  "length" len "angle" (clampTo180 angle))
  )
;; In GIMP 2: (plug-in-mblur 1 img layer 2 (abs r) 0 x y)
;; (('run-mode', 'The run mode'), ('image', '(unused)'), ('drawable', 'Input drawable'),
;; ('type', 'Type of motion blur { LINEAR (0), RADIAL (1), ZOOM (2) } (0 <= type <= 2)'),
;; ('length', 'Length'), ('angle', 'Angle (0 <= angle <= 360)'),
;; ('center-x', 'Center X'),
;; ('center-y', 'Center Y'))
;; gegl:motion-blur-zoom
;;     center-x: default: 0.50, minimum: -10.00, maximum: 10.00
;;     center-y: default: 0.50, minimum: -10.00, maximum: 10.00
;;     factor: default: 0.10, minimum: -10.00, maximum: 1.00

(define (zoom_motion_blur layer stength cx cy)
  "
  cx, cy: center x, y in pixels
  strength: (-500..1000)
  "
  (let* ((old-v3 *ssiun-v3*) (blur-length ))
    (script-fu-use-v3)
    (set! *ssiun-v3* #t)
    (let*  (  (image (gimp-item-get-image layer))
              (lw (gimp-drawable-get-width layer))
              (lh (gimp-drawable-get-height layer))
              (iw (gimp-image-get-width image))
              (ih (gimp-image-get-height image))
              (factor (/ stength 1000))
              )
      (set! factor (min 1.0 factor))
      (set! factor (max -10.0 factor))
      (define (x-lw x) (/ x lw) )
      (define (y-lh x) (/ x lh) )
      (define (x-iw x) (/ x iw) )
      (define (y-ih x) (/ x ih) )
      (gimp-drawable-merge-new-filter layer "gegl:motion-blur-zoom" 0 LAYER-MODE-REPLACE 1.0
                                      "center-x" (x-lw cx) "center-y" (y-lh cy) "factor" factor)
      (set! *ssiun-v3* old-v3)
      layer )))

(define (zoom_motion_blur_inward layer stength cx cy)
  (zoom_motion_blur layer (- stength) cx cy))

;; motion blur
(define (tsh-animstack-mb img params)
  (define (fmodulo n d)
    (- n (* (truncate (/ n d)) d)))
  (define (clampTo180 angle)
    (let* ((c_angle (fmodulo angle 360)))
      (if (> c_angle 180)
          (+ -180 (fmodulo c_angle 180))
          c_angle)))

  (tsh-with-params
   ((dx 0) (dy 0))
   (let ((bp (tsh-descartes-to-blur-params dx dy)))
     (cons (lambda (layer target)
             (if (not (= dx dy 0))
                 (let* ((len (car bp))
                        (angle (cdr bp)))
                   (linear_motion_blur layer len angle)
                   )))
           #f))))


;; zoom blur (inward if r<0)
(define (tsh-animstack-zb img params)
  (tsh-with-params
   ((r 0) (cx 0.5) (cy 0.5))
   (let ((zoomfn (if (< r 0) zoom_motion_blur_inward zoom_motion_blur)))
     (cons (lambda (layer target)
             (if (not (= r 0))
                 (let* ((width (car (gimp-drawable-get-width layer)))
                        (height (car (gimp-drawable-get-height layer)))
                        (x (* cx width))
                        (y (* cy height)))
                   (zoomfn layer (abs r) x y)
                   )))
           #f))))

;; radial blur (seems to be symmetrical counter and clockwise...)
(define (tsh-animstack-rb img params)
  (tsh-with-params
   ((angle 0) (cx 0.5) (cy 0.5))
   (cons (lambda (layer target)
           (if (not (= angle 0))
               (let* ((width (car (gimp-drawable-get-width layer)))
                      (height (car (gimp-drawable-get-height layer)))
                      (x (* cx width))
                      (y (* cy height)))
                 ;;"gegl:motion-blur-circular"
                 ;; center-x:double, default: 0.50, minimum: -inf, maximum: +inf
                 ;; center-y:double, default: 0.50, minimum: -inf, maximum: +inf
                 ;; angle:double, default: 5.00, minimum: 0.00, maximum: 360.00
                 (gimp-drawable-merge-new-filter layer "gegl:motion-blur-circular" 0 LAYER-MODE-REPLACE 1.0
                                                 "center-x" x "center-y" y "angle" (tsh-angle-constraint angle))
                 )))
         #f)))

;; gaussian blur
(define (gaussian_blur layer rx ry)
  (gimp-drawable-merge-new-filter layer "gegl:gaussian-blur" 0 LAYER-MODE-REPLACE 1.0
                                  "std-dev-x" (* 0.32 rx) "std-dev-y" (* 0.32 ry) "filter" "auto"))

(define (tsh-animstack-gb img params)
  (tsh-with-params
   ((rx 0) (ry rx))
   (cons (lambda (layer target)
           (if (not (= rx ry 0))
               (gaussian_blur layer rx ry)
               ))
         #f)))

;; sampler blur
(define (tsh-animstack-sb img params)
  (tsh-with-params
   ((kl 10) (kz 10))
   (let* ((kl (/ kl 30)) (kz (/ kz 100)))
     (cons (lambda (layer target)
             (let* ((motion (tsh-animstack-get-motion))
                    (dx (car motion))
                    (dy (cadr motion))
                    (width (car (gimp-drawable-get-width layer)))
                    (height (car (gimp-drawable-get-height layer)))
                    (mb (car (tsh-animstack-mb img (list (* kl dx width) (* kl dy height)))))
                    (zx (- (caddr motion)))
                    (zy (- (cadddr motion)))
                    (zv (if (< (abs zx) (abs zy)) zx zy))
                    (p-axis (if (< (abs zx) (abs zy)) width height))
                    ;; yeahhh idk there's no zoom blur in gimp that works when zx!=zy
                    (mz (car (tsh-animstack-zb img (list (* kz zv p-axis)))))
                    )
               (mb layer target)
               (mz layer target)))
           #f))))

;; invert colors
(define (tsh-animstack-invert img params)
  (cons (lambda (layer target)
          (gimp-drawable-invert layer FALSE))
        #f))

;; threshold:lower:upper
;; if no parameters, replace every visible pixel with black
(define (tsh-animstack-threshold img params)
  (tsh-with-params
   (lower upper)
   (if (or lower upper)
       (begin
         (set! lower (or lower 128))
         (set! upper (or upper 255))
         (set! upper (min (max upper 0) 255))
         (set! lower (min (max lower 0) upper))))
   (cons (lambda (layer target)
           (if lower
               (gimp-threshold layer lower upper)
               (begin
                 (gimp-threshold layer 0 255)
                 (gimp-drawable-invert layer FALSE))))
         #f)))

;; desaturate:mode
(define (tsh-animstack-desaturate img params)
  (tsh-with-params
   ((mode 0))
   (set! mode (cond ((< mode 0) 0)   ;;lightness
                    ((= mode 0) 2)   ;;average
                    ((> mode 0) 1))) ;;luminosity
   (cons (lambda (layer target)
           (gimp-desaturate-full layer mode))
         #f)))

;; gradient map
;; uses current gradient or rgb -> white
(define (tsh-animstack-gradmap img params)
  (tsh-with-params
   (r g b)
   (if (or r g b)
       (begin
         (set! r (min (max (or r 0) 0) 255))
         (set! g (min (max (or g 0) 0) 255))
         (set! b (min (max (or b 0) 0) 255))
         (gimp-context-set-gradient "FG to BG (RGB)") ;; is this translated? i hope not!
         (gimp-context-set-foreground (list r g b))
         (gimp-context-set-background (list 255 255 255))))
   (cons (lambda (layer target)
           (plug-in-gradmap 1 img layer))
         #f)))

;; repeat:n repeat current layer in a roll n times (n=1 by default)
(define (tsh-animstack-repeat img params)
  (tsh-with-params
   ((n 2))
   (cons (lambda (layer target)
           (tsh-animstack-set-repeat-value n))
         #t)))

(define (tsh-animstack-seed img params)
  (tsh-with-params
   ((n 0))
   (cons (lambda (layer target)
           (srand n))
         #t)))
;; end


(define (tsh-resolve-bindings params bindings)
  (let ((resolve-symbol
         (lambda (p)
           (if (symbol? p)
               (let ((binding (assoc p bindings)))
                 (if binding
                     (cadr binding)
                     (error (string-append "Unbound symbol: " (symbol->string p)))))
               p))))
    (map (lambda (p)
           (cond ((pair? p) (+ (car p) (* (cadr p) (resolve-symbol (caddr p)))))
                 (else (resolve-symbol p))))
         params)))

(define (tsh-animstack-effect fn mode default-range . params)
  (let ((count 0))
    (lambda (img layer target bindings group . extra)
      (let* ((effect/stackable (fn img (tsh-resolve-bindings params bindings)))
             (effect (car effect/stackable))
             (stackable (cdr effect/stackable))
             (range (if (and (pair? extra) (car extra)) (car extra) default-range))
             (count (if (and (>= (length extra) 2) (cadr extra)) (cadr extra) count))
             )
        (and (or (char=? mode #\!)
                 (and (not range) (> count 0)) ;; if no range set, skip the first layer
                 (and range (range count)))
             (if (and group (or (char=? mode #\!) (char=? mode #\+)))
                 (if stackable
                     (effect group target)
                     (for-each (lambda (layer) (effect layer target))
                               (tsh-get-roll-layer-list group)))
                 (effect layer target))))
      (set! count (+ count 1)))))


(define *tsh-animstack-effect-tag-assocs*
  (tsh-make-animstack-hash
   `(("scatter" ,tsh-animstack-scatter)
     ("move" ,tsh-animstack-move)
     ("offset" ,tsh-animstack-offset)
     ("resize" ,tsh-animstack-resize)
     ("delay" ,tsh-animstack-delay)
     ("replace" ,tsh-animstack-replace)
     ("erase" ,tsh-animstack-erase)
     ("shrink" ,tsh-animstack-shrink)
     ("scale" ,tsh-animstack-scale)
     ("stretch" ,tsh-animstack-stretch)
     ("rotate" ,tsh-animstack-rotate)
     ("drotate" ,tsh-animstack-drotate)
     ("crop" ,tsh-animstack-crop)
     ("dup" ,tsh-animstack-dup)
     ("mask" ,tsh-animstack-mask)
     ("opacity" ,tsh-animstack-opacity)
     ("mb" ,tsh-animstack-mb)
     ("zb" ,tsh-animstack-zb)
     ("rb" ,tsh-animstack-rb)
     ("gb" ,tsh-animstack-gb)
     ("sb" ,tsh-animstack-sb)
     ("thr" ,tsh-animstack-threshold)
     ("threshold" ,tsh-animstack-threshold)
     ("des" ,tsh-animstack-desaturate)
     ("desaturate" ,tsh-animstack-desaturate)
     ("gradmap" ,tsh-animstack-gradmap)
     ("invert" ,tsh-animstack-invert)
     ("repeat" ,tsh-animstack-repeat)
     )))

(define (tsh-process-effect-tag tag normal)
  (let* ((tag-defn (car tag))
         (tag-params (cdr tag))
         (tag-mode (string-ref tag-defn 0))
         (tag-rest (if (char=? tag-mode #\;) tag-defn (substring tag-defn 1 (string-length tag-defn))))
         (tag-defn-parsed (tsh-string-split tag-rest #\;))
         (len-tdp (length tag-defn-parsed))
         (tag-range #f)
         (tag-name #f))
    (cond ((and (char=? tag-mode #\!) normal)
           (if (not (= len-tdp 1))
               (error (string-append "Range not allowed in before effects: " tag-defn)))
           (if (not (tsh-check-tag-params tag-params number?))
               (error (string-append "Invalid parameter: " tag-defn)))
           (set! tag-name (car tag-defn-parsed)))
          (else
           (if (or (char=? tag-mode #\;) (and (not normal) (char=? tag-mode #\!)))
               (set! tag-mode #\-))
           (cond ((> len-tdp 2)
                  (error (string-append "Invalid tag syntax: " tag-defn)))
                 ((= len-tdp 2)
                  (set! tag-name (cadr tag-defn-parsed))
                  (set! tag-range (tsh-parse-range (car tag-defn-parsed))))
                 (else
                  (set! tag-name (car tag-defn-parsed))))))

    (let ((tag-assoc (*tsh-animstack-effect-tag-assocs* 'assoc tag-name)))
      (and tag-assoc
           (apply tsh-animstack-effect (cadr tag-assoc) tag-mode tag-range tag-params)))))

(define (tsh-map-filter fn lst . params)
  (let ((test (and (pair? params) (car params)))
        (res (map fn lst)))
    (let loop ((res res))
      (cond ((null? res) (list))
            ((if test (test (car res)) (car res)) (cons (car res) (loop (cdr res))))
            (else (loop (cdr res)))))))

(define (tsh-process-effect-tags tags normal)
  (tsh-map-filter (lambda (tag) (tsh-process-effect-tag tag normal)) tags))


;; Main processing

(define (tsh-sort-animstack-tags tags)
  (let ((action-tags (list))
        (generator-tags (list))
        (before-tags (list))
        (during-tags (list)))
    (let loop ((tail tags))
      (if (pair? tail)
          (let* ((cur-tag (car tail))
                 (cur-tag-name (car cur-tag)))
            (cond ((= (string-length cur-tag-name) 0) (loop (cdr tail)))
                  ((memv #\= (string->list cur-tag-name))
                   (loop (cdr tail))
                   (set! generator-tags (cons cur-tag generator-tags)))
                  (else
                   (let ((chr (string-ref cur-tag-name 0)))
                     (cond ((char=? chr #\!)
                            (loop (cdr tail))
                            (set! before-tags (cons cur-tag before-tags)))
                           ((or (char=? chr #\-)
                                (char=? chr #\+)
                                (char=? chr #\;))
                            (loop (cdr tail))
                            (set! during-tags (cons cur-tag during-tags)))
                           (else
                            (loop (cdr tail))
                            (set! action-tags (cons cur-tag action-tags))))))))))
    (list action-tags generator-tags before-tags during-tags)))

(define (tsh-animstack-process-layer img layer dup-options)
  (let* ((tags (tsh-sort-animstack-tags (tsh-extract-animstack-tags layer)))
         (action-tags (list-ref tags 0))
         ;; add default inc generator
         (generator-tags (cons '("i=inc") (list-ref tags 1)))
         (before-tags (list-ref tags 2))
         (during-tags (list-ref tags 3)))
    (if (or (pair? action-tags) (pair? during-tags) dup-options)
        (let* ((generator-alist (tsh-init-generators generator-tags))
               (before-effects (tsh-process-effect-tags before-tags #t))
               (during-effects (tsh-process-effect-tags during-tags #t)))
          (if dup-options
              (set! generator-alist (append (caddar dup-options) generator-alist)))
          ;; if no action tag, but during tag present, add a simple noop action tag
          (if (null? action-tags) (set! action-tags (list (list "noop"))))
          (animstack-process-tag img layer (car action-tags)
                                 generator-alist before-effects during-effects
                                 (if dup-options (list dup-options) '()))))))

(define (tsh-is-multiply-tag? tag)
  (let ((name (car tag)))
    (and (null? (cdr tag))
         (> (string-length name) 1)
         (char=? (string-ref name 0) #\*)
         (tsh-string2number (substring name 1 (string-length name))))))

(define (tsh-process-multiply-tag img layer tag)
  (let* ((tag-name (car tag))
         (layer-name (tsh-strip-tags (car (gimp-item-get-name layer)) tsh-is-multiply-tag?))
         (num (tsh-string2number (substring tag-name 1 (string-length tag-name))))
         (pos (car (gimp-image-get-item-position img layer)))
         (first-new-layer #f))
    (do ((i 0 (+ i 1)))
        ((>= i num))
      (let ((new (car (gimp-layer-copy layer))))
        (gimp-item-set-name new layer-name)
        (gimp-image-insert-layer img new 0 pos)
        (if (= i 0) (set! first-new-layer new))))
    (gimp-image-remove-layer img layer)
    first-new-layer))

(define (tsh-is-label-tag? tag)
  (and (= (string-length (car tag)) 0)))

(define tsh-animstack-reset-labels #f)
(define tsh-animstack-set-layer-labels #f)
(define tsh-animstack-layer-has-label #f)
(define tsh-animstack-copy-layer-labels #f)

(let* ((label-hash (tsh-make-animstack-hash '()))
       (label-tag-symbol
        (lambda (tag)
          (if (null? (cdr tag))
              (string->symbol "")
              (cadr tag)))))
  (set! tsh-animstack-reset-labels
        (lambda () (set! label-hash (tsh-make-animstack-hash '()))))
  (set! tsh-animstack-set-layer-labels
        (lambda (layer tags)
          (apply label-hash 'add layer (map label-tag-symbol tags))))
  (set! tsh-animstack-copy-layer-labels
        (lambda (oldlayer newlayer)
          (let ((labels (cond ((label-hash 'assoc oldlayer) => cdr))))
            (if labels (apply label-hash 'add newlayer labels)))))
  (set! tsh-animstack-layer-has-label
        (lambda (layer label)
          (let ((lst (cond ((label-hash 'assoc layer) => cdr) (else #f))))
            (and lst (memv label lst))))))

(define (tsh-animstack-process-all-layers img)
  (srand (realtime))
  (gimp-image-undo-group-start img)
  (gimp-image-freeze-layers img)
  (let ((layers (car (gimp-image-get-layers img))))
    ;; make everylayer visible. this is because it might be extremely
    ;; annoying to make them visible again after everything is jumbled up
    (tsh-vector-for-each
     (lambda (layer)
       (gimp-progress-pulse)
       (gimp-item-set-visible layer TRUE))
     layers)
    ;; preprocessing: find multiply tags and label tags and execute them
    (tsh-animstack-reset-labels)
    (tsh-vector-for-each
     (lambda (layer)
       (gimp-progress-pulse)
       (let ((tags (tsh-extract-animstack-tags layer tsh-is-multiply-tag?))
             (labeltags (tsh-extract-animstack-tags layer tsh-is-label-tag?)))
         (if (pair? labeltags)
             (gimp-item-set-name layer (tsh-strip-tags (car (gimp-item-get-name layer)) tsh-is-label-tag?)))
         (if (pair? tags)
             (let ((newlayer (tsh-process-multiply-tag img layer (car tags))))
               (set! layer newlayer)))
         (if (and layer (pair? labeltags)) (tsh-animstack-set-layer-labels layer labeltags))))
     layers))
  ;; now the main part
  (gimp-context-push)
  (let* ((layers (car (gimp-image-get-layers img))))
    (tsh-vector-for-each
     (lambda (layer)
       (gimp-progress-pulse)
       (tsh-animstack-process-layer img layer #f))
     layers))
  (gimp-context-pop)
  (gimp-image-thaw-layers img)
  (gimp-image-undo-group-end img)
  (gimp-progress-end)
  (gimp-displays-flush))

(define (script-fu-tsh-animstack-process-all-filter InImage InDrawables)
  (tsh-animstack-process-all-layers InImage))

(script-fu-register-filter
 "script-fu-tsh-animstack-process-all-filter"
 _"Process AnimStack tags (T. Shatrov)"
 _"Process all AnimStack tags"
 "Timofei Shatrov"
 "Copyright 2012-2016"
 "April 13, 2016"
 "RGB RGBA GRAY GRAYA" ;; no layer groups in indexed :(
 SF-ONE-OR-MORE-DRAWABLE
 )

(script-fu-menu-register "script-fu-tsh-animstack-process-all-filter"
                         ;; FOR TRANSLATORS: Don't translate '<Image>/Filters/Animation/'
                         "<Image>/Filters/Animation/Timofei Shatrov")
(script-fu-register-i18n "script-fu-tsh-animstack-process-all-filter" "Standard")

;; Layer group helpers (release as a separate script maybe?)
(define (tsh-walk-layers-recursive img test fn)
  (let loop ((layers (car (gimp-image-get-layers img))))
    (tsh-vector-for-each
     (lambda (layer)
       (cond ((test layer) (fn layer))
             ((tsh-is-true? gimp-item-is-group layer)
              (loop (car (gimp-item-get-children layer))))))
     layers)))

;; Reverse/Mirror

;; Note By Ssiun Enuy on April 1, 2023
;; In GIMP 2.99.14 ,
;;    Reverse layers' does not work in main layer stack.
;;    The reason is that gimp-image-reorder-item has a bug. It does not work
;;    in the topmost level of the layer stack.
(define (tsh-animstack-swap-layers img layer1 layer2 parent)
  (let* ((pos1 (car (gimp-image-get-item-position img layer1)))
         (pos2 (car (gimp-image-get-item-position img layer2))))
    (gimp-image-reorder-item img layer1 parent pos2)
    (gimp-image-reorder-item img layer2 parent pos1)))


(define (tsh-animstack-reverse-layers img parent layers copy?)
  (let ((len (vector-length layers)))
    (tsh-vector-for-each-i
     (if copy?
         (lambda (layer i)
           (gimp-progress-pulse)
           (let ((new (car (gimp-layer-copy layer))))
             (gimp-image-insert-layer img new parent 0)))
         (lambda (layer i)
           (gimp-progress-pulse)
           (if (< (* 2 i) (- len 1))
               (tsh-animstack-swap-layers img layer
                                          (vector-ref layers (- len i 1)) parent))))
     layers)))

(define (tsh-animstack-mirror-layers img parent layers)
  (let ((len (vector-length layers)))
    (if (> len 2)
        (let ((middle (make-vector (- len 2))))
          (tsh-vector-for-each-i
           (lambda (layer i)
             (gimp-progress-pulse)
             (if (not (or (= i 0) (= i (- len 1))))
                 (vector-set! middle (- i 1) layer)))
           layers)
          (tsh-animstack-reverse-layers img parent middle #t)))))


(define (script-fu-tsh-reverse-mirror-layers img drw mode ignore-tagged)
  (let ((parent (car (gimp-item-get-parent drw)))
        (layers #f))
    (cond ((= parent -1)
           (set! parent 0)
           (set! layers (car (gimp-image-get-layers img))))
          (else
           (set! layers (car (gimp-item-get-children parent)))))
    (if (or (eqv? ignore-tagged TRUE) (eqv? ignore-tagged #t))
        (set! layers (list->vector
                      (tsh-map-filter (lambda (x) x) (vector->list layers)
                                      tsh-is-untagged?))))
    (gimp-image-undo-group-start img)
    (gimp-image-freeze-layers img)
    (cond ((= mode 0) (tsh-animstack-reverse-layers img parent layers #f))
          ((= mode 1) (tsh-animstack-mirror-layers img parent layers)))
    (gimp-image-thaw-layers img)
    (gimp-image-undo-group-end img)))

(define (script-fu-tsh-reverse-mirror-layers-filter InImage InDrawables mode ignore-tagged)
  (let* ((img InImage) (drawable (vector-ref InDrawables 0)))
    (script-fu-tsh-reverse-mirror-layers img drawable mode ignore-tagged)))

(script-fu-register-filter
 "script-fu-tsh-reverse-mirror-layers-filter"
 _"Reverse OR Mirror layers (T. Shatrov)..."
 _"Reverse or mirror layers at the same level as selected layer"
 "Timofei Shatrov"
 "Copyright 2012"
 "October 11, 2012"
 "RGB RGBA GRAY GRAYA"
 SF-ONE-DRAWABLE
 SF-OPTION _"O_peration" '(_"R_everse" _"_Mirror")
 SF-TOGGLE _"_Ignore tagged layers" FALSE
 )

(script-fu-menu-register "script-fu-tsh-reverse-mirror-layers-filter"
                         _"<Image>/Image/Timofei Shatrov")
(script-fu-register-i18n "script-fu-tsh-reverse-mirror-layers-filter" "Standard")
