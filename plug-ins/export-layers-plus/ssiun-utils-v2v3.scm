;;ssiun-utils.scm

;; init.scm               is "%LOCALAPPDATA%\Programs\GIMP 3\share\gimp\3.0\scripts\scriptfu-init\init.scm"
;; palette-export.scm     is "%LOCALAPPDATA%\Programs\GIMP 3\share\gimp\3.0\scripts\palette-export.scm"
;; script-fu-compat.scm   is "%LOCALAPPDATA%\Programs\GIMP 3\share\gimp\3.0\scripts\scriptfu-init\script-fu-compat.scm"

;; script-fu.init         is "%PROGRAMFILES%\GIMP 2\share\gimp\2.0\scripts\script-fu.init"
;; script-fu-compat.init  is "%PROGRAMFILES%\GIMP 2\share\gimp\2.0\scripts\script-fu-compat.init"
;; palette-export.scm     is "%PROGRAMFILES%\GIMP 2\share\gimp\2.0\scripts\palette-export.scm"
;; grid-system.scm        is "%PROGRAMFILES%\GIMP 2\share\gimp\2.0\scripts\grid-system.scm"

;; convert-decimal-to-base        : (gimp3)palette-export.scm, (gimp2)palette-export.scm
;; pre-pad-number                 : (gimp3)palette-export.scm, (gimp2)palette-export.scm
;; caddr                          : (gimp3)init.scm, (gimp2)script-fu.init
;; cadr                           : (gimp3)init.scm, (gimp2)script-fu.init
;; catch                          : (gimp3)init.scm, (gimp2)script-fu.init
;; char=?                         : (gimp3)init.scm, (gimp2)script-fu.init
;; equal?                         : (gimp3)init.scm, (gimp2)script-fu.init
;; for-each                       : (gimp3)init.scm, (gimp2)script-fu.init
;; list                           : (gimp3)init.scm, (gimp2)script-fu.init
;; list->string                   : (gimp3)init.scm, (gimp2)script-fu.init
;; list->vector                   : (gimp3)init.scm, (gimp2)script-fu.init
;; list-ref                       : (gimp3)init.scm, (gimp2)script-fu.init
;; map                            : (gimp3)init.scm, (gimp2)[grid-system.scm,script-fu.init]
;; member                         : (gimp3)init.scm, (gimp2)script-fu.init
;; number->string                 : (gimp3)init.scm, (gimp2)script-fu.init

;; random                         : (gimp3)[init.scm,script-fu-compat.scm], (gimp2)[script-fu-compat.init,script-fu.init]
;; string                         : (gimp3)[init.scm,script-fu-compat.scm], (gimp2)[script-fu-compat.init,script-fu.init]
;; string->list                   : (gimp3)init.scm, (gimp2)script-fu.init
;; string->number                 : (gimp3)init.scm, (gimp2)script-fu.init
;; string=?                       : (gimp3)[init.scm,script-fu-compat.scm], (gimp2)[script-fu-compat.init,script-fu.init]
;; vector->list                   : (gimp3)init.scm, (gimp2)script-fu.init

;; zero?                          : (gimp3)init.scm, (gimp2)script-fu.init
;; negative?                      : (gimp3)init.scm, (gimp2)script-fu.init
;; positive?                      : (gimp3)init.scm, (gimp2)script-fu.init

;; define-with-return             : (gimp3)init.scm, (gimp2)script-fu.init
;; return                         : (gimp3)init.scm, (gimp2)script-fu.init


;; +----------------------------------------------------------------------+
;; | YOU SHOULD (set! *ssiun-v3* #t) after (script-fu-use-v3)             |
;; | AND (set! *ssiun-v3* #f) after (script-fu-use-v2)                    |
;; +----------------------------------------------------------------------+
(define *ssiun-v3* #f)


(define (ssiun-string-has-char? a-string a-char)
  (cond ( (member a-char (string->list a-string)) #t) (else #f)))

(define (ssiun-TRUE? value)
  (not (eqv? FALSE value)))
(define (ssiun-FALSE? value)
  (eqv? FALSE value))

;; Python's logical False
(define (py-False? k)
  (cond
   ((number? k) (= 0 k))
   ((boolean? k) (eqv? k #f))
   ((string? k) (string=? k ""))
   ((list? k) (equal? k '()))
   ((vector? k) (equal? k #()))
   (else k)))

;; Python's logical True.
(define (py-True? k)
  (not (py-False? k)))

;; BEGIN ; ssiun-displayfunc-sep-end* 에 필요한 함수들

;; from "%LOCALAPPDATA%\Programs\GIMP 3\share\gimp\3.0\scripts\palette-export.scm"
;; For all the operations below, this is the order of respectable digits:
(define ssiun-conversion-digits (list "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"
                                "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k"
				"l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v"
				"w" "x" "y" "z"))

;; from "%LOCALAPPDATA%\Programs\GIMP 3\share\gimp\3.0\scripts\palette-export.scm"
;; Converts a decimal number to another base. The returned number is a string
(define (ssiun-convert-decimal-to-base num base)
  (if (< num base)
    (list-ref ssiun-conversion-digits num)
    (let loop ((val num)
               (order (inexact->exact (truncate (/ (log num)
                                                   (log base)))))
               (result ""))
      (let* ((power (expt base order))
             (digit (quotient val power)))
        (if (zero? order)
          (string-append result (list-ref ssiun-conversion-digits digit))
          (loop (- val (* digit power))
                (pred order)
                (string-append result (list-ref ssiun-conversion-digits digit))))))))

;; from "%LOCALAPPDATA%\Programs\GIMP 3\share\gimp\3.0\scripts\palette-export.scm"
;; Convert a string representation of a number in some base, to a decimal number
(define (ssiun-convert-base-to-decimal base num-str)
  (define (convert-char num-char)
    (if (char-numeric? num-char)
        (string->number (string num-char))
        (+ 10 (- (char->integer num-char) (char->integer #\a)))
        )
    )
  (define (calc base num-str num)
    (if (equal? num-str "")
        num
        (calc base
              (substring num-str 1)
              (+ (* num base) (convert-char (string-ref num-str 0)))
              )
        )
    )
  (calc base num-str 0)
  )

;; from "%LOCALAPPDATA%\Programs\GIMP 3\share\gimp\3.0\scripts\palette-export.scm"  
;; If a string num-str is shorter then size, pad it with pad-str in the
;; beginning until it's at least size long
(define (ssiun-pre-pad-number num-str size pad-str)
  (if (< (string-length num-str) size)
      (ssiun-pre-pad-number (string-append pad-str num-str) size pad-str)
      num-str
      )
  )

;;  See, https://stackoverflow.com/a/58659355
;; convert-decimal-to-base is defined in palette-export.scm
(define (ssiun-hex num)
  (ssiun-convert-decimal-to-base num 16))

;; pre-pad-number is defined in palette-export.scm
(define (ssiun-hex-width num width)
  (ssiun-pre-pad-number (ssiun-convert-decimal-to-base num 16) width "0"))

(define-with-return (ssiun-string-repr strg)
  (let* ((port2 (open-output-string)) (j 0) (ch 0) (order 0) (retval ""))
    (display "\"" port2)
    (do ((j 0 (+ j 1)))
        ((= j (string-length strg)))
      (set! ch (string-ref strg j))
      (set! order (char->integer ch))
      (if (< order 32)
          (begin
            (display "\\" port2)
            (cond
             ((= order 10)
              (display "n" port2))
             ((= order 13)
              (display "r" port2))
             ((= order 9)
              (display "t" port2))
             ((= order 7)
              (display "a" port2))
             (else
              (display (string-append "x" (ssiun-hex-width order 2)) port2))
             ); cond
            )
          (display ch port2)))
    (display "\"" port2)
    (set! retval (get-output-string port2))
    (close-output-port port2)
    (return retval)))

;; END ; ssiun-displayfunc-sep-end* 에 필요한 함수들 끝.


(define ssiun-SEP-END*-REPR-STRING #t)
;; TRUE이면 벡터나 리스트의 원소가 아닌 문자열을 표현형(repr)으로 바꾸어 출력. . FALSE이면 문자열 출력.
;; 예를 들어 "abc\n" 를 따옴표 포함해서 그대로 출력, FAALSE이면 abc와 줄바꿈 출력

(define ssiun-SEP-END*-REPR-STRING-IN-VECTOR #t)
;; TRUE 이면 벡터 안의 문자열을 표현형(repr)으로 바꾸어 출력. FALSE이면 문자열 출력.

(define ssiun-SEP-END*-REPR-STRING-IN-LIST #t)

;; TRUE이면 리스트 안의 문자열을 표현형(repr)으로 바꾸어 출력. FALSE이면 문자열로 출력.


;;@param repr-str : TRUE/FALSE
;;@param repr-vector-str : TRUE/FALSE
;;@param repr-list-str : TRUE/FALSE
(define (ssiun-sep-end*-use-repr repr-str repr-vector-str repr-list-str)
  (define ssiun-SEP-END*-REPR-STRING repr-str)
  (define ssiun-SEP-END*-REPR-STRING-IN-VECTOR repr-vector-str)
  (define ssiun-SEP-END*-REPR-STRING-IN-LIST repr-list-str))

(define (ssiun-sep-end*-get-use-repr)
  (list ssiun-SEP-END*-REPR-STRING
  ssiun-SEP-END*-REPR-STRING-IN-VECTOR
  ssiun-SEP-END*-REPR-STRING-IN-LIST))

(define (ssiun-outstring-sep-end* sep end . args)
  (let* ((port (open-output-string)) (outstring ""))
    (for-each
     (lambda (arg)
       (cond
        ((vector? arg)
         (display "#(" port)
         (do ((i 0 (+ i 1)))
             ((>= i (vector-length arg)))
           (cond
            ( (and
                (string? (vector-ref arg i))
                (py-True? ssiun-SEP-END*-REPR-STRING-IN-VECTOR))
              (display (ssiun-string-repr (vector-ref arg i)) port))
            (else
              (if (closure? (vector-ref arg i))
                (display "#<closure>" port)
                (display (vector-ref arg i) port))))
           (display " " port))
         (display ")" port))
        ((list? arg)
         (display "'(" port)
         (do ((i 0 (+ i 1)))
             ((>= i (vector-length (list->vector arg))))
           (cond
            ( (and (string? (list-ref arg i))
                   (py-True? ssiun-SEP-END*-REPR-STRING-IN-LIST))
              (display (ssiun-string-repr (list-ref arg i)) port))
            (else
              (if (closure? (list-ref arg i))
                (display "#<closure>" port)
                (display (list-ref arg i) port))))
           (display " " port))
         (display ")" port))
        (else
         (if (and (string? arg) (py-True? ssiun-SEP-END*-REPR-STRING))
             (display (ssiun-string-repr arg) port)
             (if (closure? arg)
              (display "#<closure>" port)
              (display arg port)))))
       (display sep port))
     args)
    (display end port)
    (set! outstring (get-output-string port))
    (close-output-port port)
    outstring))

(define (ssiun-displayfunc-sep-end* displayfunc sep end . args)
    (displayfunc (apply ssiun-outstring-sep-end* (append (list sep end) args))))

(define (ssiun-display-sep-end* sep end . args)
  (apply ssiun-displayfunc-sep-end* (append (list display sep end) args)))

(define (ssiun-display* . args)
  (apply ssiun-displayfunc-sep-end* (append (list display " " "") args)))

(define (ssiun-display-sep* sep . args)
  (apply ssiun-displayfunc-sep-end* (append (list display sep "") args)))

(define (ssiun-displayln* . args)
  (apply ssiun-displayfunc-sep-end* (append (list display " " "\n") args)))

(define (ssiun-displayln-sep* sep . args)
  (apply ssiun-displayfunc-sep-end* (append (list display sep "\n") args)))

(define (ssiun-message-sep-end* sep end . args)
  (apply ssiun-displayfunc-sep-end* (append (list gimp-message sep end) args)))

(define (ssiun-message* . args)
  (apply ssiun-displayfunc-sep-end* (append (list gimp-message " " "") args)))

(define (ssiun-message-sep* sep . args)
  (apply ssiun-displayfunc-sep-end* (append (list gimp-message sep "") args)))

(define (ssiun-messageln* . args)
  (apply ssiun-displayfunc-sep-end* (append (list gimp-message " " "\n") args)))

(define (ssiun-messageln-sep* sep . args)
  (apply ssiun-displayfunc-sep-end* (append (list gimp-message sep "\n") args)))

(define (ssiun-findchar a-string c)
  (define (char-in c ls)
    (let loop ((ls0 ls))
      (if (null? ls0)
          #f
          (or (char=? c (car ls0))
              (loop (cdr ls0))))))
  (char-in c (string->list a-string)))

(define (ssiun-findstr-from a-str sub-str from-index)
  (let* ((len (string-length sub-str))
         (max-i (- (string-length a-str) len)))
    (let loop ((i from-index))
      (cond
       ((> i max-i)
        -1)
       ((string=? sub-str (substring a-str i (+ i len)))
        i)
       (else
        (loop (+ i 1)))))))

(define (ssiun-findstr a-str sub-str)
  (ssiun-findstr-from a-str sub-str 0))

(define (ssiun-replacestr a-str sub-str repl-str)
  (let* ((newstring "") (substr-i -1) (substr-len 0) (substr-stop 0) (str-len 0)
         (before "") (after ""))
    (set! str-len (string-length a-str))
    (set! substr-len (string-length sub-str))
    (set! substr-i (ssiun-findstr a-str sub-str))
    (if (not (= -1 substr-i))
        (let* ((dummy 0))
          (set! substr-stop (+ substr-i substr-len))
          (set! before (substring a-str 0 substr-i))
          (set! after (substring a-str substr-stop str-len))
          (set! newstring (string-append before repl-str after))
          )
        (let* ((dummy 0))
          (set! newstring a-str)))
    newstring
    )
  )

(define (ssiun-gimp-message-all . args)
  (let* ((port (open-output-string)))
    (for-each
     (lambda (arg)
       (display arg port)
       (display " " port))
     args)
    (gimp-message (get-output-string port))))

(define (ssiun-list-set! list k val)
  (if (zero? k)
      (set-car! list val)
      (ssiun-list-set! (cdr list) (- k 1) val)))

;; if (fnx elem1 elem2) returns zero or negative positive number,
;; elem1 is put before elem2 in the list
;; descending : TRURE(1) or FALSE(0)
(define (ssiun-vector-sort! V fnx descending)
  (let* ((i 0) (j 0) (k 0) (temp 0))
    (while (< i (- (vector-length V) 1))
           (set! k i)
           (set! j (+ i 1))
           (while (< j (vector-length V))
                  (if (not (ssiun-TRUE? descending ))
                      (if (positive? (fnx (vector-ref V k) (vector-ref V j)))
                          (set! k j))
                      (if (negative? (fnx (vector-ref V k) (vector-ref V j)))
                          (set! k j)))
                  (set! j (+ j 1)))
           (if (not (= k i))
               (begin
                 (set! temp (vector-ref V i))
                 (vector-set! V i (vector-ref V k))
                 (vector-set! V k temp)))
           (set! i (+ i 1)))))

(define (ssiun-vector-reverse! a-vector)
  (let* ((vlen 0)
         (lastI 0)
         (mid 0)
         (i 0)
         (temp 0)
         (i2 0))
    (set! vlen (vector-length a-vector))
    (set! lastI (- vlen 1))
    (set! mid (quotient vlen 2))
    (while (< i mid)
           (set! i2 (- lastI i))
           (set! temp (vector-ref a-vector i))
           (vector-set! a-vector i (vector-ref a-vector i2))
           (vector-set! a-vector i2 temp)
           (set! i (+ i 1)))))

(define (ssiun-copy-vector old-vector)
  (let* ((new-vector (make-vector (vector-length old-vector)))
         (i 0))
    (while (< i (vector-length old-vector))
           (vector-set! new-vector i (vector-ref old-vector i))
           (set! i (+ i 1)))
    new-vector))

(define (ssiun-get-sub-vector old-vector start stop)
  (let* ((new-vector #())
         (i 0)
         (k 0))
    (if (> stop (vector-length old-vector))
        (set! stop (vector-length old-vector)))
    (if (< start 0)
        (set! start 0))
    (set! new-vector (make-vector (- stop start)))
    (set! i start)
    (while (and (< i stop) (>= i 0))
           (begin
             (vector-set! new-vector k (vector-ref old-vector i))
             (set! k (+ k 1)))
           (set! i (+ i 1)))
    new-vector))


;; fn : 벡터의 원소를 매개변수로 받고 #t, #f, 1(TRUE), 또는 0(FALSE)을 반환하는 함수
(define (ssiun-copy-vector-if old-vector fn)
  (let* ((new-vector 0)
         (new-vector-len 0)
         (i 0)
         (j 0)
         (tmp 0))
    (set! i 0)
    (while (< i (vector-length old-vector))
           (if (fn (vector-ref old-vector i))
               (begin
                 (set! new-vector-len (+ new-vector-len 1))))
           (set! i (+ i 1)))
    (set! new-vector (make-vector new-vector-len 0))
    (set! i 0)
    (set! j 0)
    (while (< i (vector-length old-vector))
           (set! tmp (fn (vector-ref old-vector i)))
           (if (or (equal? tmp 1) (equal? tmp #t))
               (begin
                 (vector-set! new-vector j (vector-ref old-vector i))
                 (set! j (+ j 1))))
           (set! i (+ i 1)))
    new-vector))

(define (ssiun-copy-vector-pylogic-if old-vector fn)
  (define (newfn l)
    (py-True? (fn l)))
  (ssiun-copy-vector-if old-vector newfn))

(define (ssiun-copy-vector-reversed old-vector)
  (let* ((new-vector 0))
    (set! new-vector (ssiun-copy-vector old-vector))
    (ssiun-vector-reverse! new-vector)
    new-vector))

(define (ssiun-zero-pad ndigits an-integer)
  (let* ((numstr (number->string an-integer))
         (numstrlen (string-length numstr))
         (padding (if (>= numstrlen ndigits) ""
                        (make-string (- ndigits numstrlen) #\0)
                        )))
    (string-append padding numstr)))

(define (ssiun-space-pad ndigits an-integer)
  (let* ((numstr "")
         (pad ""))
    (set! numstr (number->string an-integer))
    (set! pad (make-string (- ndigits (string-length numstr)) #\ ))
    (string-append pad numstr)))

;; add-eol #t or #f or TRUE or FALSE
(define (ssiun-readlines filename add-eol)
  (let* ((inport (open-input-file filename))
         (a-char #\x00)
         (a-line "")
         (a-list '())
         (mac-eol #f)
         (linux-eol #f)
         (windows-eol #f))
    (while (not (eof-object? a-char))
           (set! a-char (read-char inport))
           (begin
             (set! mac-eol #f)
             (set! linux-eol #f)
             (set! windows-eol #f)
             (if (and (not (eof-object? a-char)) (equal? a-char #\return))
                 (begin
                   (if (eof-object? (peek-char inport))
                       (begin
                         (set! mac-eol #t)
                         (set! a-char #\newline))
                       (if (equal? (peek-char inport) #\newline)
                           (begin
                             (set! windows-eol #t)
                             (read-char inport)
                             (set! a-char #\newline))
                           (begin
                             (set! mac-eol #t)
                             (set! a-char #\newline))))))
             (if (and (not (eof-object? a-char)) (equal? a-char #\newline))
                 (begin
                   (if (and (not windows-eol) (not mac-eol))
                       (set! linux-eol #t))))
             (if (or (eof-object? a-char) (equal? a-char #\newline))
                 (begin
                   (set! a-list (append a-list (list a-line)))
                   (if (equal? a-char #\newline)
                       (if (and (not (equal? #f add-eol ))
                                (not (equal? 0 add-eol)))
                           (set! a-line
                                 (string-append a-line (string #\newline)))))
                   (set! a-line ""))
                 (begin
                   (set! a-line (string-append a-line (string a-char)))))))
    (close-input-port inport)
    a-list))


(define (ssiun-vector-append a-vector other-vector)
  (list->vector
   (append
    (vector->list a-vector)
    (vector->list other-vector)
    )
   )
  )

(define (ssiun-vector-append* . vectorargs)
  (let* ((the-list '()))
    (for-each
     (lambda (another-vector)
       (set! the-list
             (append
              the-list
              (vector->list another-vector)
              )
             )
       )
     vectorargs
     )
    (list->vector the-list)
    )
  )

(define (ssiun-make-circle-stroke image vector-object cx cy radi)
  (define (float cx) (* 1.0 cx))
  (let* ((k 0.0)
         (controlpoints '())
         (stroke-id 0)
         (pi 3.145192653858)
         (num-points 0))
    ;;cx cy radi=float(cx) float(cy) float(radi)
    (set! cx (float cx))
    (set! cy (float cy))
    (set! radi (float radi))
    (set! k (* (/ 4.0 3.0) (tan (/ pi (* 2.0 4))) radi))
    (set! controlpoints
          (list
           (- cx radi) (- cy k) (- cx radi) cy (- cx radi) (+ cy k)
           (- cx k) (+ cy radi) cx (+ cy radi) (+ cx k) (+ cy radi)
           (+ cx radi) (+ cy k) (+ cx radi) cy (+ cx radi) (- cy k)
           (+ cx k) (- cy radi) cx (- cy radi) (- cx k) (- cy radi)
           ))
    (set! num-points (length controlpoints))
    (cond 
      ( (py-True? (ssiun-is-gimp-ver-geq-n2 3 0))
        (set! stroke-id
              (if *ssiun-v3*
                (gimp-path-stroke-new-from-points
                    vector-object PATH-STROKE-TYPE-BEZIER (list->vector controlpoints) #t)
                (car (gimp-path-stroke-new-from-points
                    vector-object PATH-STROKE-TYPE-BEZIER (list->vector controlpoints) TRUE)) )) )
      ( (py-True? (ssiun-is-gimp-ver-leq-n2 2 99))
        (set! stroke-id
              (car (gimp-vectors-stroke-new-from-points
                    vector-object 0 num-points (list->vector controlpoints) TRUE))) ) )
    (list vector-object stroke-id)))

(define (ssiun-random-uniform minval maxval)
  (let* ((maxT (* maxval 1000))
         (minT (* minval 1000))
         (cntT (- maxT minT -1))
         (rndT (+ (random cntT) minT))
         )
  (/ rndT 1000.0)))

(define (ssiun-int n) (inexact->exact (truncate n)))

(define (ssiun-float cx) (* cx 1.0))

(define (ssiun-string-lower a-string)
  (let* ((a-list '()) (i 0))
    (set! a-list '())
    (for-each
     (lambda (ch)
       (if (char-upper-case? ch)
           (set! a-list (append a-list (list (char-downcase ch))))
           (set! a-list (append a-list (list ch)))))
     (string->list a-string))
    (list->string a-list)))

(define (ssiun-string-upper a-string)
  (let* ((a-list '()) (i 0))
    (set! a-list '())
    (for-each
     (lambda (ch)
       (if (char-upper-case? ch)
           (set! a-list (append a-list (list (char-upcase ch))))
           (set! a-list (append a-list (list ch)))))
     (string->list a-string))
    (list->string a-list)))

(define (ssiun-rstrip s stripchars)
  ;; @param stripchars : string containg list chars to strip.
  ;; example: (str-rstrip s " \n")
  (define (haschar? a-string c)
    (define (char-in c ls)
      (let loop ((ls0 ls))
        (if (null? ls0)
            #f
            (or (char=? c (car ls0))
                (loop (cdr ls0))))))
    (char-in c (string->list a-string)))
  (define (str-len s)
    (string-length s))
  (define (str[] s i)
    (substring s i (+ i 1)))
  (define (chr[] s i)
    (string-ref s i))
  (let* ((i 0) (end 0) (breakwhile #f))
    (set! end (str-len s))
    (do ((i (- (str-len s) 1) (- i 1)))
        ( (not (and (eq? breakwhile #f) (>= i 0))) #t)
      (if (haschar? stripchars (chr[] s i))
          (set! end i)
          (set! breakwhile #t)))
    (if (= (str-len s) end)
        (begin
          s)
        (begin
          (substring s 0 end)))))

(define (ssiun-string-split text delimchar)
  (let* ((token "")
         (tokens (list))
         (i 0))
    (while (< i (string-length text))
           (let* ((ch (string-ref text i)) (strch ""))
             (cond
              ((eqv? ch delimchar)
               (set! tokens (append tokens (list token)))
               (set! token ""))
              (else
               (set! strch (string ch))
               (set! token (string-append token strch))))
             (if (= (+ i 1) (string-length text))
                 (if (not (string=? token ""))
                     (set! tokens (append tokens (list token)))))
             (set! i (+ i 1))))
    tokens))


(define (ssiun-list-delete-item the-list the-item)
  (define (deleteitem list1 item)
    (cond
     ((null? list1) '())
     ((equal? (car list1) item) (deleteitem (cdr list1) item))
     (else (cons (car list1) (deleteitem (cdr list1) item)))
     ))
  (deleteitem the-list the-item))

;;https://stackoverflow.com/a/26744865
;;by Jie
(define (ssiun-list-delete-item-recursive the-list the-item)
  (define (delete2 list1 item)
    ( cond
      ((null? list1) '())
      ((pair? (car list1))
       (cons (delete2 (car list1) item) (delete2 (cdr list1) item)))
      ((equal? (car list1) item)
       (delete2 (cdr list1) item))
      (else
       (cons (car list1) (delete2 (cdr list1) item)))
      ))
  (delete2 the-list the-item))

(define (ssiun-vector-delete-item the-vector the-item)
  (list->vector (ssiun-list-delete-item (vector->list the-vector) the-item)))

(define (ssiun-vector-delete-item-recursive the-vector the-item)
  (list->vector
   (ssiun-list-delete-item-recursive (vector->list the-vector) the-item)))

(define (ssiun-get-drawable-width aDrawable)
  (catch (gimp-drawable-get-width aDrawable)
         (gimp-drawable-width aDrawable)))

(define (ssiun-get-drawable-height aDrawable)
  (catch (gimp-drawable-get-height aDrawable)
         (gimp-drawable-height aDrawable)))

(define (ssiun-get-image-width anImage)
  (catch (gimp-image-get-width anImage)
         (gimp-image-width anImage)))

(define (ssiun-get-image-height anImage)
  (catch (gimp-image-get-height anImage)
         (gimp-image-height anImage)))

(define (ssiun-get-drawable-offsets aDrawable)
  (catch (gimp-drawable-get-offsets aDrawable)
         (gimp-drawable-offsets aDrawable)))

(define (ssiun-layer-get-selected layer)
  (if (py-True? (ssiun-is-gimp-ver-lss-n3 2 99 14))
      (error "layer-get-selected: requires GIMP 2.99.14 or above."))
  (cond 
    ((py-True? (ssiun-is-gimp-ver-geq-n2 3 0))
      (let* ((image (if *ssiun-v3* 
                        (gimp-item-get-image layer)
                        (car (gimp-item-get-image layer))))
             (selected-layers (if *ssiun-v3*
                                    (gimp-image-get-selected-layers image)
                                    (car (gimp-image-get-selected-layers image))))
             (selected-layers-list  (vector->list selected-layers))
             (is-member (member layer selected-layers-list))
             (selected (not (equal? #f is-member)))
             )
        (if (equal? #t selected)
            (if *ssiun-v3*
              #t
              (list TRUE))
            (if *ssiun-v3*
              #f
              (list FALSE)))))
    ((py-True? (ssiun-is-gimp-ver-equ-n2 2 99))
      (let* ((image (car (gimp-item-get-image layer)))
             (selected-layers (cadr (gimp-image-get-selected-layers image)))
             (selected-layers-list  (vector->list selected-layers))
             (is-member (member layer selected-layers-list))
             (selected (not (equal? #f is-member)))
             )
        (if (equal? #t selected)
            (list TRUE)
            (list FALSE))))))
    

(define (ssiun-layer-set-selected layer setSelected)
  (if (= TRUE (ssiun-is-gimp-ver-lss 2 99))
      (error "layer-set-selected: requires GIMP 2.99 or above."))
  (let* 
      (
       (image (car (gimp-item-get-image layer)))
       (selected-layers 
          (cond
            ((ssiun-py-True (ssiun-is-gimp-ver-geq 3 0))
              (if *ssiun-v3*
                (gimp-image-get-selected-layers image)
                (car (gimp-image-get-selected-layers image))))
            ((ssiun-py-True (ssiun-is-gimp-ver-eq 2 99))
              (cadr (gimp-image-get-selected-layers image)))
          ))
       (selected-layers-list  (vector->list selected-layers))
       (is-member (member layer selected-layers-list))
       (selected (not (equal? #f is-member)))
       )
  (if (py-True? setSelected)
      (if (equal? #f selected)
          (begin
            (define (ssiun-vector-append a-vector other-vector)
              (list->vector
               (append
                (vector->list a-vector)
                (vector->list other-vector))))
            (set! selected-layers
                  (ssiun-vector-append selected-layers (vector layer)))
            (cond 
                ((ssiun-py-True (ssiun-is-gimp-ver-geq 3 0))
                  (gimp-image-set-selected-layers
                     image selected-layers)
                    )
                ((ssiun-py-True (ssiun-is-gimp-ver-eq 2 99))
                  (gimp-image-set-selected-layers
                   image (vector-length selected-layers) selected-layers)))
             ))
      (if (equal? #t selected)
          (begin
            (define (ssiun-list-delete-item the-list the-item)
              (define (deleteitem list1 item)
                (cond
                 ((null? list1) '())
                 ((equal? (car list1) item) (deleteitem (cdr list1) item))
                 (else (cons (car list1) (deleteitem (cdr list1) item)))
                 ))
              (deleteitem the-list the-item))
            (define (ssiun-vector-delete-item the-vector the-item)
              (list->vector
               (ssiun-list-delete-item (vector->list the-vector) the-item)))
            (set! selected-layers
                  (ssiun-vector-delete-item selected-layers layer))
            (cond
              ((ssiun-py-True (ssiun-is-gimp-ver-geq 3 0)
                (gimp-image-set-selected-layers
                 image selected-layers)))
              ((ssiun-py-True (ssiun-is-gimp-ver-eq 2 99)
                (gimp-image-set-selected-layers
                 image (vector-length selected-layers) selected-layers)))))))))


(define (ssiun-get-gimp-ver as-number)
  (let* ((verstr (if *ssiun-v3*
                      (gimp-version)
                      (car (gimp-version))))
         (versioninfo (ssiun-string-split verstr #\.))
         (majorstr (car versioninfo))
         (minorstr (cadr versioninfo))
         (buildstr (caddr versioninfo)))
    (if (py-True? as-number)
        (list (string->number majorstr) (string->number minorstr))
        (list majorstr minorstr))))

(define (ssiun-is-gimp-ver-equ major minor)
  (if (and
       (= (car (ssiun-get-gimp-ver #t)) major)
       (= (cadr (ssiun-get-gimp-ver #t)) minor))
      #t
      #f))
  

(define (ssiun-is-gimp-ver-lss major minor)
  (if (or
       (< (car (ssiun-get-gimp-ver #t)) major)
       (and
        (= (car (ssiun-get-gimp-ver #t)) major)
        (< (cadr (ssiun-get-gimp-ver #t)) minor)))
      #t
      #f))

(define (ssiun-is-gimp-ver-leq major minor)
  (if (or
       (ssiun-is-gimp-ver-equ major minor)
       (ssiun-is-gimp-ver-lss major minor))
      #t
      #f))

(define (ssiun-is-gimp-ver-geq major minor)
  (if (not (ssiun-is-gimp-ver-lss major minor))
      #t
      #f))

(define (ssiun-is-gimp-ver-gtr major minor)
  (if (not (ssiun-is-gimp-ver-leq major minor))
      #t
      #f))

(define (ssiun-get-gimp-ver-n2)
  (let* (
         (versioninfo (ssiun-string-split (if *ssiun-v3
                                                  (gimp-version)
                                                  (car (gimp-version))) #\.))
         (major (car versioninfo))
         (minor (cadr versioninfo)))
    (list (string->number major) (string->number minor))))

(define (ssiun-is-gimp-ver-equ-n2 major minor)
  (if (and
       (= (car (ssiun-get-gimp-ver-n2)) major)
       (= (cadr (ssiun-get-gimp-ver-n2)) minor))
      #t
      #f))

(define (ssiun-is-gimp-ver-lss-n2 major minor)
  (if (or
       (< (car (ssiun-get-gimp-ver-n2)) major)
       (and
        (= (car (ssiun-get-gimp-ver-n2)) major)
        (< (cadr (ssiun-get-gimp-ver-n2)) minor)))
      #t
      #f))

(define (ssiun-is-gimp-ver-leq-n2 major minor)
  (if (or
       (ssiun-is-gimp-ver-lss-n2 major minor)
       (ssiun-is-gimp-ver-equ-n2 major minor))
      #t
      #f))

(define (ssiun-is-gimp-ver-geq-n2 major minor)
  (if (not (ssiun-is-gimp-ver-lss-n2 major minor))
      #t
      #f))

(define (ssiun-is-gimp-ver-gtr-n2 major minor)
  (if (not (ssiun-is-gimp-ver-leq-n2 major minor))
      #t
      #f))


;;(define (ssiun-get-gimp-ver-n3)
;;  (let* ((versioninfo (ssiun-string-split (if *ssiun-v3*
;;                                                    (gimp-version)
;;                                                    (car (gimp-version))) #\.))
;;         (major (car versioninfo))
;;         (minor (cadr versioninfo))
;;         (build (caddr versioninfo)))
;;    (map (lambda (s) (string->number s)) (list major minor build))))

(define (ssiun-get-gimp-ver-n3)
  (let* ( (verstr (car (gimp-version)))
          (buf (make-vector 4 '())))
    (re-match "^(\\d+)\.(\\d+)\.(\\d+)" verstr buf)    
    (set! buf (vector->list buf))
    (set! buf (cdr buf))
    (map string->number
      (list (substring verstr (caar buf) (cdar buf))
          (substring verstr (caadr buf) (cdadr buf))
          (substring verstr (caaddr buf) (cdaddr buf)) ))))

(define ssiun-get-gimp-verinfo ssiun-get-gimp-ver-n3)
;;;;  alternative version of ssiun-get-gimp-ver-n3.  
;;;; does not use ssiun-string-split
;;(define (ssiun-get-gimp-verinfo)
;;  (let* ((dotpos2 '())
;;         (verstr (if *ssiun-v3*
;;                      (gimp-version)
;;                      (car (gimp-version)))))
;;    (set! dotpos2
;;          (do ((verls (string->list (if *ssiun-v3*
;;                                            (gimp-version)
;;                                            (car (gimp-version)))) (cdr verls))
;;               (i 0 (+ i 1))
;;               (chpos2 '()))
;;              ((null? verls) chpos2)
;;            (if (char=? (car verls) #\.)
;;                (set! chpos2 (append chpos2 (list i))))))
;;    (map string->number
;;         (list (substring verstr 0 (car dotpos2))
;;               (substring verstr (+ (car dotpos2) 1) (cadr dotpos2))
;;               (substring verstr (+ (cadr dotpos2) 1))
;;               ))))

(define (ssiun-is-gimp-ver-equ-n3 major minor build)
  (if (and
       (= (car (ssiun-get-gimp-ver-n3)) major)
       (= (cadr (ssiun-get-gimp-ver-n3)) minor)
       (= (caddr (ssiun-get-gimp-ver-n3 #t)) build))
      #t
      #f))

(define (ssiun-is-gimp-ver-lss-n3 major minor build)
  (if (or
       (< (car (ssiun-get-gimp-ver-n3)) major)
       (and
        (= (car (ssiun-get-gimp-ver-n3)) major)
        (< (cadr (ssiun-get-gimp-ver-n3)) minor))
       (and
        (= (car (ssiun-get-gimp-ver-n3)) major)
        (= (cadr (ssiun-get-gimp-ver-n3)) minor)
        (< (caddr (ssiun-get-gimp-ver-n3)) build)))
      #t
      #f))

(define (ssiun-is-gimp-ver-leq-n3 major minor build)
  (if (or
       (ssiun-is-gimp-ver-lss-n3 major minor build)
       (ssiun-is-gimp-ver-equ-n3 major minor build))
      #t
      #f))

(define (ssiun-is-gimp-ver-geq-n3 major minor build)
  (if (not (ssiun-is-gimp-ver-lss-n3 major minor build))
      #t
      #f))

(define (ssiun-is-gimp-ver-gtr-n3 major minor build)
  (if (not (ssiun-is-gimp-ver-leq-n3 major minor build))
      #t
      #f))

(define (ssiun-pairstring-pairsep-end* pairsep end . args)
  (let* ((pair-port (open-output-string))
          (result-string "")
          (i 0) (old-repr-string ssiun-SEP-END*-REPR-STRING)
          (arglen (length args)))
    (for-each
     (lambda (arg)
       (if (= (modulo i 2) 0)
           (begin
             (set! ssiun-SEP-END*-REPR-STRING #f)
             (display (apply ssiun-outstring-sep-end* (list "" "" arg)) pair-port))
           (begin
             (set! ssiun-SEP-END*-REPR-STRING #t)
             (display (apply ssiun-outstring-sep-end* (list "" "" arg)) pair-port)
             (set! ssiun-SEP-END*-REPR-STRING #f)
             (if (= arglen (+ i 1))
                 (display (apply ssiun-outstring-sep-end* (list "" end "")) pair-port)
                 (display (apply ssiun-outstring-sep-end* (list pairsep "" "")) pair-port))))
       (set! i (+ i 1)))
     args)
    (set! ssiun-SEP-END*-REPR-STRING old-repr-string)
    (set! result-string (get-output-string pair-port))
    (close-output-port pair-port)
    result-string))

(define (ssiun-displayfunc-pair-pairsep-end* displayfunc pairsep end . args)
  (displayfunc (apply ssiun-pairstring-pairsep-end* (append (list pairsep end)  args))))

(define (ssiun-display-pair-pairsep-end* pairsep end . args)
  (apply
   ssiun-displayfunc-pair-pairsep-end*
   (append (list display pairsep end) args)))

(define (ssiun-display-pair* pairsep . args)
  (apply ssiun-display-pair-pairsep-end* (append (list pairsep "") args)))

(define (ssiun-displayln-pair* pairsep . args)
  (apply ssiun-display-pair-pairsep-end* (append (list pairsep "\n") args)))

(define (ssiun-message-pair-pairsep-end* pairsep end . args)
  (apply
   ssiun-displayfunc-pair-pairsep-end*
   (append (list gimp-message pairsep end) args)))

(define (ssiun-messageln-pair* pairsep . args)
  (apply ssiun-message-pair-pairsep-end* (append (list pairsep "\n") args)))

(define (ssiun-message-pair* pairsep . args)
  (apply ssiun-message-pair-pairsep-end* (append (list pairsep "") args)))

;;> (ssiiun-displayln-vars* "Hello" "Hello"  "3" 3)
;;Hello="Hello", 3=3
(define (ssiun-displayln-vars* . args)
  (let* ((newls (list)))
    (do ((i 0 (+ i 1)) (arg 0))
        ((= i (length args)) newls)
      (set! arg (list-ref args i))
      (if (= 0 (remainder i 2))
          (set! arg (string-append arg "=")))
      (set! newls (append newls (list arg))))
    (apply ssiun-displayln-pair* (append (list ", ") newls))))

(define (ssiun-messageln-vars* . args)
  (let* ((newls (list)))
    (do ((i 0 (+ i 1)) (arg 0))
        ((= i (length args)) newls)
      (set! arg (list-ref args i))
      (if (= 0 (remainder i 2))
          (set! arg (string-append arg "=")))
      (set! newls (append newls (list arg))))
    (apply ssiun-messageln-pair* (append (list ", ") newls))))

;; https://en.wikipedia.org/wiki/HSL_and_HSV#HSL_to_RGB_alternative
;; in:  h: [0, 360], s: [0,100], l:[0,100]
;; out: r:[0,255], g:[0,255], b:[0,255]
(define (ssiun-hsl-to-rgb h s l)
  (define (fmodulo n d)
    (- n (* (truncate (/ n d)) d)))

  (let* ((H h) (S_L (/ s 100)) (L (/ l 100)))
    (define (f n)
      (let* (
             (k (fmodulo (+ n (/ H 30)) 12) )
             (a (* S_L (min L (- 1 L)))))
        (- L (* a (max -1 (min (- k 3) (- 9 k) 1))))))
    (map (lambda (n) (inexact->exact (round (* n 255))))
         (list (f 0) (f 8) (f 4)))))

;; https://en.wikipedia.org/wiki/HSL_and_HSV#HSL_to_RGB_alternative
;; in:  h: [0, 360], s: [0,100], l:[0,100]
;; out: r:[0,255], g:[0,255], b:[0,255]
(define (ssiun-hsv-to-rgb h s v)
  (define vars ssiun-displayln-pair*)
  
  ;; fmod is in script-fu-compat.init, but marked as deprecated.
  (define (fmodulo n d)
    (- n (* (truncate (/ n d)) d)))

  (let* ((H h) (S_V (/ s 100)) (V (/ v 100))
         )
    (vars "H" H "S_V" S_V "V" V)
    (define (f n)
      (let* (
             (k (fmodulo (+ n (/ H 60)) 6))
             )
        (vars "k" k)
        (- V (* V S_V (max 0 (min k (- 4 k) 1 ))))))
    (map (lambda (n) (inexact->exact (round (* n 255))))
         (list (f 5) (f 3) (f 1)))))


;; https://en.wikipedia.org/wiki/HSL_and_HSV#From_RGB
;; in : r, g, b : [0,255]
;; out: '('(h s v) '(h s l))
;;     h: [0,360]
;;     s, v, l : [0, 100]
(define (ssiun-rgb-to-hsv-hsl r g b)
  (define (fmodulo n d)
    (- n (* (truncate (/ n d)) d)))
  (let* (
         (R (/ r 255)) (G (/ g 255)) (B (/ b 255))
         (X_max (max R G B)) (V X_max)
         (X_min (min R G B))
         (C (- X_max X_min))
         (L 0.0) (H 0.0) (S_V 0.0) (S_L 0.0)
         (L (- V (/ C 2)))
         (h 0.0) (s_v 0.0) (s_l 0.0) (v 0.0) (l 0.0))
    (set!
     H
     (cond
      ((= C 0.0)
       0)
      ((= V R)
       (* 60 (fmodulo (/ (- G B) C) 6)))
      ((= V G)
       (* 60 (+ (/ (- B R) C) 2)))
      ((= V B)
       (* 60 (+ (/ (- R G) C) 4)))
      ))
    (set!
     S_V
     (cond
      ((= V 0)
       0)
      (else
       (/ C V))))
    (set!
     S_L
     (cond
      ((or (= L 0) (= L 1))
       0)
      (else
       (/ (- V L) (min L (- 1 L))))))
    (set! h H)
    (set! s_v (* S_V 100))
    (set! s_l (* S_L 100))
    (set! v (* V 100))
    (set! l (* L 100))
    (list (list h s_v v) (list h s_l l))))

(define (ssiun-round-n n rounding)
  (set! n (round (* n (expt 10 rounding))))
  (set! n (/ n (expt 10 rounding)))
  n)

;; in : r, g, b : [0,255]
;; in rounding:
;;    when given, leaves 'rounding' digits after decimal point
;; out: '(h s v)
;;     h: [0.0, 360.0]
;;     s, v : [0.0, 100.0]
(define (ssiun-rgb-to-hsv r g b . rounding)
  (define round-n ssiun-round-n)
  (if (pair? rounding)
      (map (lambda (n) (round-n n (car rounding)))
           (car (ssiun-rgb-to-hsv-hsl r g b)))
      (car (ssiun-rgb-to-hsv-hsl r g b))))

;; in : r, g, b : [0,255]
;; in rounding:
;;    when given, leaves 'rounding' digits after decimal point
;; out: '(h s v)
;;     h: [0.0, 360.0]
;;     s, v : [0.0, 100.0]
(define (ssiun-rgb-to-hsl r g b . rounding)
  (define round-n ssiun-round-n)
  (if (pair? rounding)
      (map (lambda (n) (round-n n (car rounding)))
           (cadr (ssiun-rgb-to-hsv-hsl r g b)))
      (cadr (ssiun-rgb-to-hsv-hsl r g b))))

(define (ssiun-errmsg-sep-end* sep end . args)
    (let* ( (returnvalue '()) (oldh (if *ssiun-v3* (gimp-message-get-handler )
                                        (car(gimp-message-get-handler )))) )
        (gimp-message-set-handler ERROR-CONSOLE)
        (set! returnvalue (apply ssiun-message-sep-end* (append (list sep end) args)))
        (gimp-message-set-handler oldh)
        returnvalue))

(define (ssiun-errmsg* . args)
    (let* ( (returnvalue '()) (oldh (if *ssiun-v3* (gimp-message-get-handler )
                                    (car(gimp-message-get-handler )))) )
        (gimp-message-set-handler ERROR-CONSOLE)
        (set! returnvalue (apply ssiun-message* args))
        (gimp-message-set-handler oldh)
        returnvalue))


(define (ssiun-errmsg-sep* sep . args)
    (let* ( (returnvalue '()) (oldh (if *ssiun-v3* (gimp-message-get-handler )
                                        (car(gimp-message-get-handler )))) )
        (gimp-message-set-handler ERROR-CONSOLE)
        (set! returnvalue (apply ssiun-message-sep* (append (list sep) args)))
        (gimp-message-set-handler oldh)
        returnvalue))

(define (ssiun-errmsgln* . args)
    (let* ( (returnvalue '()) (oldh (if *ssiun-v3* (gimp-message-get-handler )
                                        (car(gimp-message-get-handler )))) )
        (gimp-message-set-handler ERROR-CONSOLE)
        (set! returnvalue (apply ssiun-messageln* args))
        (gimp-message-set-handler oldh)
        returnvalue))

(define (ssiun-errmsgln-sep* sep . args)
    (let* ( (returnvalue '()) (oldh (if *ssiun-v3* (gimp-message-get-handler )
                                        (car(gimp-message-get-handler )))) )
        (gimp-message-set-handler ERROR-CONSOLE)
        (set! returnvalue (apply ssiun-messageln-sep* (append (list sep) args)))
        (gimp-message-set-handler oldh)
        returnvalue))

(define (ssiun-errmsgln-vars* . args)
    (let* ( (returnvalue '()) (oldh (if *ssiun-v3* (gimp-message-get-handler )
                                        (car(gimp-message-get-handler )))) )
        (gimp-message-set-handler ERROR-CONSOLE)
        (set! returnvalue (apply ssiun-messageln-vars* args))
        (gimp-message-set-handler oldh)
        returnvalue))

(define (ssiun-string-repeat s n)
  (let loop ((result "") (count n))
    (if (zero? count)
        result
        (loop (string-append result s) (- count 1)))))
