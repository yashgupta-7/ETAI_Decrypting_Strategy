#lang racket/base
;;;;LC
(define-syntax lc
  (syntax-rules (: <- @)
    [(lc expr : var <- drawn-from) (map (lambda (var) expr) drawn-from)]
    [(lc expr : @ guard) (if guard (list expr) `())]
    [(lc expr : @ guard  qualifier ...) 
     (append* (lc (lc expr : qualifier ...) : @ guard))]
    [(lc expr : var <- drawn-from  qualifier ...) 
     (append* (lc (lc expr :  qualifier ... ) : var <- drawn-from))]))

;; You can require more modules of your choice.
(require racket/list
         racket/string
         (prefix-in utils: "utils.rkt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                 ;;
;; Ciphertext Statistics                                                           ;;
;; =====================                                                           ;;
;;                                                                                 ;;
;; The module should provide a bunch of functions that do statistical analysis of  ;;
;; ciphertext. The output is almost always just an ordered list (counts are        ;;
;; omitted).                                                                       ;;
;;                                                                                 ;;
;; Fill in the body for the skeletons and do not change the arguments. You can     ;;
;; define as many functions as you require, there's no special credit for          ;;
;; implementing/using all of them.                                                 ;;
;;                                                                                 ;;
;; CAUTION:                                                                        ;;
;; 1. Be mindful that bi, tri and quadgrams do not cross word boundaries. Hence,   ;;
;; you must process each word separately.                                          ;;
;;                                                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Analyses
(provide cipher-monograms
         cipher-bigrams
         cipher-unique-neighbourhood
         cipher-neighbourhood
         cipher-trigrams
         cipher-quadgrams
         cipher-common-words-single
         cipher-common-words-double
         cipher-common-words-triple
         cipher-common-words-quadruple
         cipher-common-initial-letters
         cipher-common-final-letters
         cipher-common-double-letters
         ;; any other functions of your design come below:
         sorting-function
         cipher-bigrams-2
         is-lowercase-letter?
         is-uppercase-letter?
         ;; my-fundoo-analysis
         )


;;;Checks Nature of Alphabet
(define (is-lowercase-letter? char)
  (define char-num (utils:cipher-char-offset char))
  (and (>= char-num 0) (<= char-num 25)))

(define (is-uppercase-letter? char)
  (define char-num (utils:plain-char-offset char))
  (and (>= char-num 0) (<= char-num 25)))


;; Takes ciphertext and produces a list of cipher chars sorted in decreasing
;; order of frequency.
(define alphabet-lowercase
         (utils:encryption-key "abcdef"))

(define (sorting-function pair1 pair2)
         (> (cdr pair1) (cdr pair2)))

(define (cipher-monograms ciphertext)          ;;;;;;;
         (map car (sort (word->char-count ciphertext initial-char-count) sorting-function)))

(define initial-char-count
        (map (lambda (x) (cons x 0)) alphabet-lowercase))

(define (word->char-count s l)
        (if (equal? s "") l
           (let* ((f-char (string-ref s 0))
                  (f-char-num (utils:cipher-char-offset f-char)))
                         (if (and (>= f-char-num 0) (<= f-char-num 25))
                               (word->char-count (substring s 1) (list-update l f-char-num (lambda (x) (cons (car x) (+ 1 (cdr x))))))
                               (word->char-count (substring s 1) l)))))
        
       
;; Takes the cipher-word-list and produces a list of 2-letter bigram (strings)
;; sorted in decreasing order of frequency. Each element must be a string!

(define (cipher-bigrams cipher-word-list)      ;;;;;;;;;;
        (filter-map (lambda (x) (and (positive? (cdr x)) (car x))) (sort (bigram-count cipher-word-list in-big-freq-list) sorting-function)))

(define (cipher-bigrams-2 cipher-word-list)
        (filter-map (lambda (x) (and (positive? (cdr x)) x)) (sort (bigram-count cipher-word-list in-big-freq-list) sorting-function)))

(define in-big-freq-list
       (let  ((bigram-list (append* (map (lambda (x) (map (lambda (y) (list->string (list x y))) alphabet-lowercase)) alphabet-lowercase))))
          (map (lambda (x) (cons x 0)) bigram-list)))

(define (word->bigram-count s l)
       (if (< (string-length s) 2) l
          (let* ((f-letter (string-ref s 0))
                 (s-letter (string-ref s 1))
                 (bigram-pos (+ (* 26 (utils:cipher-char-offset f-letter)) (utils:cipher-char-offset s-letter))))
                                (if (and (is-lowercase-letter? f-letter) (is-lowercase-letter? s-letter))
                                           (word->bigram-count (substring s 1) (list-update l bigram-pos (lambda (x) (cons (car x) (+ 1 (cdr x))))))
                                           (word->bigram-count (substring s 1) l)))))


(define (bigram-count l-words curr-list)
      (if (null? l-words) curr-list
                          (bigram-count (cdr l-words) (word->bigram-count (car l-words) curr-list))))
   
;; Takes the bigram frequency order (output of `cipher-bigrams`) and computes
;; the neighbourhood of each letter with every other letter. Only unique
;; neighbours are to be counted.
;; Consider the character #\o.
;;
;; Takes an argument `mode`:
;; 1. (cipher-unique-neighbourhood cipher-bigrams 'predecessor)
;;    - Count only those unique occurrences where the (given) char preceeds
;;      other char.
;;    - Patterns of the form: "o?"
;; 2. (cipher-unique-neighbourhood cipher-bigrams 'successor)
;;    - Count only those unique occurrences where the (given) char succeeds
;;      other char.
;;    - Patterns of the form: "?o"
;; 3. (cipher-unique-neighbourhood cipher-bigrams 'both)
;;    - Count all unique occurrences where the (given) char neighbours the other
;;      char.
;;    - Patterns of the form "?o" and "o?". Note that this is not sum of (1) and
;;    (2) always.
;;
;; The output is a list of pairs of cipher char and the count of it's
;; neighbours. The list must be in decreasing order of the neighbourhood count.
(define (cipher-unique-neighbourhood cipher-bigrams-list mode)
  
  ;; You must match against or test (using cond) for the `mode` argument. Possibilities are:
  ;; 'predecessor, 'successor, 'both
  ;; Figure out experimentally which of these is a good indicator for E vs T.
  (cond
         [(equal? mode 'predecessor) (sort (map (lambda (x) (cons x (count (lambda (y) (equal? (string-ref y 0) x)) cipher-bigrams-list))) alphabet-lowercase) sorting-function)]
         [(equal? mode 'successor) (sort (map (lambda (x) (cons x (count (lambda (y) (equal? (string-ref y 1) x)) cipher-bigrams-list))) alphabet-lowercase) sorting-function)]
         [(equal? mode 'both) (sort (map (lambda (x) (cons x (count (lambda (y) (or (equal? (string-ref y 0) x) (equal? (string-ref y 1) x))) cipher-bigrams-list))) alphabet-lowercase) sorting-function)]
  ))

;; Takes the bigram frequency order (output of `cipher-bigrams`) and computes
;; the neighbourhood of each letter with every other letter, but counts each
;; occurrence distinctly. This comment contains 6 bigrams with "w", all with "i" or "h".
;; So "w" has:
;; when mode is 'both,        6 neighbours
;; when mode is 'predecessor, 6 neighbours
;; when mode is 'successor,   0 neighbours
(define (cipher-neighbourhood cipher-bigrams-list mode)
  ;; You must match against or test (using cond) for the `mode` argument. Possibilities are:
  ;; 'predecessor, 'successor, 'both
  ;; Figure out experimentally which of these is a good indicator for E vs T.
  (cond
         [(equal? mode 'predecessor) (sort (map (lambda (x) (cons x (apply + (filter-map (lambda (y) (and (equal? (string-ref (car y) 0) x) (cdr y))) cipher-bigrams-list)))) alphabet-lowercase) sorting-function)]
         [(equal? mode 'successor) (sort (map (lambda (x) (cons x (apply + (filter-map (lambda (y) (and (equal? (string-ref (car y) 1) x) (cdr y))) cipher-bigrams-list)))) alphabet-lowercase) sorting-function)]
         [(equal? mode 'both) (sort (map (lambda (x) (cons x (apply + (filter-map (lambda (y) (and (or (equal? (string-ref (car y) 0) x) (equal? (string-ref (car y) 1) x)) (cdr y))) cipher-bigrams-list)))) alphabet-lowercase) sorting-function)]
  ))

;; Takes the cipher-word-list and produces a list of 3-letter bigram (strings)
;; sorted in decreasing order of frequency. Each element must be a string!
(define (cipher-trigrams cipher-word-list)
  (filter-map (lambda (x) (and (positive? (cdr x)) (car x))) (sort (trigram-count cipher-word-list in-trig-freq-list) sorting-function)))

(define in-trig-freq-list
       (let  ((trigram-list (lc (list->string (list x y z)) : x <- alphabet-lowercase y <- alphabet-lowercase z <- alphabet-lowercase)))
          (map (lambda (x) (cons x 0)) trigram-list)))

(define (word->trigram-count s l)
       (if (< (string-length s) 3) l
          (let* ((f-letter (string-ref s 0))
                 (s-letter (string-ref s 1))
                 (t-letter (string-ref s 2))
                 (trigram-pos (+ (* 26 (+ (* 26 (utils:cipher-char-offset f-letter)) (utils:cipher-char-offset s-letter))) (utils:cipher-char-offset t-letter))))
                                (if (and (is-lowercase-letter? f-letter) (is-lowercase-letter? s-letter) (is-lowercase-letter? t-letter))
                                           (word->trigram-count (substring s 1) (list-update l trigram-pos (lambda (x) (cons (car x) (+ 1 (cdr x))))))
                                           (word->trigram-count (substring s 1) l)))))


(define (trigram-count l-words curr-list)
      (if (null? l-words) curr-list
                          (trigram-count (cdr l-words) (word->trigram-count (car l-words) curr-list))))

;; Takes the cipher-word-list and produces a list of 4-letter bigram (strings)
;; sorted in decreasing order of frequency. Each element must be a string!
(define (cipher-quadgrams cipher-word-list)
  '())

;; Takes the cipher word list and produces a list of single letter words, sorted
;; in decreasing order of frequency. Each element must be a string!
(define (cipher-common-words-single cipher-word-list)
  (map car (single-word-count cipher-word-list initial-char-count)))

(define (single-word-count l-words curr-list)
   (if (null? l-words) (sort (filter (lambda (x) (positive? (cdr x))) curr-list) sorting-function)
                       (let ((f-word (car l-words))
                             (rest (cdr l-words)))
                              (if (= 1 (string-length f-word)) (single-word-count (cdr l-words) (list-update curr-list (utils:cipher-char-offset (car (string->list f-word))) (lambda (x) (cons (car x) (+ 1 (cdr x))))))
                                                               (single-word-count (cdr l-words) curr-list)))))


;; Takes the cipher word list and produces a list of double letter words, sorted
;; in decreasing order of frequency.
(define (cipher-common-words-double cipher-word-list)
  '())

;; Takes the cipher word list and produces a list of triple letter words, sorted
;; in decreasing order of frequency.
(define (cipher-common-words-triple cipher-word-list)
  '())

;; Takes the cipher word list and produces a list of four letter words, sorted
;; in decreasing order of frequency.
(define (cipher-common-words-quadruple cipher-word-list)
  '())

;; Takes the cipher word list and produces a list of chars that appear at the
;; start of words, sorted in decreasing order of frequency. Each element must be
;; a char!
(define (cipher-common-initial-letters cipher-word-list)
  '())

;; Takes the cipher word list and produces a list of chars that appear at the
;; end of words, sorted in decreasing order of frequency. Each element must be
;; a char!
(define (cipher-common-final-letters cipher-word-list)
  '())

;; Takes the cipher word list and produces a list of chars that appear as
;; consecutive double letters in some word, sorted in decreasing order of
;; frequency. Each element must thus be a char!
(define (cipher-common-double-letters cipher-word-list)
  '())
