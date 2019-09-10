#lang racket
;;;;LC
(define-syntax lc
  (syntax-rules (: <- @)
    [(lc expr : var <- drawn-from) (map (lambda (var) expr) drawn-from)]
    [(lc expr : @ guard) (if guard (list expr) `())]
    [(lc expr : @ guard  qualifier ...) 
     (append* (lc (lc expr : qualifier ...) : @ guard))]
    [(lc expr : var <- drawn-from  qualifier ...) 
     (append* (lc (lc expr :  qualifier ... ) : var <- drawn-from))]))
;;;;;;;;;;;;;

;; You can require more modules of your choice.
(require racket/list
         (prefix-in utils: "utils.rkt")
         (prefix-in stats: "statistics.rkt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                     ;;
;; Strategies                                                                          ;;
;; ==========                                                                          ;;
;; For the purpose of this assignment, just the `etai` strategy is expected, since     ;;
;; we have dictionary-closure and secret-word-enumeration to leap-frog to the right    ;;
;; key. This technique would fail for harder keys which are arbitrary permutations of  ;;
;; the alphabet. We will be forced to explore many more strategies (along with         ;;
;; dictionary-closure of course).                                                      ;;
;;                                                                                     ;;
;; Strategies to guess substitutions for the key using statistical information about   ;;
;; - the English language from utils.rkt                                               ;;
;; - the cipher text      from statistics.rkt                                          ;;
;;                                                                                     ;;
;; Follow the function signature as indicated below. Deviations will make it           ;;
;; impossible for automatic grading of your submission.                                ;;
;; Moreover, we do not expect your strategies to require any more/different            ;;
;; arguments. Note that you recieve the key as argument, so you can at the very        ;;
;; least ensure that all the substitutions are monoalphabetic wrt this key.            ;;
;;                                                                                     ;;
;; Signature:                                                                          ;;
;; ```                                                                                 ;;
;; (define (my-fundoo-strategy key)                                                    ;;
;;   ;; Make use of `utils:ciphertext`, `utils:cipher-word-list`                       ;;
;;   ...)                                                                              ;;
;; ```                                                                                 ;;
;;                                                                                     ;;
;; Substitutions                                                                       ;;
;; -------------                                                                       ;;
;; In order to extend the key incrementally, we use `utils:add-substitution` to        ;;
;; extend a given key with a substitution.                                             ;;
;;                                                                                     ;;
;; A substitution is a list of pairs, each pair mapping a plaintext char to a          ;;
;; ciphertext char. For example, to extend the key with T -> a and O -> r              ;;
;; (simultaneously), we use the substitution:                                          ;;
;; ```                                                                                 ;;
;; (list (cons #\T #\a) (cons #\O #\r))                                                ;;
;; ```                                                                                 ;;
;; For a single substitution use a singleton list (containing just one pair).          ;;
;;                                                                                     ;;
;; **CAUTION**                                                                         ;;
;; -----------                                                                         ;;
;; 1. Note that add-substitution does not do sanity checks on the substitution and use ;;
;;    of `utils:is-monoalphabetic` is recommended to ensure that you don't             ;;
;;    inadvertently create invalid keys.                                               ;;
;; 2. You must provide a list called `compositions` in this module.                    ;;
;;                                                                                     ;;
;; See docs in "utils.rkt" for more information.                                       ;;
;;                                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; You must add "public" functions of this module to this list.
(provide etai
         trigrams
         ;; Some more suggested strategies:
         
         ;; common-words-double
         ;; bigrams
         ;; common-initial-letters
         ;; common-final-letters
         ;; common-words-triple
         ;; trigrams
         ;; common-double-letters
         ;; common-words-quadruple
         ;; quadgrams
         
         ;; lists of strategies
         composition)

;; A strategy that uses some statistical information to generate potential
;; substitutions for E, T, A and I.
;; Refer the assignment manual for tips on developing this strategy. You can
;; interact with our etai with the executable we provide.
(define (etai key)
    (define substitutions (lc (append x y) : x <- e-t y <- a-i ))

    (for/list ([subs substitutions] #:when (utils:is-monoalphabetic? subs key))
       subs))

;;;;;;;;;;TOP 5

(define (top5-cipher ciphertext)
  (take (stats:cipher-monograms ciphertext) 5)) 

;;;;;;;;;;;A I

(define top5-cipher-after-ai
  (remove* (stats:cipher-common-words-single utils:cipher-word-list) (top5-cipher utils:ciphertext)))

(define a-i
  (match (stats:cipher-common-words-single utils:cipher-word-list)
   ['() (lc (list (cons #\A a) (cons #\I i)) : a <- top5-cipher-after-ai i <- top5-cipher-after-ai @(not (equal? a i)))]
   [(cons c '()) (append (lc (list (cons #\A c) (cons #\I i)) : i <- top5-cipher-after-ai) (lc (list (cons #\A a) (cons #\I c)) : a <- top5-cipher-after-ai))]
   [(list a b) (list (list (cons #\A a) (cons #\I b)) (list (cons #\A b) (cons #\I a)))]))


;;;;;;;;;;;;;;;;; T

(define unique-neighbour-list (stats:cipher-unique-neighbourhood (stats:cipher-bigrams utils:cipher-word-list) 'both))

(define (unfriendly-order-unique cipher-word-list l)
  (sort (map (lambda (c) (findf (lambda (y) (equal? (car y) c)) unique-neighbour-list)) l) stats:sorting-function))

(define unique-neighbour-list-5 (reverse (unfriendly-order-unique utils:cipher-word-list top5-cipher-after-ai)) )

;;;; E

(define neighbour-list (stats:cipher-neighbourhood (stats:cipher-bigrams-2 utils:cipher-word-list) 'both))

(define (unfriendly-order cipher-word-list l)
  (sort (map (lambda (c) (findf (lambda (y) (equal? (car y) c)) neighbour-list)) l) stats:sorting-function))

(define neighbour-list-5 (unfriendly-order utils:cipher-word-list top5-cipher-after-ai) )

;;;;;;;;;;; E-T

(define e-t
  (lc (list (cons #\E e) (cons #\T t)) :  e <- (map car neighbour-list-5) t <- (map car unique-neighbour-list-5)  @(not (equal? e t)))) 
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (trigrams key)
  (let ((subs (lc (append x y) : x <- t-h-e y <- n ))) (if (utils:is-monoalphabetic? (car subs) key) subs
                                                                         (if (utils:is-monoalphabetic? (car t-h-e) key) t-h-e '()))))
  ;(define substitutions (lc (append x y) : x <- t-h-e y <- n ))

   ; (for/list ([subs substitutions] #:when (utils:is-monoalphabetic? subs key))
    ;   subs))

(define top-2-trigrams
  (take (stats:cipher-trigrams utils:cipher-word-list) 2))

(define t-h-e
  (list (map cons (list #\T #\H #\E) (string->list (car top-2-trigrams)))))

(define n
  (list (list (cons #\N (cadr (string->list (list-ref top-2-trigrams 1)))))))
  


;; A suggested composition of strategies that might work well. Has not been
;; exhaustively tested by us. Be original ;)
(define composition (list trigrams etai))
                  ;; common-words-double
                  ;; bigrams
                  ;; common-initial-letters
                  ;; common-final-letters
                  ;; common-words-triple
                  ;; trigrams
                  ;; common-double-letters))
                  ;; common-words-quadruple
                  ;; quadgrams))

