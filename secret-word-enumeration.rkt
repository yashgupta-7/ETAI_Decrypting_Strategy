#lang racket

;; You can require more modules of your choice.
(require racket/string
         racket/list
         (prefix-in utils: "utils.rkt"))

(provide secret-word-enumeration)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                           ;;
;; Secret Word Enumeration                                                                   ;;
;; =======================                                                                   ;;
;;                                                                                           ;;
;; This step exploits the fact that all keys begin with a secret word that is a              ;;
;; proper SIX-LETTER word from the English dictionary.                                       ;;
;;                                                                                           ;;
;; Given a partial key, we can query the dictionary for a list of 6 letter words             ;;
;; that can potentially begin the key.                                                       ;;
;; We can then filter out potential candidates whose keys do not agree with our partial key. ;;
;;                                                                                           ;;
;; It is possible that given a (wrong) partial key, we discover that none of these           ;;
;; potential candidates can agree with our partial key. This really implies a                ;;
;; mistake in the choice of substitution, since dictionary-closure is completely             ;;
;; deterministic (modulo any bugs in your implementation :)                                  ;;
;;                                                                                           ;;
;; Hence, this function must return either of:                                               ;;
;; a. `#f` if there are no consistent candidates for this key.                               ;;
;; b. the original key if there are multiple consistent candidates.                          ;;
;; c. the complete key if there's only one consistent candidate!                             ;;
;;                                                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (secret-word-enumeration key-after-dictionary-closure) ;; Returns a key or false (#f)
   
  (let* ((sw (take key-after-dictionary-closure 6))
         (match-list   (word-match-list sw utils:dictionary)))
               ; (for/list ([word utils:dictionary] #:when (wordmatch sw (string->list (string-downcase word)))) word)))
   (match match-list
    [(cons a '()) (utils:encryption-key a)]
    ['() #f]
    [(cons a b) key-after-dictionary-closure])))

(define (wordmatch sw word)
  (match (list sw word)
    [(list '() '()) #t]
    [(list (cons #\_ b) (cons c d)) (wordmatch (cdr sw) (cdr word))]
    [(list (cons a b) (cons a c)) (wordmatch (cdr sw) (cdr word))]
    [else #f]))
    
(define (word-match-list sw dict)
  (word-match-list-helper sw dict '()))

(define (word-match-list-helper sw dict l-acc)
  (if (or (= (length l-acc) 2) (null? dict)) l-acc
                      (let ((match? (wordmatch sw (string->list (string-downcase (car dict)))))) (if match? (word-match-list-helper sw (cdr dict) (cons (car dict) l-acc))
                                                                                    (word-match-list-helper sw (cdr dict) l-acc)))))


  
