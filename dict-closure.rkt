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
         racket/string
         (prefix-in utils: "utils.rkt")
         (prefix-in stats: "statistics.rkt")
         (prefix-in strat: "strategies.rkt")
         (prefix-in SWE:"secret-word-enumeration.rkt"))

(provide dictionary-closure)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Dictionary Closure                                                               ;;
;; ==================                                                               ;;
;;                                                                                  ;;
;; A choice of substitution can really trigger more substitutions by looking at the ;;
;; partially decrypted text - surely there will be some words which can be uniquely ;;
;; determined using the dictionary. It is prudent to concretize this "choice" as    ;;
;; this purely deterministic (involving absolutely no guess-work). In more          ;;
;; technical terms, this is called "maintaining arc-consistency" (look it up on     ;;
;; Wikipedia).                                                                      ;;
;;                                                                                  ;;
;; This function must utilise the dictionary and the cipher-word-list. Decrypt each ;;
;; word (`utils:decrypt`) and check if the dictionary has:                          ;;
;;                                                                                  ;;
;; 1. a unique completetion!                                                        ;;
;;    - Extend your key using the information with this match. Continue exploring   ;;
;;      the words under the extended key.                                           ;;
;; 2. many completions.                                                             ;;
;;    - Do nothing, just continue exploring the words under the same key. If none   ;;
;;      of the words fall into (1), return with the key that you have built so far. ;;
;; 3. no completions!                                                               ;;
;;    - Return `#f` (false), indicating that this partial key is wrong, and we must ;;
;;      revert to the original key.                                                 ;;
;;                                                                                  ;;
;; Returns either of:                                                               ;;
;; a. a (possibly) extended-key.                                                    ;;
;; b. `#f` if this key led to case (3) for some word.                               ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (dictionary-closure key)
  (let* ((part-decode-ciphertext (utils:decrypt key utils:ciphertext))
         (p-decode-cipher-word-list (utils:cipher-word-list-f part-decode-ciphertext)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;(begin (displayln p-decode-cipher-word-list) (displayln new-key)
(define (word-iterator pd-word-list current-key original-key)
  
    (if (null? pd-word-list)
         ;(let ((swe (SWE:secret-word-enumeration current-key))) (if swe swe (original-key)))
         current-key
         (match (word-completion (car pd-word-list) utils:dictionary)
          [(cons a '()) (if (andmap (lambda(x) (stats:is-uppercase-letter? x)) (string->list (utils:decrypt current-key (car pd-word-list)))) (word-iterator (cdr pd-word-list) current-key original-key) (start-again (car pd-word-list) a current-key))];;unique
          [(cons a b) (word-iterator (cdr pd-word-list) current-key original-key)];;multiple
          ['() #f])));;no completion

(define (start-again cipherword plainword ckey)
  (define n-key (utils:add-substitution (remove-duplicates (filter (lambda (x) (not (equal? (car x) (cdr x)))) (map cons (string->list plainword) (string->list cipherword)))) ckey))
  (word-iterator p-decode-cipher-word-list n-key key))

  (word-iterator p-decode-cipher-word-list key key)))

(define (word-completion word dict)
  (word-completion-helper word dict '()))

(define (word-completion-helper cipherword pw-list l-acc)
  (if (or (= (length l-acc) 2) (null? pw-list)) l-acc
                      (let ((pword (word-match cipherword (car pw-list)))) (if pword (word-completion-helper cipherword (cdr pw-list) (cons pword l-acc))
                                                                                    (word-completion-helper cipherword (cdr pw-list) l-acc)))))

(define (word-match cipherword plainword)
   (if (not (equal? (string-length cipherword) (string-length plainword))) #f
                                                                           (let* ((lcw (string->list cipherword))
                                                                                 (lpw (string->list plainword))
                                                                                 (lmap (map cons lpw lcw)))
                                                                             (if (findf (lambda (x) (and (not (equal? (car x) (cdr x))) (stats:is-uppercase-letter? (cdr x)))) lmap) #f
                                                                                 (let ((lmap2 (remove-duplicates lmap)))
                                                                                  (if (locally-consistent lmap2) plainword #f))))))
                                                                             
  
  
(define (locally-consistent substitution)
  (not (or
        (check-duplicates (for/list ([subst-pair substitution])
                            (car subst-pair)))
        (check-duplicates (for/list ([subst-pair substitution])
                            (cdr subst-pair))))))  

;;;
(define empty-key (build-list 26 (lambda (_) #\_)))
