#lang racket/base
(require racket/format
         racket/match
         racket/list
         racket/string
         scribble/core
         scribble/decode  ; decode-content
         scribble/manual
         scribble/html-properties
         "i18n.rkt"
         (for-syntax syntax/parse racket/base))

(provide aligned
         author
         def-index
         exercise
         proof
         section
         subsection
         theorem
         remark
         example
         lemma
         title
         translate
         definition
         tt
         current-language)

(define current-language (make-parameter #f)) ; #f = english
(define english-language-data (hash-ref language-data "english"))

; translate : string -> string
;   if the string s has a translation in the (current-language)
;   hash table, use that translation, otherwise return the s.
(define (translate s)
  (match (current-language)
    [#f s]
    [lang (hash-ref (hash-ref language-data
                              lang
                              (λ() english-language-data))
                    s
                    (λ() s))]))

#;(define sigplan-extras
    (let ([abs (lambda (s)
                 (path->collects-relative
                  (collection-file-path s "scribble" "sigplan")))])
      (list
       (make-css-addition (abs "sigplan.css"))
       (make-tex-addition (abs "sigplan.tex")))))

#;(define abstract-style (make-style "abstract" sigplan-extras))

#;(define (abstract . strs)
    (make-nested-flow
     abstract-style
     (decode-flow strs)))


(define (define-new-theorem name #:classes [classes '()])
  (define (theorem-internal strs #:name [theorem-name #f])
    (let-values ([(s) (match theorem-name [#f ""] [_ (string-append "(" theorem-name ")")])]
                 [(contents rest) (splitf-at strs content?)]
                 [(attr-classes) (string-join (list* "theorem-like"
                                                     (string-downcase name)
                                                     classes))])
      (nested-flow (style #f (list (attributes `((class . ,attr-classes)))))
                   (list* (paragraph plain
                                     (list* (make-element 'bold (~a (translate name) " "))
                                            (make-element 'italic s)
                                            (decode-content contents)))
                          (decode-flow rest)))))
  (make-keyword-procedure
    (λ (kws kw-args . rest)
      (keyword-apply theorem-internal kws kw-args (list rest)))))


(define theorem (define-new-theorem "Theorem" #:classes '("box")))
(define lemma (define-new-theorem "Lemma" #:classes '("box")))
(define corollary (define-new-theorem "Corollary"))
(define proposition (define-new-theorem "Proposition"))
(define conjecture (define-new-theorem "Conjecture"))
(define criterion (define-new-theorem "Criterion"))
(define algorithm (define-new-theorem "Algorithm"))
(define exercise (define-new-theorem "Exercise"))

(define definition (define-new-theorem "Definition" #:classes '("box")))
(define example (define-new-theorem "Example" #:classes '("box")))
;(define-new-theorems condition problem)

(define remark (define-new-theorem "Remark"))
;(define-new-theorems note notation claim summart acknowledgment #;case conclusion)

(define proof (define-new-theorem "Proof"))

;; ----------------------------------------
;; Math Environments

(define (aligned . s)
  (make-element (make-style "relax" '(exact-chars))
                `("\\[\\begin{aligned}"
                  ,@s
                  "\\end{aligned}\\]")))


;;;
;;; INDEX
;;;

; definition-index : ...
;   emphasize the word where it appears,
;   also put the word in the index
(define (def-index . s)
  (emph (apply as-index s)))
