#!/usr/bin/env racket
#lang racket/base

(require racket/cmdline racket/dict racket/file racket/list racket/string)

(define-syntax-rule (def b ...) (define b ...))
(def : list)
(def :* list*)
(def :^ car)
(def :> cdr)
(def :1 second)
(def :2 third)
(def :3 fourth)
(def :4 fifth)
(def :5 sixth)
(def :6 seventh)
(def :7 eighth)
(def :8 ninth)
(def :9 tenth)
(def str string-append)
(define-syntax-rule (@ b ...) (lambda b ...))
(define-syntax-rule (=: k v b ...) (let ((k v)) b ...))

; use custom destructuring macro b/c importing racket/match slows startup time
(define-syntax-rule (mlet2 k1 k2 v b ...)
  (let ((t v)) (let ((k1 (:^ t)) (k2 (:1 t))) b ...)))

(struct F (e t v)) ; fn (params ret-type body)
(struct C (k v t)) ; c-code (key value type)

(def (nonsense! . s)
  (displayln (string-join (:* "Nonsense! " s) ""))
  (exit 0))

(def (whitespace? c) (or (char=? c #\space) (char=? c #\tab) (char=? c #\return) (char=? c #\newline)))

(def cmd (command-line #:args args args))

(def ast
  (=: s (str (file->string (:^ cmd)) "\x29")
  (mlet2 v j
    (let read-list ((i 0))
      (if (= i (string-length s))
        (nonsense! "Too few \x29s")
        (=: c (string-ref s i)
          (cond ((whitespace? c) (read-list (+ i 1)))
                ((char=? c #\u28) (mlet2 v j (read-list (+ i 1))
                                  (mlet2 w k (read-list j)
                                    (: (:* v w) k))))
                ((char=? c #\u29) (: '() (+ i 1)))
                (else (mlet2 v j
                        (let read-sym ((i i))
                          (if (= i (string-length s))
                            (: "" i)
                            (=: c (string-ref s i)
                              (if (or (whitespace? c) (char=? c #\u28) (char=? c #\u29))
                                (: "" i)
                                (mlet2 v j (read-sym (+ i 1))
                                  (: (str (string c) v) j))))))
                      (mlet2 w k (read-list j)
                        (: (:* v w) k))))))))
    (if (= j (string-length s)) v (nonsense! "Too many \x29s")))))

(def fns (map (@(i) (:* (:1 i) (F (map (@(j) (:* (:^ j) (:1 j))) (:2 i)) (:3 i) (list-tail i 4))))
         (filter (@(i) (and (pair? i) (or (string=? (:^ i) "fn") (string=? (:^ i) "c_fn")))) ast)))

(def (lookup e k) (or (dict-ref e k #f) (nonsense! "\"" k "\" not defined")))

(def fout (str (:^ cmd) ".c"))
(when (file-exists? fout) (delete-file fout))

(def uniq-cnt -1)
(def (uniq)
  (set! uniq-cnt (+ uniq-cnt 1))
  (str "_u_" (list->string
    (let uniq-gen ((i uniq-cnt))
      (:* (integer->char (=: r (remainder i 63)
            (+ r (cond ((< r 10) 48) ((< r 36) 55) ((< r 62) 61) (else 33)))))
          (if (< i 63) '() (uniq-gen (- (quotient i 63) 1))))))))

(display-to-file (str (file->string "pdl.h")
  (string-join (map (@(i) (if (and (pair? i) (string=? (:^ i) "fn"))
                            (str (:3 i) " " (:1 i) "\u28" (if (null? (:2 i)) "void" (string-join (map :1 (:2 i)) ",")) "\u29;\n") "")) ast) "")
  (string-join (map (@(i) (cond
    ((and (pair? i) (string=? (:^ i) "fn"))
      (str (:3 i) " " (:1 i) "\u28"
           (if (null? (:2 i)) "void" (string-join (map (@(j) (str (:1 j) " " (:^ j))) (:2 i)) ","))
           (=: a (let gen ((k (uniq)) (t (:4 i)) (e (F-e (lookup fns (:1 i)))))
                   (cond ((pair? t) (if (string=? (:^ t) "?")
                                      (let* ((a (uniq)) (b (gen a (:1 t) e)) (c (gen k (:2 t) e)) (d (gen k (:3 t) e)))
                                        (C k (str "{" (C-t b) " " a ";" (C-v b) "if(" a ")" (C-v c) "else " (C-v d) "}")
                                           (if (string=? (C-t c) (C-t d)) (C-t c)
                                               (nonsense! "Then types " (C-t c) " and " (C-t d) " don't match"))))
                                      (=: a (map (@(i) (gen (uniq) i e)) (:> t)) (=: b (lookup fns (:^ t))
                                        (unless (= (length a) (length (F-e b)))
                                          (nonsense! (:^ t) " takes " (number->string (length (F-e b))) " arguments but you gave it " (number->string (length a))))
                                        (map (@(i j) (unless (equal? (C-t i) (:> j))
                                                       (nonsense! (:^ t) " parameter " (:^ j) " must be a " (:> j) " but you gave it a " (C-t i))))
                                             a (F-e b))
                                        (C k (str "{" (string-join (map (@(i) (str (C-t i) " " (C-k i) ";" (C-v i))) a) "")
                                                  k "=" (:^ t) "(" (string-join (map (@(i) (C-k i)) a) ",") ");}") (F-t b))))))
                         ((string? t) (C k (str k "=" t ";") (if (char-alphabetic? (string-ref t 0)) (lookup e t) "i4")))))
             (unless (equal? (C-t a) (:3 i)) (nonsense! (:1 i) " says it returns a " (:3 i) " but it actually returns a " (C-t a)))
             (str "\u29{" (C-t a) " " (C-k a) ";" (C-v a) "return " (C-k a) ";}\n"))))
    ((and (pair? i) (string=? (:^ i) "c_fn")) "")
    (else (nonsense! "Bad top-level expression")))) ast) "")) fout)
