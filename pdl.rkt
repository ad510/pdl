#!/usr/bin/env racket
#lang racket/base

(require racket/cmdline racket/dict racket/file racket/list racket/string)

; use custom destructuring macro b/c importing racket/match slows startup time
(define-syntax-rule (mlet2 k1 k2 v b ...)
  (let ((t v)) (let ((k1 (first t)) (k2 (second t))) b ...)))

(define (read-list s i)
  (if (= i (string-length s))
    (nonsense! "Too few \x29s")
    (let ((c (string-ref s i)))
      (cond ((whitespace? c) (read-list s (+ i 1)))
            ((char=? c #\u28) (mlet2 v j (read-list s (+ i 1))
                              (mlet2 w k (read-list s j)
                                (list (cons v w) k))))
            ((char=? c #\u29) (list '() (+ i 1)))
            (else (mlet2 v j (read-sym s i)
                  (mlet2 w k (read-list s j)
                    (list (cons v w) k))))))))

(define (read-sym s i)
  (if (= i (string-length s))
    (list "" i)
    (let ((c (string-ref s i)))
      (if (or (whitespace? c) (char=? c #\u28) (char=? c #\u29))
        (list "" i)
        (mlet2 v j (read-sym s (+ i 1))
          (list (string-append (string c) v) j))))))

(define (whitespace? c) (or (char=? c #\space) (char=? c #\tab) (char=? c #\return) (char=? c #\newline)))

(define (gen-glo t)
  (string-append (file->string "pdl.h") (string-join (for/list ((i t))
    (cond ((and (pair? i) (string=? (first i) "fn"))
            (string-append (fourth i) " " (second i) "\u28"
                           (let ((a (string-join (for/list ((j (third i)))
                                      (string-append (second j) " " (first j))) ",")))
                             (if (string=? a "") "void" a))
                           (let* ((k (uniq)) (a (gen k (fifth i) (first (dict-ref fns (second i))))))
                             (string-append "\u29{" (second a) " " k ";" (first a) "return " k ";}\n"))))
          ((and (pair? i) (string=? (first i) "c_fn")) "")
          (else (nonsense! "Bad top-level expression")))) "")))

(define (gen k t e)
  (cond ((pair? t) (if (string=? (first t) "?")
                     (let* ((a (uniq)) (b (gen a (second t) e)) (c (gen k (third t) e)) (d (gen k (fourth t) e)))
                       (list (string-append "{" (second b) " " a ";" (first b) "if(" a ")" (first c) "else " (first d) "}")
                             (if (string=? (second c) (second d)) (second c) (nonsense! "Then types don't match"))))
                     (let ((a (for/list ((i (cdr t))) (let ((u (uniq))) (cons u (gen u i e))))))
                       (list (string-append "{" (string-join (for/list ((i a)) (string-append (third i) " " (first i) ";" (second i))) "")
                                            k "=" (car t) "(" (string-join (for/list ((i a)) (first i)) ",") ");}") (second (dict-ref fns (car t)))))))
        ((string? t) (list (string-append k "=" t ";") (if (char-alphabetic? (string-ref t 0)) (dict-ref e t) "i4")))))

(define uniq-cnt -1)

(define (uniq)
  (set! uniq-cnt (+ uniq-cnt 1))
  (string-append "_u_" (list->string (uniq-gen uniq-cnt))))

(define (uniq-gen i)
  (cons (integer->char (let ((r (remainder i 63)))
          (+ r (cond ((< r 10) 48) ((< r 36) 55) ((< r 62) 61) (else 33)))))
        (if (< i 63) '() (uniq-gen (- (quotient i 63) 1)))))

(define (nonsense! s)
  (displayln (string-append "Nonsense! " s))
  (exit 0))

(define cmd (command-line #:args args args))
(define ast (let ((s (file->string (car cmd))))
            (mlet2 v j (read-list (string-append s "\x29") 0)
              (if (= (- j 1) (string-length s)) v (nonsense! "Too many \x29s")))))
(define fns (for/list ((i (filter (lambda (i) (and (pair? i) (or (string=? (first i) "fn") (string=? (first i) "c_fn")))) ast)))
  (list* (second i) (for/list ((j (third i))) (cons (first j) (second j))) (list-tail i 3))))

(let ((f (string-append (car cmd) ".c")))
  (cond ((file-exists? f) (delete-file f)))
  (display-to-file (gen-glo ast) f))
