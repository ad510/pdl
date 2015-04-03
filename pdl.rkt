#!/usr/bin/env racket
#lang racket/base

(require racket/cmdline racket/file racket/list racket/string)

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
    (if (and (pair? i) (string=? (first i) "fn"))
      (string-append "pdl_t " (second i) "\u28"
                     (let ((a (string-join (for/list ((j (third i)))
                                (string-append "pdl_t " j)) ",")))
                       (if (string=? a "") "void" a))
                     (let* ((k (uniq)) (a (gen k (fourth i))))
                       (string-append "\u29{pdl_t " k ";" a "return " k ";}\n")))
      (nonsense! "Bad top-level expression"))) "")))

(define (gen k t)
  (cond ((pair? t) (if (string=? (first t) "?")
                     (let ((a (uniq)))
                       (string-append "{pdl_t " a ";" (gen a (second t)) "if(pdl_if(" a "))"
                                      (gen k (third t)) "else " (gen k (fourth t)) "}"))
                     (let ((a (for/list ((i (cdr t))) (let ((u (uniq))) (cons u (gen u i))))))
                       (string-append "{" (string-join (for/list ((i a)) (string-append "pdl_t " (car i) ";" (cdr i))) "")
                                      k "=" (car t) "(" (string-join (for/list ((i a)) (car i)) ",") ");}"))))
        ((string? t) (if (char-alphabetic? (string-ref t 0))
                       (string-append k "=" t ";")
                       (string-append "{" k ".t=pdl_i4;" k ".i4=" t ";}")))))

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

(let ((f (string-append (car cmd) ".c")))
  (cond ((file-exists? f) (delete-file f)))
  (display-to-file (gen-glo ast) f))
