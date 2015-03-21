#!/usr/bin/env racket
#lang racket/base

(require racket/cmdline racket/file racket/list racket/match racket/string)

(define (read-list s i)
  (if (= i (string-length s))
    (nonsense! "Too few \x29s")
    (let ((c (string-ref s i)))
      (cond ((whitespace? c) (read-list s (+ i 1)))
            ((char=? c #\u28) (match-let* (((list v j) (read-list s (+ i 1)))
                                           ((list w k) (read-list s j)))
                                (list (cons v w) k)))
            ((char=? c #\u29) (list '() (+ i 1)))
            (else (match-let* (((list v j) (read-sym s i))
                               ((list w k) (read-list s j)))
                    (list (cons v w) k)))))))

(define (read-sym s i)
  (if (= i (string-length s))
    (list "" i)
    (let ((c (string-ref s i)))
      (if (or (whitespace? c) (char=? c #\u28) (char=? c #\u29))
        (list "" i)
        (match-let (((list v j) (read-sym s (+ i 1))))
          (list (string-append (string c) v) j))))))

(define (whitespace? c) (or (char=? c #\space) (char=? c #\tab) (char=? c #\return) (char=? c #\newline)))

(define (gen-glo t)
  (string-append (file->string "pdl.h") (string-join (for/list ((i t))
    (if (and (pair? i) (string=? (first i) "fn"))
      (string-append "int32_t " (second i) "\u28"
                     (let ((a (string-join (for/list ((j (third i)))
                                (string-append "int32_t " j)) ",")))
                       (if (string=? a "") "void" a))
                     "\u29{return " (gen (fourth i)) ";}\n")
      (nonsense! "Bad top-level expression"))) "")))

(define (gen t)
  (cond ((pair? t) (if (string=? (first t) "?")
                     (string-append "\u28" (gen (second t)) "?" (gen (third t)) ":" (gen (fourth t)) "\u29")
                     (string-append (car t) "\u28" (string-join (map gen (cdr t)) ",") "\u29")))
        ((string? t) t)))

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
(define ast (match-let* ((s (file->string (car cmd)))
                         ((list v j) (read-list (string-append s "\x29") 0)))
              (if (= (- j 1) (string-length s)) v (nonsense! "Too many \x29s"))))

(let ((f (string-append (car cmd) ".c")))
  (cond ((file-exists? f) (delete-file f)))
  (display-to-file (gen-glo ast) f))
