#!/usr/bin/env racket
#lang racket/base

(require racket/cmdline
         racket/file
         racket/match)

(struct pdl-fn (name v))

(define (read-list s i g)
  (if (= i (string-length s))
    (if g (list '() i) (nonsense! "Too few \x29s"))
    (let ((c (string-ref s i)))
      (cond ((whitespace? c) (read-list s (+ i 1) g))
            ((char=? c #\u28) (match-let* (((list v j) (read-list s (+ i 1) #f))
                                          ((list w k) (read-list s j g)))
                               (list (cons v w) k)))
            ((char=? c #\u29) (if g (nonsense! "Too many \x29s") (list '() (+ i 1))))
            (else (match-let* (((list v j) (read-sym s i))
                               ((list w k) (read-list s j g)))
                    (list (cons v w) k)))))))

(define (read-sym s i)
  (if (= i (string-length s))
    (list "" i)
    (let ((c (string-ref s i)))
      (if (or (whitespace? c) (char=? c #\u29))
        (list "" i)
        (match-let (((list v j) (read-sym s (+ i 1))))
          (list (string-append (string c) v) j))))))

(define (whitespace? c)
  (or (char=? c #\space) (char=? c #\tab) (char=? c #\return) (char=? c #\newline)))

(define (nonsense! s)
  (displayln (string-append "Nonsense! " s))
  (exit 0))

(print (read-list (file->string (car (command-line #:args args args))) 0 #t))
