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
(def str:: string-append)
(define-syntax-rule (@ b ...) (lambda b ...))
(define-syntax-rule (=: k v b ...) (let ((k v)) b ...))

; use custom destructuring macro b/c importing racket/match slows startup time
(define-syntax-rule (mlet2 k1 k2 v b ...)
  (let ((t v)) (let ((k1 (:^ t)) (k2 (:1 t))) b ...)))

(def (nonsense! s)
  (displayln (str:: "Nonsense! " s))
  (exit 0))

(def (whitespace? c) (or (char=? c #\space) (char=? c #\tab) (char=? c #\return) (char=? c #\newline)))

(def cmd (command-line #:args args args))

(def ast
  (=: s (str:: (file->string (:^ cmd)) "\x29")
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
                                  (: (str:: (string c) v) j))))))
                      (mlet2 w k (read-list j)
                        (: (:* v w) k))))))))
    (if (= j (string-length s)) v (nonsense! "Too many \x29s")))))

(def fns (map (@(i) (:* (:1 i) (map (@(j) (:* (:^ j) (:1 j))) (:2 i)) (list-tail i 3)))
         (filter (@(i) (and (pair? i) (or (string=? (:^ i) "fn") (string=? (:^ i) "c_fn")))) ast)))

(def fout (str:: (:^ cmd) ".c"))
(when (file-exists? fout) (delete-file fout))

(def uniq-cnt -1)
(def (uniq)
  (set! uniq-cnt (+ uniq-cnt 1))
  (str:: "_u_" (list->string
    (let uniq-gen ((i uniq-cnt))
      (:* (integer->char (=: r (remainder i 63)
            (+ r (cond ((< r 10) 48) ((< r 36) 55) ((< r 62) 61) (else 33)))))
          (if (< i 63) '() (uniq-gen (- (quotient i 63) 1))))))))

(display-to-file (let gen-glo ((t ast))
  (str:: (file->string "pdl.h") (string-join (map (@(i)
    (cond ((and (pair? i) (string=? (:^ i) "fn"))
            (str:: (:3 i) " " (:1 i) "\u28"
                   (=: a (string-join (map (@(j) (str:: (:1 j) " " (:^ j))) (:2 i)) ",")
                     (if (string=? a "") "void" a))
                   (=: k (uniq) (=: a
                     (let gen ((k k) (t (:4 i)) (e (:^ (dict-ref fns (:1 i)))))
                       (cond ((pair? t) (if (string=? (:^ t) "?")
                                          (let* ((a (uniq)) (b (gen a (:1 t) e)) (c (gen k (:2 t) e)) (d (gen k (:3 t) e)))
                                            (: (str:: "{" (:1 b) " " a ";" (:^ b) "if(" a ")" (:^ c) "else " (:^ d) "}")
                                               (if (string=? (:1 c) (:1 d)) (:1 c) (nonsense! "Then types don't match"))))
                                          (=: a (map (@(i) (=: u (uniq) (:* u (gen u i e)))) (:> t))
                                            (: (str:: "{" (string-join (map (@(i) (str:: (:2 i) " " (:^ i) ";" (:1 i))) a) "")
                                                      k "=" (:^ t) "(" (string-join (map (@(i) (:^ i)) a) ",") ");}") (:1 (dict-ref fns (:^ t)))))))
                             ((string? t) (: (str:: k "=" t ";") (if (char-alphabetic? (string-ref t 0)) (dict-ref e t) "i4")))))
                     (str:: "\u29{" (:1 a) " " k ";" (:^ a) "return " k ";}\n")))))
          ((and (pair? i) (string=? (:^ i) "c_fn")) "")
          (else (nonsense! "Bad top-level expression")))) t) ""))) fout)
