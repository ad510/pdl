#!/usr/bin/env racket
#lang racket/base

(require racket/cmdline racket/dict racket/file racket/list racket/string)

(define-syntax-rule (def b ...) (define b ...))
(def : list)
(def :* list*)
(def :^ car)
(def :> cdr)
(def :2 second)
(def :3 third)
(def :4 fourth)
(def :5 fifth)
(def :6 sixth)
(def :7 seventh)
(def :8 eighth)
(def :9 ninth)
(def :10 tenth)
(def str:: string-append)
(define-syntax-rule (@ b ...) (lambda b ...))
(define-syntax-rule (=: k v b ...) (let ((k v)) b ...))

; use custom destructuring macro b/c importing racket/match slows startup time
(define-syntax-rule (mlet2 k1 k2 v b ...)
  (let ((t v)) (let ((k1 (:^ t)) (k2 (:2 t))) b ...)))

(def (nonsense! s)
  (displayln (str:: "Nonsense! " s))
  (exit 0))

(def (whitespace? c) (or (char=? c #\space) (char=? c #\tab) (char=? c #\return) (char=? c #\newline)))

(def cmd (command-line #:args args args))

(def ast
  (=: s (file->string (:^ cmd))
  (mlet2 v j
    (let read-list ((s (str:: s "\x29")) (i 0))
      (if (= i (string-length s))
        (nonsense! "Too few \x29s")
        (=: c (string-ref s i)
          (cond ((whitespace? c) (read-list s (+ i 1)))
                ((char=? c #\u28) (mlet2 v j (read-list s (+ i 1))
                                  (mlet2 w k (read-list s j)
                                    (: (:* v w) k))))
                ((char=? c #\u29) (: '() (+ i 1)))
                (else (mlet2 v j
                        (let read-sym ((s s) (i i))
                          (if (= i (string-length s))
                            (: "" i)
                            (=: c (string-ref s i)
                              (if (or (whitespace? c) (char=? c #\u28) (char=? c #\u29))
                                (: "" i)
                                (mlet2 v j (read-sym s (+ i 1))
                                  (: (str:: (string c) v) j))))))
                      (mlet2 w k (read-list s j)
                        (: (:* v w) k))))))))
    (if (= (- j 1) (string-length s)) v (nonsense! "Too many \x29s")))))

(def fns (map (@(i) (:* (:2 i) (map (@(j) (:* (:^ j) (:2 j))) (:3 i)) (list-tail i 3)))
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
            (str:: (:4 i) " " (:2 i) "\u28"
                   (=: a (string-join (map (@(j) (str:: (:2 j) " " (:^ j))) (:3 i)) ",")
                     (if (string=? a "") "void" a))
                   (=: k (uniq) (=: a
                     (let gen ((k k) (t (:5 i)) (e (:^ (dict-ref fns (:2 i)))))
                       (cond ((pair? t) (if (string=? (:^ t) "?")
                                          (let* ((a (uniq)) (b (gen a (:2 t) e)) (c (gen k (:3 t) e)) (d (gen k (:4 t) e)))
                                            (: (str:: "{" (:2 b) " " a ";" (:^ b) "if(" a ")" (:^ c) "else " (:^ d) "}")
                                               (if (string=? (:2 c) (:2 d)) (:2 c) (nonsense! "Then types don't match"))))
                                          (=: a (map (@(i) (=: u (uniq) (:* u (gen u i e)))) (:> t))
                                            (: (str:: "{" (string-join (map (@(i) (str:: (:3 i) " " (:^ i) ";" (:2 i))) a) "")
                                                      k "=" (:^ t) "(" (string-join (map (@(i) (:^ i)) a) ",") ");}") (:2 (dict-ref fns (:^ t)))))))
                             ((string? t) (: (str:: k "=" t ";") (if (char-alphabetic? (string-ref t 0)) (dict-ref e t) "i4")))))
                     (str:: "\u29{" (:2 a) " " k ";" (:^ a) "return " k ";}\n")))))
          ((and (pair? i) (string=? (:^ i) "c_fn")) "")
          (else (nonsense! "Bad top-level expression")))) t) ""))) fout)
