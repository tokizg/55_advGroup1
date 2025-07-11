#lang br/quicklang

(require br/datum)

;; リーダ
;; 各行を (handle "関数名" 引数1 引数2 ...) に変換する
(define (read-syntax path port)
  (define lines (port->lines port))
  (define datums
    (map (lambda (l)
           (define parts (string-split l))
           (format-datum
             `(handle ,@parts)
             #f))
         lines))
  (define mod `(module loadraw-mod "loadraw.rkt" ,@datums))
  (datum->syntax #f mod))
(provide read-syntax)

;; エクスパンダ
(define-macro (loadraw-module-begin body ...)
  #'(#%module-begin body ...))
(provide (rename-out [loadraw-module-begin #%module-begin]))

;; 関数名と引数を読んで、適当にRacketの式へ変換する
(define (handle fn:id . args)
  (displayln (cons fn args)) ; とりあえず読んだモノを表示しておく
  (case fn
    [(関数A) (when (null? args) (FuncA))]
    [(関数B) (when (= (length args) 1) (FuncB (first args)))]
    [(関数C) (when (= (length args) 2) (FuncC (first args) (second args)))]
    [else (error 'handle "Unknown function or wrong args" fn args)]))

(provide handle FuncA FuncB FuncC)

;; 仮実装
;; 関数名と引数を表示する
(define (FuncA)
  (displayln 'FuncA)
  (void))
(define (FuncB x)
  (displayln (list 'FuncB x))
  (void))
(define (FuncC x y)
  (displayln (list 'FuncC x y))
  (void))
