#lang br/quicklang

(define (read-syntax path port)
  (define src-lines (port->lines port))
  (define src-datums (format-datums ''(handle ~a) src-lines))
  (define module-datum `(module loadDraw-mod br "LoadDrawer.rkt"
                          ,@src-datums))
  (datum->syntax #f module-datum))
(provide read-syntax)

(define-macro (loadDraw-module-begin HANDLE-EXPR ...)
  #'(#%module-begin
     HANDLE-EXPR ...))
(provide (rename-out [loadDraw-module-begin #%module-begin]))

(define (handle [arg #f])
    (cond
        [(equal? arg "直進") (GoSt)]
        [(equal? arg "T字路") (TJunc)]))
(provide handle)

(define (GoSt)
  (display "直進"))
  (provide GoSt)

(define (TJunc direction)
  (display "T字路を曲がります"))