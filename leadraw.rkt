#lang br/quicklang

;;リーダ
(define (read-syntax path port)
  (define src-lines (port->lines port))
  (define nested-s-exp-form-src
    `(,@(strings->nested-s-exp src-lines "init-state")))
  (define module-datum `(module leadraw-mod br
                          (require "functions.rkt")
                          (define result-state ,nested-s-exp-form-src)
                          (save-image (current-map-img result-state)
                                      "leadraw-out.png")
                          (current-map-img result-state)))
  ;; (printf "ソースコード: ~a\n" src-lines)
  ;; (printf "変換後(ソースに対応する部分のみ):\n~a\n" nested-s-exp-form-src)
  ;; (printf "module-datum: ~a" module-datum)
  (datum->syntax #f module-datum))
(provide read-syntax)


;;====文字列変換関数====;;
;; fw: 全角full-width, hw: 半角half-width

(define (fw->hw/space str)
  (regexp-replace* #px"　+" str " " ))

(define (fw->hw/number str)
  (define fw-hw-digit-hash
    (for/hash ([fw (in-range (char->integer #\０) (add1 (char->integer #\９)))]
               [hw (in-range (char->integer #\0) (add1 (char->integer #\9)))])
      (values (string (integer->char fw)) (string (integer->char hw)))))
  (apply string-append
         (map (lambda (c)
                (hash-ref fw-hw-digit-hash (string c) (lambda () (string c))))
              (string->list str))))

(define (fw-meters->num str)
  (regexp-replace* #px"([0-9]+)ｍ" str
                   (lambda (all-matching group-matching)
                     group-matching)))

(define (fw-kilo-meters->num str)
  (regexp-replace* #px"([0-9]+)ｋｍ" str
                   (lambda (all-matching group-matching)
                     (let ([n (string->number group-matching)])
                       (number->string (* n 1000))))))

(define (strings->nestify-datums strs init-arg)
  (define (iter strs acc)
    (cond [(null? strs) acc]
          [(string=? (car strs) "") (iter (cdr strs) acc)]
          [else (iter (cdr strs) (format-datum '(~a ~a) (car strs) acc))]
          ))
  ;; (printf "nestify ~a\ninit: ~a\n" strs init-arg)
  (iter strs (string-append (car strs) init-arg)))

;; 変換関数を統合 ;;
(define (strings->nested-s-exp strs init-arg)
  (strings->nestify-datums  (map fw-kilo-meters->num
                                 (map fw-meters->num
                                      (map fw->hw/number
                                           (map fw->hw/space strs))))
                            init-arg))
