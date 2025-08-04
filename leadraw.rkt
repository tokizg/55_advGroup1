#lang br/quicklang

;; ==== リーダ ==== ;;
(define (read-syntax path port)
  (define src-lines (port->lines port))
  (define nested-s-exp-form-src
    `(,@(strings->nested-s-exp src-lines "INIT-STATE")))
  (define module-datum `(module leadraw-mod br
                          (require "functions.rkt")
                          (define result-state ,nested-s-exp-form-src)
                          (define result-map-added-margin
                            (add-margin (get-map-image result-state)))
                          (save-image result-map-added-margin
                                      "leadraw-out.png")
                          result-map-added-margin))
  ;; 変換確認用
  ;; (printf "ソースコード:\n ~a\n" src-lines)
  ;; (printf "変換後(ソースに対応する部分のみ):\n~a\n" nested-s-exp-form-src)
  ;; (printf "module-datum:\n ~a" module-datum)

  (datum->syntax #f module-datum))
(provide read-syntax)


;; ==== 文字列変換関数 ==== ;;
;; fw: 全角full-width, hw: 半角half-width

;; 全角スペースから半角スペースへ変換。
;; "　　" -> "  "
(define (fw->hw/space str)
  (regexp-replace* #px"　+" str " " ))

;; 全角数字から半角数字へ変換。
;; "１００" -> "100"
(define (fw->hw/number str)
  (define fw-hw-digit-hash
    (for/hash ([fw (in-range (char->integer #\０) (add1 (char->integer #\９)))]
               [hw (in-range (char->integer #\0) (add1 (char->integer #\9)))])
      (values (string (integer->char fw)) (string (integer->char hw)))))
  (apply string-append
         (map (lambda (c)
                (hash-ref fw-hw-digit-hash (string c) (lambda () (string c))))
              (string->list str))))

;; 全角小数点から半角小数点へ変換
;; "1．2" -> "1.2"
(define  (fw->hw/decimal-point str)
  (regexp-replace #px"．" str "."))

;; メートル表記を数字のみへ変換。
;; "10m" -> "10"
;; "20ｍ" -> "20"
(define (meters->num str)
  (regexp-replace #px"([0-9]+(.[0-9]+)?)(m|ｍ|メートル)" str
                   (lambda (all-matching num decimal meter)
                     num)))

;; キロメートル表記をメートル換算し数字のみへ変換。
;; "1km" -> "1000"
;; "2ｋｍ" -> "2000"
(define (kilo-meters->num str)
  (regexp-replace #px"([0-9]+(.?[0-9]+)?)(km|ｋｍ|キロメートル)" str
                   (lambda (all-matching num decimal kilo-meter)
                     (let ([n (string->number num)])
                       (number->string (* n 1000))))))

;; 括弧表記をリストへ変換。
;; "（左 前 右）" -> "(list 左 前 右)"
;; "(左前 右前)" -> "(list 左前 右前)"
(define (paren->list str)
  (regexp-replace #px"(（|\\()(.*?)(）|\\))"
                   str
                   (lambda (all-matching  left-paren inner-text right-paren)
                     (format "(list ~a)" inner-text))))

;; 文字列リストを受け取り、文字列毎にネストが深くなる()の入れ子構造にする。
;; 最もネストが深い部分（リストの先頭）にinit-argを付加する。
;; (strings->nestify-datums '("a b" "c d e") "init")
;; >'(c d e (a b init))
(define (strings->nestify-datums strs init-str)
  (define (iter strs acc)
    (cond [(null? strs) acc]
          [(string=? (car strs) "") (iter (cdr strs) acc)]
          [else (iter (cdr strs) (format-datum '(~a ~a) (car strs) acc))]))
  ;; (printf "nestify ~a\ninit: ~a\n" strs init-str)
  (iter strs init-str))

;; 変換関数を統合した関数
(define (strings->nested-s-exp strs init-arg)
  (strings->nestify-datums (map paren->list
                                (map kilo-meters->num
                                     (map meters->num
                                          (map fw->hw/decimal-point
                                               (map fw->hw/number
                                                    (map fw->hw/space strs))))))
                           init-arg))
