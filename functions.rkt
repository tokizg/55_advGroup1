#lang br

(require 2htdp/image)
(require racket/math)

;;===== 定数・基本関数定義 =====;;
(define road-width 50); 道幅
(define frame-width 1); 道の脇の枠線の幅
(define intersection-length 50);交差点を描くときに伸ばす距離
(define meter 2); 距離単位(m)の画素数
(define road-color "Light Steel Blue"); 道の色
(define frame-color "Gray"); 枠の色
(define background-color "White Smoke"); 背景色
(define margin 50); 余白

(define (make-straight-road-img len/m); 直進路の画像生成 
  (let ([road-unit-img
         (rectangle road-width (* len/m meter) "solid" road-color)]
        [frame-unit-img
         (rectangle frame-width (* len/m meter) "solid" frame-color)])
    (beside frame-unit-img road-unit-img frame-unit-img)))



;;==== データ型 ====;;
(define (set-state x y dir map-img)
  (cons (cons (cons x y) ; 座標(image座標系)
              dir)       ; 方向(0~359)
        map-img))        ; マップimage
;; (define (set-state-xy xy dir map-img); 座標をpairで受け取るバージョン
;;   (cons (cons xy 
;;               dir)
;;         map-img))

(define init-state (set-state 0 0 270 empty-image)); 右向きスタート

(define (current-pos state) (caar state))
(define (current-x state) (caaar state))
(define (current-y state) (cdaar state))
(define (current-dir state) (cdar state))
(define (current-map-img state) (cdr state))

(define front 0)
(define front-left 45)
(define left 90)
(define back-left 135)
(define back 180)
(define back-right 225)
(define right 270)
(define front-right 315)



;;==== リファレンス ====;;

;結合画像と補正値のpairを返す
(define (add-image/align image x y x-place y-place scene)
  (define (non-negative n) (if (> n 0) n 0))
  (define (over-amounts image-size pos place scene-size)
    (cond [(or (string=? "left" place) (string=? "top" place))
           (cons 0
                 (let ([plus-over (- (+ pos image-size) scene-size)])
                   (non-negative plus-over)))]
          [(string=? "center" place)
           (cons (let ([minus-over (- (* image-size (/ 1 2)) pos)])
                   (non-negative minus-over))
                 (let ([plus-over (- (+ pos (* image-size (/ 1 2))) scene-size)])
                   (non-negative plus-over)))]
          [(or (string=? "right" place) (string=? "bottom" place))
           (cons (let ([minus-over (- image-size pos)])
                   (non-negative minus-over))           
                 0)]))
  
  (let* ([image-x (image-width image)]
         [image-y (image-height image)]
         [x-expand-sizes (over-amounts image-x x x-place (image-width scene))]
         [y-expand-sizes (over-amounts image-y y y-place (image-height scene))]
         [x-left-expansion (car x-expand-sizes)]
         [x-right-expansion (cdr x-expand-sizes)]
         [y-top-expansion (car y-expand-sizes)]
         [y-bottom-expansion (cdr y-expand-sizes)])
    (cons     
     (place-image/align image
                        (+ x x-left-expansion)
                        (+ y y-top-expansion)
                        x-place
                        y-place
                        (place-image/align scene
                                           x-left-expansion
                                           y-top-expansion
                                           "left"
                                           "top"
                                           (rectangle (+ (image-width scene)
                                                         x-left-expansion
                                                         x-right-expansion)
                                                      (+ (image-height scene)
                                                         y-top-expansion
                                                         y-bottom-expansion)
                                                      "solid"
                                                      background-color)))
     (cons x-left-expansion y-top-expansion))))
;; (define add-image-test
;;   (add-image/align (rectangle 50 50 "solid" "green")
;;                    25 0
;;                    "center" "bottom"
;;                    (add-image/align (rectangle 50 100 "solid" "blue")
;;                                     25 0
;;                                     "center" "bottom"
;;                                     (rectangle 50 50 "solid" "red"))))


(define (amounts-of-movement dist/m dir)
  (let ([cos-pi/4 (cos (/ pi 4))]
        [dist-m (* dist/m meter)])
    (cons (cond [(or (eq? dir front) (eq? dir back)) 0]
                [(eq? dir left) (- dist-m)]
                [(eq? dir right) dist-m]
                [(or (eq? dir front-left) (eq? dir back-left))
                 (* dist-m (- cos-pi/4))]
                [(or (eq? dir front-right) (eq? dir back-right))
                 (* dist-m cos-pi/4)])
          (cond [(or (eq? dir left) (eq? dir right)) 0]
                [(eq? dir back) dist-m]
                [(eq? dir front) (- dist-m)]
                [(or (eq? dir front-left) (eq? dir front-right))
                 (* dist-m (- cos-pi/4))]
                [(or (eq? dir back-left) (eq? dir back-right))
                 (* dist-m cos-pi/4)]))))



(define (make-dirs-road-img dirs state)
   (let* ([pos (current-pos state)]
         [x (car pos)]
         [y (cdr pos)]
         [dir (current-dir state)]
         [map-img (current-map-img state)]
         [pos-offsets-move (amounts-of-movement intersection-length dir)])
     (define (draw-dir-road arg-dir arg-state)
       (define add-image-result
         (add-image/align (rotate arg-dir (make-straight-road-img intersection-length))
                          (+ x (/ (car pos-offsets-move) 2));;ここを場合分け
                          (+ y (/ (cdr pos-offsets-move) 2))
                          "center"
                          "center"
                          (current-map-img arg-state)))
       (printf "add-image-result: ~a\n" add-image-result)
       (set-state (+ x (cadr add-image-result))
                  (+ y (cddr add-image-result))
                  dir
                  (car add-image-result)))
     (foldl draw-dir-road state dirs)))


;;==== 最上級関数 ====;;
;; ソースコードに書き込む関数。エイリアスの定義とprovideをする。

;; 直進

(define (go-straight dist/m state)
  (let* ([pos (current-pos state)]
         [x (car pos)]
         [y (cdr pos)]
         [dir (current-dir state)]
         [map-img (current-map-img state)]
         [pos-offsets-move (amounts-of-movement dist/m dir)]
         [add-image-result (add-image/align
                            (rotate dir (make-straight-road-img dist/m))
                            (+ x (/ (car pos-offsets-move) 2))
                            (+ y (/ (cdr pos-offsets-move) 2))
                            "center"
                            "center"
                            map-img)]
         [next-map (car add-image-result)]
         [pos-offsets-add-image (cdr add-image-result)])
    ;; (for-each displayln (list "move-offsets"
    ;;                           pos-offsets-move
    ;;                           "add-image-offsets"
    ;;                           pos-offsets-add-image))
    (set-state (+ x (car pos-offsets-move) (car pos-offsets-add-image))
               (+ y (cdr pos-offsets-move) (cdr pos-offsets-add-image))
               dir
               next-map)))
(define 直進 go-straight)
(provide 直進)

;; 交差点, カーブ
;;
;; 交差点　右　左　直進
;; 
;;
;; カーブ　右
;;
;;
;; (define (intersec/curve dir dir-list state)
;;   (let* ([pos (current-pos state)]
;;          [x (car pos)]
;;          [y (cdr pos)]
;;          [dir (current-dir state)]
;;          [map-img (current-map-img state)]
;;          [add-image-result (for/fold ([acc #f])
;;                                      (dir-list)
;;                              (add-image/align (car acc) (+ x (car (cdr acc))) (+ y (cdr (cdr acc))) "center" "center" map-img))]
;;          [next-map (car add-image-result)]
;;          [pos-offsets-add-image (cdr add-image-result)])
;;     (set-state (+ x (car pos-offsets-add-image))
;;                (+ y (car pos-offsets-add-image))
;;                dir
;;                next-map)))
;; (define 交差点 intersec/curve)
;; (define (十字路 進行方向 state)
;;   (intersec/curve 進行方向  (list 左 前 右)) state)
;; (define (T字路 進行方向 state)
;;   (intersec/curve 進行方向 (list 右 左 )))
;; (define (カーブ 進行方向)
;;   (intersec/curve 進行方向 (list 進行方向)))




;;==== テスト ====;;
(define (test1) (直進 100 (set-state 0 0 45 empty-image)))
(define (test2) (直進 50 (test1)))



;;==== リーダ内で追加する処理に必要な識別子 ====;;
(provide save-image
         current-map-img
         init-state)
