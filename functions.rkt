#lang br

;; ==== ライブラリ ==== ;;
;; Racket標準
;; https://docs.racket-lang.org/reference/index.html

;; 図形描画
;; https://docs.racket-lang.org/teachpack/2htdpimage.html
(require 2htdp/image)

;; 数学
;; https://docs.racket-lang.org/math/index.html
(require racket/math)




;; ===== 定数 ===== ;;
;; -- 長さ -- ;;
;; 道幅
(define ROAD-WIDTH 50)
;; 道の枠の幅
(define FRAME-WIDTH 2)
;; 交差点の各方向に伸ばす道の長さ
(define INTERSEC-LENGTH ROAD-WIDTH)
;; 距離単位あたりの画素数
(define METER 2)


;; -- 色 -- ;;
;; 道の色
(define ROAD-COLOR "Light Steel Blue")
;; 枠の色
(define FRAME-COLOR "Dark Gray")
;; 背景色
(define BACKGROUND-COLOR "White Smoke")
;; 余白の太さ #|最終結果のmap-imgに追加したい|#
(define MARGIN 50)


;; ---- イメージ ---- ;;
(define CORNER-IMAGE
  (let ([octagon-side-length (* (+ ROAD-WIDTH FRAME-WIDTH) (- (sqrt 2) 1))])
    (overlay (regular-polygon octagon-side-length
                              8
                              "outline"
                              (pen FRAME-COLOR FRAME-WIDTH "solid" "butt" "miter"))
             (regular-polygon octagon-side-length
                              8
                              "solid"
                              ROAD-COLOR))))


;; ---- 方向 ---- ;;
#|
2つの用途で方向定数を使用する。
仮引数名などで方向を扱う際はabsかrelかを明示する。

・絶対方向の指定(absolute)
imageライブラリの関数に使用するときは

・方向の相対的な変化量(relative)
例えばポインタ方向を現在の方向から見て右向きに更新する際には、
(+ (get-abs-dir state) RIGHT)
のようにする。
|#
(define FRONT 0)
(define FRONT-LEFT 45)
(define LEFT 90)
(define BACK-LEFT 135)
(define BACK 180)
(define BACK-RIGHT 225)
(define RIGHT 270)
(define FRONT-RIGHT 315)

;; ---- リーダ用エイリアス定義・purovide ---- ;;
(define 前 FRONT)
(define 左前 FRONT-LEFT)
(define 左 LEFT)
(define 左後ろ BACK-LEFT)
(define 後ろ BACK)
(define 右後ろ BACK-RIGHT)
(define 右 RIGHT)
(define 右前 FRONT-RIGHT)
(provide 前 左前 左 左後ろ 後ろ 右後ろ 右 右前)




;; ==== state(状態) ==== ;;
#|
--------------------------------------------------------------
** state **
そこまでの処理までのポインターとマップの状態。
描画関数が連鎖的にこれを返す。

** 構造 **
(((x . y) . dir) . map-image)

** 各要素 **
x, y
--ポインターの座標。
--image座標系で表現(原点：画像左上、x軸方向：右、y軸方向：下)

abs-dir(abstruct-direction)
--道を描画する方向。ポインターの絶対方向。

map-image
--そこまでの処理の時点での地図イメージ。
--------------------------------------------------------------
|#
;; stateセッタ。
;; 返り値：(((num . num) . num) . image)
(define (set-state x y abs-dir map-image)
  (cons (cons (cons x y) abs-dir) map-image))

;; 初期状態定数。
;; 右向きで開始。empty-imageはimageライブラリ内の定数。
(define INIT-STATE (set-state 0 0 0 empty-image))

;; 各要素のセレクタ ;;
(define (get-x state) (caaar state))
(define (get-y state) (cdaar state))
(define (get-abs-dir state) (cdar state))
(define (get-map-image state) (cdr state))





;; ==== リファレンス関数 ==== ;;

;; 方向転換関数。
;; 方向の値が0~359の間の値を取るようにする。
;; (rotate-dir 315 90)
;; >45
(define (rotate-dir abs-dir rel-dir)
  (modulo (+ abs-dir rel-dir) 360))


;; 最小を0として範囲を調整する関数。
;; 返り値：num
;; (clamp-min-zero -20)
;; >0
;; (clamp-min-zero 50)
;; >50
(define (clamp-min-zero n) (if (> n 0) n 0))


;; scene上にimageを配置することを想定し、引数として、同じ座標軸方向の
;; imageの大きさ、座標pos、配置オプションplace、sceneの大きさ４つを受け取ると、
;; imageがどれだけsceneの両方向それぞれにどれだけはみ出るかを2値で返す。
  (define (axis-excesses-over-scene image-size axis-pos axis-place scene-size)
    (cond [(or (string=? "left" axis-place) (string=? "top" axis-place))
           (values (clamp-min-zero (- axis-pos))
                 (clamp-min-zero (- (+ axis-pos image-size) scene-size)))]
          
          [(string=? "center" axis-place)
           (values (clamp-min-zero (- (* image-size (/ 1 2)) axis-pos))
                 (clamp-min-zero (- (+ axis-pos (* image-size (/ 1 2)))
                                    scene-size)))]
          [(or (string=? "right" axis-place) (string=? "bottom" axis-place))
           (values (clamp-min-zero (- image-size axis-pos))         
                 (clamp-min-zero (- axis-pos scene-size)))]))


;; scene上の(x,y)位置に、imageのx-placeとy-placeを合わせて配置した画像を生成する。
;; imageライブラリのplace-image/alignを拡張した関数であり、引数を同じように指定できる。
;; place-image/alignとの違いは、sceneのサイズを超えた範囲への描画があった際に、
;; 画像サイズを拡大したイメージを生成する。
;; さらに、オプション引数として、引数の末尾に #:with-expansions? #t を追加すると、
;; 各方向へのイメージ拡大量も付したリストで返す。
;; 返り値：image or '(image 左 右 上 下)※各拡大量
(define (add-image/align additional-image x y x-place y-place scene
                         #:with-expansions? [with-expansions? #f])
  ;; 各方向のイメージサイズ増分
  (define-values (left-expansion right-expansion)
    (axis-excesses-over-scene
     (image-width additional-image) x x-place (image-width scene)))
  (define-values (top-expansion bottom-expansion)
    (axis-excesses-over-scene
     (image-height additional-image) y y-place (image-height scene)))
  ;; 配置後image
  (define added-image
    (place-image/align additional-image
                       (+ x left-expansion)
                       (+ y top-expansion)
                       x-place
                       y-place
                       (place-image/align scene
                                          left-expansion
                                          top-expansion
                                          "left"
                                          "top"
                                          (rectangle (+ (image-width scene)
                                                        left-expansion
                                                        right-expansion)
                                                     (+ (image-height scene)
                                                        top-expansion
                                                        bottom-expansion)
                                                     "solid"
                                                     BACKGROUND-COLOR))))
  (if with-expansions?
      (list added-image
            left-expansion right-expansion top-expansion bottom-expansion)
      added-image))
;; add-image/alignテスト用関数
(define (test-add-image)
  (add-image/align (rectangle 50 50 "solid" "green")
                   25 0
                   "center" "bottom"
                   (add-image/align (rectangle 50 100 "solid" "blue")
                                    25 0
                                    "center" "bottom"
                                    (rectangle 50 50 "solid" "red"))))
(define (test-add-image-with-expansions)
   (add-image/align (rectangle 80 50 "solid" "green")
                   -30 -20
                   "right" "top"
                   (rectangle 50 100 "solid" "blue")
                   #:with-expansions? #t))


;; 移動の距離と絶対角度から、xとyの変化量のペアを求める。極座標→直交座標。
;; (dist+dir->xy 10 FRONT)
;; >(0 . -10)
;; (dist+dir->xy 10 LEFT)
;; >(-10 . 0)
(define (dist+dir->xy dist/m abs-dir)
  (let ([cos-pi/4 (cos (/ pi 4))]
        [dist-m (* dist/m METER)])
    (cons (cond [(or (eq? abs-dir FRONT) (eq? abs-dir BACK)) 0]
                [(eq? abs-dir LEFT) (- dist-m)]
                [(eq? abs-dir RIGHT) dist-m]
                [(or (eq? abs-dir FRONT-LEFT) (eq? abs-dir BACK-LEFT))
                 (* dist-m (- cos-pi/4))]
                [(or (eq? abs-dir FRONT-RIGHT) (eq? abs-dir BACK-RIGHT))
                 (* dist-m cos-pi/4)])
          (cond [(or (eq? abs-dir LEFT) (eq? abs-dir RIGHT)) 0]
                [(eq? abs-dir BACK) dist-m]
                [(eq? abs-dir FRONT) (- dist-m)]
                [(or (eq? abs-dir FRONT-LEFT) (eq? abs-dir FRONT-RIGHT))
                 (* dist-m (- cos-pi/4))]
                [(or (eq? abs-dir BACK-LEFT) (eq? abs-dir BACK-RIGHT))
                 (* dist-m cos-pi/4)]))))




;; ==== 低級描画関数 ==== ;;
;; 上向きにまっすぐな道の画像を指定距離の長さで生成。
;; オプション引数を#fで指定すると枠なしの道を生成する。指定しなければ枠あり。
;; 返り値：image
(define (make-straight-road-image len/m ;; 引数の単位はメートル
                                  #:with-frame? [with-frame? #t]) 
  (if with-frame?
      (let ([road-image
             (rectangle ROAD-WIDTH (* len/m METER) "solid" ROAD-COLOR)]
            [frame-image
             (rectangle FRAME-WIDTH (* len/m METER) "solid" FRAME-COLOR)])
        ;; 枠 + 道 + 枠で構成
        (beside frame-image road-image frame-image))
      ;; 道単体(枠無し)で生成
      (rectangle ROAD-WIDTH (* len/m METER) "solid" ROAD-COLOR)))


;; 指定された方向にまっすぐな道を描画。
;; オプション引数を２つ指定できる。
;; move-pointer?：ポインターをその方向に向きを変え、座標を移動するかどうか。
;; with-frame?：生成する道を枠付きで生成するかどうか。
;; 初期値（指定無し）ではポインターを更新せず、枠付きの道を描画する。
(define (draw-dir-road target-rel-dir dist/m state
                       #:move-pointer? [move-pointer? #f]
                       #:with-frame? [with-frame? #t])
  (let* ([x (get-x state)]
         [y (get-y state)]
         [dir (get-abs-dir state)]
         [map-image (get-map-image state)]
         [target-abs-dir (rotate-dir dir target-rel-dir)]
         
         [pos-move-dists (dist+dir->xy dist/m target-abs-dir)]
         [x-move-dist (car pos-move-dists)]
         [y-move-dist (cdr pos-move-dists)]
         
         [drawn-map-image-with-offsets
          (add-image/align (rotate target-abs-dir
                                   (make-straight-road-image
                                    dist/m
                                    #:with-frame? with-frame?))
                           (+ x (/ x-move-dist 2))
                           (+ y (/ y-move-dist 2))
                           "center"
                           "center"
                           map-image
                           #:with-expansions? #t)]
         [drawn-map (first drawn-map-image-with-offsets)]
         [map-origin-x-offset (second drawn-map-image-with-offsets)]
         [map-origin-y-offset (fourth drawn-map-image-with-offsets)])
    ;; (for-each displayln (list target-abs-dir
    ;;                           x-move-dist
    ;;                           y-move-dist
    ;;                           map-origin-x-offset
    ;;                           map-origin-y-offset))
    (if move-pointer?
        (set-state (+ x x-move-dist map-origin-x-offset)
                   (+ y y-move-dist map-origin-y-offset)
                   target-abs-dir
                   drawn-map)
        (set-state (+ x map-origin-x-offset)
                   (+ y map-origin-y-offset)
                   dir
                   drawn-map))))


;; 角を滑らかにし、間を埋めるための図形(コーナー)を現在位置に描画。
(define (draw-corner state)
  (define map-image-added-corner
    (add-image/align CORNER-IMAGE
                     (get-x state)
                     (get-y state)
                     "center"
                     "center"
                     (get-map-image state)
                     #:with-expansions? #t))
  (set-state (+ (get-x state) (second map-image-added-corner))
             (+ (get-y state) (fourth map-image-added-corner))
             (get-abs-dir state)
             (first map-image-added-corner)))




;;==== 上級描画関数 ====;;
;; ソースコードで書く関数。エイリアスの定義とprovideをする。

;; 直進
(define (go-straight dist/m state)
  (draw-dir-road FRONT dist/m state #:move-pointer? #t))

;; 交差点とカーブ
(define (intersec-or-curve travel-rel-dir branching-rel-dirs state)
  (let* (;; 指定方向すべてに道を生成。
         [state-drawn-dirs-road
          (foldl
           (lambda (rel-dir state)
             (draw-dir-road rel-dir INTERSEC-LENGTH state))
           state
           branching-rel-dirs)]
         ;; 現在位置にコーナーを生成。
         [state-drawn-corner
          (draw-corner state-drawn-dirs-road)]
         ;; 進行方向を除いた指定方向に枠無し道を生成。
         [state-drawn-filling-road
          (foldl
           (lambda (rel-dir state)
             (draw-dir-road rel-dir INTERSEC-LENGTH state #:with-frame? #f))
           state-drawn-corner
           (cons BACK (remove travel-rel-dir branching-rel-dirs)))]
         ;; 進行方向に枠なし道を描き、その道の方向を向き、移動する。
         [moved-state
          (draw-dir-road travel-rel-dir INTERSEC-LENGTH state-drawn-filling-road
                         #:with-frame? #f #:move-pointer? #t)])
    moved-state))


;; ---- エイリアス定義・provide ---- ;;
(define 直進 go-straight)
(define 交差点 intersec-or-curve)
(define (カーブ travel-rel-dir state)
  (intersec-or-curve travel-rel-dir (list travel-rel-dir) state))

(provide 直進
         交差点
         カーブ)

;; シンタックスシュガーたち
(define (T字路 travel-rel-dir state)
  (交差点 travel-rel-dir (list 左 右) state))
(define (十字路 travel-rel-dir state)
  (交差点 travel-rel-dir (list 左 前 右) state))

(provide T字路
         十字路)



;;==== テスト ====;;
(define (test1)
  (go-straight 100 INIT-STATE))
(define (test2)
  (intersec-or-curve RIGHT (list LEFT RIGHT FRONT) (test1)))
(define (test3)
  (go-straight 130 (test2)))
(define (test4)
  (intersec-or-curve BACK-RIGHT (list BACK-RIGHT) (test3)))
(define (test5)
  (go-straight 30 (test4)))
(define (test6)
  (intersec-or-curve BACK-RIGHT
                     (list FRONT FRONT-LEFT LEFT BACK-LEFT
                           BACK-RIGHT RIGHT FRONT-RIGHT)
                     (test5)))
(define (test7)
  (go-straight 120 (test6)))
(define (test8)
  (intersec-or-curve RIGHT (list FRONT-RIGHT RIGHT) (test7)))
(define (test9)
  (go-straight 100 (test8)))

(define (test);; まとめて評価。キモ地図が出力される。
  (values (test1) (test2) (test3) (test4) (test5) (test6) (test7) (test8) (test9)))




;;==== リーダ内で追加する処理に必要な識別子 ====;;
(provide save-image
         get-map-image
         INIT-STATE)
