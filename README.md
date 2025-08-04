# 道案内用DSL Leadraw

Leadrawは道案内のための画像を描画するためのDSLです。
日本語を使って直感的に記述することができます。

## 使用例
- ソースコード:
```racket
#lang reader "leadraw.rkt"

直進　３０ｍ
T字路　右
直進　１５ｍ
カーブ　左
```
  - 実行結果

<img width="257" height="200" alt="leadraw-out" src="https://github.com/user-attachments/assets/24c5a93e-51fd-4cec-b07d-22b096952041" />
