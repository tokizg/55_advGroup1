# 道案内用DSL Leadraw

Leadrawは道案内のための画像を描画するためのDSLです。
日本語を使って直感的に記述することができます。

ソースコードを実行するとディレクトリ内に"leadraw-out.png"という画像が生成されます。

## 使用例
ソースコード:
```racket
#lang reader "leadraw.rkt"

直進　３０ｍ
T字路　右
直進　１５ｍ
カーブ　左
```
実行結果:
><img width="356" height="276" alt="leadraw-out" src="https://github.com/user-attachments/assets/96e523de-03cb-4026-9c13-6b291ddbddd6" />
