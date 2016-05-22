#lang racket

(provide language-data)

(define english-language #hash())
(define danish-language
  #hash( ("Theorem" . "Sætning")
         ("Proof"   . "Bevis")
         ("Example" . "Eksempel")))
(define thai-language
  #hash( ("Theorem" . "ทฤษฎีบท")
         ("Proof"   . "พิสูจน์")
         ("Example" . "ตัวอย่าง")))

(define language-data
  (hash "english" english-language
        "danish"  danish-language
        "thai"    thai-language))