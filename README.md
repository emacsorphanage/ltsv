# ltsv.el

[http://ltsv.org/](http://ltsv.org/)


## Introduction

ltsv.el provides LTSV parser and utility functions.


## Interfaces

`ltsv:parse-line` parses string and return alist. You can pass `want` and
`ignore` keyword parameters.

````elisp
(ltsv:parse-line "hoge:foo\tbar:baz\ttime:20:30:58\n")
;; => (("hoge" . "foo") ("bar" . "baz") ("time" . "20:30:58"))

(ltsv:parse-line "hoge:foo\tbar:baz\ttime:20:30:58\n" :want '("time"))
;; => (("time" . "20:30:58"))

(ltsv:parse-line "hoge:foo\tbar:baz\ttime:20:30:58\n" :ignore '("bar"))
;; => (("hoge" . "foo") ("time" . "20:30:58"))
````


`ltsv:parse-file` parses specified file and return list of alist.
You can pass keyword parameters same as `ltsv:parse-line`

If you pass following file to `ltsv:parse-file`(`(ltsv:parse-file "test.ltsv")`)
````
hoge:foo	bar:baz
perl:5.17.8	ruby:2.0	python:2.6
sushi:寿司	tennpura:天ぷら	ramen:ラーメン	gyoza:餃子
````

then returns as below

````
((("hoge" . "foo") ("bar" . "baz"))
 (("perl" . "5.17.8") ("ruby" . "2.0") ("python" . "2.6"))
 (("sushi" . "寿司") ("tennpura" . "天ぷら") ("ramen" . "ラーメン") ("gyoza" . "餃子")))
````


## Utility

`ltsv:view-file` pretty-prints specified LTSV file.
And `ltsv:view-buffer` pretty-prints current-buffer like following screenshot.

![screenshot](https://github.com/syohex/emacs-ltsv/raw/master/images/ltsv-el.png)
