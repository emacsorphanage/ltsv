;;; test-ltsv.el --- Test for ltsv.el

;; Copyright (C) 2013 by Syohei YOSHIDA

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'ert)
(require 'ltsv)

(ert-deftest ltsv-parse-line ()
  "Test of ltsv:parse-line"
  (let ((parsed (ltsv:parse-line "hoge:foo\tbar:baz\ttime:20:30:58\n")))
    (should (string= (assoc-default "hoge" parsed) "foo"))
    (should (string= (assoc-default "bar" parsed) "baz"))
    (should (string= (assoc-default "time" parsed) "20:30:58"))))

(ert-deftest ltsv:parse-line-wanted ()
  "Test of ltsv:parse-line with wanted parameter"
  (let* ((wants '("hoge" "time"))
         (parsed (ltsv:parse-line "hoge:foo\tbar:baz\ttime:20:30:58\n" :want wants)))
    (should (string= (assoc-default "hoge" parsed) "foo"))
    (should (null (assoc-default "bar" parsed)))
    (should (string= (assoc-default "time" parsed) "20:30:58"))))

(ert-deftest ltsv:parse-line-ignored ()
  "Test of ltsv:parse-line with ignored parameter"
  (let* ((ignores "bar")
         (parsed (ltsv:parse-line "hoge:foo\tbar:baz\ttime:20:30:58\n" :ignore ignores)))
    (should (string= (assoc-default "hoge" parsed) "foo"))
    (should (null (assoc-default "bar" parsed)))
    (should (string= (assoc-default "time" parsed) "20:30:58"))))

(ert-deftest ltsv:parse-file ()
  "Test of ltsv:parse-file"
  (let ((parsed (ltsv:parse-file "./tests/test.ltsv")))
    (let ((first-line (first parsed))
          (second-line (second parsed))
          (third-line (third parsed)))
      (should (string= (assoc-default "hoge" first-line) "foo"))
      (should (string= (assoc-default "bar" first-line) "baz"))
      (should (string= (assoc-default "perl" second-line) "5.17.8"))
      (should (string= (assoc-default "tennpura" third-line) "天ぷら")))))

(ert-deftest ltsv:to-string ()
  "Test of ltsv:to-string"
  (let ((str (ltsv:to-string (ltsv:parse-line "hoge:foo\tbar:baz"))))
    (should (string= str "hoge:foo\tbar:baz"))
    (let ((parsed (ltsv:parse-line str)))
      (should (string= (assoc-default "hoge" parsed) "foo"))
      (should (string= (assoc-default "bar" parsed) "baz")))))
