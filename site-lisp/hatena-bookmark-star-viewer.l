; -*- mode: lisp; package: hatena-bookmark-star-viewer -*-

;;; hatena-bookmark-star-viewer.l --- Hatena Bookmark & Star viewer
;;
;; Copyright (c) 2008 MIYAMUKO Katsuyuki.
;;
;; Author: MIYAMUKO Katsuyuki <miyamuko@gmail.com>
;; Version: 0.4
;; Keywords: hatena, star, bookmark
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:
;;
;; * �J�[�\������ URI �̂͂Ăȃu�b�N�}�[�N�R�����g�A�͂ĂȃX�^�[��\���ł��܂��B
;;
;; * ���[�W�������� URI �����ׂĒ��o���Ă͂ĂȃX�^�[�E�͂Ăȃu�b�N�}�[�N�R�����g��
;;   �\���ł��܂��B
;;
;; * clickable-uri ���g���Ă���̂ŁANetInstaller ����C���X�g�[�����Ă��������B
;;
;; * �g����
;;
;;   - URI �̏�� C-c i
;;   - URI ���܂ޔ͈͂����[�W�����ň͂��� M-x show-region-hatena-star-and-bookmarks
;;


;;; Code:

(in-package :lisp)

(require "clickable-uri")
(require "hatena-star/api")
(require "xml-http-request")
(require "json")

(defpackage :hatena-bookmark-star-viewer
  (:use
   :lisp :editor

   :hatena-star.api
   :xml-http-request
   :json
   ))
(use-package :hatena-bookmark-star-viewer :user)


(in-package :hatena-bookmark-star-viewer)

(export '(show-region-hatena-star-and-bookmarks
          show-current-link-hatena-star-and-bookmark
          show-hatena-star/bookmark-later
          ))

(defvar *html-special-chars-alist*
  '(("&nbsp;" . " ")
    ("&lt;" . "<")
    ("&gt;" . ">")
    ("&quot;" . "\"")
    ("&amp;" . "&")))

(defun html-unescape (html)
  (reduce #'(lambda (r pair)
              (substitute-string r (car pair) (cdr pair)))
          *html-special-chars-alist*
          :initial-value html))

(defmacro with-color ((&rest color) &body body)
  (let ((s (gensym)))
    `(let ((,s (buffer-stream-point *standard-output*)))
       (progn ,@body)
       (set-text-attribute ,s (buffer-stream-point *standard-output*) nil ,@color))))
(setf (get 'with-color 'ed::lisp-indent-hook) 'with-output-to-buffer)

(defmacro puts (fmt &rest args)
  `(progn
     (toggle-read-only nil)
     (format t ,fmt ,@args)
     (toggle-read-only t)))

(defmacro $ (item alist)
  `(cdr (assoc ,item ,alist :test 'equal)))

(defmacro invoke-later (dt &body body)
  `(start-timer ,dt #'(lambda () ,@body) t))
(setf (get 'invoke-later 'ed::lisp-indent-hook) '1)

(defmacro with-render-buffer ((buffer) &body body)
  (let ((gbuffer (gensym))
        (gorg (gensym)))
    `(let ((,gorg (selected-buffer))
           (,gbuffer ,buffer))
       (when (and ,gbuffer (not (deleted-buffer-p ,gbuffer)))
         (unless (equal ,gbuffer ,gorg)
           (set-buffer ,gbuffer))
         (unwind-protect
             (with-output-to-buffer (,gbuffer (point-max))
               (progn ,@body))
           (unless (equal ,gbuffer ,gorg)
             (set-buffer ,gorg)))
         (refresh-screen)
         ))))
(setf (get 'with-render-buffer 'ed::lisp-indent-hook) 'with-output-to-buffer)


(defun show-region-hatena-star-and-bookmarks (s e)
  (interactive "r")
  (dolist (uri (scan-uris-region s e))
    (show-hatena-star/bookmark-later uri)))

(defun show-current-link-hatena-star/bookmark ()
  (interactive)
  (let ((*clickable-uri-open-command-alist*
         (acons "^https?://" #'show-hatena-star/bookmark-later nil))
        (*clickable-uri-keep-focus* nil))
    (clickable-uri-open)))
(global-set-key '(#\C-c #\i) 'show-current-link-hatena-star/bookmark)


(defun show-hatena-star/bookmark-later (uri)
  (let* ((uri (remove-query-string uri))
         (bookmark (xhr-get-future (hatena-bookmark-entry-json-uri uri)
                                   :key 'xhr-response-text))
         (org (selected-buffer))
         (win (selected-window))
         (b (get-buffer-create "*Hatena:Star/Bookmark*")))
    (setup-temp-buffer b)
    (set-buffer-fold-width t b)
    (hatena-star-get-entry uri :callback
                           #'(lambda (star)
                               (render-hatena-star/bookmark b uri star bookmark)))
    (let ((w (get-buffer-window b)))
      (if w
          (set-window w)
        (split-window (- (truncate (/ (window-height) 2)))))
      (set-buffer b)
      (view-mode))
    (set-window win)))

(defun render-hatena-star/bookmark (buffer uri entry bookmark)
  (invoke-later 0.1
    (render-star uri entry buffer)
    (render-bookmark uri bookmark buffer)))

(defun render-star (uri entry buffer)
  (with-render-buffer (buffer)
    (puts "----------------------------------------------------------------------\n")
    (puts "~A ��~%" uri)
    (if (not entry)
        (puts "���̐�: 0~%~%")
      (progn
        (puts "���̐�: ~D~%~%" (hatena-star-stars-count entry))
        (dolist (star (hatena-star-stars-by-user entry :list-quote t))
          (multiple-value-bind (name quote count)
              (hatena-star-star-values star)
            (if count
                (progn
                  (puts "~@20A" name)
                  (puts (make-sequence 'string count :initial-element #\��)))
              (puts name))
            (puts "\n")
            (dolist (q quote)
              (puts "~20A�u~A�v\n" "" (html-unescape q)))))
        (puts "\n")))))

(defun render-bookmark (uri future buffer)
  (let ((bookmark (xhr-future-value future :timeout 0)))
    (if (not bookmark)
        (invoke-later 0.5
          (render-bookmark uri future buffer))
      (with-render-buffer (buffer)
        (puts "----------------------------------------------------------------------\n")
        (puts "~A ��~%" uri)
        (let ((bookmark (json-decode bookmark :strict nil)))
          (puts "�͂Ăȃu�b�N�}�[�N��: ~A~%~%" (or ($ "count" bookmark) "0"))
          (dolist (b ($ "bookmarks" bookmark))
            (with-color (:foreground 15)
              (puts "~A ~A ~{[~A]~}~%~%"
                    ($ "timestamp" b) ($ "user" b)
                    ($ "tags" b)))
            (let ((c ($ "comment" b)))
              (when (and c (string/= c ""))
                (puts "~A~%~%" (html-unescape c)))))
          (puts "\n")
          (when (and ($ "count" bookmark) (string/= ($ "count" bookmark) "0"))
            (invoke-later 0.5
              (render-bookmark (hatena-bookmark-entry-uri uri)
                               (xhr-get-future (hatena-bookmark-entry-json-uri (hatena-bookmark-entry-uri uri))
                                               :key 'xhr-response-text)
                               buffer)))
          )))))

(defun remove-query-string (uri)
  uri)
;  (substitute-string uri "\\?.*$" ""))

(defun hatena-bookmark-entry-uri (uri)
  (concat "http://b.hatena.ne.jp/entry/" uri))

(defun hatena-bookmark-entry-json-uri (uri)
  (concat "http://b.hatena.ne.jp/entry/json/" uri))

(defun bookmark-permalink (boomkark)
  (let ((eid (cdr (assoc "eid" bookmark :test 'equal))))
    (mapcar #'(lambda (b)
                (format nil "http://b.hatena.ne.jp/~A/~A#bookmark-~A"
                        (cdr (assoc "user" b :test 'equal))
                        (remove #\/ (subseq (cdr (assoc "timestamp" b :test 'equal)) 0 10))
                        eid))
            (cdr (assoc "bookmarks" bookmark :test 'equal)))))


(defun scan-uris-buffer ()
  (scan-uris-buffer (point-min) (point-max)))

(defun scan-uris-region (s e)
  (if (< e s) (rotatef s e))
  (labels ((scan1 (&optional acc)
             (if (scan-buffer ed::*clickable-uri-regexp*
                              :regexp t :case-fold t :tail t)
                 (scan1 (cons (match-string 0) acc))
               acc)))
    (save-excursion
      (save-restriction
        (narrow-to-region s e)
        (goto-char s)
        (nreverse (scan1))))))


(provide "hatena-bookmark-star-viewer")

;;; hatena-bookmark-star-viewer.l ends here