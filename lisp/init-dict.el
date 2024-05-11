;; init-dict.el --- Initialize dictionaries.	-*- lexical-binding: t -*-

;; Copyright (C) 2021 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; URL: https://github.com/seagle0128/.emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; Multiple dictionaries.
;;

;;; Code:

(eval-when-compile
  (require 'init-const))

;; A multi dictionaries interface
(use-package fanyi
  :bind (("C-c d f" . fanyi-dwim)
         ("C-c d d" . fanyi-dwim2)
         ("C-c d h" . fanyi-from-history))
  :custom (fanyi-providers '(fanyi-haici-provider fanyi-longman-provider)))

(use-package go-translate
  :bind (("C-c y"   . gt-do-translate)
         ("C-c d g" . gt-do-translate))
  :init (setq gt-langs '(en zh))
  :config
  (setq gt-default-translator
        (gt-translator :engines (list (gt-bing-engine) (gt-youdao-dict-engine))))

  (when (childframe-workable-p)
    (defclass gt-posframe-pos-render (gt-posframe-pop-render)
      ((width       :initarg :width        :initform 60)
       (height      :initarg :height       :initform 15)
       (padding     :initarg :padding      :initform 8)
       (bd-width    :initarg :bd-width     :initform 2)
       (bd-color    :initarg :bd-color     :initform nil)
       (backcolor   :initarg :backcolor    :initform nil))
      "Pop up a childframe to show the result at point.
The frame will disappear when do do anything but focus in it.
Manually close the frame with `q'.")

    (cl-defmethod gt-init ((render gt-posframe-pos-render) translator)
      (with-slots (width height min-width min-height bd-width forecolor backcolor bd-color padding position) render
        (let ((inhibit-read-only t)
              (buf gt-posframe-pop-render-buffer))
          ;; create
          (unless (buffer-live-p (get-buffer buf))
            (posframe-show buf
                           :string "Loading..."
                           :timeout gt-posframe-pop-render-timeout
                           :width width
                           :height height
                           :min-width width
                           :min-height height
                           :foreground-color (or forecolor (face-foreground 'tooltip nil t))
                           :background-color (or backcolor (face-background 'tooltip nil t))
                           :internal-border-width bd-width
                           :border-color (or bd-color (face-background 'posframe-border nil t))
                           :left-fringe padding
                           :right-fringe padding
                           :accept-focus t
                           :position (point)
                           :poshandler gt-posframe-pop-render-poshandler))

          ;; render
          (gt-buffer-render-init buf render translator)
          (posframe-refresh buf)
          ;; setup
          (with-current-buffer buf
            (gt-buffer-render-key ("q" "Close") (posframe-delete buf))))))

    (cl-defmethod gt-output ((render gt-posframe-pos-render) translator)
      (when-let (buf (get-buffer gt-posframe-pop-render-buffer))
        (gt-buffer-render-output buf render translator)
        (posframe-refresh buf)
        (add-hook 'post-command-hook #'gt-posframe-render-auto-close-handler)))

    (setq gt-default-translator
          (gt-translator :engines (gt-youdao-dict-engine)
                         :render (gt-posframe-pos-render)))))

;; OSX dictionary
(when sys/macp
  (use-package osx-dictionary
    :bind (("C-c d i" . osx-dictionary-search-input)
           ("C-c d x" . osx-dictionary-search-pointer))))

(provide 'init-dict)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dict.el ends here
