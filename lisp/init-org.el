;; init-org.el --- Initialize Org configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2006-2025 Vincent Zhang

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
;; Org configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-custom))

(use-package org
  :ensure nil
  :custom-face (org-ellipsis ((t (:foreground unspecified))))
  :pretty-hydra
  ;; See `org-structure-template-alist'
  ((:title (pretty-hydra-title "Org Template" 'sucicon "nf-custom-orgmode" :face 'nerd-icons-green)
   :color blue :quit-key ("q" "C-g"))
   ("Basic"
    (("a" (hot-expand "<a") "ascii")
     ("c" (hot-expand "<c") "center")
     ("C" (hot-expand "<C") "comment")
     ("x" (hot-expand "<e") "example")
     ("E" (hot-expand "<E") "export")
     ("h" (hot-expand "<h") "html")
     ("l" (hot-expand "<l") "latex")
     ("n" (hot-expand "<n") "note")
     ("o" (hot-expand "<q") "quote")
     ("v" (hot-expand "<v") "verse"))
    "Head"
    (("i" (hot-expand "<i") "index")
     ("A" (hot-expand "<A") "ASCII")
     ("I" (hot-expand "<I") "INCLUDE")
     ("H" (hot-expand "<H") "HTML")
     ("L" (hot-expand "<L") "LaTeX"))
    "Source"
    (("s" (hot-expand "<s") "src")
     ("e" (hot-expand "<s" "emacs-lisp") "emacs-lisp")
     ("y" (hot-expand "<s" "python :results output") "python")
     ("p" (hot-expand "<s" "perl") "perl")
     ("w" (hot-expand "<s" "powershell") "powershell")
     ("r" (hot-expand "<s" "ruby") "ruby")
     ("S" (hot-expand "<s" "sh") "sh")
     ("g" (hot-expand "<s" "go :imports '\(\"fmt\"\)") "golang"))
    "Misc"
    (("m" (hot-expand "<s" "mermaid :file chart.png") "mermaid")
     ("u" (hot-expand "<s" "plantuml :file chart.png") "plantuml")
     ("Y" (hot-expand "<s" "ipython :session :exports both :results raw drawer\n$0") "ipython")
     ("P" (progn
          (insert "#+HEADERS: :results output :exports both :shebang \"#!/usr/bin/env perl\"\n")
          (hot-expand "<s" "perl")) "Perl tangled")
     ("<" self-insert-command "ins"))))
  :bind (("C-c a" . org-agenda)
       ("C-c b" . org-switchb)
       ("C-c x" . org-capture)
       :map org-mode-map
       ("<" . (lambda ()
             "Insert org template."
             (interactive)
             (if (or (region-active-p) (looking-back "^\s*" 1))
                (org-hydra/body)
               (self-insert-command 1)))))
  :hook (((org-babel-after-execute org-mode) . org-redisplay-inline-images) ; display image
       (org-mode . (lambda ()
                 "Beautify org symbols."
                 (when centaur-prettify-org-symbols-alist
                   (if prettify-symbols-alist
                      (push centaur-prettify-org-symbols-alist prettify-symbols-alist)
                     (setq prettify-symbols-alist centaur-prettify-org-symbols-alist)))
                 (prettify-symbols-mode 1)))
       (org-indent-mode . (lambda()
                     (diminish 'org-indent-mode)
                     ;; HACK: Prevent text moving around while using brackets
                     ;; @see https://github.com/seagle0128/.emacs.d/issues/88
                     (make-variable-buffer-local 'show-paren-mode)
                     (setq show-paren-mode nil))))
  :config
  ;; For hydra
  (defun hot-expand (str &optional mod)
   "Expand org template.

STR is a structure template string recognised by org like <s. MOD is a
string with additional parameters to add the begin line of the
structure element. HEADER string includes more parameters that are
prepended to the element after the #+HEADER: tag."
   (let (text)
     (when (region-active-p)
       (setq text (buffer-substring (region-beginning) (region-end)))
       (delete-region (region-beginning) (region-end)))
     (insert str)
     (if (fboundp 'org-try-structure-completion)
        (org-try-structure-completion) ; < org 9
       (progn
         ;; New template expansion since org 9
         (require 'org-tempo nil t)
         (org-tempo-complete-tag)))
     (when mod (insert mod) (forward-line))
     (when text (insert text))))

  ;; To speed up startup, don't put to init section
  (setq org-modules nil                 ; Faster loading
      org-directory centaur-org-directory
      org-capture-templates
      `(("i" "Inbox" entry (file ,(concat org-directory "/inbox.org"))
        "*  %^{Title}\n%U\n%?")
       ("t" "Todo" entry (file ,(concat org-directory "/agenda.org"))
        "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
       ("w" "Work" entry (file+headline ,(concat org-directory "/work.org") ,(format-time-string "[%Y-%m-%d %a]"))
        "**  %^{Title}\n%U\n%?")
       )

      org-todo-keywords
      '((sequence "TODO(t)" "|" "DOING(i@/!)" "HANGUP(h@/!)" "InReview(r@/!)" "|" "DONE(d@/!)" "CANCEL(c@/!)")
       (sequence "⚑(T)" "🏴(I)" "❓(H)" "|" "✔(D)" "✘(C)"))
      org-todo-keyword-faces '(("HANGUP"    . (:inherit (bold warning org-todo)))
                       ("InReview"    . (:inherit (bold success font-lock-doc-face org-todo)))
                       ("DOING"     . (:inherit (bold font-lock-constant-face org-todo)))
                       ("CANCEL"    . (:inherit (bold error org-todo)))
                       )
      org-priority-faces '((?A . error)
                    (?B . warning)
                    (?C . success))

      ;; Agenda styling
      org-agenda-files (list centaur-org-directory)
      org-agenda-block-separator ?─
      org-agenda-time-grid
      '((daily today require-timed)
       (800 1000 1200 1400 1600 1800 2000)
       " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
      org-agenda-current-time-string
      "⭠ now ─────────────────────────────────────────────────"

      org-tags-column -80
      ;; org-log-done 'time
      ;; org-log-repeat 'time
      org-log-into-drawer t
      org-catch-invisible-edits 'smart
      org-startup-indented t
      org-agenda-sticky t
      org-pretty-entities nil
      org-hide-emphasis-markers t)

  ;; Add new template
  (add-to-list 'org-structure-template-alist '("n" . "note"))

  ;; Use embedded webkit browser if possible
  (when (and (featurep 'xwidget-internal) (display-graphic-p))
   (push '("\\.\\(x?html?\\|pdf\\)\\'"
         .
         (lambda (file _link)
           (centaur-webkit-browse-url (concat "file://" file) t)))
       org-file-apps))

  (setq org-file-apps
      '((auto-mode . emacs)
       (directory       . emacs)
       ("\\.docx\\'" . default)
       ("\\.doc\\'" . default)
       ("\\.xls\\'" . default)
       ("\\.xlsx\\'" . default)
       ("\\.ppt\\'" . default)
       ("\\.pptx\\'" . default)
       ("\\.drawio\\'" . default)
       ("\\.djvu\\'" . default)
       ("\\.png\\'"     . default)
       ("\\.mm\\'"      . default)
       ("\\.x?html?\\'" . default)
       ("\\.pdf\\'"     . default)
       ("\\.md\\'"      . emacs)
       ;; ("\\.gif\\'"     . my-func/open-and-play-gif-image)
          ("\\.svg\\'"     . default)
          )
        )

  ;; Add md/gfm backends
  (add-to-list 'org-export-backends 'md)
  (use-package ox-gfm
    :init (add-to-list 'org-export-backends 'gfm))

  ;; Prettify UI
  ;; (use-package org-modern
  ;;   :hook ((org-mode . org-modern-mode)
  ;;          (org-agenda-finalize . org-modern-agenda)
  ;;          (org-modern-mode . (lambda ()
  ;;                               "Adapt `org-modern-mode'."
  ;;                               ;; Disable Prettify Symbols mode
  ;;                               (setq prettify-symbols-alist nil)
  ;;                               (prettify-symbols-mode -1)))))

  ;; Babel
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t)

  (defconst load-language-alist
    '((emacs-lisp . t)
      (perl       . t)
      (python     . t)
      (ruby       . t)
      (js         . t)
      (css        . t)
      (sass       . t)
      (C          . t)
      (java       . t)
      (shell      . t)
      (plantuml   . t))
    "Alist of org ob languages.")

  (setq org-image-actual-width '(300))
  (setq org-export-with-sub-superscripts nil)
  (setq org-footnote-section "注")
  (setq org-log-refile t)
  (setq org-refile-allow-creating-parent-nodes 'confirm)

  (use-package ob-go
    :init (cl-pushnew '(go . t) load-language-alist))

  (use-package ob-powershell
    :init (cl-pushnew '(powershell . t) load-language-alist))

  (use-package ob-rust
    :init (cl-pushnew '(rust . t) load-language-alist))

  ;; Install: npm install -g @mermaid-js/mermaid-cli
  (use-package ob-mermaid
    :init (cl-pushnew '(mermaid . t) load-language-alist))

  (org-babel-do-load-languages 'org-babel-load-languages
                               load-language-alist)

  (use-package org-rich-yank
    :bind (:map org-mode-map
           ("C-M-y" . org-rich-yank)))

  ;; Table of contents
  (use-package toc-org
    :hook (org-mode . toc-org-mode))

  ;; Export text/html MIME emails
  (use-package org-mime
    :bind (:map message-mode-map
           ("C-c M-o" . org-mime-htmlize)
           :map org-mode-map
           ("C-c M-o" . org-mime-org-buffer-htmlize)))

  ;; Auto-toggle Org LaTeX fragments
  (use-package org-fragtog
    :diminish
    :hook (org-mode . org-fragtog-mode))

  ;; Preview
  (use-package org-preview-html
    :diminish
    :bind (:map org-mode-map
           ("C-c C-h" . org-preview-html-mode))
    :init (when (and (featurep 'xwidget-internal) (display-graphic-p))
            (setq org-preview-html-viewer 'xwidget)))

  ;; Presentation
  (use-package org-tree-slide
    :diminish
    :functions (org-display-inline-images
                org-remove-inline-images)
    :bind (:map org-mode-map
           ("s-<f7>" . org-tree-slide-mode)
           :map org-tree-slide-mode-map
           ("<left>" . org-tree-slide-move-previous-tree)
           ("<right>" . org-tree-slide-move-next-tree)
           ("S-SPC" . org-tree-slide-move-previous-tree)
           ("SPC" . org-tree-slide-move-next-tree))
    :hook ((org-tree-slide-play . (lambda ()
                                    (text-scale-increase 4)
                                    (org-display-inline-images)
                                    (read-only-mode 1)))
           (org-tree-slide-stop . (lambda ()
                                    (text-scale-increase 0)
                                    (org-remove-inline-images)
                                    (read-only-mode -1))))
    :init (setq org-tree-slide-header nil
                org-tree-slide-slide-in-effect t
                org-tree-slide-heading-emphasis nil
                org-tree-slide-cursor-init t
                org-tree-slide-modeline-display 'outside
                org-tree-slide-skip-done nil
                org-tree-slide-skip-comments t
                org-tree-slide-skip-outline-level 3))

  ;; Pomodoro
  (use-package org-pomodoro
    :custom-face
    (org-pomodoro-mode-line ((t (:inherit warning))))
    (org-pomodoro-mode-line-overtime ((t (:inherit error))))
    (org-pomodoro-mode-line-break ((t (:inherit success))))
    :bind (:map org-mode-map
           ("C-c C-x m" . org-pomodoro))
    :config
    (add-hook 'org-pomodoro-finished-hook
              (lambda ()
                (org-notify "A pomodoro is finished, take a break !!!")
                ))
    (add-hook 'org-pomodoro-short-break-finished-hook
              (lambda ()
                (org-notify "A short break done, ready a new pomodoro !!!")
                ))
    (add-hook 'org-pomodoro-long-break-finished-hook
              (lambda ()
                (org-notify "A long break done, ready a new pomodoro !!!")
                ))
    :init
    (with-eval-after-load 'org-agenda
      (bind-keys :map org-agenda-mode-map
        ("K" . org-pomodoro)
        ("C-c C-x m" . org-pomodoro)))))

;;org-download配置
(use-package org-download
  :hook ((org-mode dired-mode) . org-download-enable)
  :config
  (setq org-download-method 'directory)
  (setq-default org-download-image-dir "~/OneDrive/Notes/Attached/img/")
  (setq org-download-heading-lvl 'nil)
  (setq org-download-image-org-width 600)
  (setq org-download-screenshot-method "powershell -c Add-Type -AssemblyName System.Windows.Forms;$image = [Windows.Forms.Clipboard]::GetImage();$image.Save('%s', [System.Drawing.Imaging.ImageFormat]::Png)")
)

;;pandoc配置
(use-package ox-pandoc
  :after ox
  :init
  (add-to-list 'org-export-backends 'pandoc)
  :config
  (setq org-pandoc-options
     '((standalone . t)
     (mathjax . t)
     (variable . "revealjs-url=https://revealjs.com")))
  ;; default options for all output formats
  (setq org-pandoc-options '((standalone . t)))
  ;; special settings for beamer-pdf and latex-pdf exporters
  (setq org-pandoc-options-for-beamer-pdf '((pdf-engine . "xelatex")))
  (setq org-pandoc-options-for-latex-pdf '((pdf-engine . "xelatex")))
  ;; special extensions for markdown_github output
  (setq org-pandoc-format-extensions '(markdown_github+pipe_tables+raw_html))
  )

;; Org-transclusion lets you insert a copy of text content via a file link or ID link within an Org file.
(use-package org-transclusion
  :after org
  :config
  ;; 绑定 F12 键到 org-transclusion-add
  (global-set-key (kbd "<f12>") #'org-transclusion-add)
  )

(provide 'init-org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
