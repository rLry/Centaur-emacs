;;;;个人定义函数及快捷键
(add-hook 'org-mode-hook
            (lambda ()
              (setq-local time-stamp-active t
                          time-stamp-line-limit 18
                          time-stamp-start "^#\\+LAST_MODIFIED: [ \t]*"
                          time-stamp-end "$"
                          time-stamp-format "\[%Y-%m-%d %a %H:%M:%S\]")
              (add-hook 'before-save-hook 'time-stamp nil 'local)))

;; 一键copy 整行的内容到剪切板
(defun my/copy-line()
  (interactive)
  (clipboard-kill-region (point-at-bol) (point-at-eol))
  )

(global-set-key (kbd "C-c q c") 'my/copy-line)

;; 一键删除当前光标到行尾的内容
(defun my/kill-line()
  (interactive)
  (kill-region (point) (point-at-eol))
  )

(global-set-key (kbd "C-c q k") 'my/copy-line)

(global-set-key (kbd "C-c q b") 'org-mark-ring-goto)

(global-set-key (kbd "C-c R") 'restart-emacs)

;; kill buffer
(defun my/kill-all-buffer ()
  "Kill all buffer."
  (interactive)
  (dolist (buffer (buffer-list)) (kill-buffer buffer)))

(defun my/kill-other-buffer ()
  "Close all of other buffer."
  (interactive)
  (dolist (buffer (delq (current-buffer) (buffer-list))) (kill-buffer buffer)))

;; Word count
;; https://emacs-china.org/t/advance-words-count-el/2562/16
(defun my/wc-non-ascii (&optional start end)
  "count non-ascii"
  (interactive)
  (let ((start (if mark-active (region-beginning) (point-min)))
    (end (if mark-active (region-end) (point-max))))
   (save-excursion
    (save-restriction
    (narrow-to-region start end)
    (goto-char start)
    (message "lines: %3d non ascii words: %3d chars: %3d"
     (count-lines start end)
     (count-matches "[^[:ascii:]]")
     (- end start))))))
(global-set-key (kbd "C-c q w") 'my/wc-non-ascii)

;; 延续centaur-org-directory设定，将其子目录都加入org-agenda-files
(defun update-org-agenda-files ()
  "Update `org-agenda-files` to include all Org files in `centaur-org-directory` and its subdirectories."
  (let ((org-directory (expand-file-name centaur-org-directory)))
    (setq org-agenda-files
          (directory-files-recursively org-directory "\\.org$"))))

(defun advice-org-agenda-update-files (&rest _)
  "Advice function to update `org-agenda-files` before `org-agenda` is called."
  (update-org-agenda-files))

;; 为 org-agenda 函数添加 advice，每次调用 org-agenda 时，org-agenda-files 变量都会被重新设置。
(advice-add 'org-agenda :before #'advice-org-agenda-update-files)

;; 记账
(use-package beancount
  :demand
  :load-path "site-lisp/beancount-mode/"
  :mode
  ("\\.bean\\(?:count\\)?\\'" . beancount-mode)
  :config
  (setq beancount-accounts-files
        (directory-files "~/OneDrive/Documents/Beancount/"
                         'full
                         (rx ".bean" eos)))
)

; Anki
(use-package anki-editor
  :config
  (setq anki-editor-create-decks t ;; Allow anki-editor to create a new deck if it doesn't exist
        anki-editor-org-tags-as-anki-tags t)

  (defun anki-editor-cloze-region-auto-incr (&optional arg)
    "Cloze region without hint and increase card number."
    (interactive)
    (anki-editor-cloze-region my-anki-editor-cloze-number "")
    (setq my-anki-editor-cloze-number (1+ my-anki-editor-cloze-number))
    (forward-sexp))
  (defun anki-editor-cloze-region-dont-incr (&optional arg)
    "Cloze region without hint using the previous card number."
    (interactive)
    (anki-editor-cloze-region (1- my-anki-editor-cloze-number) "")
    (forward-sexp))
  (defun anki-editor-reset-cloze-number (&optional arg)
    "Reset cloze number to ARG or 1"
    (interactive)
    (setq my-anki-editor-cloze-number (or arg 1)))
  (defun anki-editor-push-tree ()
    "Push all notes under a tree."
    (interactive)
    (anki-editor-push-notes '(4))
    (anki-editor-reset-cloze-number))
  ;; Initialize
  (anki-editor-reset-cloze-number)
)

;; d2
(use-package d2-mode
  :config
  (setq d2-output-format ".png")
  (setq d2-tmp-dir "~/.emacs.d/tmp/")
    ;; Enable d2-mode for d2 files
  (add-to-list 'auto-mode-alist '("\\.d2\\'" . d2-mode))
)
;; typst
(use-package typst-ts-mode
  :demand
  :load-path "site-lisp/typst-ts-mode/"
  :custom
  ;; don't add "--open" if you'd like `watch` to be an error detector
  (typst-ts-mode-watch-options "--open")
  ;; experimental settings (I'm the main dev, so I enable these)
  (typst-ts-mode-enable-raw-blocks-highlight t)
  (typst-ts-mode-highlight-raw-blocks-at-startup t))
