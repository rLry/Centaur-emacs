;;; 个人定义函数及快捷键
;;Update a field (#+LAST_MODIFIED) at save
(setq time-stamp-active t
      time-stamp-start "#\\+LAST_MODIFIED:[ \t]*"
      time-stamp-end "$"
      time-stamp-format "\[%Y-%m-%d %a %H:%M:%S\]")
(add-hook 'before-save-hook 'time-stamp nil)

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

;; 删除光标以后的空行
(fset 'my/delete-empty-lines (kbd "M-x flush-lines RET ^\s-*$ RET"))

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

(defun my/count-org-words (directory)
  "Count words in all .org files in DIRECTORY and its subdirectories."
  (interactive "DDirectory: ")
  (let ((org-files (directory-files-recursively directory "\\.org$"))
        (english-word-count 0)
        (number-count 0)
        (chinese-character-count 0)
        (total-character-count 0)
        (total-words-numbers-chinese 0))
    (dolist (file org-files)
      (with-temp-buffer
        (insert-file-contents file)
        (setq total-character-count (+ total-character-count (buffer-size)))
        (goto-char (point-min))
        (while (re-search-forward "\\b\\([a-zA-Z]+\\)\\b" nil t)
          (setq english-word-count (1+ english-word-count)))
        (goto-char (point-min))
        (while (re-search-forward "\\b\\([0-9]+\\)\\b" nil t)
          (setq number-count (1+ number-count)))
        (goto-char (point-min))
        (while (re-search-forward "\\cC" nil t)
          (setq chinese-character-count (1+ chinese-character-count)))))
    (setq total-words-numbers-chinese (+ english-word-count
                                         number-count
                                         chinese-character-count))
    (message "English: %d, Numbers: %d, Chinese: %d, Total chinese-character: %d, Total characters: %d"
             english-word-count number-count chinese-character-count total-words-numbers-chinese total-character-count)))

;; ;; 延续centaur-org-directory设定，将其子目录都加入org-agenda-files
;; (defun update-org-agenda-files ()
;;   "Update `org-agenda-files` to include all Org files in `centaur-org-directory` and its subdirectories."
;;   (let ((org-directory (expand-file-name centaur-org-directory)))
;;     (setq org-agenda-files
;;           (directory-files-recursively org-directory "\\.org$"))))

;; (defun advice-org-agenda-update-files (&rest _)
;;   "Advice function to update `org-agenda-files` before `org-agenda` is called."
;;   (update-org-agenda-files))

;; ;; 为 org-agenda 函数添加 advice，每次调用 org-agenda 时，org-agenda-files 变量都会被重新设置。
;; (advice-add 'org-agenda :before #'advice-org-agenda-update-files)

(defun my/toggle-window-split ()
  "Vertical split shows more of each line, horizontal split shows more lines. This code toggles between them. It only works for frames with exactly two windows. "
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

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

;; 支持 Windows 从剪贴板复制图片
(defun my/insert-image-from-clipboard ()
  "Insert an image from the clipboard into the current org buffer."
  (interactive)
  (let* ((current-dir (file-name-directory buffer-file-name))
     (file-name-base (file-name-base buffer-file-name))
     (attach-dir (concat current-dir "attach/" file-name-base "/"))
     ;; (attach-dir (concat current-dir "attach/"  (file-name-nondirectory buffer-file-name) "/"))
     (image-file (concat attach-dir (format-time-string "%Y%m%d_%H%M%S") ".png")))
   ;; Ensure attach directory exists
   (unless (file-exists-p attach-dir)
   (make-directory attach-dir t))
   ;; Save the clipboard image to the attach directory
   (if (eq system-type 'windows-nt)
   (progn
    (shell-command (concat "powershell -command \"Add-Type -AssemblyName System.Windows.Forms; [System.Windows.Forms.Clipboard]::GetImage().Save('" image-file "', [System.Drawing.Imaging.ImageFormat]::Png)\""))
    ;; 转换格式, 从 c:/Users/Jack/Desktop/attact/test/clipboard.png 之类
    ;; 转换成 ./attach/test/clipboard.png
    (setq image-file (replace-regexp-in-string
              "^[^:]+:/.*\\(/attach/.*\\)" ".\\1" image-file))
    (when (eq major-mode 'org-mode)
     ;; 输入文件所在位置文本
     (insert (concat "[[file:" image-file "]]"))
     ;; 显示图片
     (org-display-inline-images))
     (when (eq major-mode 'markdown-mode)
     (insert (concat "![]" "(" image-file ")"))
     (markdown-display-inline-images))
     )
    (error "Unsupported OS"))
   ))
