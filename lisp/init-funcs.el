;; init-funcs.el --- Define functions.	-*- lexical-binding: t -*-

;; Copyright (C) 2018-2024 Vincent Zhang

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
;; Define some useful functions.
;;

;;; Code:

(require 'cl-lib)

;; Suppress warnings
(eval-when-compile
  (require 'init-const)
  (require 'init-custom))

(defvar socks-noproxy)
(defvar socks-server)

(declare-function browse-url-interactive-arg "browse-url")
(declare-function chart-bar-quickie "chart")
(declare-function consult-theme "ext:consult")
(declare-function nerd-icons-install-fonts "ext:nerd-icons")
(declare-function xwidget-buffer "xwidget")
(declare-function xwidget-webkit-current-session "xwidget")

;;Update a field (#+LAST_MODIFIED) at save
(setq time-stamp-active t
     time-stamp-line-limit 18
     time-stamp-start "#\\+LAST_MODIFIED:[ \t]*"
     time-stamp-end "$"
     time-stamp-format "\[%Y-%m-%d %a %H:%M:%S\]")
(add-hook 'before-save-hook 'time-stamp nil)

(add-hook 'org-insert-heading-hook
         (lambda () (org-set-property "CREATED" (format-time-string "%Y-%m-%d %a %H:%M:%S"))))

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

;; Font
(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

;; Dos2Unix/Unix2Dos
(defun dos2unix ()
  "Convert the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun unix2dos ()
  "Convert the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

(defun delete-dos-eol ()
  "Delete `' characters in current region or buffer.
Same as '`replace-string' `C-q' `C-m' `RET' `RET''."
  (interactive)
  (save-excursion
    (when (region-active-p)
      (narrow-to-region (region-beginning) (region-end)))
    (goto-char (point-min))
    (let ((count 0))
      (while (search-forward "\r" nil t)
        (replace-match "" nil t)
        (setq count (1+ count)))
      (message "Removed %d " count))
    (widen)))

;; File and buffer
(defun revert-this-buffer ()
  "Revert the current buffer."
  (interactive)
  (unless (minibuffer-window-active-p (selected-window))
    (revert-buffer t t)
    (message "Reverted this buffer")))

(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(defun rename-this-file (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

(defun browse-this-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))

(defun copy-file-name ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (if filename
        (progn
          (kill-new filename)
          (message "Copied '%s'" filename))
      (warn "Current buffer is not attached to a file!"))))

(defun create-scratch-buffer ()
  "Create a scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

(defun save-buffer-as-utf8 (coding-system)
  "Revert a buffer with `CODING-SYSTEM' and save as UTF-8."
  (interactive "zCoding system for visited file (default nil):")
  (revert-buffer-with-coding-system coding-system)
  (set-buffer-file-coding-system 'utf-8)
  (save-buffer))

(defun save-buffer-gbk-as-utf8 ()
  "Revert a buffer with GBK and save as UTF-8."
  (interactive)
  (save-buffer-as-utf8 'gbk))

(defun selected-region-or-symbol-at-point ()
  "Return the selected region, otherwise return the symbol at point."
  (if (region-active-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (thing-at-point 'symbol t)))

;; Browse URL
(defun centaur-browse-url (url)
  "Open URL using a configurable method.
See `browse-url' for more details."
  (interactive (progn
                 (require 'browse-url)
                 (browse-url-interactive-arg "URL: ")))
  (if (and (featurep 'xwidget-internal) (display-graphic-p))
      (centaur-webkit-browse-url url t)
    (browse-url url)))

(defun centaur-webkit-browse-url (url &optional pop-buffer new-session)
  "Browse URL with xwidget-webkit' and switch or pop to the buffer.
  POP-BUFFER specifies whether to pop to the buffer.
  NEW-SESSION specifies whether to create a new xwidget-webkit session.
  Interactively, URL defaults to the string looking like a url around point."
  (interactive (progn
                 (require 'browse-url)
                 (browse-url-interactive-arg "URL: ")))
  (xwidget-webkit-browse-url url new-session)
  (let ((buf (xwidget-buffer (xwidget-webkit-current-session))))
    (when (buffer-live-p buf)
      (and (eq buf (current-buffer)) (quit-window))
      (if pop-buffer
          (pop-to-buffer buf)
        (switch-to-buffer buf)))))

;; Mode line
(defun mode-line-height ()
  "Get the height of the mode-line."
  (- (elt (window-pixel-edges) 3)
     (elt (window-inside-pixel-edges) 3)
     (if (bound-and-true-p window-divider-mode)
         window-divider-default-bottom-width
       0)))

;; Reload configurations
(defun reload-init-file ()
  "Reload Emacs configurations."
  (interactive)
  (load user-init-file))
(defalias 'centaur-reload-init-file #'reload-init-file)

;; Browse the homepage
(defun browse-homepage ()
  "Browse the Github page of Centaur Emacs."
  (interactive)
  (browse-url centaur-homepage))

;; Open custom file
(defun find-custom-file()
  "Open custom files."
  (interactive)
  (unless (file-exists-p custom-file)
    (if (file-exists-p centaur-custom-example-file)
        (copy-file centaur-custom-example-file custom-file)
      (user-error "The file `%s' doesn't exist" centaur-custom-example-file)))
  (when (file-exists-p custom-file)
    (find-file custom-file))
  (when (file-exists-p centaur-custom-post-file)
    (find-file-other-window centaur-custom-post-file)))

;; Misc
(defun byte-compile-elpa ()
  "Compile packages in elpa directory. Useful if you switch Emacs versions."
  (interactive)
  (if (fboundp 'async-byte-recompile-directory)
      (async-byte-recompile-directory package-user-dir)
    (byte-recompile-directory package-user-dir 0 t)))

(defun byte-compile-site-lisp ()
  "Compile packages in site-lisp directory."
  (interactive)
  (let ((dir (locate-user-emacs-file "site-lisp")))
    (if (fboundp 'async-byte-recompile-directory)
        (async-byte-recompile-directory dir)
      (byte-recompile-directory dir 0 t))))

(defun native-compile-elpa ()
  "Native-compile packages in elpa directory."
  (interactive)
  (if (fboundp 'native-compile-async)
      (native-compile-async package-user-dir t)))

(defun native-compile-site-lisp ()
  "Native compile packages in site-lisp directory."
  (interactive)
  (let ((dir (locate-user-emacs-file "site-lisp")))
    (if (fboundp 'native-compile-async)
        (native-compile-async dir t))))

(defun icons-displayable-p ()
  "Return non-nil if icons are displayable."
  (and centaur-icon
       (or (featurep 'nerd-icons)
           (require 'nerd-icons nil t))))

(defun centaur-treesit-available-p ()
  "Check whether tree-sitter is available.
  Native tree-sitter is introduced since 29.1."
  (and centaur-tree-sitter
       (fboundp 'treesit-available-p)
       (treesit-available-p)))

(defun centaur-set-variable (variable value &optional no-save)
  "Set the VARIABLE to VALUE, and return VALUE.

  Save to option `custom-file' if NO-SAVE is nil."
  (customize-set-variable variable value)
  (when (and (not no-save)
             (file-writable-p custom-file))
    (with-temp-buffer
      (insert-file-contents custom-file)
      (goto-char (point-min))
      (while (re-search-forward
              (format "^[\t ]*[;]*[\t ]*(setq %s .*)" variable)
                               nil t)
  (replace-match (format "(setq %s '%s)" variable value) nil nil))
      (write-region nil nil custom-file)
      (message "Saved %s (%s) to %s" variable value custom-file))))

(defun too-long-file-p ()
  "Check whether the file is too long."
  (or (> (buffer-size) 500000)
      (and (fboundp 'buffer-line-statistics)
           (> (car (buffer-line-statistics)) 10000))))

(define-minor-mode centaur-read-mode
  "Minor Mode for better reading experience."
  :init-value nil
  :group centaur
  (if centaur-read-mode
      (progn
        (and (fboundp 'olivetti-mode) (olivetti-mode 1))
        (and (fboundp 'mixed-pitch-mode) (mixed-pitch-mode 1))
        (text-scale-set +1))
    (progn
      (and (fboundp 'olivetti-mode) (olivetti-mode -1))
      (and (fboundp 'mixed-pitch-mode) (mixed-pitch-mode -1))
      (text-scale-set 0))))

;; Pakcage repository (ELPA)
(defun set-package-archives (archives &optional refresh async no-save)
  "Set the package ARCHIVES (ELPA).

REFRESH is non-nil, will refresh archive contents.
ASYNC specifies whether to perform the downloads in the background.
Save to option `custom-file' if NO-SAVE is nil."
  (interactive
   (list
    (intern
     (completing-read "Select package archives: "
                      (mapcar #'car centaur-package-archives-alist)))))
  ;; Set option
  (centaur-set-variable 'centaur-package-archives archives no-save)

  ;; Refresh if need
  (and refresh (package-refresh-contents async))

  (message "Set package archives to `%s'" archives))
(defalias 'centaur-set-package-archives #'set-package-archives)

;; Refer to https://emacs-china.org/t/elpa/11192
(defun centaur-test-package-archives (&optional no-chart)
  "Test connection speed of all package archives and display on chart.

Not displaying the chart if NO-CHART is non-nil.
Return the fastest package archive."
  (interactive)

  (let* ((durations (mapcar
                     (lambda (pair)
                       (let ((url (concat (cdr (nth 2 (cdr pair)))
                                          "archive-contents"))
                             (start (current-time)))
                         (message "Fetching %s..." url)
                         (ignore-errors
                           (url-copy-file url null-device t))
                         (float-time (time-subtract (current-time) start))))
                     centaur-package-archives-alist))
         (fastest (car (nth (cl-position (apply #'min durations) durations)
                            centaur-package-archives-alist))))

    ;; Display on chart
    (when (and (not no-chart)
               (require 'chart nil t)
               (require 'url nil t))
      (chart-bar-quickie
       'vertical
       "Speed test for the ELPA mirrors"
       (mapcar (lambda (p) (symbol-name (car p))) centaur-package-archives-alist)
       "ELPA"
       (mapcar (lambda (d) (* 1e3 d)) durations) "ms"))

    (message "`%s' is the fastest package archive" fastest)

    ;; Return the fastest
    fastest))

(defun set-from-minibuffer (sym)
  "Set SYM value from minibuffer."
  (eval-expression
   (minibuffer-with-setup-hook
       (lambda ()
         (add-function :before-until (local 'eldoc-documentation-function)
           #'elisp-eldoc-documentation-function)
         (run-hooks 'eval-expression-minibuffer-setup-hook)
         (goto-char (minibuffer-prompt-end))
         (forward-char (length (format "(setq %S " sym))))
     (read-from-minibuffer
      "Eval: "
      (let ((sym-value (symbol-value sym)))
        (format
         (if (or (consp sym-value)
                 (and (symbolp sym-value)
                      (not (null sym-value))
                      (not (keywordp sym-value))))
             "(setq %s '%S)"
           "(setq %s %S)")
         sym sym-value))
      read-expression-map t
      'read-expression-history))))

;; WORKAROUND: fix blank screen issue on macOS.
(defun fix-fullscreen-cocoa ()
  "Address blank screen issue with child-frame in fullscreen.
This issue has been addressed in 28."
  (and sys/mac-cocoa-p
       (not emacs/>=28p)
       (bound-and-true-p ns-use-native-fullscreen)
       (setq ns-use-native-fullscreen nil)))



;; Update
(defun update-config ()
  "Update Centaur Emacs configurations to the latest version."
  (interactive)
  (let ((dir (expand-file-name user-emacs-directory)))
    (unless (file-exists-p dir)
      (user-error "\"%s\" doesn't exist" dir))

    (message "Updating configurations...")
    (cd dir)
    (shell-command "git pull")
    (message "Updating configurations...done")))
(defalias 'centaur-update-config #'update-config)

(defun update-packages ()
  "Refresh package contents and update all packages."
  (interactive)
  (message "Updating packages...")
  (package-upgrade-all)
  (message "Updating packages...done"))
(defalias 'centaur-update-packages #'update-packages)

(defun update-config-and-packages()
  "Update confgiurations and packages."
  (interactive)
  (update-config)
  (update-packages))
(defalias 'centaur-update #'update-config-and-packages)

(defun update-dotfiles ()
  "Update the dotfiles to the latest version."
  (interactive)
  (let ((dir (or (getenv "DOTFILES")
                 (expand-file-name "~/.dotfiles/"))))
    (if (file-exists-p dir)
        (progn
          (message "Updating dotfiles...")
          (cd dir)
          (shell-command "git pull")
          (message "Updating dotfiles...done"))
      (message "\"%s\" doesn't exist" dir))))
(defalias 'centaur-update-dotfiles #'update-dotfiles)

(defun update-org ()
  "Update Org files to the latest version."
  (interactive)
  (let ((dir (expand-file-name "~/org/")))
    (if (file-exists-p dir)
        (progn
          (message "Updating org files...")
          (cd dir)
          (shell-command "git pull")
          (message "Updating org files...done"))
      (message "\"%s\" doesn't exist" dir))))
(defalias 'centaur-update-org #'update-org)

(defun update-all()
  "Update dotfiles, org files, configurations and packages to the latest."
  (interactive)
  (update-org)
  (update-dotfiles)
  (update-config-and-packages))
(defalias 'centaur-update-all #'update-all)


;; Fonts
(defun centaur-install-fonts ()
  "Install necessary fonts."
  (interactive)
  (nerd-icons-install-fonts))




;; UI
(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")
(defun run-after-load-theme-hook (&rest _)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))
(advice-add #'load-theme :after #'run-after-load-theme-hook)

(defun childframe-workable-p ()
  "Whether childframe is workable."
  (not (or noninteractive
           emacs-basic-display
           (not (display-graphic-p)))))

(defun childframe-completion-workable-p ()
  "Whether childframe completion is workable."
  (and (eq centaur-completion-style 'childframe)
       (childframe-workable-p)))

(defun centaur--theme-name (theme)
  "Return internal THEME name."
  (or (alist-get theme centaur-theme-alist) theme 'doom-one))

(defun centaur-compatible-theme-p (theme)
  "Check if the THEME is compatible. THEME is a symbol."
  (or (memq theme '(auto random system))
      (string-prefix-p "doom" (symbol-name (centaur--theme-name theme)))))

(defun centaur-dark-theme-p ()
  "Check if the current theme is a dark theme."
  (eq (frame-parameter nil 'background-mode) 'dark))

(defun centaur-theme-enable-p (theme)
  "The THEME is enabled or not."
  (and theme
       (not (memq centaur-theme '(auto random system)))
       (memq (centaur--theme-name theme) custom-enabled-themes)))

(defun centaur--load-theme (theme)
  "Disable others and enable new THEME."
  (when-let ((theme (centaur--theme-name theme)))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme theme t)))

(defun centaur--load-system-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (centaur--load-theme (alist-get appearance centaur-system-themes)))

(defun centaur-load-random-theme ()
  "Load the random theme."
  (interactive)
  (let* ((themes (mapcar #'cdr centaur-theme-alist))
         (theme (nth (random (length themes)) themes)))
    (if (eq theme centaur-theme)
        (centaur-load-random-theme)
      (centaur--load-theme theme))))

(defun centaur-load-theme (theme &optional no-save)
  "Load color THEME. Save to option `custom-file' if NO-SAVE is nil."
  (interactive
   (list
    (intern
     (completing-read "Load theme: "
                      `(auto
                        random
                        system
                        ,@(mapcar #'car centaur-theme-alist))))))

  ;; Disable time-switching themes
  (when (fboundp #'circadian-activate-latest-theme)
    (cancel-function-timers #'circadian-activate-latest-theme))

  ;; Disable system theme
  (when (bound-and-true-p auto-dark-mode)
    (setq auto-dark--last-dark-mode-state 'unknown)
    (auto-dark-mode -1))

  (pcase theme
    ('auto
     ;; Time-switching themes
     (use-package circadian
       :ensure t
       :commands circadian-setup circadian-activate-latest-theme
       :custom (circadian-themes centaur-auto-themes)
       :init (circadian-setup)))
    ('system
     ;; System-appearance themes
     (use-package auto-dark
       :ensure t
       :diminish
       :commands auto-dark-mode
       :init
       (setq auto-dark-light-theme (alist-get 'light centaur-system-themes)
             auto-dark-dark-theme (alist-get 'dark centaur-system-themes))
       (when (and sys/macp (not (display-graphic-p)))
         (setq auto-dark-detection-method 'osascript))
       (auto-dark-mode 1)))
    ('random
     (centaur-load-random-theme))
    (_
     (centaur--load-theme theme)))

  ;; Set option
  (centaur-set-variable 'centaur-theme theme no-save))

(advice-add #'consult-theme :after
            (lambda (theme)
              "Save theme."
              (centaur-set-variable 'centaur-theme theme)))



;; Frame
(defvar centaur-frame--geometry nil)
(defun centaur-frame--save-geometry ()
  "Save current frame's geometry."
  (setq centaur-frame--geometry
        `((left   . ,(frame-parameter nil 'left))
          (top    . ,(frame-parameter nil 'top))
          (width  . ,(frame-parameter nil 'width))
          (height . ,(frame-parameter nil 'height))
          (fullscreen))))

(defun centaur-frame--fullscreen-p ()
  "Return Non-nil if the frame is fullscreen."
  (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth)))

(defun centaur-frame-maximize ()
  "Maximize the frame."
  (interactive)
  (centaur-frame--save-geometry)
  (unless (eq (frame-parameter nil 'fullscreen) 'maximized)
    (set-frame-parameter nil 'fullscreen 'maximized)))

(defun centaur-frame-restore ()
  "Restore the frame's size and position."
  (interactive)
  (modify-frame-parameters nil centaur-frame--geometry))

(defun centaur-frame-left-half ()
  "Put the frame to the left-half."
  (interactive)
  (unless (centaur-frame--fullscreen-p)
    (centaur-frame--save-geometry)
    (let* ((attr (frame-monitor-workarea))
           (width (- (/ (nth 2 attr) 2) 20))
           (height (- (nth 3 attr) 30))
           (left (nth 0 attr))
           (top (nth 1 attr)))
      (set-frame-parameter nil 'fullscreen nil)
      (set-frame-position nil left top)
      (set-frame-size nil width height t))))

(defun centaur-frame-right-half ()
  "Put the frame to the right-half."
  (interactive)
  (unless (centaur-frame--fullscreen-p)
    (centaur-frame--save-geometry)
    (let* ((attr (frame-monitor-workarea))
           (width (- (/ (nth 2 attr) 2) 20))
           (height (- (nth 3 attr) 30))
           (left (+ (nth 0 attr) width 20))
           (top (nth 1 attr)))
      (set-frame-parameter nil 'fullscreen nil)
      (set-frame-position nil left top)
      (set-frame-size nil width height t))))

(defun centaur-frame-top-half ()
  "Put the frame to the top-half."
  (interactive)
  (unless (centaur-frame--fullscreen-p)
    (centaur-frame--save-geometry)
    (let* ((attr (frame-monitor-workarea))
           (width (- (nth 2 attr) 20))
           (height (- (/ (nth 3 attr) 2) 30))
           (left (nth 0 attr))
           (top (nth 1 attr)))
      (set-frame-parameter nil 'fullscreen nil)
      (set-frame-position nil left top)
      (set-frame-size nil width height t))))

(defun centaur-frame-bottom-half ()
  "Put the frame to the bottom-half."
  (interactive)
  (unless (centaur-frame--fullscreen-p)
    (centaur-frame--save-geometry)
    (let* ((attr (frame-monitor-workarea))
           (width (- (nth 2 attr) 20))
           (height (- (/ (nth 3 attr) 2) 30))
           (left (nth 0 attr))
           (top (+ (nth 1 attr) height 30)))
      (set-frame-parameter nil 'fullscreen nil)
      (set-frame-position nil left top)
      (set-frame-size nil width height t))))



;; Network Proxy
(defun show-http-proxy ()
  "Show HTTP/HTTPS proxy."
  (interactive)
  (if url-proxy-services
      (message "Current HTTP proxy is `%s'" centaur-proxy)
    (message "No HTTP proxy")))

(defun enable-http-proxy ()
  "Enable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services
        `(("http" . ,centaur-proxy)
          ("https" . ,centaur-proxy)
          ("no_proxy" . "^\\(localhost\\|192.168.*\\|10.*\\)")))
  (show-http-proxy))

(defun disable-http-proxy ()
  "Disable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services nil)
  (show-http-proxy))

(defun toggle-http-proxy ()
  "Toggle HTTP/HTTPS proxy."
  (interactive)
  (if (bound-and-true-p url-proxy-services)
      (disable-http-proxy)
    (enable-http-proxy)))

(defun show-socks-proxy ()
  "Show SOCKS proxy."
  (interactive)
  (if (bound-and-true-p socks-noproxy)
      (message "Current SOCKS%d proxy is %s:%s"
               (cadddr socks-server) (cadr socks-server) (caddr socks-server))
    (message "No SOCKS proxy")))

(defun enable-socks-proxy ()
  "Enable SOCKS proxy."
  (interactive)
  (require 'socks)
  (setq url-gateway-method 'socks
        socks-noproxy '("localhost"))
  (let* ((proxy (split-string centaur-socks-proxy ":"))
         (host (car proxy))
         (port (string-to-number (cadr proxy))))
    (setq socks-server `("Default server" ,host ,port 5)))
  (setenv "all_proxy" (concat "socks5://" centaur-socks-proxy))
  (show-socks-proxy))

(defun disable-socks-proxy ()
  "Disable SOCKS proxy."
  (interactive)
  (setq url-gateway-method 'native
        socks-noproxy nil
        socks-server nil)
  (setenv "all_proxy" "")
  (show-socks-proxy))

(defun toggle-socks-proxy ()
  "Toggle SOCKS proxy."
  (interactive)
  (if (bound-and-true-p socks-server)
      (disable-socks-proxy)
    (enable-socks-proxy)))

(defun enable-proxy ()
  "Enbale proxy."
  (interactive)
  (enable-http-proxy)
  (enable-socks-proxy))

(defun disable-proxy ()
  "Disable proxy."
  (interactive)
  (disable-http-proxy)
  (disable-socks-proxy))

(defun toggle-proxy ()
  "Toggle proxy."
  (interactive)
  (toggle-http-proxy)
  (toggle-socks-proxy))

(provide 'init-funcs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-funcs.el ends here
