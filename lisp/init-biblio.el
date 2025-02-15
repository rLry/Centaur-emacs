;;; init-biblio.el --- Tools for personal learning   -*- lexical-binding: t; -*-

;; Copyright (C) 2024  rLry
;; Author: rLry <Liuryme@outlook.com>

;; denote
;;; Notes
(use-package denote
  :bind (("C-c n n" . denote)
         ("C-c n d" . denote-date)
         ("C-c n t" . denote-type)
         ("C-c n s" . denote-subdirectory)
         ("C-c n g" . denote-signature)
         ("C-c n f" . denote-open-or-create)
         ("C-c n l" . denote-link-or-create)
         ("C-c n L" . denote-link)
         ("C-c n B" . denote-backlinks)
         ("C-c n j" . denote-journal-extras-new-or-existing-entry))
  :config
  (setq denote-directory (expand-file-name "~/OneDrive/Notes/"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-file-type nil)
  (setq denote-prompts '(title keywords subdirectory))
  (setq denote-allow-multi-word-keywords t)
  (setq denote-known-keywords '("history" "Ming"
                    "philosophy" "logic" "psychology"
                    "theology"
                    "code" "math" "emacs" "medicine"
                    "social" "politics" "economics" "law"
                    "literature" "language"
                    "art"
                    "clippings"
                    ))
  (setq denote-date-format nil)
  (setq denote-rename-no-confirm t)
  (setq denote-backlinks-show-context t) ;; backlink显示文本
                                        ; Better backlink display
  (setq denote-link-backlinks-display-buffer-action
    (quote ((display-buffer-reuse-window
       display-buffer-in-side-window)
      (inhibit-same-window . t)
      (side . bottom)
      (slot . 99)
      (window-height . 10))))
  (add-hook 'dired-mode-hook #'denote-dired-mode)
  )

(defun my/denote-info ()
  "Count number of Denote text files,keywords and attachments."
  (interactive)
  (let* ((all-files (length (denote-directory-files)))
		 (denote-files (length (denote-directory-files nil nil t)))
		 (attachments (- all-files denote-files))
		 (keywords (length (denote-keywords))))
   (message "%s Denote files (%s Attachments), %s Distinct Keywords."
		  denote-files attachments keywords)))

(use-package denote-explore
  :custom
  ;; Location of graph files
  (denote-explore-network-directory "~/OneDrive/Notes/Attached/graphs/")
  (denote-explore-network-filename "denote-network")
  ;; Output format
  (denote-explore-network-format 'graphviz)
  (denote-explore-network-graphviz-filetype "svg")
  ;; Exlude keywords or regex
  (denote-explore-network-keywords-ignore '("bib"))
)

(use-package org-noter
  :config
  (setq org-noter-notes-search-path '("~/OneDrive/Notes/Literature")) ;; 默认笔记路径
  (setq org-noter-auto-save-last-location t) ;; 自动保存上次阅读位置
  ;; (setq org-noter-insert-selected-text-inside-note t)
  ;; (setq org-noter-insert-note-no-questions t)
  ;; (setq org-noter-kill-frame-at-session-end t)
  (setq org-noter-max-short-selected-text-length 20) ;; 默认为 80
  )

(use-package org-noter-plus
  :demand
  :load-path "site-lisp/org-noter-plus/"
  :config
  (setq org-noter-plus-image-dir "~/OneDrive/Notes/Attached/img/") ;; Directory to store images extracted from pdf files
  )

(use-package djvu
  :ensure t
  :after org-noter-plus)

(use-package consult-notes
  :commands (consult-notes
            consult-notes-search-in-all-notes)
  :bind (("C-c n r" . consult-notes))
  :config
  (setq consult-notes-sources
       '(
         ("Org"               ?o "~/OneDrive/Notes/Org/")
         ("Slipbox"           ?s "~/OneDrive/Notes/Slipbox/")
         ("Literature"        ?l "~/OneDrive/Notes/Literature/")
         ("Novel"             ?n "~/OneDrive/Notes/Novel/")
         ("Journal"           ?j "~/OneDrive/Notes/Journal/")
         ("Work"              ?w "~/OneDrive/Notes/Work/")
         ("Clippings"         ?c "~/OneDrive/Notes/Clippings/")
         ))
  (when (locate-library "denote")
   (consult-notes-denote-mode))
  )

;; personal diary / journal
(use-package org-journal
  :ensure t
  :defer t
  :bind (("C-c n e" . org-journal-new-entry))
  :config
  (setq org-journal-dir "~/OneDrive/Notes/Journal/")
  (setq org-journal-date-format "%a, %b %d %Y")
  (setq org-journal-file-type 'yearly)
  (setq org-journal-file-format "%Y.org")

  ;; Kill journal buffer after saving buffer
  (defun my/org-journal-save-entry-and-exit()
   "Saves the buffer of the current day's entry and kills the window.
    Similar to org-capture like behavior"
   (interactive)
   (save-buffer)
   (kill-buffer-and-window))
  (define-key org-journal-mode-map (kbd "C-x C-s") 'my/org-journal-save-entry-and-exit)
  )

;; biblio
(use-package bibtex
  :custom
  (bibtex-align-at-equal-sign t)
  (bibtex-autokey-titleword-separator "-")
  (bibtex-autokey-year-title-separator "-")
  (bibtex-autokey-name-year-separator "-")
  (bibtex-autokey-name-case-convert-function 'capitalize)
  (bibtex-autokey-titleword-case-convert 'capitalize)
  (bibtex-autokey-titlewords 0)
  (bibtex-autokey-year-length 4)
  (bibtex-dialect 'biblatex)
  )

(with-eval-after-load 'oc
  (setq org-cite-global-bibliography '("~/OneDrive/Notes/Biblio/reference.bib")
        org-cite-csl-styles-dir "~/OneDrive/Notes/Biblio/styles"
        org-cite-csl-locales-dir "~/OneDrive/Notes/Biblio/locales"
        org-cite-export-processors '((xelatex biblatex)
                                     (beamer biblatex)
                                     (latex biblatex)
                                     (t csl))
       org-support-shift-select t))

(use-package citar
  :bind (("C-c n c" . citar-open)
        ("C-c n b" . citar-open-notes))
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup)
  :config
  (setq citar-bibliography  '("~/OneDrive/Notes/Biblio/reference.bib"))
  (setq citar-library-paths '("~/OneDrive/Library"))
  (setq citar-notes-paths   '("~/OneDrive/Notes/Literature"))
  (setq org-cite-insert-processor 'citar)
  (setq org-cite-follow-processor 'citar)
  (setq org-cite-activate-processor 'citar)
  (setq citar-at-point-function 'embark-act)
  (setq citar-library-file-extensions nil)
  (setq citar-citeproc-csl-styles-dir "~/OneDrive/Notes/Biblio/styles")
  (setq citar-citeproc-csl-locales-dir "~/OneDrive/Notes/Biblio/locales")
  (setq citar-format-reference-function 'citar-citeproc-format-reference)
  (setq citar-citeproc-format-reference "social-sciences-in-china.csl")
  (add-to-list 'citar-file-open-functions '("pdf" . citar-file-open-external))
  (setq citar-file-additional-files-separator "-")
  (setq citar-at-point-function 'embark-act)
  )

;; Integration of denote with citar
(use-package citar-denote
  :demand t ;; Ensure minor mode is loaded upon init
  :config
  (citar-denote-mode)
  :custom
  (citar-open-always-create-notes t)
  (citar-denote-subdir t)
  (citar-denote-signature nil)
  )

(use-package citar-embark
  :defer t
  :init
  (citar-embark-mode 1))

(use-package ebib
  :bind ("<f2>" . ebib)
  :config
  (setq ebib-use-timestamp t)
  (setq ebib-create-backups nil)  
  (setq ebib-bibtex-dialect 'biblatex)
  (setq ebib-citation-commands
       (quote ((org-mode (("cite" "\\cite{%K}"))))))
  (setq ebib-file-search-dirs  '("~/OneDrive/Library"))
  (setq ebib-preload-bib-files '("~/OneDrive/Notes/Biblio/reference.bib"))
  (setq ebib-notes-directory   '("~/OneDrive/Notes/Literature"))
  (setq ebib-file-associations '(("pdf" . embark-open-externally)))
  (setq ebib-reading-list-file "~/OneDrive/Notes/Org/read.org")
  (setq ebib-keywords "~/OneDrive/Notes/Biblio/ebib-keywords.txt")
  (setq ebib-keywords-save-on-exit 'always)
  (setq ebib-keywords-field-keep-sorted t)
  (setq ebib-index-default-sort '("Year" . ascend)) ;;Sort columns displayed in index buffer 对索引缓冲区中显示的列进行排序
  (setq ebib-citations-insert-multiple t) ;;Enable multiple citations 启用多个引用
  (setq ebib-filters-default-file "~/OneDrive/Notes/Biblio/ebib-filters") ;;
  )

;; (defun my-ebib-name-transform-function (key)
;;   "Generate a filename based on the year, title, and publisher of the entry."
;;   (let* ((year (ebib-get-field-value "year" key ebib--cur-db "XXXX" t))
;;          (title (ebib-get-field-value "title" key ebib--cur-db "No_Title" t))
;;          (publisher (ebib-get-field-value "publisher" key ebib--cur-db "No_Publisher" t))
;;          (journaltitle (ebib-get-field-value "journaltitle" key ebib--cur-db "No_Journal" t))
;;          (entrytype (ebib-get-field-value "=type=" key ebib--cur-db "misc" t)))
;;     (cond ((string= entrytype "Book")
;;            (format "%s-%s-%s" year title publisher))
;;           ((string= entrytype "Article")
;;            (format "%s-%s-%s" year title journaltitle))
;;           (t
;;            (format "%s-%s" year title)))))

(defun my-ebib-name-transform-function (key)
  "Generate a filename based on the year and title of the entry, with defaults for missing data."
  (let* ((year (ebib-get-field-value "year" key ebib--cur-db "XXXX" t))
         (title (ebib-get-field-value "title" key ebib--cur-db "No_Title" t)))
    (format "%s-%s" year title)))

(setq ebib-name-transform-function 'my-ebib-name-transform-function)

(use-package biblio
  :ensure t)

(use-package org-ql
  :ensure t
)

;; (use-package fsrs
;;   :load-path "site-lisp/lisp-fsrs/"
;;   :defer t)

;; (use-package org-srs
;;   :load-path "site-lisp/org-srs/"
;;   :defer t
;;   :hook (org-mode . org-srs-embed-overlay-mode)
;;   :bind (:map org-mode-map
;;          ("<f5>" . org-srs-review-rate-easy)
;;          ("<f6>" . org-srs-review-rate-good)
;;          ("<f7>" . org-srs-review-rate-hard)
;;          ("<f8>" . org-srs-review-rate-again)))

(provide 'init-biblio)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-biblio.el ends here
