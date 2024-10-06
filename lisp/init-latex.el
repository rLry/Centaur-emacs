;;; init-latex.el --- LaTeX   -*- lexical-binding: t; -*-

;; Copyright (C) 2024  rLry
;; Author: rLry <Liuryme@outlook.com>


(use-package org
  :config
  (setq org-preview-latex-default-process 'dvisvgm)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2)))

(use-package tex
  :ensure nil
  :mode ("\\.tex\\'" . LaTeX-mode)
  :init
  (add-to-list 'display-buffer-alist '((derived-mode . LaTeX-mode)
                                       (display-buffer-in-tab)
                                       (tab-name . "Edit") (tab-group . "Edit")
                                       (select . t)))
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-save-query nil)
  (setq TeX-electric-sub-and-superscript t)
  (setq TeX-auto-local ".auctex-auto")
  (setq TeX-style-local ".auctex-style")
  (setq TeX-source-correlate-mode t)
  (setq TeX-source-correlate-method 'synctex)
  (setq TeX-source-correlate-start-server nil)
  (setq TeX-command-default "XeLaTeX")
  (setq TeX-show-compilation t)
  ;pdf反向搜索
  (setq TeX-PDF-mode t)
  (setq TeX-source-correlate-mode t) ;; 编译后开启正反向搜索
  (setq TeX-source-correlate-method 'synctex) ;; 正反向搜索的执行方式
  (setq TeX-source-correlate-start-server t) ;; 不再询问是否开启服务器以执行反向搜索

  (setq-default TeX-master t)
  (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
  (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Tools"))
  (add-to-list 'TeX-view-program-list '("PDF Tools" TeX-pdf-tools-sync-view))
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  )

;;AUCTEX配置
(use-package auctex
  :defer t
  :ensure t)

(use-package auctex-latexmk
  :hook (LaTeX-mode . auctex-latexmk-setup))

(use-package reftex
  :hook ((LaTeX-mode . turn-on-reftex)
         (reftex-toc-mode . menu-bar--visual-line-mode-enable))
  :config
  (setq reftex-toc-split-windows-horizontally t)
  (setq reftex-toc-split-windows-fraction 0.25))

(use-package cdlatex
  :ensure t
  :hook ((LaTeX-mode . turn-on-cdlatex)
         (org-mode . org-cdlatex-mode)))

(use-package ox-latex
  :ensure nil
  :after org
  :config
  (setq org-latex-src-block-backend 'minted)
  (setq org-latex-minted-options '(("breaklines" "true")
                                   ("breakanywhere" "true")))
  (setq org-export-preserve-breaks t) ;;导出自动加入换行符
  (setq org-latex-pdf-process
        '("xelatex -8bit --shell-escape  -interaction=nonstopmode -output-directory %o %f"
          "biblatex -shell-escape %b"
          "xelatex -8bit --shell-escape  -interaction=nonstopmode -output-directory %o %f"
          "xelatex -8bit --shell-escape  -interaction=nonstopmode -output-directory %o %f"
          "rm -fr %b.out %b.log %b.tex %b.brf %b.bbl %b.bcf %b.run.xml"))

  (setq org-latex-logfiles-extensions '("lof" "lot" "tex~" "tex" "aux" "idx" "log" "bcf"
                                        "out" "toc" "nav" "snm" "vrb" "dvi" "bcf"
                                        "fdb_latexmk" "blg" "brf" "fls" "run.xml" "xdv"
                                        "entoc" "ps" "spl" "bbl")) ;; 生成PDF后清理辅助文件

  (setq org-latex-prefer-user-labels t)
  ;; ox-latex样式
  (with-eval-after-load 'ox-latex
    (setq org-latex-listings t)
    (add-to-list 'org-latex-classes
                 '("elegantnote"
                   "\\documentclass[cn,hazy,blue,12pt,screen]{elegantnote}
                    [NO-DEFAULT-PACKAGES]
                    [PACKAGES]
                    [EXTRA]"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

    (add-to-list 'org-latex-classes
                 '("elegantpaper"
                   "\\documentclass[lang=cn,a4paper,newtx]{elegantpaper}
                    [NO-DEFAULT-PACKAGES]
                    [PACKAGES]
                    [EXTRA]"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

    (add-to-list 'org-latex-classes
                 '("elegantbook"
                   "\\documentclass[lang=cn,newtx,11pt,scheme=chinese]{elegantbook}
                    [NO-DEFAULT-PACKAGES]
                    [PACKAGES]
                    [EXTRA]"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
    ))

(provide 'init-latex)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-latex.el ends here