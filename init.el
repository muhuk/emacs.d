;;; init.el --- Configuration

;;; Commentary:

;;; Code:

(defconst emacs-config-dir (file-name-directory (or load-file-name
                                                    buffer-file-name)))

;; Package repositories
;; default: ("gnu" . "http://elpa.gnu.org/packages/")
(require 'package)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
(when (not (package-installed-p 'use-package))
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;; Manually installed packages
(defconst vendor-path
  (concat emacs-config-dir "vendor/"))
(defconst vendor-plantuml-jar-path
  (concat vendor-path "plantuml.1.2020.15.jar"))
(defconst vendor-reveal-js-root
  (concat vendor-path "reveal.js-master-33bed47/"))
(add-to-list 'load-path vendor-path)
(let ((default-directory  vendor-path))
  (normal-top-level-add-subdirs-to-load-path))


;; Character Encoding
(set-language-environment 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)


;; Saving
(setq backup-directory-alist `(("." . ,(concat emacs-config-dir "backups"))))
(setq auto-save-default nil)
(require 'recentf)
(setq recentf-save-file (concat emacs-config-dir ".recentf"))
(setq recentf-max-menu-items 40)
(setq recentf-max-saved-items 40)
(recentf-mode 1)


;; UI tweaks
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(show-paren-mode 1)
;; instead of enabling line numbers for all modes
;; via (global-linum-mode 1), we enable only for
;; programming modes:
(add-hook 'prog-mode-hook 'linum-mode)
(global-hl-line-mode 1)
(set-face-attribute 'default nil :height 120)
(blink-cursor-mode 0)
;; full path in title bar
(setq-default frame-title-format "%b (%f)")
(fset 'yes-or-no-p 'y-or-n-p)
(setq column-number-mode t)


;; Keyboard
;;
;; Disable CMD-z minimizing Emacs:
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;; Bind Home & End keys only on MacOS.  This would work when using a
;; standard keyboard.
(when (string= system-type "darwin")
  (global-set-key (kbd "<home>") 'beginning-of-line)
  (global-set-key (kbd "<end>") 'end-of-line))


;; Formatting
(setq-default indent-tabs-mode nil)
(setq-default fill-column 78)


;; Date/Time
(setq-default calendar-week-start-day 1
	      display-time-mail-string ""
	      display-time-24hr-format t)
(setq display-time-day-and-date t
      holiday-christian-holidays nil
      holiday-islamic-holidays nil
      holiday-bahai-holidays nil
      holiday-hebrew-holidays nil
      holiday-oriental-holidays nil
      holiday-solar-holidays nil
      holiday-general-holidays nil)


;; Elisp / Scheme
;; eldoc-mode shows documentation in the minibuffer when writing code
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)


;; Custom
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Customizations.html
(setq custom-file (concat emacs-config-dir "custom.el"))
(when (file-exists-p custom-file)
  (load-file custom-file))


;; Switch Buffers
(defconst skippable-buffers (rx string-start
                                (or "*Backtrace*"
                                    "*cider"
                                    "*Completions*"
                                    "*Ibuffer*"
                                    "*Messages*"
                                    "*scratch*")))
(defun change-buffer-skipping (change-buffer)
  "Call CHANGE-BUFFER until current buffer is not in `skippable-buffers'."
  (let ((initial (current-buffer)))
    (funcall change-buffer)
    (let ((first-change (current-buffer)))
      (catch 'loop
        (while (string-match-p skippable-buffers (buffer-name))
          (funcall change-buffer)
          (when (eq (current-buffer) first-change)
            (switch-to-buffer initial)
            (throw 'loop t)))))))
(defun next-buffer-skipping ()
  "Variant of `next-buffer' that skips `skippable-buffers'."
  (interactive)
  (change-buffer-skipping 'next-buffer))
(defun previous-buffer-skipping ()
  "Variant of `previous-buffer' that skips `skippable-buffers'."
  (interactive)
  (change-buffer-skipping 'previous-buffer))
(global-set-key [remap next-buffer] 'next-buffer-skipping)
(global-set-key [remap previous-buffer] 'previous-buffer-skipping)


;; Start Server
(run-at-time "1 min" nil #'server-start)


;; IBuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)


;; Themes
;;   dracula-theme
;;   flatland-theme
(use-package dracula-theme
  :ensure t)


(use-package cargo
  :ensure t
  :init
  (add-hook 'rust-mode-hook #'cargo-minor-mode)
  (add-hook 'cargo-process-mode-hook #'visual-line-mode))


(use-package cider
  :ensure t
  :bind
  (("C-c C-x C-m" . cider-macroexpand-1)
   ("C-c C-t u" . cider-test-run-unit-tests))
  :init
  (defvar cider-test-unit-tests-include)
  (defvar cider-test-unit-tests-exclude)
  :config
  (setq cider-repl-history-file (concat emacs-config-dir "cider-history")
        cider-repl-wrap-history t
        cider-test-unit-tests-include '()
        cider-test-unit-tests-exclude '("integration"))
  (setq-default org-babel-clojure-backend 'cider
                org-babel-clojure-sync-nrepl-timeout 5000)
  (defun cider-test-run-unit-tests ()
    (interactive)
    (execute-kbd-macro (kbd (concat "C-u C-c C-t p "
                                    (mapconcat 'identity
                                               cider-test-unit-tests-include
                                               " ")
                                    " <RET> "
                                    (mapconcat 'identity
                                               cider-test-unit-tests-exclude
                                               " ")
                                    " <RET>")))))


(use-package cider-hydra
  :ensure t
  :config
  (add-hook 'clojure-mode-hook #'cider-hydra-mode))


(use-package clj-refactor
  :ensure t
  :init
  (add-hook 'clojure-mode-hook
	    (lambda ()
	      (clj-refactor-mode 1)
	      (yas-minor-mode 1) ; for adding require/use/import statements
	      ;; This choice of keybinding leaves cider-macroexpand-1 unbound
	      (cljr-add-keybindings-with-prefix "C-c C-m"))))


(use-package clojure-mode
  :ensure t
  :init
  (add-hook 'clojure-mode-hook #'enable-paredit-mode)
  (add-hook 'clojure-mode-hook #'subword-mode))


(use-package clojure-mode-extra-font-locking
  :ensure t)


(use-package company
  :ensure t
  :defer t
  :init
  (setq company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-tooltip-align-annotations t)
  :config
  (global-company-mode))


(use-package company-lsp
  :ensure t)


(use-package default-text-scale
  :ensure t
  :config
  (default-text-scale-mode))


(use-package edit-indirect
  :ensure t)


(use-package elm-mode
  :ensure t
  :init
  (setq elm-format-on-save t)
  (setq elm-sort-imports-on-save t)
  (setq elm-tags-on-save t)
  (setq elm-tags-exclude-elm-stuff nil))


(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(use-package flycheck
  :ensure t
  :commands flycheck-mode
  :config
  (global-flycheck-mode)
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (setq flycheck-global-modes '(not org-mode)))


(use-package flycheck-clojure
  :ensure t
  :config
  (eval-after-load 'flycheck '(flycheck-clojure-setup)))


(use-package flycheck-elm
  :ensure t
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-elm-setup))


(use-package flycheck-ghcmod
  :ensure t)


(use-package flycheck-haskell
  :ensure t
  :init
  (add-hook 'flycheck-mode-hook 'flycheck-haskell-setup))


(use-package flycheck-ledger
  :ensure t)


(use-package flycheck-rust
  :ensure t
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))


(use-package glsl-mode
  :ensure t)


(use-package golden-ratio-scroll-screen
  :ensure t
  :config
  (global-set-key [remap scroll-down-command] 'golden-ratio-scroll-screen-down)
  (global-set-key [remap scroll-up-command] 'golden-ratio-scroll-screen-up))

(use-package haskell-mode
  :ensure t
  :config
  (load "haskell-mode-autoloads")
  :init
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode))


(use-package htmlize
  :ensure t)


(use-package hydra
  :ensure t
  :config
  (mapc 'load (file-expand-wildcards (concat emacs-config-dir "include/hydra/*.inc"))))


(use-package ibuffer-vc
  :ensure t
  :config
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-vc-set-filter-groups-by-vc-root)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

(use-package ido
  :ensure t
  :config
  (setq ido-enable-flex-matching t)
  (setq ido-use-filename-at-point nil)
  (setq ido-auto-merge-work-directories-length -1)
  (setq ido-everywhere t)
  (ido-mode 1))


(use-package intero
  :ensure t
  :init
  (add-hook 'haskell-mode-hook 'intero-mode))


(use-package jinja2-mode
  :ensure t
  :mode "\\.html\\'")


(use-package ledger-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))
  :config
  (setq ledger-post-amount-alignment-column 66)
  (setq-default ledger-post-use-completion-engine :ido)
  (setq ledger-reports
	'(("monthly-expenses" "ledger --monthly --empty --collapse -f %(ledger-file) reg ^expenses")
	  ("details" "ledger -f %(ledger-file) -s --current --no-color --no-total bal")
	  ("overview" "ledger -f %(ledger-file) -s --current --real --no-color bal Assets or Liabilities")
	  ("bal" "ledger -f %(ledger-file) bal")
	  ("reg" "ledger -f %(ledger-file) reg")
	  ("payee" "ledger -f %(ledger-file) reg @%(payee)")
	  ("account" "ledger -f %(ledger-file) reg %(account)"))))


(use-package love-minor-mode
  :ensure t)


(use-package lsp-mode
  :ensure t
  :hook (scala-mode . lsp))


(use-package lsp-ui
  :ensure t)


(use-package lua-mode
  :ensure t)


(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  :init
  (let ((github-markdown-css
         (with-temp-buffer
           ;; See https://github.com/sindresorhus/github-markdown-css
           (insert-file-contents (concat vendor-path
                                         "markdown/github-markdown.css"))
           (buffer-string))))
    (setq markdown-command "pandoc")
    (setq markdown-xhtml-header-content
          (concat "<style>" github-markdown-css "</style>"))
    (setq markdown-xhtml-body-preamble "<div class=\"markdown-body\">")
    (setq markdown-xhtml-body-epilogue "</div>")))


(use-package org
  :ensure t
  :bind
  (("C-c c" . org-capture))
  :preface
  (defconst diary-path "~/Documents/diary")
  (defun diary-file (fname)
    "Build an absolute path for an org-file.  FNAME is the file name."
    (format "%s/%s" diary-path fname))
  (setq diary-enabled (file-exists-p diary-path))
  :init
  (add-hook 'org-mode-hook #'turn-on-visual-line-mode)
  (when diary-enabled
    (add-hook 'emacs-startup-hook
              (lambda ()
                (interactive)
                (org-agenda nil "A")))
    (add-hook 'org-mode-hook (lambda ()
                               (auto-revert-mode 1)
                               (require 'ob-clojure)))
    ;; http://orgmode.org/worg/org-hacks.html
    (defadvice org-archive-subtree (before
                                    add-inherited-tags-before-org-archive-subtree
                                    activate)
      "add inherited tags before org-archive-subtree"
      (org-set-tags-to (org-get-tags-at)))
    (setq org-log-done 'time
          org-ellipsis " ▼"
          org-enforce-todo-dependencies t
          org-enforce-todo-checkbox-dependencies t
          org-archive-location "archived.org::datetree/* Finished Tasks"
          org-agenda-files `(,(diary-file "calendar.org")
                             ,(diary-file "gtd.org")
                             ,(diary-file "journal.org"))
          org-modules (append org-modules '(org-habit))
          org-refile-targets `((org-agenda-files . (:maxlevel . 3))
                               (,(diary-file "someday.org") . (:maxlevel . 3)))
          org-habit-show-habits-only-for-today nil
          org-habit-graph-column 70
          org-habit-preceding-days 35
          org-habit-following-days 14
          org-log-into-drawer t)
    (setq-default org-capture-templates `(("t"
                                           "Todo"
                                           entry
                                           (file+headline ,(diary-file "gtd.org") "Tasks")
                                           (file ,(diary-file "templates/todo.txt")))
                                          ("j"
                                           "Journal entry"
                                           entry
                                           (file+datetree ,(diary-file "journal.org"))
                                           (file ,(diary-file "templates/journal.txt")))
                                          ("s"
                                           "Someday"
                                           entry
                                           (file+olp ,(diary-file "someday.org") "Backlog")
                                           (file ,(diary-file "templates/someday.txt")))
                                          ("d"
                                           "Daily review"
                                           entry
                                           (file+datetree ,(diary-file "journal.org"))
                                           (file ,(diary-file "templates/daily_review.txt")))
                                          ("w"
                                           "Weekly review"
                                           entry
                                           (file+datetree ,(diary-file "journal.org"))
                                           (file ,(diary-file "templates/weekly_review.txt"))))
                  org-agenda-window-setup 'current-window
                  org-agenda-dim-blocked-tasks t
                  org-agenda-skip-deadline-if-done t
                  org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled
                  org-agenda-custom-commands '(("A" "Customised agenda"
                                                ((agenda "" ((org-agenda-span 1)))
                                                 (todo "ACTIVE")
                                                 (todo "TODO"))))
                  org-stuck-projects '("LEVEL=2&CATEGORY=\"Projects\"" ("TODO" "ACTIVE") nil "")
                  org-src-fontify-natively t
                  org-html-htmlize-output-type 'css
                  org-plantuml-jar-path vendor-plantuml-jar-path))
  :config
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((clojure . t)
                                 (emacs-lisp . t)
                                 (plantuml . t)
                                 (shell . t))))


(use-package org-bullets
  :ensure t
  :commands org-bullets-mode
  :hook (org-mode . org-bullets-mode))


(use-package org-re-reveal
  :ensure t
  :config
  (setq-default org-re-reveal-root vendor-reveal-js-root))


(use-package paredit
  :ensure t
  :init
  (autoload 'enable-paredit-mode "paredit"
    "Turn on pseudo-structural editing of Lisp code." t)
  (add-hook 'emacs-lisp-mode-hook        #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook              #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook              #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook  #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook            #'enable-paredit-mode)
  (add-hook 'cider-repl-mode-hook        #'paredit-mode))


(use-package plantuml-mode
  :ensure t
  :init
  (setq plantuml-jar-path vendor-plantuml-jar-path
        plantuml-default-exec-mode 'jar))


;; Python tweaks
(setq python-shell-interpreter "python3")
(setq flycheck-python-pycompile-executable "python3"
      flycheck-python-pylint-executable "python3"
      flycheck-python-flake8-executable "python3")


;; Add these to toolchain:
;;
;;     rustup component add rust-src
;;     cargo +nightly install racer
;;
(use-package racer
  :ensure t
  :init
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode)
  :config
  (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common))


(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))


;; Add these to toolchain:
;;
;;     rustup component add clippy rustfmt
;;
(use-package rust-mode
  :ensure t
  :init
  (add-hook 'rust-mode-hook #'flycheck-rust-setup)
  :config
  (setq rust-format-on-save t))


(use-package sbt-mode
  :ensure t
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

(use-package scala-mode
  :ensure t
  :mode "\\.s\\(cala\\|bt\\)$")


(use-package seq
  :ensure t)


(use-package smex
  :ensure t
  :bind
  (("M-x" . smex))
  :config
  (smex-initialize))


(use-package tomatinho
  :ensure t
  :bind
  (("<f12>" . tomatinho)))


(use-package toml-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)


(use-package which-key
  :ensure t
  :config
  (setq which-key-popup-type 'minibuffer)
  (which-key-mode))


(provide 'init)
;;; init.el ends here
