;;; init.el --- Configuration

;;; Commentary:

;;; Code:

;; Package repositories
;; default: ("gnu" . "http://elpa.gnu.org/packages/")
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
(when (not (package-installed-p 'use-package))
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))


;; Manually installed packages
(add-to-list 'load-path "~/.emacs.d/vendor")


;; Saving
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq auto-save-default nil)
(require 'recentf)
(setq recentf-save-file "~/.emacs.d/.recentf")
(recentf-mode 1)
(setq recentf-max-menu-items 40)


;; UI tweaks
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(show-paren-mode 1)
(global-linum-mode 1)
(global-hl-line-mode 1)
(set-face-attribute 'default nil :height 140)
(blink-cursor-mode 0)
;; full path in title bar
(setq-default frame-title-format "%b (%f)")
(fset 'yes-or-no-p 'y-or-n-p)
(setq column-number-mode t)


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


;; Themes
;;   darkburn-theme
;;   flatland-theme
;;   flatui-theme
;;   twilight-bright-theme
(use-package flatland-theme
  :ensure t)


;; Packages
(use-package auto-complete
  :ensure t
  :config
  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
  (ac-config-default))


(use-package auto-complete-rst
  :ensure t)


(use-package ac-cider
  :ensure t
  :init
  (add-hook 'cider-mode-hook 'ac-flyspell-workaround)
  (add-hook 'cider-mode-hook 'ac-cider-setup)
  (add-hook 'cider-repl-mode-hook 'ac-cider-setup)
  :config
  (add-to-list 'ac-modes 'cider-mode)
  (add-to-list 'ac-modes 'cider-repl-mode))


(use-package cider
  :ensure t
  :init
  (add-hook 'cider-mode-hook 'eldoc-mode)
  (setq cider-repl-pop-to-buffer-on-connect t)
  (setq cider-show-error-buffer t)
  (setq cider-auto-select-error-buffer t)
  (setq cider-repl-history-file "~/.emacs.d/cider-history")
  (setq cider-repl-wrap-history t)
  (add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojure-mode))
  (add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode)))


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
  (add-hook 'clojure-mode-hook 'enable-paredit-mode)
  (add-hook 'clojure-mode-hook 'subword-mode))


(use-package clojure-mode-extra-font-locking
  :ensure t)


(use-package elm-mode
  :ensure t
  :init
  (setq elm-format-on-save t)
  (setq elm-sort-imports-on-save t)
  (setq elm-tags-on-save t)
  (setq elm-tags-exclude-elm-stuff nil))


(use-package flycheck
  :ensure t
  :commands flycheck-mode
  :config
  (global-flycheck-mode)
  (setq flycheck-emacs-lisp-load-path 'inherit))


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


(use-package haskell-mode
  :ensure t
  :config
  (load "haskell-mode-autoloads")
  :init
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode))


(use-package ido
  :ensure t
  :config
  (ido-mode 1)
  (setq ido-enable-flex-matching t)
  (setq ido-use-filename-at-point nil)
  (setq ido-auto-merge-work-directories-length -1)
  (setq ido-use-virtual-buffers t)
  (setq ido-everywhere t)
  (global-set-key (kbd "C-x C-b") 'ibuffer))


(use-package intero
  :ensure t
  :init
  (add-hook 'haskell-mode-hook 'intero-mode))


(use-package ledger-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))
  :config
  (setq ledger-post-amount-alignment-column 66)
  (setq ledger-post-use-completion-engine :ido))


(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))


(use-package org
  :ensure t
  :bind
  (("C-c c" . org-capture))
  :preface
  (defconst org-file-path "~/Documents/code/diary")
  (defun org-file (fname)
    "Build an absolute path for an org-file.  FNAME is the file name."
    (format "%s/%s" org-file-path fname))
  :init
  (add-hook 'emacs-startup-hook
	    (lambda ()
	      (interactive)
	      (org-agenda nil "n")))
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
	org-agenda-files `(,(org-file "gtd.org")
			   ,(org-file "journal.org"))
	org-refile-targets `((org-agenda-files . (:maxlevel . 3))
			     (,(org-file "someday.org") . (:maxlevel . 3))))
  (setq-default org-capture-templates `(("t"
					 "Todo"
					 entry
					 (file+headline ,(org-file "gtd.org") "Tasks")
					 (file ,(org-file "templates/todo.txt")))
					("j"
					 "Journal entry"
					 entry
					 (file+datetree ,(org-file "journal.org"))
					 (file ,(org-file "templates/journal.txt")))
					("s"
					 "Someday"
					 entry
					 (file+olp ,(org-file "someday.org") "Backlog" "Other")
					 (file ,(org-file "templates/someday.txt")))
					("d"
					 "Daily review"
					 entry
					 (file+datetree ,(org-file "journal.org"))
					 (file ,(org-file "templates/daily_review.txt")))
					("w"
					 "Weekly review"
					 entry
					 (file+datetree ,(org-file "journal.org"))
					 (file ,(org-file "templates/weekly_review.txt"))))
		org-agenda-window-setup 'current-window
		org-agenda-dim-blocked-tasks t
		org-agenda-skip-deadline-if-done t
		org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled
		org-stuck-projects '("LEVEL=2&CATEGORY=\"Projects\"" ("TODO" "ACTIVE") nil "")))


(use-package paredit
  :ensure t
  :init
  (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           #'enable-paredit-mode)
  (add-hook 'cider-repl-mode-hook 'paredit-mode))


(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))


(use-package smex
  :ensure t
  :bind
  (("M-x" . smex)
   ;; ("M-x" . smex-major-mode-commands)
   ;; Vanilla M-x
   ("C-c C-c C-c" . execute-extended-command))
  :init
  (smex-initialize))


(use-package tomatinho
  :ensure t
  :bind
  (("<f12>" . tomatinho)))


(use-package yaml-mode
  :ensure t)


(provide 'init)
;;; init.el ends here
