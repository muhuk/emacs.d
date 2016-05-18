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


;; ELisp / Scheme
;; eldoc-mode shows documentation in the minibuffer when writing code
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)


;; Themes
;;   darkburn-theme
;;   flatland-theme
;;   flatui-theme
;;   planet-theme
;;   twilight-bright-theme
(use-package twilight-bright-theme
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


(use-package ac-haskell-process
  :ensure t
  :config
  (add-hook 'interactive-haskell-mode-hook 'ac-haskell-process-setup)
  (add-hook 'haskell-interactive-mode-hook 'ac-haskell-process-setup)
  (eval-after-load "auto-complete"
    '(add-to-list 'ac-modes 'haskell-interactive-mode)))


(use-package cider
  :ensure t
  :init
  (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
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


(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode))


(use-package flycheck-clojure
  :ensure t
  :config
  (eval-after-load 'flycheck '(flycheck-clojure-setup)))


(use-package flycheck-ghcmod
  :ensure t)


(use-package flycheck-haskell
  :ensure t)


(use-package flycheck-ledger
  :ensure t)


(use-package ghc
  :ensure t
  :init
  (autoload 'ghc-init "ghc" nil t)
  (autoload 'ghc-debug "ghc" nil t)
  (add-hook 'haskell-mode-hook (lambda () (ghc-init))))


(use-package haskell-mode
  :ensure t
  :config
  (load "haskell-mode-autoloads"))


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


(use-package ledger-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))
  :config
  (setq ledger-post-amount-alignment-column 66)
  (setq ledger-post-use-completion-engine :ido))


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


(use-package powerline
  :ensure t
  :config
  (display-time-mode -1))


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


(use-package yaml-mode
  :ensure t)


(provide 'init)
;;; init.el ends here
