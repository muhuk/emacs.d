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


;; Manually installed packages
(add-to-list 'load-path "~/.emacs.d/vendor")


;; Saving
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq auto-save-default nil)
(setq recentf-save-file "~/.emacs.d/.recentf")
(require 'recentf)
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
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(set-face-attribute 'default nil :height 140)
(blink-cursor-mode 0)
;; full path in title bar
(setq-default frame-title-format "%b (%f)")
(fset 'yes-or-no-p 'y-or-n-p)


;; UI - Themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")
;; (load-theme 'tango-dark t)
;; (load-theme 'tomorrow-day t)
;; (load-theme 'tomorrow-night t)
(load-theme 'tomorrow-night-bright t)
;; (load-theme 'tomorrow-night-eighties t)
;; (load-theme 'zenburn t)


;; InteractivelyDoThings
(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point nil)
(setq ido-auto-merge-work-directories-length -1)
(setq ido-use-virtual-buffers t)
(setq ido-everywhere t)
(global-set-key (kbd "C-x C-b") 'ibuffer)


;; ELisp / Scheme
;; eldoc-mode shows documentation in the minibuffer when writing code
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)


(use-package cider
  :ensure t
  :init
  (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
  :config
  (setq cider-repl-pop-to-buffer-on-connect t)
  (setq cider-show-error-buffer t)
  (setq cider-auto-select-error-buffer t)
  (setq cider-repl-history-file "~/.emacs.d/cider-history")
  (setq cider-repl-wrap-history t)
  (add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojure-mode))
  (add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode)))


(use-package clojure-mode
  :ensure t
  :init
  (add-hook 'clojure-mode-hook 'enable-paredit-mode)
  (add-hook 'clojure-mode-hook 'subword-mode))


(use-package clojure-mode-extra-font-locking
  :ensure t)


(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode))


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
  :ensure t)

