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
  (concat vendor-path "plantuml.1.2019.5.jar"))
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


;; IBuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Themes
;;   dracula-theme
;;   flatland-theme
(use-package dracula-theme
  :ensure t
  :config
  (set-face-attribute 'linum nil :height 100))


(use-package cargo
  :ensure t
  :init
  (setq-default cider-repl-history-file (concat emacs-config-dir "cider-history")
                cider-repl-wrap-history t)
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
  (setq-default org-babel-clojure-backend 'cider
                org-babel-clojure-sync-nrepl-timeout 5000
                cider-test-unit-tests-include '()
                cider-test-unit-tests-exclude '("integration"))
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
  (defhydra hydra-dired (:hint nil :color pink)
    "
_+_ mkdir          _v_iew           _m_ark             _(_ details        _i_nsert-subdir    wdired
_C_opy             _O_ view other   _U_nmark all       _)_ omit-mode      _$_ hide-subdir    C-x C-q : edit
_D_elete           _o_pen other     _u_nmark           _l_ redisplay      _w_ kill-subdir    C-c C-c : commit
_R_ename           _M_ chmod        _t_oggle           _g_ revert buf     _e_ ediff          C-c ESC : abort
_Y_ rel symlink    _G_ chgrp        _E_xtension mark   _s_ort             _=_ pdiff
_S_ymlink          ^ ^              _F_ind marked      _._ toggle hydra   \\ flyspell
_r_sync            ^ ^              ^ ^                ^ ^                _?_ summary
_z_ compress-file  _A_ find regexp
_Z_ compress       _Q_ repl regexp

T - tag prefix
"
    ("\\" dired-do-ispell)
    ("(" dired-hide-details-mode)
    (")" dired-omit-mode)
    ("+" dired-create-directory)
    ("=" diredp-ediff) ;; smart diff
    ("?" dired-summary)
    ("$" diredp-hide-subdir-nomove)
    ("A" dired-do-find-regexp)
    ("C" dired-do-copy) ;; Copy all marked files
    ("D" dired-do-delete)
    ("E" dired-mark-extension)
    ("e" dired-ediff-files)
    ("F" dired-do-find-marked-files)
    ("G" dired-do-chgrp)
    ("g" revert-buffer) ;; read all directories again (refresh)
    ("i" dired-maybe-insert-subdir)
    ("l" dired-do-redisplay) ;; relist the marked or singel directory
    ("M" dired-do-chmod)
    ("m" dired-mark)
    ("O" dired-display-file)
    ("o" dired-find-file-other-window)
    ("Q" dired-do-find-regexp-and-replace)
    ("R" dired-do-rename)
    ("r" dired-do-rsynch)
    ("S" dired-do-symlink)
    ("s" dired-sort-toggle-or-edit)
    ("t" dired-toggle-marks)
    ("U" dired-unmark-all-marks)
    ("u" dired-unmark)
    ("v" dired-view-file) ;; q to exit, s to search, = gets line #
    ("w" dired-kill-subdir)
    ("Y" dired-do-relsymlink)
    ("z" diredp-compress-this-file)
    ("Z" dired-do-compress)
    ("q" nil)
    ("." nil :color blue))
  (eval-after-load "dired"
    '(define-key dired-mode-map "." 'hydra-dired/body))

  ;; ibuffer
  (defhydra hydra-ibuffer-main (:color pink :hint nil)
    "
^Mark^         ^Actions^         ^View^          ^Select^              ^Navigation^
_m_: mark      _D_: delete       _g_: refresh    _q_: quit             _k_:   ↑    _h_
_u_: unmark    _s_: save marked  _S_: sort       _TAB_: toggle         _RET_: visit
_*_: specific  _a_: all actions  _/_: filter     _o_: other window     _j_:   ↓    _l_
_t_: toggle    _._: toggle hydra _H_: help       C-o other win no-select
"
    ("m" ibuffer-mark-forward)
    ("u" ibuffer-unmark-forward)
    ("*" hydra-ibuffer-mark/body :color blue)
    ("t" ibuffer-toggle-marks)

    ("D" ibuffer-do-delete)
    ("s" ibuffer-do-save)
    ("a" hydra-ibuffer-action/body :color blue)

    ("g" ibuffer-update)
    ("S" hydra-ibuffer-sort/body :color blue)
    ("/" hydra-ibuffer-filter/body :color blue)
    ("H" describe-mode :color blue)

    ("h" ibuffer-backward-filter-group)
    ("k" ibuffer-backward-line)
    ("l" ibuffer-forward-filter-group)
    ("j" ibuffer-forward-line)
    ("RET" ibuffer-visit-buffer :color blue)

    ("TAB" ibuffer-toggle-filter-group)

    ("o" ibuffer-visit-buffer-other-window :color blue)
    ("q" quit-window :color blue)
    ("." nil :color blue))
  (eval-after-load "ibuffer"
    '(define-key ibuffer-mode-map "." 'hydra-ibuffer-main/body))
  (add-hook 'ibuffer-hook #'hydra-ibuffer-main/body)

  (defhydra hydra-ibuffer-mark (:color teal :columns 5
                                       :after-exit (hydra-ibuffer-main/body))
    "Mark"
    ("*" ibuffer-unmark-all "unmark all")
    ("M" ibuffer-mark-by-mode "mode")
    ("m" ibuffer-mark-modified-buffers "modified")
    ("u" ibuffer-mark-unsaved-buffers "unsaved")
    ("s" ibuffer-mark-special-buffers "special")
    ("r" ibuffer-mark-read-only-buffers "read-only")
    ("/" ibuffer-mark-dired-buffers "dired")
    ("e" ibuffer-mark-dissociated-buffers "dissociated")
    ("h" ibuffer-mark-help-buffers "help")
    ("z" ibuffer-mark-compressed-file-buffers "compressed")
    ("b" hydra-ibuffer-main/body "back" :color blue))

  (defhydra hydra-ibuffer-action (:color teal :columns 4
                                         :after-exit
                                         (if (eq major-mode 'ibuffer-mode)
                                             (hydra-ibuffer-main/body)))
    "Action"
    ("A" ibuffer-do-view "view")
    ("E" ibuffer-do-eval "eval")
    ("F" ibuffer-do-shell-command-file "shell-command-file")
    ("I" ibuffer-do-query-replace-regexp "query-replace-regexp")
    ("H" ibuffer-do-view-other-frame "view-other-frame")
    ("N" ibuffer-do-shell-command-pipe-replace "shell-cmd-pipe-replace")
    ("M" ibuffer-do-toggle-modified "toggle-modified")
    ("O" ibuffer-do-occur "occur")
    ("P" ibuffer-do-print "print")
    ("Q" ibuffer-do-query-replace "query-replace")
    ("R" ibuffer-do-rename-uniquely "rename-uniquely")
    ("T" ibuffer-do-toggle-read-only "toggle-read-only")
    ("U" ibuffer-do-replace-regexp "replace-regexp")
    ("V" ibuffer-do-revert "revert")
    ("W" ibuffer-do-view-and-eval "view-and-eval")
    ("X" ibuffer-do-shell-command-pipe "shell-command-pipe")
    ("b" nil "back"))

  (defhydra hydra-ibuffer-sort (:color amaranth :columns 3)
    "Sort"
    ("i" ibuffer-invert-sorting "invert")
    ("a" ibuffer-do-sort-by-alphabetic "alphabetic")
    ("v" ibuffer-do-sort-by-recency "recently used")
    ("s" ibuffer-do-sort-by-size "size")
    ("f" ibuffer-do-sort-by-filename/process "filename")
    ("m" ibuffer-do-sort-by-major-mode "mode")
    ("b" hydra-ibuffer-main/body "back" :color blue))

  (defhydra hydra-ibuffer-filter (:color amaranth :columns 4)
    "Filter"
    ("m" ibuffer-filter-by-used-mode "mode")
    ("M" ibuffer-filter-by-derived-mode "derived mode")
    ("n" ibuffer-filter-by-name "name")
    ("c" ibuffer-filter-by-content "content")
    ("e" ibuffer-filter-by-predicate "predicate")
    ("f" ibuffer-filter-by-filename "filename")
    (">" ibuffer-filter-by-size-gt "size")
    ("<" ibuffer-filter-by-size-lt "size")
    ("/" ibuffer-filter-disable "disable")
    ("b" hydra-ibuffer-main/body "back" :color blue)))


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
  (ido-mode 1)
  (setq ido-enable-flex-matching t)
  (setq ido-use-filename-at-point nil)
  (setq ido-auto-merge-work-directories-length -1)
  (setq ido-use-virtual-buffers t)
  (setq ido-everywhere t))


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
          org-refile-targets `((org-agenda-files . (:maxlevel . 3))
                               (,(diary-file "someday.org") . (:maxlevel . 3))))
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
                                           (file+olp ,(diary-file "someday.org") "Backlog" "Other")
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
  (setq plantuml-jar-path vendor-plantuml-jar-path))


(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))


(use-package rust-mode
  :ensure t
  :init
  (add-hook 'rust-mode-hook #'flycheck-rust-setup)
  :config
  (setq rust-format-on-save t))


(use-package seq
  :ensure t)


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


(use-package toml-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)


(provide 'init)
;;; init.el ends here
