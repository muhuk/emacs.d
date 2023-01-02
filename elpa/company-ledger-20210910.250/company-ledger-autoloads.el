;;; company-ledger-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "company-ledger" "company-ledger.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from company-ledger.el

(autoload 'company-ledger "company-ledger" "\
Fuzzy company back-end for ledger, beancount and other ledger-like modes.
Provide completion info based on COMMAND and ARG.  IGNORED, not used.

\(fn COMMAND &optional ARG &rest IGNORED)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "company-ledger" '("company-ledger-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; company-ledger-autoloads.el ends here
