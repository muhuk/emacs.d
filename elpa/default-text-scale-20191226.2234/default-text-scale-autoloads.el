;;; default-text-scale-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "default-text-scale" "default-text-scale.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from default-text-scale.el

(autoload 'default-text-scale-increase "default-text-scale" "\
Increase the height of the default face by `default-text-scale-amount'.

\(fn)" t nil)

(autoload 'default-text-scale-decrease "default-text-scale" "\
Decrease the height of the default face by `default-text-scale-amount'.

\(fn)" t nil)

(autoload 'default-text-scale-reset "default-text-scale" "\
Reset the height of the default face.
With prefix argument SET-CURRENT, set the current size as the
default to which subsequent sizes would be reset.

\(fn &optional SET-CURRENT)" t nil)

(defvar default-text-scale-mode nil "\
Non-nil if Default-Text-Scale mode is enabled.
See the `default-text-scale-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `default-text-scale-mode'.")

(custom-autoload 'default-text-scale-mode "default-text-scale" nil)

(autoload 'default-text-scale-mode "default-text-scale" "\
Change the size of the \"default\" face in every frame.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "default-text-scale" '("default-text-scale-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; default-text-scale-autoloads.el ends here
