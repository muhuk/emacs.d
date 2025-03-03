;; -*- no-byte-compile: t; lexical-binding: nil -*-
(define-package "lsp-ui" "9.0.0"
  "UI modules for lsp-mode."
  '((emacs         "27.1")
    (dash          "2.18.0")
    (lsp-mode      "6.0")
    (markdown-mode "2.3"))
  :url "https://github.com/emacs-lsp/lsp-ui"
  :commit "8aa8b175fc4cdf2d16f6f3fdb2904e8874610c8a"
  :revdesc "8aa8b175fc4c"
  :keywords '("languages" "tools")
  :authors '(("Sebastien Chapuis" . "sebastien@chapu.is")
             ("Fangrui Song" . "i@maskray.me"))
  :maintainers '(("Sebastien Chapuis" . "sebastien@chapu.is")
                 ("Fangrui Song" . "i@maskray.me")))
