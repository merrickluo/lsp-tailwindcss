;;; lsp-tailwindcss.el --- A lsp-mode client for tailwindcss  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  A.I.

;; Author: A.I. <merrick@luois.me>
;; Keywords: language tools
;; Version: 0.2
;; Package-Requires: ((lsp-mode "7.1") (emacs "26.1"))
;; Keywords: tailwindcss
;; URL: https://github.com/merrickluo/lsp-tailwindcss

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; provide the connection to lsp-mode and tailwindcss language server

;;; Code:
(require 'lsp-mode)

(defgroup lsp-tailwindcss nil
  "lsp support for tailwind css"
  :group 'lsp-mode)

(defcustom lsp-tailwindcss-add-on-mode nil
  "Specify lsp-tailwindcss as add-on so it can work with other language servers."
  :type 'boolean
  :group 'lsp-tailwindcss)

(defcustom lsp-tailwindcss-major-modes '(rjsx-mode web-mode html-mode css-mode)
  "Specify lsp-tailwindcss should only starts when major-mode in the list or derived from them."
  :type 'list
  :group 'lsp-tailwindcss
  :package-version '(lsp-tailwindcss . "0.2"))

;;; Language server global settings:
(defcustom lsp-tailwindcss-emmet-completions nil
  "Enable completions when using Emmet-style syntax, for example div.bg-red-500.uppercase."
  :type 'boolean
  :group 'lsp-tailwindcss
  :package-version '(lsp-tailwindcss . "0.2"))

(defcustom lsp-tailwindcss-show-pixel-equivalents t
  "Show px equivalents for rem CSS values in completions and hovers."
  :type 'boolean
  :group 'lsp-tailwindcss
  :package-version '(lsp-tailwindcss . "0.2"))

(defcustom lsp-tailwindcss-root-font-size 16
 "Root font size in pixels. Used to convert rem CSS values to their px equivalents.
see `lsp-tailwindcss-show-pixel-equivalents'"
  :type 'number
  :group 'lsp-tailwindcss
  :package-version '(lsp-tailwindcss . "0.2"))

(defcustom lsp-tailwindcss-validate t
  "Enable linting. Rules can be configured individually using the lsp-tailwindcss-lint-* settings:
    ignore: disable lint rule entirely
    warning: rule violations will be considered \"warnings\", typically represented by a yellow underline
    error: rule violations will be considered \"errors\", typically represented by a red underline."
  :type 'boolean
  :group 'lsp-tailwindcss
  :package-version '(lsp-tailwindcss . "0.2"))

(defcustom lsp-tailwindcss-lint-invalid-screen "error"
  "Unknown screen name used with the @screen directive."
  :type '(choice (const "ignore")
                 (const "warning")
                 (const "error"))
  :group 'lsp-tailwindcss
  :package-version '(lsp-tailwindcss . "0.2"))

(defcustom lsp-tailwindcss-lint-invalid-variant "error"
  "Unknown variant name used with the @variants directive."
  :type '(choice (const "ignore")
                 (const "warning")
                 (const "error"))
  :group 'lsp-tailwindcss
  :package-version '(lsp-tailwindcss . "0.2"))

(defcustom lsp-tailwindcss-lint-invalid-tailwind-directive "error"
  "Unknown value used with the @tailwind directive."
  :type '(choice (const "ignore")
                 (const "warning")
                 (const "error"))
  :group 'lsp-tailwindcss
  :package-version '(lsp-tailwindcss . "0.2"))

(defcustom lsp-tailwindcss-lint-invalid-apply "error"
  "Unsupported use of the @apply directive."
  :type '(choice (const "ignore")
                 (const "warning")
                 (const "error"))
  :group 'lsp-tailwindcss
  :package-version '(lsp-tailwindcss . "0.2"))

(defcustom lsp-tailwindcss-lint-invalid-config-path "error"
  "Unknown or invalid path used with the theme helper."
  :type '(choice (const "ignore")
                 (const "warning")
                 (const "error"))
  :group 'lsp-tailwindcss
  :package-version '(lsp-tailwindcss . "0.2"))

(defcustom lsp-tailwindcss-lint-css-conflict "warning"
  "Class names on the same HTML element which apply the same CSS property or properties."
  :type '(choice (const "ignore")
                 (const "warning")
                 (const "error"))
  :group 'lsp-tailwindcss
  :package-version '(lsp-tailwindcss . "0.2"))

(defcustom lsp-tailwindcss-lint-recommended-variant-order "warning"
  "Class variants not in the recommended order (applies in JIT mode only)."
  :type '(choice (const "ignore")
                 (const "warning")
                 (const "error"))
  :group 'lsp-tailwindcss
  :package-version '(lsp-tailwindcss . "0.2"))

(defcustom lsp-tailwindcss-inspect-port nil
  "Enable the Node.js inspector agent for the language server and listen on the specified port."
  :type 'number
  :group 'lsp-tailwindcss
  :package-version '(lsp-tailwindcss . "0.2"))

(lsp-register-custom-settings
 '(("tailwindCSS.emmetCompletions" lsp-tailwindcss-emmet-completions t)
   ("tailwindCSS.showPixelEquivalents" lsp-tailwindcss-show-pixel-equivalents t)
   ("tailwindCSS.rootFontSize" lsp-tailwindcss-root-font-size)
   ("tailwindCSS.validate" lsp-tailwindcss-validate t)
   ("tailwindCSS.lint.invalidScreen" lsp-tailwindcss-lint-invalid-screen)
   ("tailwindCSS.lint.invalidVariant" lsp-tailwindcss-lint-invalid-variant)
   ("tailwindCSS.lint.invalidTailwindDirective" lsp-tailwindcss-lint-invalid-tailwind-directive)
   ("tailwindCSS.lint.invalidApply" lsp-tailwindcss-lint-invalid-apply)
   ("tailwindCSS.lint.invalidConfigPath" lsp-tailwindcss-lint-invalid-config-path)
   ("tailwindCSS.lint.cssConflict" lsp-tailwindcss-lint-css-conflict)
   ("tailwindCSS.lint.recommendedVariantOrder" lsp-tailwindcss-lint-recommended-variant-order)
   ("tailwindCSS.inspectPort" lsp-tailwindcss-inspect-port)))
;;; Language server global settings ends here

(defvar-local lsp-tailwindcss-workspace-configuration ()
  "Alist of (SECTION . VALUE) entries overrides the global settings.
The tailwindCSS. is ommited, example setting it in .dir-locals.el:
`((web-mode . ((\"rootFontSize\" . 18))))'")

;;;###autoload
(put 'lsp-tailwindcss-workspace-configuration 'safe-local-variable 'listp)

(lsp-dependency 'tailwindcss-language-server
                '(:system "tailwindcss-language-server")
                '(:npm
                  :package "@tailwindcss/language-server"
                  :path "tailwindcss-language-server"))

(defun lsp-tailwindcss--activate-p (&rest _args)
  (and (lsp-workspace-root)
       (or (file-exists-p (f-join (lsp-workspace-root) "tailwind.config.js"))
           (file-exists-p (f-join (lsp-workspace-root) "assets" "tailwind.config.js"))
           (locate-dominating-file (buffer-file-name) "tailwind.config.js"))
       (apply #'provided-mode-derived-p major-mode lsp-tailwindcss-major-modes)))

(defun lsp-tailwindcss--company-dash-hack (w)
  "Append - to the lsp completion-trigger-characters,
workaround the problem that company-mode completion not work when typing \"-\" in classname."
  (with-lsp-workspace w
    (let* ((caps (lsp--workspace-server-capabilities w))
           (comp (lsp:server-capabilities-completion-provider? caps))
           (trigger-chars (append (lsp:completion-options-trigger-characters? comp) nil)))
      (lsp:set-completion-options-trigger-characters?
       comp
       (vconcat
        (cl-pushnew "-" trigger-chars :test #'string=))))))

(lsp-defun lsp-tailwindcss--workspace-configuration (workspace (&ConfigurationParams :items))
  "Override default workspace configuration response,
append `lsp-tailwindcss-workspace-configuration' to the tailwindCSS section."
  (->> items
       (-map (-lambda ((&ConfigurationItem :section?))
               (-let* ((path-parts (split-string section? "\\."))
                       (path-without-last (s-join "." (-slice path-parts 0 -1)))
                       (path-parts-len (length path-parts)))
                 (cond
                  ((string= section? "tailwindCSS")
                   (ht-merge
                    (ht-get (lsp-configuration-section section?)
                            (car-safe path-parts)
                            (ht-create))
                    (ht<-alist lsp-tailwindcss-workspace-configuration)))
                  ((<= path-parts-len 1)
                   (ht-get (lsp-configuration-section section?)
                           (car-safe path-parts)
                           (ht-create)))
                  ((> path-parts-len 1)
                   (when-let ((section (lsp-configuration-section path-without-last))
                              (keys path-parts))
                     (while (and keys section)
                       (setf section (ht-get section (pop keys))))
                     section))))))
       (apply #'vector)))

(defun lsp-tailwindcss--initialization-options ()
  "tailwindcss-language-server requires configuration not be null."
  (ht ("configuration" (lsp-configuration-section "tailwindcss"))))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection
                   (lambda ()
                     `(,(lsp-package-path 'tailwindcss-language-server) "--stdio")))
  :activation-fn #'lsp-tailwindcss--activate-p
  :server-id 'tailwindcss
  :priority -1
  :add-on? lsp-tailwindcss-add-on-mode
  :initialization-options #'lsp-tailwindcss--initialization-options
  :initialized-fn #'lsp-tailwindcss--company-dash-hack
  :request-handlers (ht ("workspace/configuration" #'lsp-tailwindcss--workspace-configuration))
  :download-server-fn (lambda (_client callback error-callback _update?)
                        (when lsp-tailwindcss-auto-install-server
                          (lsp-package-ensure 'tailwindcss-language-server callback error-callback)))))

(provide 'lsp-tailwindcss)
;;; lsp-tailwindcss.el ends here
