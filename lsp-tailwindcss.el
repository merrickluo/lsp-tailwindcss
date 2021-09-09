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

(defcustom lsp-tailwindcss-server-dir (expand-file-name "tailwindcss" lsp-server-install-dir)
  "Local directory for tailwindcss-intellisense."
  :type 'string
  :group 'lsp-tailwindcss)
(make-obsolete-variable 'lsp-tailwindcss-server-dir "Not used anymore." "0.2")

(defcustom lsp-tailwindcss-server-file (expand-file-name "extension/dist/server/tailwindServer.js" lsp-tailwindcss-server-dir)
  "The index.js file location of tailwindcss-intellisense, do not change when auto install."
  :type 'string
  :group 'lsp-tailwindcss)
(make-obsolete-variable 'lsp-tailwindcss-server-file "Not used anymore." "0.2")

(defcustom lsp-tailwindcss-auto-install-server t
  "Install tailwindcss language server automatically."
  :type 'boolean
  :group 'lsp-tailwindcss)
(make-obsolete-variable 'lsp-tailwindcss-auto-install-server "Not used anymore." "0.2")

(defcustom lsp-tailwindcss-server-version "0.6.13"
  "Specify the version of tailwindcss intellisence."
  :type 'string
  :group 'lsp-tailwindcss)
(make-obsolete-variable 'lsp-tailwindcss-server-version "Not used anymore." "0.2")

(defcustom lsp-tailwindcss-major-modes '(rjsx-mode web-mode html-mode css-mode typescript-mode)
  "Specify lsp-tailwindcss should only starts when major-mode in the list or derived from them."
  :type 'list
  :group 'lsp-tailwindcss
  :package-version '(lsp-tailwindcss . "0.2"))

(defcustom lsp-tailwindcss-rustywind-command (executable-find "rustywind")
  "[Experimental] Command use for sort the tailwindcss classes."
  :type 'string
  :group 'lsp-tailwindcss)

(defun lsp-tailwindcss-rustywind ()
  "[Experimental] Sort tailwindcss class name using rustywind.
This is an *experimental* feature, please be careful when use."
  (interactive)
  (if (and lsp-tailwindcss-rustywind-command
           (f-executable-p lsp-tailwindcss-rustywind-command))
    (let ((tmpfile (make-nearby-temp-file "rustywind" nil nil))
          (coding-system-for-read 'utf-8)
          (coding-system-for-write 'utf-8))

      (unwind-protect
          (save-restriction
            (widen)
            (write-region nil nil tmpfile)

            (let ((rustywind-args (list "--write" (file-local-name tmpfile))))
              (when (zerop (apply #'process-file lsp-tailwindcss-rustywind-command nil nil nil rustywind-args))
                (insert-file-contents tmpfile nil nil nil t))))

        (delete-file tmpfile)))
    (error (format "Can't find rustywind executable at %s" lsp-tailwindcss-rustywind-command))))

(defun lsp-tailwindcss-rustywind-before-save()
  "[Experimental] Add this to `before-save-hook' to run rustywind when saving buffer.
(add-hook 'before-save-hook #'lsp-tailwindcss-rustywind-before-save).

It only runs when lsp-tailwindcss can be activated determined by `lsp-tailwindcss--activate-p'.
This is an *experimental* feature, please be careful when use.
"
  (when (lsp-tailwindcss--activate-p)
    (lsp-tailwindcss-rustywind)))

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
   ("tailwindCSS.lint.recommendedVariantOrder" lsp-tailwindcss-lint-recommended-variant-order)))
;;; Language server global settings ends here

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
  :download-server-fn (lambda (_client callback error-callback _update?)
                        (lsp-package-ensure 'tailwindcss-language-server callback error-callback))))

(provide 'lsp-tailwindcss)
;;; lsp-tailwindcss.el ends here
