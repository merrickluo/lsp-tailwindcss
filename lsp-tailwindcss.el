;;; lsp-tailwindcss.el --- the lsp-mode client for tailwindcss  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  A.I.

;; Author: A.I. <merrick@luois.me>
;; Keywords: language tools

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

(require 'lsp-mode)

(defgroup lsp-tailwindcss nil
  "lsp support for tailwind css"
  :group 'lsp-mode)

;; (defcustom lsp-tailwindcss-download-location nil)

(defcustom lsp-tailwindcss-server-file "index.js"
  "index.js location of vscode-tailwindcss"
  :group 'lsp-tailwindcss)

(defun lsp-tailwindcss--callback (workspace &rest _)
  (message "lsp-tailwindcss callback %s" workspace))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection
                   '("node" lsp-tailwindcss-server-file "--stdio")
                   (lambda () (f-exists? lsp-tailwindcss-server-file)))
  :major-modes '(web-mode css-mode)
  :server-id 'tailwindcss
  :priority 1
  :notification-handlers (lsp-ht ("tailwindcss/configUpdated" 'lsp-tailwindcss--callback))))

;; (add-hook 'web-mode-hook #'lsp)
;; (add-hook 'css-mode-hook #'lsp)

(provide 'lsp-tailwindcss)
