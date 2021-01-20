;;; lsp-tailwindcss.el --- A lsp-mode client for tailwindcss  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  A.I.

;; Author: A.I. <merrick@luois.me>
;; Keywords: language tools
;; Version: 0.1
;; Package-Requires: ((lsp-mode "3.0") (emacs "24.3"))
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

(defcustom lsp-tailwindcss-server-dir (expand-file-name "tailwindcss" lsp-server-install-dir)
  "Local directory for tailwindcss-intellisense."
  :type 'string
  :group 'lsp-tailwindcss)

(defcustom lsp-tailwindcss-server-file (expand-file-name "extension/dist/server/index.js" lsp-tailwindcss-server-dir)
  "The index.js file location of tailwindcss-intellisense, do not change when auto install."
  :type 'string
  :group 'lsp-tailwindcss)

(defcustom lsp-tailwindcss-auto-install-server t
  "Install tailwindcss language server automatically."
  :type 'boolean
  :group 'lsp-tailwindcss)

(defcustom lsp-tailwindcss-server-version "0.5.7"
  "Specify the version of tailwindcss intellisence."
  :type 'string
  :group 'lsp-tailwindcss)

(defcustom lsp-tailwindcss-add-on-mode nil
  "Specify lsp-tailwindcss as add-on so it can work with other langauge servers."
  :type 'boolean
  :group 'lsp-tailwindcss)

(defvar lsp-tailwindcss-server-installed-p
  (file-exists-p lsp-tailwindcss-server-file)
  "Check if server is installed.")

(defun lsp-tailwindcss--download-url ()
  (let ((version lsp-tailwindcss-server-version))
    (format "https://github.com/tailwindlabs/tailwindcss-intellisense/releases/download/v%s/vscode-tailwindcss-%s.vsix"
            version version)))

(defun lsp-tailwindcss--callback (_workspace &rest _args)
  ;; no action needed right now
  )

(defun lsp-tailwindcss--install-server (_client callback error-callback update?)
  (if (and (not update?) lsp-tailwindcss-server-installed-p)
      (lsp--info "tailwindcss language server already installed.")
    (let ((tempfile (make-temp-file "ext" nil ".zip")))
      (lsp--info "installing tailwindcss language server, please wait.")
      (delete-file tempfile)
      (lsp-download-install
       (lambda (&rest _)
         (condition-case err
             (progn
               (lsp-unzip tempfile lsp-tailwindcss-server-dir)
               (funcall callback))
           (error (funcall error-callback err))))
       error-callback
       :url (lsp-tailwindcss--download-url)
       :store-path tempfile))))

(defun lsp-tailwindcss--configuration (_workspace args)
  (let ((id (gethash "_id" args)))
    (lsp-request "tailwindcss/getConfigurationResponse" `(:_id ,id) :no-wait t)))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection
                   (list "node" lsp-tailwindcss-server-file "--stdio")
                   (lambda () (f-exists? lsp-tailwindcss-server-file)))
  :major-modes '(web-mode css-mode html-mode rjsx-mode)
  :server-id 'tailwindcss
  :priority -1
  :add-on? lsp-tailwindcss-add-on-mode
  :notification-handlers (ht ("tailwindcss/configUpdated" #'lsp-tailwindcss--callback)
                             ("tailwindcss/getConfiguration" #'lsp-tailwindcss--configuration))
  :download-server-fn (lambda (client callback error-callback update?)
                        (when lsp-tailwindcss-auto-install-server
                          (lsp-tailwindcss--install-server client callback error-callback update?)))))

(provide 'lsp-tailwindcss)
;;; lsp-tailwindcss.el ends here
