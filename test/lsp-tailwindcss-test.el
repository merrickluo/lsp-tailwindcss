(require 'ert)

;; Load the file to be tested
(load-file (expand-file-name "../lsp-tailwindcss.el" (file-name-directory (or load-file-name buffer-file-name))))

(ert-deftest lsp-tailwindcss--package-version-test ()
  "Test `lsp-tailwindcss--package-version` function."
  (let ((temp-dir (make-temp-file "test-lsp-tailwindcss" t)))
    (unwind-protect
        (progn
          ;; Test case 1: tailwindcss in dependencies
          (let* ((package-json (expand-file-name "package.json" temp-dir))
                 (buffer-file-name (expand-file-name "index.js" temp-dir)))
            (with-temp-file package-json
              (insert "{\"dependencies\": {\"tailwindcss\": \"^3.0.0\"}}"))
            (should (string= (lsp-tailwindcss--package-version) "^3.0.0")))

          ;; Test case 2: tailwindcss in devDependencies
          (let* ((package-json (expand-file-name "package.json" temp-dir))
                 (buffer-file-name (expand-file-name "index.js" temp-dir)))
            (with-temp-file package-json
              (insert "{\"devDependencies\": {\"tailwindcss\": \"~2.1.0\"}}"))
            (should (string= (lsp-tailwindcss--package-version) "~2.1.0")))

          ;; Test case 3: tailwindcss in both, dependencies is preferred
          (let* ((package-json (expand-file-name "package.json" temp-dir))
                 (buffer-file-name (expand-file-name "index.js" temp-dir)))
            (with-temp-file package-json
              (insert "{\"dependencies\": {\"tailwindcss\": \"1.0.0\"}, \"devDependencies\": {\"tailwindcss\": \"2.0.0\"}}"))
            (should (string= (lsp-tailwindcss--package-version) "1.0.0")))

          ;; Test case 4: tailwindcss not present
          (let* ((package-json (expand-file-name "package.json" temp-dir))
                 (buffer-file-name (expand-file-name "index.js" temp-dir)))
            (with-temp-file package-json
              (insert "{\"dependencies\": {\"another-package\": \"1.0.0\"}}"))
            (should (null (lsp-tailwindcss--package-version))))

          ;; Test case 5: no package.json
          (let ((buffer-file-name (expand-file-name "index.js" (expand-file-name "foo" temp-dir))))
            (delete-file (expand-file-name "package.json" temp-dir))
            (should (null (lsp-tailwindcss--package-version)))))
      (delete-directory temp-dir t))))

(ert-deftest lsp-tailwindcss--version-v4-p-test ()
  "Test `lsp-tailwindcss--version-v4-p` function."
  ;; Versions that should be considered v4 or greater
  (should (lsp-tailwindcss--version-v4-p "4.0.0"))
  (should (lsp-tailwindcss--version-v4-p "4.1.2"))
  (should (lsp-tailwindcss--version-v4-p "^4.0.0"))
  (should (lsp-tailwindcss--version-v4-p "~4.0.0"))
  (should (lsp-tailwindcss--version-v4-p "5.0.0"))
  (should (lsp-tailwindcss--version-v4-p "10.0.0"))
  (should (lsp-tailwindcss--version-v4-p "^11.2.3"))

  ;; Versions that should not be considered v4
  (should-not (lsp-tailwindcss--version-v4-p "3.0.0"))
  (should-not (lsp-tailwindcss--version-v4-p "3.9.9"))
  (should-not (lsp-tailwindcss--version-v4-p "^3.1.0"))
  (should-not (lsp-tailwindcss--version-v4-p "~3.1.0"))
  (should-not (lsp-tailwindcss--version-v4-p "0.4.0"))
  (should-not (lsp-tailwindcss--version-v4-p "1.2.3"))
  (should-not (lsp-tailwindcss--version-v4-p "latest"))
  (should-not (lsp-tailwindcss--version-v4-p "next")))
