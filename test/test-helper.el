;; Required testing libraries
(require 'cl)
(require 'el-mock)

(add-to-list 'load-path (file-name-directory (directory-file-name (file-name-directory load-file-name))))
(require 'bter-api)

(setq test-directory (file-name-directory (directory-file-name (file-name-directory load-file-name))))

(defun read-fixture (file)
  (let* ((file-path (concat test-directory "/fixtures/" file))
         (file-contents (with-temp-buffer
                          (insert-file-contents file-path)
                          (buffer-string))))
    (json-read-from-string file-contents)))

;; Mock Helpers

(defmacro mock-request (path query-vars fixture)
  (let ((uri (bter-api--create-endpoint path query-vars)))
    `(mock (bter-api--get-uri ,uri) => (read-fixture ,fixture))))
