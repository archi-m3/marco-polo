;;; marco-polo-saved-addresses.el --- Define saved address view.   -*- lexical-binding: t; -*-

;; Helper functions to remember addresses

;; Code:

(define-derived-mode marco-polo-saved-addresses-mode fundamental-mode "marco-polo-saved-addresses"
  "Major mode for saved addresses.")

(defun marco-polo-format-saved-address-row (address-hash label)
  "Format saved address row FROM ADDRESS-HASH and LABEL."
  (concat
    (marco-polo-format-address-hash address-hash)
    (format "[%s]" label)))

(defun marco-polo-view-saved-addresses (config-file-path)
  "View saved addresses.   CONFIG-FILE-PATH should be a json file."
  (let* ((config (marco-polo-read-sensitive-config config-file-path))
         (addresses (gethash "addresses" config))
         (buffer-name "*Saved Addresses*")
         (buffer (get-buffer-create buffer-name)))
    (switch-to-buffer buffer)
    (with-current-buffer buffer
      (erase-buffer)
      (read-only-mode 0)
      (marco-polo-saved-addresses-mode)
      (insert (propertize "Saved Addresses" 'face 'header-line))
      (insert "\n\n\n")
      (dolist (address-entry addresses)
        (insert (marco-polo-format-saved-address-row
                  (gethash "address" address-entry)
                  (gethash "id" address-entry)))
        (insert "\n"))
      (read-only-mode 1))))

;; End:
;;; marco-polo-saved-addresses.el ends here
