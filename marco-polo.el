;;; marco-polo.el --- Define marco-polo commands   -*- lexical-binding: t; -*-
;; Version: 0.1.0

;; Copyright (C) 2018 Archi

;; Author: Archi <archi.m3@protonmail.com>
;; Homepage: https://github.com/archi-m3/marco-polo

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3 of the License,
;; or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU GPL see http://www.gnu.org/licenses.

;;; Commentary:

;; Define marco-polo commands.

;;; Code:

(require 'json)
(require 'cl-lib)

(defvar marco-polo/root-api "https://api.pieverse.workers.dev")
(defvar marco-polo/json-config-path (concat (getenv "HOME") "/.marco-polo.json"))
(cl-defstruct marco-polo/account name stakeaddr)

(define-derived-mode marco-polo-mode fundamental-mode "marco-polo"
  "Major mode for marco-polo.")

;; faces
(defgroup marco-polo/faces nil
  "Faces used by Marco Polo."
  :group 'marco-polo
  :group 'faces)

(defface marco-polo/address-face
  '((((class color) (background light)) :foreground "#4A4F4D")
    (((class color) (background  dark)) :foreground "#FCFCFC"))
  "Face for address."
  :group 'marco-polo/faces)

(defface marco-polo/amount-face
  '((((class color) (background light)) :foreground "#3DDC97")
    (((class color) (background  dark)) :foreground "#3DDC97"))
  "Face for token value"
  :group 'marco-polo/faces)

;; helpers
(defun marco-polo/decimalize (quantity decimals)
  "Convert QUANTITY in string to an ammount using DECIMALS."
  (cond
    ((> decimals 0) (/ (float quantity) (expt 10 decimals)))
    (t quantity)))

(defun marco-polo/mask-decimal (string length character)
  "Mask a number STRING with CHARACTER to LENGTH."
  (let* ((decimal-length (length (car (cdr (split-string string "\\.")))))
          (mask-length (if (> length decimal-length) (- length decimal-length) 0)))
    (concat string (make-string mask-length character))))

(defun marco-polo/group-number (num &optional size char)
  "Format NUM as string grouped to SIZE with CHAR."
  ;; Based on code for `math-group-float' in calc-ext.el
  (let* ((size (or size 3))
         (char (or char ","))
         (str (if (stringp num) num (number-to-string num)))
         ;; omitting any trailing non-digit chars
         ;; NOTE: Calc supports BASE up to 36 (26 letters and 10 digits ;)
         (pt (or (string-match "[^0-9a-zA-Z]" str) (length str))))
    (while (> pt size)
      (setq str (concat (substring str 0 (- pt size))
                  char
                  (substring str (- pt size)))
        pt (- pt size)))
    str))

(defun marco-polo/format-decimalized-quantity (decimalized-quantity decimals)
  "Format a decimalized quantity that has been converted from string using DECIMALS."
  (marco-polo/mask-decimal (marco-polo/group-number decimalized-quantity) decimals ?0))

(defun marco-polo/parse-response-to-hash-table (json-string)
  "Parse JSON-STRING to hash table."
  (let ((json-object-type 'hash-table)
        (json-array-type 'list)
        (json-key-type 'string))
    (json-read-from-string json-string)))

(defun marco-polo/http-post (url payload callback)
  "Post request WITH URL, PAYLOAD AND CALLBACK"
  (let ((url-request-method "POST")
         (url-request-extra-headers
           '(("Content-Type" . "application/json")))
         (url-request-data payload))
    (url-retrieve
      url
      (lambda (_status)
        (with-current-buffer (current-buffer)
          (goto-char (point-min))
          (re-search-forward "^$" nil 'move)
          (setq json-string (buffer-substring-no-properties (point) (point-max)))
          (kill-buffer (current-buffer)))
        (funcall callback (marco-polo/parse-response-to-hash-table json-string))))))

(defun marco-polo/read-config-accounts ()
  "Read stake address from config. format: {accounts: [{name, stakeAddress}]}"
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
          (config (json-read-file marco-polo/json-config-path))
          (accounts (gethash "accounts" config)))
    (mapcar
      (lambda (account)
        (make-marco-polo/account
          :name (gethash "name" account)
          :stakeaddr (gethash "stakeAddress" account)))
      accounts)))

(defun marco-polo/fetch-accounts (accounts callback)
  "Fetch accounts using STAKEADDR-LIST and CALLBACK."
  (let*
    ((url (format "%s/accounts" marco-polo/root-api))
      (stakeAddrs (mapcar (lambda (account) (marco-polo/account-stakeaddr account)) accounts))
      (payload (json-encode (list (append '("stakeAddrs") stakeAddrs)))))
    (marco-polo/http-post url payload callback)))

(defun marco-polo/view-accounts ()
  "View accounts summary."
  (let* ((account-configs (marco-polo/read-config-accounts))
         (max-name-length (seq-max (mapcar (lambda (account-config) (length (marco-polo/account-name account-config))) account-configs)))
          (buffer (get-buffer-create "*Accounts*"))
          (total-ada 0))
    (marco-polo/fetch-accounts
      account-configs
      (lambda (response)
        (switch-to-buffer buffer)
        (with-current-buffer buffer
          (erase-buffer)
          (marco-polo-mode)
          (insert "Accounts:\n")
          (dolist (account-config account-configs)
            (let* ((stakeaddr (marco-polo/account-stakeaddr account-config))
                    (account (gethash stakeaddr response))
                    (lovelace (string-to-number (gethash "controlled_amount" account)))
                    (ada (marco-polo/decimalize lovelace 6))
                    (tokens (gethash "ft" account)))
              (insert
                (concat
                  (propertize
                    (format "%s" stakeaddr)
                    'face 'marco-polo/address-face)
                  (format " [%s]" (marco-polo/account-name account-config))))
              (insert "\n")
              (insert
                (propertize
                  (format "  %-8s %24s" "₳" (marco-polo/format-decimalized-quantity ada 6))
                  'face 'marco-polo/amount-face))
              (insert "\n")
              (dolist (token tokens)
                (let ((decimals (gethash "decimals" token))
                       (quantity (string-to-number (gethash "quantity" token))))
                  (insert
                  (format
                    "  %-8s %24s\n"
                    (gethash "ticker" token)
                    (marco-polo/format-decimalized-quantity
                      (marco-polo/decimalize quantity decimals) decimals)))))
              (insert "\n")
              (setq total-ada (+ total-ada ada))))
          (insert "\n")
          (insert (make-string 26 ?-))
          (insert "\n")
          (insert
            (propertize
                (format "₳ %24s" (marco-polo/format-decimalized-quantity total-ada 6))
                'face 'marco-polo/amount-face)))))))

(defun marco-polo ()
  (interactive)
  (marco-polo/view-accounts))

;;; _
(provide 'marco-polo)
;; End:
;;; marco-polo.el ends here
