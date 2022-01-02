;;; marco-polo-utils.el --- Define marco-polo utility functions   -*- lexical-binding: t; -*-

;; Define marco-polo utility functions.

;;; Code:

(require 'json)

(defvar MARCO-POLO-EXPLORER-BASE-URL "https://cardonoexplorer.com")

(defgroup marco-polo nil
  "Your marco-polo."
  :group 'tools)

(defgroup marco-polo-faces nil
  "Faces used by Magit."
  :group 'marco-polo
  :group 'faces)

(defface marco-polo-epoch-slot-face
  '((((class color) (background light)) :foreground "#ce851f")
    (((class color) (background  dark)) :foreground "#ce851f"))
  "Face for epoch and slot pair."
  :group 'marco-polo-faces)

(defface marco-polo-time-issued-face
  '((((class color) (background light)) :foreground "Goldenrod4")
    (((class color) (background  dark)) :foreground "LightGoldenrod2"))
  "Face for time issued."
  :group 'marco-polo-faces)

(defface marco-polo-tx-num-face
  nil
  "Face for transaction number."
  :group 'marco-polo-faces)

(defface marco-polo-total-sent-face
  nil
  "Face for total sent."
  :group 'marco-polo-faces)

(defface marco-polo-fee-face
  nil
  "Face for fee."
  :group 'marco-polo-faces)

(defface marco-polo-hash-face
  nil
  "Face for block hash."
  :group 'marco-polo-faces)

(defface marco-polo-address-hash-face
  '((((class color) (background light)) :foreground "#ab85a3" :height 0.8)
    (((class color) (background  dark)) :foreground "#ab85a3" :height 0.8))
  "Face for address hash."
  :group 'marco-polo-faces)

(defface marco-polo-address-hash-highlight-face
  '((((class color)) :box "#eee"))
  "Face for highlighted address hash."
  :group 'marco-polo-faces)

(defface marco-polo-slot-leader-face
  '((((class color) (background light)) :foreground "#ab85a3")
    (((class color) (background  dark)) :foreground "#ab85a3"))
  "Face for slot leader hash."
  :group 'marco-polo-faces)

(defface marco-polo-bytes-face
  nil
  "Face for payload size in bytes."
  :group 'marco-polo-faces)

(defvar marco-polo-address-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'marco-polo-view-address-from-point)
    map)
  "Keymap for `marco-polo-address'.")

(defun marco-polo-format-unix-timestamp (timestamp)
  "Format unix timestamp to friendly display from TIMESTAMP."
  (concat
    (format-time-string "%Y-%m-%d %a %H:%M:%S" (seconds-to-time timestamp))
    " "
    (car (cdr (current-time-zone)))))

(defun marco-polo-read-sensitive-config (path)
  "Read sensitive config.  Must be in the form of list({id, address}).  PATH to json/json.gpg file."
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string))
    (json-read-file path)))


(defun marco-polo-parse-response-to-hash-table (json-string)
  "Parse JSON-STRING to hash table."
  (let ((json-object-type 'hash-table)
        (json-array-type 'list)
        (json-key-type 'string))
    (json-read-from-string json-string)))

(defun marco-polo-get-json-response (url callback)
  "Get response as json from URL and CALLBACK."
  (let ((json-string nil))
    (url-retrieve
      url
      (lambda (_status)
        (with-current-buffer (current-buffer)
          (goto-char (point-min))
          (re-search-forward "^$" nil 'move)
          (setq json-string (buffer-substring-no-properties (point) (point-max)))
          (kill-buffer (current-buffer)))
        (funcall callback (marco-polo-parse-response-to-hash-table json-string))))))

(defun marco-polo-get-in-hash (path-list hash-table)
  "Get value in HASH-TABLE with PATH-LIST."
  (if path-list
    (marco-polo-get-in-hash
      (cdr path-list)
      (gethash (car path-list) hash-table))
    hash-table))

(defun marco-polo-list-block-pages (page-num callback)
  "List blocks using optional PAGE-NUM and CALLBACK."
  (let
    ((url (concat
            MARCO-POLO-EXPLORER-BASE-URL
            "/api/blocks/pages"
            (if (eq page-num nil) "" (concat "?page=" (number-to-string page-num))))))
    (marco-polo-get-json-response url callback)))

(defun marco-polo-read-block-summary (block-hash callback)
  "Read block summary from BLOCK-HASH and CALLBACK."
  (let
    ((url (concat
            MARCO-POLO-EXPLORER-BASE-URL
            "/api/blocks/summary/"
            block-hash)))
    (marco-polo-get-json-response url callback)))

(defun marco-polo-list-block-transactions (block-hash callback)
  "List block transactions from BLOCK-HASH and CALLBACK."
  (let
    ((url (concat
            MARCO-POLO-EXPLORER-BASE-URL
            "/api/blocks/txs/"
            block-hash
            "?limit=5000")))
    (marco-polo-get-json-response url callback)))

(defun marco-polo-read-address (address-hash callback)
  "Read address summary from ADDRESS-HASH and CALLBACK."
  (let
    ((url (concat
            MARCO-POLO-EXPLORER-BASE-URL
            "/api/addresses/summary/"
            address-hash)))
    (marco-polo-get-json-response url callback)))

(defun marco-polo-group-number (num &optional size char)
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

(defun marco-polo-mask-decimal (string length character)
  "Mask a number STRING with CHARACTER to LENGTH."
  (let* ((decimal-length (length (car (cdr (split-string string "\\.")))))
          (mask-length (if (> length decimal-length) (- length decimal-length) 0)))
    (concat string (make-string mask-length character))))

(defun marco-polo-format-ada (amount)
  "Format ada AMOUNT."
  (marco-polo-mask-decimal (marco-polo-group-number amount) 6 ?0))

(defun marco-polo-lovelace-to-ada (lovelace)
  "Convert LOVELACE to ADA."
  (/ (float lovelace) 1000000))

(defun marco-polo-get-epoch-num (slot-data)
  "Get epoch number from SLOT-DATA."
  (gethash "cbeEpoch" slot-data))

(defun marco-polo-get-slot-num (slot-data)
  "Get slot number from SLOT-DATA."
  (gethash "cbeSlot" slot-data))

(defun marco-polo-get-block-hash (slot-data)
  "Get block hash from SLOT-DATA."
  (gethash "cbeBlkHash" slot-data))

(defun marco-polo-get-total-sent (slot-data)
  "Get total sent coins from SLOT-DATA."
  (string-to-number (marco-polo-get-in-hash '("cbeTotalSent" "getCoin") slot-data)))

(defun marco-polo-get-fee (slot-data)
  "Get fee from SLOT-DATA."
  (string-to-number (marco-polo-get-in-hash '("cbeFees" "getCoin") slot-data)))

(defun marco-polo-format-output (output)
  "Format OUTPUT data."
  (let ((address-hash (car output))
         (sent (string-to-number (gethash "getCoin" (car (cdr output))))))
    (concat
      (propertize
        (format "%20s" (marco-polo-format-ada (marco-polo-lovelace-to-ada sent)))
        'face 'marco-polo-total-sent-face)
      " => "
      (marco-polo-format-address-hash address-hash))))

(defun marco-polo-format-transaction (transaction-data)
  "Format a transaction data instance from blocks/txs api TRANSACTION-DATA."
  (let ((transaction-id (gethash "ctbId" transaction-data))
        (time-issued (gethash "ctbTimeIssued" transaction-data))
        (inputs (gethash "ctbInputs" transaction-data))
        (outputs (gethash "ctbOutputs" transaction-data))
        (output-sum (string-to-number (marco-polo-get-in-hash '("ctbOutputSum" "getCoin") transaction-data))))
    (concat
      (propertize
        (format "<%s>" (marco-polo-format-unix-timestamp time-issued))
        'face 'marco-polo-time-issued-face)
      " "
      transaction-id
      "\n"
      (marco-polo-format-address-hash (car (car inputs)))
      "\n"
      (mapconcat 'marco-polo-format-output outputs "\n")
      "\n"
      (make-string 20 ?-)
      "\n"
      (format "%20s" (marco-polo-format-ada (marco-polo-lovelace-to-ada output-sum)))
      "\n")))

(defun marco-polo-format-transactions (transactions)
  "Format a list of TRANSACTIONS."
  (concat (mapconcat 'marco-polo-format-transaction transactions "\n")))

(defun marco-polo-view-address-from-point ()
  "View view address detail by reading text-properties at point."
  (interactive)
  (let* ((pos (point))
         (address-hash (get-text-property pos 'address-hash)))
    (marco-polo-view-address address-hash)))

(defun marco-polo-format-address-hash (address-hash)
  "Format ADDRESS-HASH."
  (propertize
    (format "%s" address-hash)
    'keymap marco-polo-address-map
    'face 'marco-polo-address-hash-face
    'address-hash address-hash))

(defun marco-polo-format-address-summary (address-response)
  "Format summary from ADDRESS-RESPONSE."
  (let* ((address-data (gethash "Right" address-response))
         (address-hash (gethash "caAddress" address-data))
         (tx-num (gethash "caTxNum"  address-data))
         (balance (string-to-number (marco-polo-get-in-hash '("caBalance" "getCoin") address-data)))
         (transactions (gethash "caTxList" address-data)))
    (concat
      (format "%-16s: " "Address")
      (marco-polo-format-address-hash address-hash)
      "\n"
      (format "%-16s: " "Final Balance")
      (propertize
        (format "%s" (marco-polo-format-ada (marco-polo-lovelace-to-ada balance)))
        'face 'marco-polo-total-sent-face)
      "\n\n"
      (propertize (format "Transactions (%d)" tx-num) 'face 'header-line)
      "\n\n"
      (marco-polo-format-transactions transactions))))

(defun marco-polo-view-address (address-hash)
  "View address detail from ADDRESS-HASH."
  (marco-polo-read-address
    address-hash
    (lambda (response)
      (let* ((buffer-name (concat "*Address-" address-hash "*"))
             (buffer (get-buffer-create buffer-name)))
        (switch-to-buffer buffer)
        (with-current-buffer buffer
          (read-only-mode 0)
          (erase-buffer)
          (insert (propertize "Address" 'face 'header-line))
          (insert "\n\n\n")
          (insert (marco-polo-format-address-summary response))
          (highlight-regexp address-hash 'marco-polo-address-hash-highlight-face)
          (read-only-mode 1)
          (goto-char (point-min)))))))

;;; marco-polo-utils.el ends here
