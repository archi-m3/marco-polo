;;; marco-polo-slots.el --- Define marco-polo commands   -*- lexical-binding: t; -*-

;; Slot related functions

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;; slot ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun marco-polo-format-slot-summary (slot-response)
  "Format slot summary from blocks/summary api SLOT-RESPONSE."
  (let* ((slot-data (marco-polo-get-in-hash '("Right" "cbsEntry") slot-response))
         (epoch-num (marco-polo-get-epoch-num slot-data))
         (slot-num (marco-polo-get-slot-num slot-data))
         (total-sent (marco-polo-get-total-sent slot-data))
         (fee (marco-polo-get-fee slot-data))
         (block-hash (marco-polo-pget-block-hash slot-data))
         (previous-block-hash (marco-polo-get-in-hash '("Right" "cbsPrevHash") slot-response))
         (next-block-hash (marco-polo-get-in-hash '("Right" "cbsNextHash") slot-response))
         (merkle-root (marco-polo-get-in-hash '("Right" "cbsMerkleRoot") slot-response)))
    (concat
      (format "%-16s: " "Epoch")
      (propertize (number-to-string epoch-num) 'face 'marco-polo-epoch-slot-face)
      "\n"
      (format "%-16s: " "Slot")
      (propertize (number-to-string slot-num) 'face 'marco-polo-epoch-slot-face)
      "\n"
      (format "%-16s: " "Total Output")
      (propertize
        (format "%s" (marco-polo-format-ada (marco-polo-lovelace-to-ada total-sent)))
        'face 'marco-polo-total-sent-face)
      "\n"
      (format "%-16s: " "Fee")
      (propertize
        (format "%s" (marco-polo-format-ada (marco-polo-lovelace-to-ada fee)))
        'face 'marco-polo-fee-face)
      "\n"
      (format "%-16s: " "Hash")
      (propertize block-hash 'face 'marco-polo-hash-face)
      "\n"
      (format "%-16s: " "Previous slot")
      (propertize previous-block-hash 'face 'marco-polo-hash-face)
      "\n"
      (format "%-16s: " "Next slot")
      (propertize next-block-hash 'face 'marco-polo-hash-face)
      "\n"
      (format "%-16s: " "Merkle root")
      (propertize merkle-root 'face 'marco-polo-hash-face))))

(defun marco-polo-load-block-summary-and-transactions (block-hash callback)
  "Load the block summary and its transactions from BLOCK-HASH and CALLBACK."
  (marco-polo-read-block-summary
    block-hash
    (lambda (slot-response)
      (marco-polo-list-block-transactions
        block-hash
        (lambda (transactions-response)
          (funcall callback slot-response transactions-response))))))

(defun marco-polo-view-slot (block-hash)
  "View detail from BLOCK-HASH."
  (marco-polo-load-block-summary-and-transactions
    block-hash
    (lambda (slot-response transactions-response)
      (let* ((slot-data (marco-polo-get-in-hash '("Right" "cbsEntry") slot-response))
             (transactions (gethash "Right" transactions-response))
             (tx-num (gethash "cbeTxNum" slot-data))
             (buffer-name (concat "*Slot-" block-hash "*"))
             (buffer (get-buffer-create buffer-name)))
        (switch-to-buffer buffer)
        (with-current-buffer buffer
          (read-only-mode 0)
          (erase-buffer)
          (insert (propertize "Slot" 'face 'header-line))
          (insert "\n\n\n")
          (insert (marco-polo-format-slot-summary slot-response))
          (insert "\n\n\n")
          (insert (propertize (format "Transactions (%d)" tx-num) 'face 'header-line))
          (insert "\n\n\n")
          (insert (marco-polo-format-transactions transactions))
          (read-only-mode 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; slots ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar marco-polo-slots-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "+") 'marco-polo-do-load-more-slots)
    (define-key map (kbd "q") 'kill-current-buffer)
    map)
  "Keymap for `marco-polo-slots-mode'.")

(defvar marco-polo-slot-row-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'marco-polo-view-slot-from-point)
    map)
  "Keymap for `marco-polo-slot-row'.")

(defun marco-polo-do-load-more-slots ()
  "Click on load more button programmatically."
  (interactive)
  (push-button (- (point-max) 1)))

(define-derived-mode marco-polo-slots-mode fundamental-mode "marco-polo-slots"
  "Major mode for slots.")

(defun marco-polo-view-slot-from-point ()
  "View slot by reading text-properties at point."
  (interactive)
  (let* ((pos (point))
         (block-hash (get-text-property pos 'block-hash)))
    (marco-polo-view-slot block-hash)))

(defun marco-polo-format-slot (slot-data)
  "Format hash-table SLOT-DATA."
  (let* ((epoch-num (marco-polo-get-epoch-num slot-data))
         (slot-num (marco-polo-get-slot-num slot-data))
         (time-issued (gethash "cbeTimeIssued" slot-data))
         (tx-num (gethash "cbeTxNum" slot-data))
         (total-sent (string-to-number (marco-polo-get-in-hash '("cbeTotalSent" "getCoin") slot-data)))
         (slot-leader (substring (gethash "cbeBlockLead" slot-data) 0 8))
         (bytes (gethash "cbeSize" slot-data))
         (column-gap "  "))
    (concat
      (propertize
        (format "[%d, %d]" epoch-num slot-num)
        'face 'marco-polo-epoch-slot-face)
      column-gap
      (propertize
        (format "<%s>" (marco-polo-format-unix-timestamp time-issued))
        'face 'marco-polo-time-issued-face)
      column-gap
      (propertize
        (format "%5sTx" tx-num)
        'face 'marco-polo-tx-num-face)
      column-gap
      (propertize
        (format "%20s" (marco-polo-format-ada (marco-polo-lovelace-to-ada total-sent)))
        'face 'marco-polo-total-sent-face)
      column-gap
      (propertize
        slot-leader
        'face 'marco-polo-slot-leader-face)
      column-gap
      (propertize
        (format "%8sB" bytes)
        'face 'marco-polo-bytes-face))))

(defun marco-polo-load-more-slots (button page-num)
  "Load slots in the given PAGE-NUM when load previous BUTTON is clicked."
  (marco-polo-list-block-pages
    page-num
    (lambda (response)
      (let* ((slots (car (cdr (gethash "Right" response))))
             (buffer-name "*Slots*"))
        (with-current-buffer buffer-name
          (read-only-mode 0)
          (goto-char (button-start button))
          (dolist (slot-data slots)
            (insert (marco-polo-slot-row slot-data))
            (insert "\n"))
          (button-put button
            'action (lambda (_button)
                      (funcall 'marco-polo-load-more-slots _button (- page-num 1))))
          (read-only-mode 1))))))

(defun marco-polo-slot-row (slot-data)
  "Generate slot row from SLOT-DATA."
  (propertize (marco-polo-format-slot slot-data)
    'keymap marco-polo-slot-row-map
    'epoch-num (marco-polo-get-epoch-num slot-data)
    'slot-num (marco-polo-get-slot-num slot-data)
    'block-hash (marco-polo-get-block-hash slot-data)))

(defun marco-polo-view-slots ()
  "View slots."
  (marco-polo-list-block-pages
    nil
    (lambda (response)
      (let* ((current-page-num (car (gethash "Right" response)))
             (slots (car (cdr (gethash "Right" response))))
             (buffer-name "*Slots*")
             (buffer (get-buffer-create buffer-name)))
        (switch-to-buffer buffer)
        (with-current-buffer buffer
          (erase-buffer)
          (marco-polo-slots-mode)
          (dolist (slot-data slots)
            (insert (marco-polo-slot-row slot-data))
            (insert "\n"))
          (insert-text-button
            (substitute-command-keys
              (format "Type \\<%s>\\[%s] to previous slots"
                'marco-polo-slots-mode-map
                'marco-polo-do-load-more-slots))
            'action (lambda (_button)
                      (funcall 'marco-polo-load-more-slots _button (- current-page-num 1)))
            'follow-link t)
          (read-only-mode 1))))))

;;; marco-polo-slots.el ends here
