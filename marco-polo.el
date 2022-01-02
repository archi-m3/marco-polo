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

(defun marco-polo-slots ()
  "Action for viewing slow."
  (interactive)
  (marco-polo-view-slots))

(defun marco-polo-saved-addresses ()
  "ACtion for saved-addresses.  CONFIG-FILE-PATH is a json file."
  (interactive)
  (marco-polo-view-saved-addresses (read-file-name "Config file:")))

(defun marco-polo-search-address ()
  "Action for searching address."
  (interactive)
  (marco-polo-view-address (read-string "Enter address: ")))

(global-set-key (kbd "C-c c d o") 'marco-polo-saved-addresses)
(global-set-key (kbd "C-c c d f") 'marco-polo-search-address)
(global-set-key (kbd "C-c c d s") 'marco-polo-slots)

;;; _
(provide 'marco-polo)
;; End:
;;; marco-polo.el ends here
