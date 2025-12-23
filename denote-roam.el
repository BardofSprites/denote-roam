;;; denote-roam.el --- A bridge between denote and org-roam -*- lexical-binding: t -*-

;; Author: Daniel Pinkston
;; Maintainer: Daniel Pinkston
;; Version: 0.1.0
;; Package-Requires: ((emacs 28.1) (denote 4.1.3) (org-roam 2.3.1))
;; Homepage: https://github.com/BardofSprites/denote-roam
;; Keywords: denote, org-roam, note taking, notes


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Some useful extensions for using denote and org-roam to manage your
;; notes.
;;
;; Denote-roam combines the power of org-roam linking and denote
;; system. Denote is intended to be used in this workflow as a means
;; of note creation and management. Org-roam is used for linking
;; between notes and possibly visualizing their relationships with
;; packages like org-roam-ui, although this functionality is outside
;; the scope of this package.

;;; Code:
(require 'denote)
(require 'org-roam)
(require 'dired)

(defgroup denote-roam ()
  "Creating, linking, and managing files with denote and org-roam."
  :group 'files
  :link '(url-link :tag "Homepage" "https://github.com/BardofSprites/denote-roam"))

(defcustom denote-roam-include-journal nil
  "Non-nil to include Denote journal files in the Org-roam linking system.
When nil, journal files are not assigned IDs and ignored by Org-roam."
  :group 'denote-roam
  :type 'boolean)

(defvar denote-roam--orig-file-type denote-file-type
  "Original value of `denote-file-type' before enabling `denote-roam-mode'.")

(defvar denote-roam--orig-denote-dir denote-directory
  "Original value of `denote-directory' before enabling `denote-roam-mode'.")

(defvar denote-roam--orig-org-roam-dir org-roam-directory
  "Original value of `org-roam-directory' before enabling `denote-roam-mode'.")

(defcustom denote-roam-directory "~/Notes/"
  "Directory, as a string, for storing notes.
This is the destination all file-creating commands."
  :group 'denote-roam
  :type 'string)

(defun denote-roam--setup ()
  "Setup `denote-roam-mode'."
  ;; remember original file type
  (setq denote-roam--orig-file-type denote-file-type)
  (setq denote-file-type 'org)
  (when (and (boundp 'org-roam-directory)
             (boundp 'denote-directory))
    (denote-roam--sync-directories))
  ;; Add hook to insert ID
  (add-hook 'denote-after-new-note-hook
            #'denote-roam-maybe-insert-id))

(defun denote-roam--reset ()
  "Reset variables to original values."
  (setq denote-file-type denote-roam--orig-file-type)
  (when denote-roam--orig-denote-dir
    (setq denote-directory denote-roam--orig-denote-dir))

  (when denote-roam--orig-org-roam-dir
    (setq org-roam-directory denote-roam--orig-org-roam-dir))
  (remove-hook 'denote-after-new-note-hook
               #'denote-roam-maybe-insert-id))

;;;###autoload
(define-minor-mode denote-roam-mode
  "Global minor mode to toggle Denote–Org-roam integration."
  :global t
  :group 'denote-roam
  (if denote-roam-mode
      (denote-roam--setup)
    (denote-roam--reset)))

(defun denote-roam--sync-directories ()
  "Set Denote and Org-roam directories to `denote-roam-directory'."
  (let ((dir (expand-file-name denote-roam-directory)))
    (unless (file-directory-p dir)
      (user-error "`denote-roam-directory' does not exist: %s" dir))
    (setq denote-directory dir)
    (setq org-roam-directory dir)))

(defun denote-roam--require-org-file-type ()
  "Signal an error unless Denote is effectively using Org files.
Accepts nil (the default) or the symbol `org'."
  (let ((ft denote-file-type))
    (unless (or (null ft)
                (eq ft 'org))
      (user-error
       "`denote-file-type' must be nil or `org' for denote-roam to work; got: %S"
       ft))))

(defmacro denote-roam--with-check (&rest body)
  "Run BODY only if `denote-file-type' is `org'."
  `(progn
     (denote-roam--require-org-file-type)
     ,@body))

(defun denote-roam-insert-id ()
  "Insert or replace a top-level :ID: property at the start of the file.
ID is placed inside a :PROPERTIES: drawer inserted at the beginning of
the buffer."
  (when (derived-mode-p 'org-mode)
    (denote-roam--with-check
     (org-with-wide-buffer
      (goto-char (point-min))
      ;; If file already starts with a :PROPERTIES: drawer, remove it
      (when (looking-at ":PROPERTIES:")
        (let ((end (save-excursion
                     (re-search-forward ":END:" nil t))))
          (when end
            (delete-region (point-min) (min (point-max) (1+ end)))))))
     ;; Insert fresh ID drawer at the very top
     (goto-char (point-min))
     (let ((id (org-id-new)))
       (insert (format ":PROPERTIES:\n:ID:       %s\n:END:\n" id)))
     (save-buffer))))

(defun denote-roam-maybe-insert-id ()
  "Insert a top-level :ID: unless this is a denote journal file."
  (let ((file (buffer-file-name)))
    (unless (and file
                 (boundp 'denote-journal-directory)
                 (string-prefix-p (expand-file-name denote-journal-directory)
                                  (expand-file-name file)))
      (denote-roam-insert-id))))

(defun denote-roam--region-text ()
  "Return region text if active, otherwise nil.
Does NOT delete the region."
  (when (use-region-p)
    (buffer-substring-no-properties
     (region-beginning)
     (region-end))))

(defun denote-roam--construct-link (id description)
  "Create an org link for insertion using ID and DESCRIPTION parameters.
If the region is active replace the description with its text."
  (let ((link (org-link-make-string (concat "id:" id) description)))
    (if (use-region-p)
        (replace-region-contents
         (region-beginning)
         (region-end)
         (lambda () link))
      (insert link))))

(defun denote-roam-insert-node (title)
  "Denote analogy for `org-roam-insert-node', takes TITLE as node title."
  (let* ((keywords (denote-keywords-prompt))
         (file (denote title keywords nil nil nil nil nil nil)))

    ;; extract uuid from file that is autogenerated by `denote-roam-insert-id-at-top'
    (when-let ((buf (find-buffer-visiting file)))
      (with-current-buffer buf
        (save-buffer)
        (kill-buffer buf)))

    (let ((uuid
           (with-temp-buffer
             (insert-file-contents file)
             (goto-char (point-min))
             (when (re-search-forward "^:ID:\\s-+\\([[:alnum:]-]+\\)" nil t)
               (match-string 1)))))
      (unless uuid
        (error "No :ID: found in Denote file %s" file))
      uuid)))


;;;###autoload
(defun denote-roam-insert-or-create-node ()
  "Insert an Org-roam link if the note exists; otherwise create via Denote."
  (interactive)
  ;; Prompt for an existing node, but do not auto-create
  (let* ((region-text (denote-roam--region-text))
         (node (org-roam-node-read region-text))
         (title (org-roam-node-title node))
         (description (or region-text title)))
    (if node
        ;; Node exists: insert Org-roam link at point
        (if-let ((id (org-roam-node-id node)))
            (progn (denote-roam--construct-link id description)
                   (message "Description %s" description))
          ;; Node does not exist (no id): create via Denote
          (let ((id (denote-roam-insert-node title)))
            (denote-roam--construct-link id description))))))

;;;###autoload
(defun denote-roam-find-or-create-node ()
  "Find an Org-roam node by title; if missing, create via Denote."
  (interactive)
  (let* ((node (org-roam-node-read nil nil)) ;; no auto-create; just selection
         (title (org-roam-node-title node)))
    (if (org-roam-node-id node)
        ;; node exists then visit
        (org-roam-node-visit node)
      ;; node missing then create via Denote
      (let ((keyword (denote-keywords-prompt)))
        (denote title keyword)))))

(defun denote-roam--node-files ()
  "Return a list of Denote files that are indexed by Org-roam."
  (denote-roam--require-org-file-type)
  (let ((denote-dir (file-truename (denote-directory))))
    (seq-filter
     (lambda (file)
       (string-prefix-p denote-dir (file-truename file)))
     (mapcar #'org-roam-node-file (org-roam-node-list)))))

(defun denote-roam--find-files-unlinked (dir recursive)
  "Return list of files in DIR whose first non-empty line is not :PROPERTIES:.
If RECURSIVE is non-nil, search subdirectories as well."
  (let* ((files
          (if recursive
              (directory-files-recursively dir ".*" nil nil t)
            (directory-files dir t directory-files-no-dot-files-regexp)))
         bad-files)
    (dolist (file files)
      (when (denote-file-is-note-p file)
        (with-temp-buffer
          (insert-file-contents file nil 0 500)
          (goto-char (point-min))
          (unless (looking-at-p "^:PROPERTIES:$")
            (push file bad-files)))))
    (nreverse bad-files)))

;;;###autoload
(defun denote-roam-dired-unlinked ()
  "Present a list of files that aren't linked with org-roam in a Dired buffer."
  (interactive)
  (let* ((recursive (y-or-n-p "Recursive searching? "))
         (directory (file-truename denote-roam-directory))
         (files (denote-roam--find-files-unlinked directory recursive))
         ;; relative files short name in dired
         (relative-files (mapcar
                          (lambda (f)
                            (file-relative-name f directory))
                          files))
         (buffer-name "*Denote Roam Unlinked*"))
    (if relative-files
        (dired (cons directory relative-files))
      (message "No unlinked Denote files found."))))

;;;###autoload
(defun denote-roam-dired ()
  "Show notes contained linked in org-roam in a Dired buffer."
  (interactive)
  (let* ((recursive (y-or-n-p "Recursive searching? "))
         (files (denote-roam--node-files))
         (directory (file-truename denote-roam-directory))
         (buffer-name "*Denote Roam Linked*")
         ;; relative files short name in dired
         (relative-files (mapcar
                          (lambda (f)
                            (file-relative-name f directory))
                          files)))
    (if relative-files
        (dired (cons directory relative-files))
      (message "No linked Denote files found."))))

(defalias 'denote-roam-linked-dired 'denote-roam-dired
  "Alias for `denote-roam-dired' command.")

(provide 'denote-roam)

;;; denote-roam.el ends here
