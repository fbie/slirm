;;; slirm.el -- Systematic Literature Review Mode for Emacs.
;;; Commentary:
;;; Code:

(require 'bibtex)

(defun slirm--bibtex-move-point-to-next-entry ()
  "Move point to the next entry."
  (when (re-search-forward "^@[a-zA-Z0-9]+{" nil t)
    (goto-char (match-beginning 0))))

(defun slirm--bibtex-move-point-to-field (field)
  "Move point to start of FIELD's text."
  (when (re-search-backward (format "\s*%s\s*=[\s\t]*{" field) nil t)
    (goto-char (match-end 0))))

(defun slirm--bibtex-get-field (field entry)
  "Nil if FIELD is not present in ENTRY, otherwise the associated value."
  (let ((val (assoc field entry)))
    (if val
	(cdr val)
      nil)))

(defun slirm--bibtex-add-field (field)
  "Add a field FIELD to the entry."
  (bibtex-make-field field t t 'nil))

(defun slirm--bibtex-maybe-add-field (field entry)
  "Add FIELD to ENTRY if not already present."
  (when (not (slirm--get-field field entry))
    (slirm--add-field field)))

(defvar slirm--review "review" "The review field name.")

(defun slirm-parse-next-entry ()
  "Testing."
  (interactive)
  (slirm--bibtex-move-point-to-next-entry)
  (let ((entry (bibtex-parse-entry t)))
    (message (slirm--bibtex-get-field slirm--review entry))
    (slirm--bibtex-maybe-add-field slirm--review entry)
    (slirm--bibtex-move-point-to-field slirm--review)
    ))

(provide 'slirm)
;;; slirm.el ends here
