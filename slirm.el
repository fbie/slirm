;;; slirm.el -- Systematic Literature Review Mode for Emacs.
;;; Commentary:
;;; Code:

(require 'bibtex)

;;; BibTeX utility functions for moving point from entry to entry and
;;; to access fields conveniently.
(defconst slirm--next 're-search-forward)
(defconst slirm--prev 're-search-backward)

(defun slirm--bibtex-move-point-to-entry (direction)
  "Move point to the next entry in DIRECTION, which is one of slirm--{next, prev}."
  (when (funcall direction "^@[a-zA-Z0-9]+{" nil t)
    (goto-char (match-beginning 0))))

(defun slirm--bibtex-parse-next ()
  "Convenience function to parse next entry."
  (slirm--bibtex-move-point-to-entry slirm--next)
  (bibtex-parse-entry t))

(defun slirm--bibtex-parse-prev ()
  "Convenience fuction to parse previous entry."
  ;; Gotta move up twice.
  (slirm--bibtex-move-point-to-entry slirm--prev)
  (slirm--bibtex-move-point-to-entry slirm--prev)
  (bibtex-parse-entry t)
  )

(defun slirm--bibtex-reparse ()
  "Re-parse an entry, useful after modifications and so on."
  (slirm--bibtex-move-point-to-entry slirm--prev)
  (bibtex-parse-entry t)
  )

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
  (bibtex-make-field field t 'nil 'nil))

(defun slirm--bibtex-maybe-add-field (field entry)
  "Add FIELD to ENTRY if not already present."
  (when (not (slirm--get-field field entry))
    (slirm--add-field field)
    t))

(defun slirm--bibtex-write-to-field (field content)
  "Fill a FIELD with CONTENT."
  (slirm--bibtex-move-point-to-field field)
  (insert content))

(defun slirm--bibtex-maybe-write-to-field (field entry content)
  "Write to FIELD if ENTRY does not contain it.  CONTENT is what is written."
  (when (slirm--bibtex-maybe-add-field field entry)
    (slirm--bibtex-write-to-field field content)))

(defconst slirm--review "review" "The review field name.")
(defconst slirm--abstract "abstract" "The abstract field name.")
(defconst slirm--full-text-url "fullTextUrl" "The fullTextUrl field name.")

(defun slirm--make-user-annotation (annotation)
  "Make a string of the form \"user-login-name: ANNOTATION\"."
  (format "%s: %s" user-login-name annotation))

(defun slirm--first-match (regex)
  "Return the first string matching REGEX in the entire buffer."
  (goto-char (point-min))
  (when (re-search-forward regex nil t)
    (match-string 0)))

;;; ACM utility functions to download full-text and abstract.
(defun slirm--acm-get-full-text-link ()
  "Return the link to the full-text from the current buffer containing an ACM website."
  (slirm--first-match "ft_gateway\.cfm\\?id=[0-9]+&ftid=[0-9]+&dwn=[0-9]+&CFID=[0-9]+&CFTOKEN=[0-9]+"))

(defun slirm--acm-get-abstract-link ()
  "Return the link to the abstract from the current buffer containing an ACM website."
  (slirm--first-match "tab_abstract\.cfm\\?id=[0-9]+&usebody=tabbody&cfid=[0-9]+&cftoken=[0-9]+"))

(defun slirm--acm-make-dl-link (link)
  "Build ACM link address from LINK."
  (format "http://dl.acm.org/%s" link))

(defun slirm--acm-get-links (acm-url)
  "Retrieves the links to the abstract and the full-text by retrieving ACM-URL."
  (with-current-buffer (url-retrieve-synchronously acm-url)
    (mapcar 'slirm--acm-make-dl-link
	    (list (slirm--acm-get-abstract-link)
		  (slirm--acm-get-full-text-link)))))

(defun slirm--acm-get-abstract (url)
  "Download and format abstract text from URL."
  (with-current-buffer (url-retrieve-synchronously url)
    (replace-regexp-in-string "<\/?[a-zA-Z]+>" "" (slirm--first-match "<p>.*</p>"))))

(defun slirm-acm-get-links ()
  "Testing."
  (interactive)
  (slirm--bibtex-move-point-to-entry slirm--next)
  (let* ((entry (bibtex-parse-entry t))
	 (url (slirm--bibtex-get-field "url" entry))
	 (urls (slirm--acm-get-links url)))
    (slirm--bibtex-maybe-add-field "abstract" entry)
    (slirm--bibtex-write-to-field "abstract" (slirm--acm-get-abstract (car urls)))
    (slirm--bibtex-maybe-add-field "fullTextUrl" entry)
    (slirm--bibtex-write-to-field "fullTextUrl" (car (cdr urls)))
    ))

(defun slirm-parse-next-entry ()
  "Testing."
  (interactive)
  (slirm--bibtex-move-point-to-entry slirm--next)
  (let ((entry (bibtex-parse-entry t)))
    (message (slirm--bibtex-get-field slirm--review entry))
    (slirm--bibtex-maybe-add-field slirm--review entry)
    (slirm--bibtex-move-point-to-field slirm--review)
    (insert (slirm--make-user-annotation "reject"))
    ))

(provide 'slirm)
;;; slirm.el ends here
