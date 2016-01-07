;;; slirm.el -- Systematic Literature Review Mode for Emacs.

;;; Commentary:
;;;
;;; Copyright (c) 2016, Florian Biermann fbie@itu.dk
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

;;; Code:

(require 'bibtex)

;;; BibTeX utility functions for moving point from entry to entry and
;;; to access fields conveniently.
(defconst slirm--next 're-search-forward)
(defconst slirm--prev 're-search-backward)

(defun slirm--bibtex-move-point-to-entry (direction)
  "Move point to the next entry in DIRECTION, which is one of slirm--{next, prev}."
  (when (save-excursion
	  (funcall direction "^@[a-zA-Z0-9]+{" nil t))
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
  (bibtex-parse-entry t))

(defun slirm--bibtex-reparse ()
  "Re-parse an entry, useful after modifications and so on."
  (slirm--bibtex-move-point-to-entry slirm--prev)
  (bibtex-parse-entry t))

(defun slirm--bibtex-move-point-to-field (field)
  "Move point to start of FIELD's text."
  (when (save-excursion
	  (re-search-backward (format "\s*%s\s*=[\s\t]*{" field) nil t))
    (goto-char (match-end 0))))

(defun slirm--bibtex-get-field (field entry)
  "Nil if FIELD is not present in ENTRY, otherwise the associated value."
  (let ((val (assoc field entry)))
    (when val
	(cdr val))))

(defun slirm--bibtex-add-field (field)
  "Add a field FIELD to the entry."
  (bibtex-make-field field t 'nil 'nil))

(defun slirm--bibtex-maybe-add-field (field entry)
  "Add FIELD to ENTRY if not already present."
  (when (not (slirm--bibtex-get-field field entry))
    (slirm--bibtex-add-field field)
    t))

(defun slirm--bibtex-write-to-field (field content)
  "Fill a FIELD with CONTENT if CONTENT is non-nil."
  (when content
    (slirm--bibtex-move-point-to-field field)
    (insert content)))

(defun slirm--bibtex-maybe-write-to-field (field entry content)
  "Write to FIELD if ENTRY does not contain it.  CONTENT is what is written if non-nil."
  (when (and content (slirm--bibtex-maybe-add-field field entry))
    (slirm--bibtex-write-to-field field content)))

(defconst slirm--review "review" "The review field name.")
(defconst slirm--accept "accepted")
(defconst slirm--reject "rejected")
(defconst slirm--abstract "abstract" "The abstract field name.")
(defconst slirm--full-text-url "fullTextUrl" "The fullTextUrl field name.")

(defun slirm--make-user-annotation (annotation)
  "Make a string of the form \"user-login-name: ANNOTATION\"."
  (format "%s: %s," user-login-name annotation))

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
  (when link
      (format "http://dl.acm.org/%s" link)))

(defun slirm--acm-get-links (acm-url)
  "Retrieves the links to the abstract and the full-text by retrieving ACM-URL."
  (with-current-buffer (url-retrieve-synchronously acm-url)
    (mapcar 'slirm--acm-make-dl-link
	    (list (slirm--acm-get-abstract-link)
		  (slirm--acm-get-full-text-link)))))

(defun slirm--acm-get-abstract (url)
  "Download and format abstract text from URL."
  (with-current-buffer (url-retrieve-synchronously url)
    (replace-regexp-in-string "<\/?[a-zA-Z]+>" "" (or (slirm--first-match "<p>.*</p>") "<Missing>"))))

;; Slirm URL handlers.
(defvar slirm--get-links-map nil)
(defvar slirm--get-abstract-map nil)

(defun slirm--lookup (map key)
  "Perform lookup in MAP for KEY."
  (car (cdr (assoc key map))))

(defun slirm-add-handlers (url links-handler abstract-handler)
  "Add handlers for URL, e.g. \"acm.org\".  LINKS-HANDLER must accept a url and return a list of links, ABSTRACT-HANDLER must accept a url and return a string."
  (setq slirm--get-links-map (cons (list url links-handler) slirm--get-links-map))
  (setq slirm--get-abstract-map (cons (list url abstract-handler) slirm--get-abstract-map)))

(slirm-add-handlers "acm.org" 'slirm--acm-get-links 'slirm--acm-get-abstract)

;; Functions for downloading webcontent, based on the mode handlers.

(defun slirm--get-base-url (url)
  "Return the base url of URL."
  (string-match "[a-zA-Z0-0+\\.-]+\\.[a-zA-Z]+" url)
  (let ((es (reverse (split-string (match-string 0 url) "\\."))))
    (format "%s.%s" (car (cdr es)) (car es))))

(defun slirm--get-links (url)
  "Get links from URL."
  (let ((getter (slirm--lookup slirm--get-links-map (slirm--get-base-url url))))
    (funcall getter url)))

(defun slirm--get-abstract (url)
  "Get abstract from URL."
  (let ((getter (slirm--lookup slirm--get-abstract-map (slirm--get-base-url url))))
    (funcall getter url)))

(defun slirm--update-abstract-fullTextUrl (entry)
  "Update abstract and fullTextURL fields if they are empty in ENTRY."
  (when (not (and ;; Any of the two fields is empty.
	      (slirm--bibtex-get-field slirm--abstract entry)
	      (slirm--bibtex-get-field slirm--full-text-url entry)))
    (let* ((url (slirm--bibtex-get-field "url" entry))
	   (urls (slirm--get-links url))) ;; Download from the article's website.
      (slirm--bibtex-maybe-write-to-field slirm--abstract entry (slirm--get-abstract (car urls)))
      (slirm--bibtex-maybe-write-to-field slirm--full-text-url entry (car (cdr urls)))
      (save-buffer))))

(defun slirm-update-abstract-fullTextUrl ()
  "Update abstract and fullTextURL fields if they are empty."
  (interactive)
  (slirm--with-bibtex-buffer
    (slirm--update-abstract-fullTextUrl (slirm--bibtex-reparse))))

;; The main Slirm interaction functions.

(defun slirm--mark-reviewed (entry verdict)
  "Mark ENTRY as reviewed with VERDICT."
  (slirm--bibtex-maybe-add-field slirm--review entry)
  (let* ((entry (slirm--bibtex-reparse))
	 (reviews (slirm--to-review-string (slirm--set-review
					    (slirm--to-review-list entry)
					    verdict))))
    (slirm--bibtex-move-point-to-field slirm--review)
    (when (save-excursion
	    (re-search-forward "},\n" nil t)) ;; Find and delete review field content.
      (delete-region (point) (match-beginning 0))
      (slirm--bibtex-write-to-field slirm--review reviews)
      (message (format
		"Marked %s as %s."
		(slirm--bibtex-get-field "=key=" entry)
		verdict)))))

(defun slirm--to-review-list (entry)
  "Return reviews in ENTRY as a list of pairs."
  (let ((reviews (slirm--bibtex-get-field slirm--review entry)))
    (when reviews
      (mapcar (lambda (s)
		(split-string s ":\s+"))
	      (split-string reviews ",\s+" t)))))

(defun slirm--to-review-string (reviews)
  "Return REVIEWS as a nicely formatted string."
  (string-join
   (mapcar (lambda (ss)
	     (format "%s: %s" (car ss) (car (cdr ss))))
	   reviews)
   ", "))

(defun slirm--format-verdict (verdict)
  "Formats VERDICT as \"VERDICT <timestamp>\"."
  (format "%s \<%s\>" verdict (format-time-string "%Y-%m-%d %T")))

(defun slirm--set-review (reviews verdict)
  "Set the current user's review in REVIEWS to VERDICT or append to the end of the list."
  (if reviews
      (let ((head (car reviews))
	    (tail (cdr reviews)))
	(if (string-equal (car head) user-login-name) ;; Current first entry is of this user.
	    (cons (list user-login-name (slirm--format-verdict verdict)) tail) ;; Exchange it for new one.
	  (cons head (slirm--set-review verdict tail)))) ;; Else keep and recurse.
    (list (list user-login-name (slirm--format-verdict verdict)))))

(defun slirm-accept ()
  "Mark current entry as accepted."
  (interactive)
  (slirm--with-bibtex-buffer
    (slirm--mark-reviewed (slirm--bibtex-reparse) slirm--accept)))

(defun slirm-reject ()
  "Mark current entry as rejected."
  (interactive)
  (slirm--with-bibtex-buffer
    (slirm--mark-reviewed (slirm--bibtex-reparse) slirm--reject)))

(defun slirm--clear ()
  "Clear current slirm buffer."
  (delete-region (point-min) (point-max)))

(defun slirm--insert-title (title)
  "Insert and format TITLE at point."
  (let ((ttitle (format "%s:" title)))
    (put-text-property 0 (length ttitle) 'face 'bold ttitle)
    (insert ttitle)))

(defun slirm--insert-paragraph (title text)
  "Insert and format a paragraph with TITLE as header and TEXT as body."
  (slirm--insert-title title)
  (insert (format " %s" text))
  (fill-paragraph t)
  (insert "\n"))

(defun slirm--insert-line (title text)
  "Insert text as TITLE: TEXT without further formatting."
  (slirm--insert-title title)
  (insert (format " %s\n" text)))

(defun slirm--insert-newline ()
  "Insert two visible newlines."
  (insert "\n"))

(defun slirm--insert-indent (indent text)
  "Insert a line indented by INDENT spaces, containing TEXT."
  (when text
    (insert (format "%s%s" (make-string indent ? ) text))))

(defun slirm--show (entry)
  "Show ENTRY in the review buffer."
  (slirm--override-readonly
    (slirm--clear)
    (save-excursion
      (slirm--insert-paragraph "Title" (slirm--bibtex-get-field "title" entry))
      (slirm--insert-newline)
      (slirm--insert-paragraph "Author(s)" (slirm--bibtex-get-field "author" entry))
      (slirm--insert-newline)
      (slirm--insert-line "Year" (slirm--bibtex-get-field "year" entry))
      (slirm--insert-newline)
      (slirm--insert-paragraph "In" (or (slirm--bibtex-get-field "booktitle" entry)
					(slirm--bibtex-get-field "journal" entry)))
      (slirm--insert-newline)
      (slirm--insert-paragraph "Abstract" (slirm--bibtex-get-field "abstract" entry))
      (slirm--insert-newline)
      (slirm--insert-paragraph "Keywords" (slirm--bibtex-get-field "keywords" entry))
      (slirm--insert-newline)
      (slirm--insert-line "Reviews" "\n")
      (let ((reviews (slirm--to-review-list entry)))
	(dolist (review reviews)
	  (slirm--insert-indent 2 (format "%s: %s\n" (car review ) (nth 1 review))))))))

(defun slirm--update-and-show (entry)
  "Show ENTRY in the review buffer after update."
  (slirm--show
   (slirm--with-bibtex-buffer
     (slirm--update-abstract-fullTextUrl entry)
     (slirm--bibtex-reparse))))

(defun slirm-show-next ()
  "Show the next entry in the review buffer."
  (interactive)
  (slirm--update-and-show (slirm--with-bibtex-buffer
			    (slirm--bibtex-parse-next))))

(defun slirm-show-prev ()
  "Show the previous entry in the review buffer."
  (interactive)
  (slirm--update-and-show (slirm--with-bibtex-buffer
			    (slirm--bibtex-parse-prev))))

(defun slirm--find-next-entry (predicate)
  "Find next entry for which PREDICATE holds or the last entry in the file."
  (let ((entry (slirm--bibtex-parse-next)))
    (while (and (funcall predicate entry)
		(< (- (point) (point-max)) 3))
      (setq entry (slirm--bibtex-parse-next)))
    entry))

(defun slirm--find-next-undecided ()
  "Return next undecided entry or the last entry in the list."
  (slirm--find-next-entry (lambda (entry)
			    (let ((review (slirm--bibtex-get-field slirm--review entry)))
			      (when review
				(string-match user-login-name review))))))

(defun slirm-show-next-undecided ()
  "Show next undecided entry after current point."
  (interactive)
  (slirm--update-and-show
   (slirm--with-bibtex-buffer
    (slirm--find-next-undecided))))

(defun slirm-show-first-undecided ()
  "Show the first not yet annotated entry."
  (interactive)
  (slirm--update-and-show
   (slirm--with-bibtex-buffer
     (goto-char 0)
     (slirm--find-next-undecided))))

(defun slirm-accept-or-reject ()
  "Choose whether to accept or reject entry and continue to next undecided."
  (interactive)
  (if (yes-or-no-p "Accept current entry? ")
      (slirm-accept)
    (slirm-reject))
  (slirm-show-next-undecided))

;; Mode hook.
(defvar slirm-mode-hook nil)

;; Local variables for keeping track of the corresponding BibTeX file
;; and the corresponding point.
(defvar slirm--bibtex-file-tmp "" "The name of the BibTeX file, temporarily.")
(defvar-local slirm--bibtex-file "" "The name of the BibTeX file.")
(defvar-local slirm--point 0 "Slirm's point in the BibTeX file.")

(defun slirm--bibtex-buffer ()
  "Return the buffer containing the BibTeX file."
  (save-window-excursion
    (let ((other (find-file slirm--bibtex-file)))
      (bury-buffer other)
      other)))

(defun slirm-start ()
  "Start a systematic literature review of the BibTeX file in the current buffer."
  (interactive)
  (let ((file (buffer-file-name)))
    (switch-to-buffer (get-buffer-create (format "*Review of %s*" file)))
    (setq slirm--bibtex-file-tmp file)
    (slirm-mode)))

;; Macros to handle with-current-buffer so that we can keep references
;; to the point of the buffer that we modify.

(defmacro slirm--with-current-buffer (buffer &rest body)
  "Like (with-current-buffer BUFFER (save-excursion &BODY)) but save the point."
  (declare (indent 1))
  ;; We jump through a bunch of hoops to keep a buffer-local reference
  ;; to our point in the BibTeX buffer.
  (let ((body-res (cl-gensym "body-res"))
	(current-point (cl-gensym "current-point")))
    `(let ((,current-point slirm--point)
	   (,body-res nil))
       (with-current-buffer ,buffer
	 (save-excursion
	   (goto-char ,current-point)	  ;; Load point.
	   (setq ,body-res (progn ,@body) ;; Execute body and bind result.
		 ,current-point (point)))) ;; Store point and return to BibTeX buffer.
       (setq slirm--point ,current-point)
       ,body-res))) ;; Return body's result.

(defmacro slirm--with-bibtex-buffer (&rest body)
  "Perform BODY in slirm--bibtex-buffer."
  (declare (indent 0))
  `(slirm--with-current-buffer (slirm--bibtex-buffer)
     ,@body))

(defmacro slirm--override-readonly (&rest body)
  "Execute BODY, overriding readonly mode."
  (declare (indent 0))
  `(progn
     (setq inhibit-read-only t)
     ,@body
     (setq inhibit-read-only nil)))

(define-derived-mode slirm-mode special-mode
  "Systematic Literature Review Mode."
  (setq slirm--bibtex-file slirm--bibtex-file-tmp)
  (bury-buffer (slirm--bibtex-buffer))
  (slirm-show-first-undecided))

(define-key slirm-mode-map (kbd "n") 'slirm-show-next)
(define-key slirm-mode-map (kbd "C-n") 'slirm-show-next-undecided)
(define-key slirm-mode-map (kbd "p") 'slirm-show-prev)
(define-key slirm-mode-map (kbd "C-f") 'slirm-show-first-undecided)
(define-key slirm-mode-map (kbd "SPC") 'slirm-accept-or-reject)

(provide 'slirm)
;;; slirm.el ends here
