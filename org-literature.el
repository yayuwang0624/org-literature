;;; org-literature.el --- Paper library on citar, org-noter and pdf-tools  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Yayu Wang

;; Author: Yayu Wang <st.saint.wyy@gmail.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1") (citar "1.4") (org "9.4") (org-noter "1.4.1") (pdf-tools "0.8"))
;; Keywords: bib, outlines, convenience
;; URL: https://github.com/yayuwang0624/org-literature

;; This program is free software; you can redistribute it and/or modify
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

;; A paper library kept in one folder: a BibTeX file, PDFs named after
;; citation keys, and one org note per paper.  Papers are added from
;; BibTeX, a DOI, an arXiv ID or a title, tagged through the keywords
;; field, and opened through `citar-open', which also offers to add a
;; missing PDF while `org-literature-mode' is on.
;;
;; The package also loads org-pdftools.el, which provides `pdf:' links,
;; and org-noter-pdftools.el, which ties org-noter notes to PDF
;; annotations.  Both come from fuxialexander/org-pdftools.

;;; Code:

(require 'bibtex)
(require 'citar)
(require 'json)
(require 'org)
(require 'org-noter-pdftools)
(require 'org-pdftools)
(require 'seq)
(require 'subr-x)
(require 'ucs-normalize)
(require 'url)

(defvar consult-preview-key)
(defvar consult-ripgrep-args)
(defvar url-http-response-status)
(declare-function consult-ripgrep "consult")
(declare-function pdf-view-goto-page "pdf-view")

(defgroup org-literature nil
  "Paper library on citar, org-noter and pdf-tools."
  :group 'org)

(defcustom org-literature-directory (expand-file-name "literature/" org-directory)
  "Paper library, holding refs.bib, pdf/ and note/."
  :type 'directory)

(defcustom org-literature-bibliography (expand-file-name "refs.bib" org-literature-directory)
  "BibTeX file with one entry per paper."
  :type 'file)

(defcustom org-literature-pdf-directory (expand-file-name "pdf/" org-literature-directory)
  "PDFs, named after the citation key or linked by the entry's file field."
  :type 'directory)

(defcustom org-literature-notes-directory (expand-file-name "note/" org-literature-directory)
  "Paper notes named <key>.org, plus older org-noter notes named by title."
  :type 'directory)

(defun org-literature-format-note (_key entry)
  "Start a new paper note with the title of ENTRY."
  (insert "#+title: " (citar-get-display-value '("title") entry) "\n\n"))

(defun org-literature-key-for-document (document)
  "Return the citation key of DOCUMENT.
Use the base name when it is a key, else the entry whose files include DOCUMENT."
  (let ((base (file-name-base document))
        (target (file-truename document))
        found)
    (if (citar-get-entry base)
        base
      (maphash (lambda (key files)
                 (when (member target (mapcar #'file-truename files))
                   (setq found key)))
               (citar-get-files))
      found)))

(defun org-literature-note-for-document (document)
  "Return the note file for DOCUMENT, creating the note when missing.
Return nil when DOCUMENT belongs to no bibliography entry."
  (let ((key (org-literature-key-for-document document)))
    (when key
      (or (car (gethash key (citar-get-notes key)))
          (save-window-excursion
            (citar-create-note key)
            (save-buffer)
            buffer-file-name)))))

(defun org-literature--split-tags (keywords)
  "Split a KEYWORDS field value into a list of tags."
  (split-string (or keywords "") "," t "[ \t\n]+"))

(defun org-literature--all-tags ()
  "Return every tag used in the bibliography, sorted."
  (let (tags)
    (maphash (lambda (_key entry)
               (setq tags (append (org-literature--split-tags (citar-get-value "keywords" entry)) tags)))
             (citar-get-entries))
    (sort (delete-dups tags) #'string<)))

(defun org-literature--current-key ()
  "Return the citation key of the paper note or PDF in the current buffer."
  (when-let* ((file (buffer-file-name (or (buffer-base-buffer) (current-buffer)))))
    (if (file-in-directory-p file org-literature-notes-directory)
        (let ((base (file-name-base file)))
          (and (citar-get-entry base) base))
      (org-literature-key-for-document file))))

(defun org-literature--set-field (name value)
  "Set field NAME of the BibTeX entry at point to VALUE.  A nil VALUE removes it."
  (bibtex-beginning-of-entry)
  (let ((field (save-excursion (bibtex-search-forward-field name t))))
    (when field
      (delete-region (bibtex-start-of-field field) (bibtex-end-of-field field))))
  (when value
    (bibtex-end-of-entry)
    (re-search-backward "}")
    (skip-chars-backward " \t\n")
    (unless (eq (char-before) ?,) (insert ","))
    (insert "\n  " name " = {" value "}"))
  (bibtex-end-of-entry)
  (backward-char)
  (skip-chars-backward " \t")
  (unless (bolp) (insert "\n")))

(defun org-literature--write-tags (tags)
  "Replace the keywords field of the BibTeX entry at point with TAGS."
  (org-literature--set-field "keywords" (and tags (string-join tags ", "))))

(defun org-literature--with-entry (key fn)
  "Call FN with point on the entry KEY in the bibliography, then save it."
  (with-current-buffer (find-file-noselect org-literature-bibliography)
    (save-excursion
      (goto-char (point-min))
      (unless (bibtex-search-entry key)
        (user-error "No entry %s in %s" key org-literature-bibliography))
      (funcall fn))
    (save-buffer)))

(defun org-literature-open-entry (key)
  "Jump to the BibTeX entry of the paper KEY in the bibliography.
In a paper note or PDF, KEY is that paper.  Elsewhere, pick one."
  (interactive (list (or (org-literature--current-key) (citar-select-ref))))
  (citar-open-entry key))

(defun org-literature-edit-tags (key)
  "Edit the tags of the paper KEY, kept in its keywords field.
In a paper note or PDF, KEY is that paper.  Elsewhere, pick one."
  (interactive (list (or (org-literature--current-key) (citar-select-ref))))
  (let* ((current (org-literature--split-tags (citar-get-value "keywords" key)))
         (tags (delete-dups (completing-read-multiple
                             (format "Tags for %s: " key) (org-literature--all-tags)
                             nil nil (string-join current ",")))))
    (org-literature--with-entry key (lambda () (org-literature--write-tags tags)))
    (message "%s: %s" key (if tags (string-join tags ", ") "no tags"))))

(defun org-literature-rename-tag (old new)
  "Rename tag OLD to NEW in every paper.  An empty NEW removes OLD."
  (interactive
   (let ((old (completing-read "Rename tag: " (org-literature--all-tags) nil t)))
     (list old (read-string (format "Rename %s to (empty removes it): " old)))))
  (let ((changed 0))
    (maphash
     (lambda (key entry)
       (let ((tags (org-literature--split-tags (citar-get-value "keywords" entry))))
         (when (member old tags)
           (let ((renamed (delete-dups (delete "" (mapcar (lambda (tag) (if (equal tag old) new tag)) tags)))))
             (org-literature--with-entry key (lambda () (org-literature--write-tags renamed)))
             (setq changed (1+ changed))))))
     (citar-get-entries))
    (message "Renamed %s to %s in %d papers" old (if (string-empty-p new) "nothing" new) changed)))

(defconst org-literature--title-stop-words
  '("a" "an" "the" "of" "on" "in" "for" "and" "to" "with" "via" "by" "from"
    "is" "are" "at" "as" "toward" "towards" "into" "your" "our")
  "Title words that never go into a citation key.")

(defun org-literature--words (text)
  "Return the lowercase words of TEXT, without TeX markup."
  (split-string (downcase (replace-regexp-in-string "\\\\[a-zA-Z]+\\|[{}\\\\]" "" text))
                "[^[:alnum:]]+" t))

(defun org-literature--key-with-title (title)
  "Return the key of the paper whose title is TITLE, or nil."
  (let ((target (string-join (org-literature--words title)))
        found)
    (maphash (lambda (key entry)
               (when (equal target (string-join (org-literature--words (or (citar-get-value "title" entry) ""))))
                 (setq found key)))
             (citar-get-entries))
    found))

(defun org-literature--input-kind (input)
  "Classify INPUT as (bibtex TEXT), (arxiv ID), (doi DOI) or (title TEXT)."
  (cond
   ((string-match-p "\\`[ \t\n]*@[A-Za-z]+[ \t]*{" input) (list 'bibtex input))
   ((string-match "arxiv\\.org/\\(?:abs\\|pdf\\)/\\([0-9]\\{4\\}\\.[0-9]\\{4,5\\}\\)" input)
    (list 'arxiv (match-string 1 input)))
   ((string-match "\\`[ \t\n]*\\(?:arXiv:\\)?\\([0-9]\\{4\\}\\.[0-9]\\{4,5\\}\\)\\(?:v[0-9]+\\)?[ \t\n]*\\'" input)
    (list 'arxiv (match-string 1 input)))
   ((string-match "10\\.[0-9]\\{4,9\\}/[^ \t\n\"<>]+" input)
    (list 'doi (string-trim-right (match-string 0 input) "[.,;)]+")))
   (t (list 'title (string-trim input)))))

(defun org-literature--read-paper-input ()
  "Read what to add, from the region, the clipboard or the minibuffer."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (let* ((clip (ignore-errors (current-kill 0 t)))
           (kind (and clip (car (org-literature--input-kind clip)))))
      (if (and kind (not (eq kind 'title))
               (y-or-n-p (format "Add the %s in the clipboard? "
                                 (alist-get kind '((bibtex . "BibTeX") (doi . "DOI") (arxiv . "arXiv paper"))))))
          clip
        (read-string "Paper (title, DOI, arXiv ID or link): ")))))

(defconst org-literature--retry-delays '(2 4)
  "Seconds to wait before each retry when a server answers 429.")

(defun org-literature--fetch-once (url accept)
  "Return the HTTP status and body of URL, asking for the ACCEPT media type."
  (let* ((url-request-extra-headers `(("Accept" . ,accept)))
         (buffer (or (url-retrieve-synchronously url t t 30)
                     (user-error "No answer from %s" (url-host (url-generic-parse-url url))))))
    (with-current-buffer buffer
      (unwind-protect
          (progn
            (goto-char (point-min))
            (re-search-forward "\r?\n\r?\n" nil 'move)
            (cons (bound-and-true-p url-http-response-status)
                  (decode-coding-string (buffer-substring-no-properties (point) (point-max)) 'utf-8)))
        (kill-buffer)))))

(defun org-literature--fetch (url accept)
  "Return the body of URL, asking for the ACCEPT media type.
Retry after a pause when the server answers 429, too many requests."
  (let ((delays org-literature--retry-delays)
        (answer (org-literature--fetch-once url accept)))
    (while (and (eql (car answer) 429) delays)
      (message "%s is busy, retrying in %d seconds..." (url-host (url-generic-parse-url url)) (car delays))
      (sleep-for (pop delays))
      (setq answer (org-literature--fetch-once url accept)))
    (pcase (car answer)
      (200 (cdr answer))
      (429 (user-error "%s is busy (429, too many requests).  Try again in a minute"
                       (url-host (url-generic-parse-url url))))
      (status (user-error "%s answered %s" (url-host (url-generic-parse-url url)) status)))))

(defun org-literature--doi-bibtex (doi)
  "Return the BibTeX entry of DOI from Crossref."
  (org-literature--fetch (concat "https://api.crossref.org/works/" doi "/transform/application/x-bibtex")
                         "application/x-bibtex"))

(defun org-literature--close-title-p (title candidate)
  "Return non-nil when CANDIDATE holds most words of TITLE."
  (let ((want (org-literature--words title))
        (have (org-literature--words (or candidate ""))))
    (and want (>= (seq-count (lambda (word) (member word have)) want)
                  (* 0.75 (length want))))))

(defun org-literature--fetch-json (url)
  "Return the JSON at URL as nested alists and lists."
  (json-parse-string (org-literature--fetch url "application/json")
                     :object-type 'alist :array-type 'list :null-object nil))

(defun org-literature--crossref-choices (title)
  "Search Crossref for TITLE and return close matches as (LABEL doi DOI)."
  (let ((items (alist-get 'items (alist-get 'message
                                            (org-literature--fetch-json
                                             (concat "https://api.crossref.org/works?rows=20"
                                                     "&select=DOI,title,author,container-title,issued"
                                                     "&query.bibliographic=" (url-hexify-string title)))))))
    (delq nil
          (mapcar (lambda (item)
                    (let ((name (car (alist-get 'title item))))
                      (when (org-literature--close-title-p title name)
                        (list (format "%s | %s %s | %s" name
                                      (or (car (alist-get 'container-title item)) "")
                                      (or (caar (alist-get 'date-parts (alist-get 'issued item))) "")
                                      (or (alist-get 'family (car (alist-get 'author item))) ""))
                              'doi (alist-get 'DOI item)))))
                  items))))

(defun org-literature--semantic-scholar-bibtex (bibtex)
  "Fix the entry type of BIBTEX from Semantic Scholar, which can be a list."
  (if (string-match "\\`[ \t\n]*@\\(\\[[^]{]*\\]\\)[ \t]*{" bibtex)
      (replace-match (if (string-match-p "Conference" (match-string 1 bibtex)) "inproceedings" "article")
                     t t bibtex 1)
    bibtex))

(defun org-literature--semantic-scholar-choices (title)
  "Search Semantic Scholar for TITLE and return close matches.
Each match is (LABEL doi DOI), (LABEL arxiv ID) or (LABEL bibtex BIBTEX PDF)."
  (let ((papers (alist-get 'data
                           (org-literature--fetch-json
                            (concat "https://api.semanticscholar.org/graph/v1/paper/search?limit=10"
                                    "&fields=title,venue,year,authors,externalIds,citationStyles,openAccessPdf"
                                    "&query=" (url-hexify-string title))))))
    (delq nil
          (mapcar (lambda (paper)
                    (let ((name (alist-get 'title paper))
                          (ids (alist-get 'externalIds paper)))
                      (when (org-literature--close-title-p title name)
                        (cons (format "%s | %s %s | %s" name
                                      (or (alist-get 'venue paper) "")
                                      (or (alist-get 'year paper) "")
                                      (or (car (last (split-string (or (alist-get 'name (car (alist-get 'authors paper))) ""))))
                                          ""))
                              (cond ((alist-get 'DOI ids) (list 'doi (alist-get 'DOI ids)))
                                    ((alist-get 'ArXiv ids) (list 'arxiv (alist-get 'ArXiv ids)))
                                    (t (list 'bibtex
                                             (org-literature--semantic-scholar-bibtex
                                              (or (alist-get 'bibtex (alist-get 'citationStyles paper)) ""))
                                             (alist-get 'url (alist-get 'openAccessPdf paper)))))))))
                  papers))))

(defun org-literature--pick-by-title (title)
  "Find the paper titled TITLE and return (KIND VALUE PDF).
Search Crossref first, then Semantic Scholar, and offer only close matches."
  (let ((choices (or (org-literature--crossref-choices title)
                     (org-literature--semantic-scholar-choices title))))
    (unless choices
      (user-error "No close match on Crossref or Semantic Scholar for \"%s\".  Paste its BibTeX instead" title))
    (cdr (assoc (completing-read
                 "Paper: "
                 (lambda (string predicate action)
                   (if (eq action 'metadata)
                       '(metadata (display-sort-function . identity))
                     (complete-with-action action choices string predicate)))
                 nil t)
                choices))))

(defun org-literature--clean-url ()
  "Keep one web link in the url field of the entry at point.
Return the PDF link the field names, if any."
  (when-let* ((url (bibtex-text-in-field "url")))
    (let (pdf web other)
      (dolist (link (split-string url "[ \t\n]+" t))
        (when (string-match "\\`\\(?:\\([A-Za-z]+\\)=\\)?\\(https?://.+\\)" link)
          (let ((label (match-string 1 link))
                (address (match-string 2 link)))
            (cond ((or (equal label "Paper") (string-suffix-p ".pdf" address))
                   (setq pdf (or pdf address)))
                  ((equal label "Web") (setq web address))
                  (t (setq other (or other address)))))))
      (org-literature--set-field
       "url" (replace-regexp-in-string "\\`https?://\\(?:dx\\.\\)?doi\\.org/" "https://doi.org/"
                                       (or web other pdf)))
      pdf)))

(defun org-literature--suggest-key ()
  "Suggest a key for the entry at point.
It is the first author, the year, and the system name before a colon,
else the first two real title words."
  (let* ((title (or (bibtex-text-in-field "title") ""))
         (head (and (string-match "\\`\\([^:]+\\):" title) (match-string 1 title)))
         (system (and head (<= (length (split-string head)) 3) head))
         (words (seq-remove (lambda (word) (member word org-literature--title-stop-words))
                            (org-literature--words (or system title)))))
    (let ((bibtex-autokey-names 1)
          (bibtex-autokey-names-stretch 0)
          (bibtex-autokey-name-length 'infty)
          (bibtex-autokey-year-length 4))
      (replace-regexp-in-string
       "[^a-z0-9]" ""
       (ucs-normalize-NFD-string
        (downcase (concat (ignore-errors (bibtex-autokey-get-names))
                          (ignore-errors (bibtex-autokey-get-year))
                          (apply #'concat (seq-take words (if system 3 2))))))))))

(defun org-literature--read-key (suggestion)
  "Read a new citation key, starting from SUGGESTION."
  (let ((key (read-string "Key: " suggestion)))
    (while (or (string-empty-p key) (citar-get-entry key))
      (setq key (read-string (format "%s is taken.  Key: " key) key)))
    key))

(defun org-literature--pdf-links (doi)
  "Return links that may serve the PDF of DOI, from Crossref and the publisher."
  (append
   (ignore-errors
     (let ((work (alist-get 'message
                            (json-parse-string
                             (org-literature--fetch (concat "https://api.crossref.org/works/" doi) "application/json")
                             :object-type 'alist :array-type 'list :null-object nil))))
       (delq nil (mapcar (lambda (link)
                           (and (member (alist-get 'content-type link) '("application/pdf" "unspecified"))
                                (alist-get 'URL link)))
                         (alist-get 'link work)))))
   (and (string-prefix-p "10.1145/" doi)
        (list (concat "https://dl.acm.org/doi/pdf/" doi)))))

(defun org-literature--download-pdf (key links)
  "Save the first of LINKS that serves a real PDF as the PDF of KEY.
Return the saved file, or nil when no link gives a PDF."
  (let ((target (expand-file-name (concat key ".pdf") org-literature-pdf-directory))
        (temp (make-temp-file "paper" nil ".pdf")))
    (unwind-protect
        (seq-some (lambda (link)
                    (when (and (ignore-errors (url-copy-file link temp t) t)
                               (with-temp-buffer
                                 (set-buffer-multibyte nil)
                                 (insert-file-contents-literally temp nil 0 5)
                                 (equal (buffer-string) "%PDF-")))
                      (copy-file temp target t)
                      target))
                  links)
      (delete-file temp))))

(defun org-literature--newest-download ()
  "Return the newest PDF in ~/Downloads, or nil."
  (car (sort (directory-files "~/Downloads/" t "\\.pdf\\'") #'file-newer-than-file-p)))

(defun org-literature-attach-pdf (key)
  "Save a PDF for the paper KEY as pdf/KEY.pdf, then open it.
The PDF comes from a file, a link, or the publisher links of the DOI.
In a paper note, KEY is that paper.  Elsewhere, pick one."
  (interactive (list (or (org-literature--current-key) (citar-select-ref))))
  (let* ((doi (citar-get-value "doi" key))
         (target (expand-file-name (concat key ".pdf") org-literature-pdf-directory))
         (choice (car (read-multiple-choice
                       (format "Add the PDF of %s from" key)
                       `((?f "file" "A PDF on this computer, newest download first")
                         (?u "url" "A link to the PDF")
                         ,@(and doi '((?d "doi" "The publisher links of the DOI"))))))))
    (pcase choice
      (?f (let ((newest (org-literature--newest-download)))
            (copy-file (read-file-name "PDF: " "~/Downloads/" newest t
                                       (and newest (file-name-nondirectory newest)))
                       target)))
      (?u (unless (org-literature--download-pdf key (list (read-string "PDF link: ")))
            (user-error "That link gave no PDF")))
      (?d (unless (org-literature--download-pdf key (org-literature--pdf-links doi))
            (when (y-or-n-p "No PDF could be downloaded.  Open the paper page? ")
              (browse-url (concat "https://doi.org/" doi)))
            (user-error "No PDF for %s yet.  Save it, then pick Add PDF again" key))))
    (find-file target)))

(defun org-literature--offer-add-pdf (get-candidates citekeys &rest args)
  "Call GET-CANDIDATES with CITEKEYS and ARGS, adding an Add PDF choice.
The choice appears for each paper in CITEKEYS that has no library file."
  (let* ((result (apply get-candidates citekeys args))
         (files (and (plist-get args :files) (citar-get-files citekeys)))
         (adds (and files
                    (mapcar (lambda (key)
                              (propertize (citar--prepend-candidate-citekey key "Add a PDF")
                                          'citar--resource 'add-pdf))
                            (seq-remove (lambda (key) (gethash key files)) citekeys)))))
    (cond
     ((not adds) result)
     ((not result) (cons 'citar-candidate adds))
     ((eq (car result) 'multi-category) (cons 'multi-category (append adds (cdr result))))
     (t (cons 'multi-category
              (append adds (mapcar (lambda (cand) (propertize cand 'multi-category (cons (car result) cand)))
                                   (cdr result))))))))

(defun org-literature--group-add-pdf (group resource transform)
  "Show Add PDF choices from `citar-open' under their own heading.
Other RESOURCE strings go to GROUP with TRANSFORM."
  (if (eq (get-text-property 0 'citar--resource resource) 'add-pdf)
      (if transform resource "Add PDF")
    (funcall group resource transform)))

(defun org-literature--open-add-pdf (open resource &optional type)
  "Attach a PDF when RESOURCE is an Add PDF choice.
Otherwise call OPEN with RESOURCE and TYPE."
  (if (eq (or type (get-text-property 0 'citar--resource resource)) 'add-pdf)
      (org-literature-attach-pdf (citar--extract-candidate-citekey resource))
    (funcall open resource type)))

(defun org-literature--rewrite-entry (key)
  "Rewrite the entry at point under KEY, with one field per line."
  (bibtex-beginning-of-entry)
  (let ((start (point))
        (fields (bibtex-parse-entry)))
    (bibtex-end-of-entry)
    (delete-region start (point))
    (insert "@" (downcase (cdr (assoc "=type=" fields))) "{" key ",\n"
            (mapconcat (lambda (field)
                         (format "  %s = %s" (downcase (car field))
                                 (replace-regexp-in-string "[ \t\n]+" " " (cdr field))))
                       (seq-remove (lambda (field) (string-prefix-p "=" (car field))) fields)
                       ",\n")
            "\n}")
    (bibtex-beginning-of-entry)))

(defun org-literature--add-bibtex (bibtex pdf)
  "Clean BIBTEX, give it a key and tags, and add it to the bibliography.
Then offer to download PDF, or the PDF link found in the entry."
  (let (key entry doi page)
    (with-temp-buffer
      (delay-mode-hooks (bibtex-mode))
      (insert bibtex)
      (goto-char (point-min))
      (unless (re-search-forward "^[ \t]*@[A-Za-z]+[ \t]*{" nil t)
        (user-error "No BibTeX entry in: %s" (truncate-string-to-width (string-trim bibtex) 70 nil nil "...")))
      (bibtex-beginning-of-entry)
      (when-let* ((title (bibtex-text-in-field "title"))
                  (old (org-literature--key-with-title title)))
        (citar-open-entry old)
        (user-error "Already in the library as %s" old))
      (setq pdf (or (org-literature--clean-url) pdf))
      (setq doi (bibtex-text-in-field "doi"))
      (setq page (or (and doi (concat "https://doi.org/" doi)) (bibtex-text-in-field "url")))
      (dolist (field '("keywords" "abstract" "month" "issn" "file"))
        (org-literature--set-field field nil))
      (setq key (org-literature--read-key (org-literature--suggest-key)))
      (org-literature--rewrite-entry key)
      (org-literature--write-tags
       (delete-dups (completing-read-multiple (format "Tags for %s: " key) (org-literature--all-tags))))
      (bibtex-beginning-of-entry)
      (setq entry (buffer-substring-no-properties (point) (progn (bibtex-end-of-entry) (point)))))
    (with-current-buffer (find-file-noselect org-literature-bibliography)
      (goto-char (point-max))
      (skip-chars-backward " \t\n")
      (delete-region (point) (point-max))
      (insert "\n\n" entry "\n")
      (save-buffer))
    (let ((links (delete-dups (delq nil (cons pdf (and doi (org-literature--pdf-links doi)))))))
      (cond
       ((and links (not (y-or-n-p "Download the PDF? ")))
        (message "Added %s" key))
       ((and links (org-literature--download-pdf key links))
        (message "Added %s with its PDF" key))
       (t
        (when (and page (y-or-n-p "No PDF could be downloaded.  Open the paper page? "))
          (browse-url page))
        (message "Added %s.  Save the PDF, then pick Add PDF in citar-open" key))))))

(defun org-literature-add-paper (input)
  "Add a paper to the bibliography from INPUT.
INPUT is BibTeX, a DOI or doi.org link, an arXiv ID or link, or a title
searched on Crossref and Semantic Scholar.  With an active region, INPUT
is the region.  When the clipboard holds BibTeX, a DOI or an arXiv paper,
offer to use it."
  (interactive (list (org-literature--read-paper-input)))
  (pcase-let* ((`(,kind ,value) (org-literature--input-kind input))
               (`(,kind ,value ,pdf) (if (eq kind 'title)
                                          (org-literature--pick-by-title value)
                                        (list kind value nil))))
    (org-literature--add-bibtex
     (pcase kind
       ('bibtex value)
       ('arxiv (org-literature--fetch (concat "https://arxiv.org/bibtex/" value) "text/plain"))
       ('doi (org-literature--doi-bibtex value)))
     (or pdf (and (eq kind 'arxiv) (concat "https://arxiv.org/pdf/" value))))))

(defun org-literature-search-notes ()
  "Search the text of every paper note."
  (interactive)
  (consult-ripgrep org-literature-notes-directory))

(defun org-literature-search-pdfs ()
  "Search the text of every PDF in the library with ripgrep-all."
  (interactive)
  (let* ((consult-ripgrep-args "rga --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --with-filename --line-number")
         (consult-preview-key nil)
         (hit (consult-ripgrep org-literature-pdf-directory)))
    (when (and (derived-mode-p 'pdf-view-mode)
               (string-match "Page \\([0-9]+\\):" hit))
      (pdf-view-goto-page (string-to-number (match-string 1 hit))))))

;;;###autoload
(define-minor-mode org-literature-mode
  "Turn on the paper library.
Register `pdf:' links, jump from a PDF annotation to its org-noter
note, and offer an Add PDF choice in `citar-open' for papers without
a PDF."
  :global t
  :group 'org-literature
  (if org-literature-mode
      (progn
        (org-pdftools-setup-link)
        (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)
        (advice-add 'citar--get-resource-candidates :around #'org-literature--offer-add-pdf)
        (advice-add 'citar--select-group-related-resources :around #'org-literature--group-add-pdf)
        (advice-add 'citar--open-resource :around #'org-literature--open-add-pdf))
    (remove-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)
    (advice-remove 'citar--get-resource-candidates #'org-literature--offer-add-pdf)
    (advice-remove 'citar--select-group-related-resources #'org-literature--group-add-pdf)
    (advice-remove 'citar--open-resource #'org-literature--open-add-pdf)))

(provide 'org-literature)
;;; org-literature.el ends here
