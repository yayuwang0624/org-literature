;;; org-literature-test.el --- Tests for org-literature  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-literature)

(defconst org-literature-test--bibtex
  "@inproceedings{kocher2019spectre,
  title = {Spectre Attacks: Exploiting Speculative Execution},
  author = {Kocher, Paul and Horn, Jann},
  year = {2019},
  keywords = {spectre, side-channel}
}

@article{yarom2016cachebleed,
  title = {CacheBleed: A Timing Attack on OpenSSL Constant Time RSA},
  author = {Yarom, Yuval},
  year = {2016},
  keywords = {side-channel, crypto}
}
")

(defmacro org-literature-test--with-library (&rest body)
  "Run BODY with a fresh library in a temporary directory."
  (declare (indent 0))
  `(let* ((root (file-name-as-directory (make-temp-file "org-literature" t)))
          (org-literature-directory root)
          (org-literature-bibliography (expand-file-name "refs.bib" root))
          (org-literature-pdf-directory (file-name-as-directory (expand-file-name "pdf" root)))
          (org-literature-notes-directory (file-name-as-directory (expand-file-name "note" root)))
          (citar-bibliography (list org-literature-bibliography))
          (citar-library-paths (list org-literature-pdf-directory))
          (citar-notes-paths (list org-literature-notes-directory))
          (citar-note-format-function #'org-literature-format-note))
     (make-directory org-literature-pdf-directory)
     (make-directory org-literature-notes-directory)
     (with-temp-file org-literature-bibliography (insert org-literature-test--bibtex))
     (unwind-protect
         (progn ,@body)
       (when-let* ((buffer (get-file-buffer org-literature-bibliography)))
         (with-current-buffer buffer (set-buffer-modified-p nil))
         (kill-buffer buffer))
       (delete-directory root t))))

(defun org-literature-test--refresh ()
  "Make citar read the bibliography again."
  (citar-cache--get-bibliography (car (citar--bibliography-files)) t))

(defun org-literature-test--write (file text)
  "Write TEXT to FILE and return FILE."
  (with-temp-file file (insert text))
  file)

(defmacro org-literature-test--with-entry (bibtex &rest body)
  "Run BODY in a BibTeX buffer holding BIBTEX, with point on the entry."
  (declare (indent 1))
  `(with-temp-buffer
     (bibtex-mode)
     (insert ,bibtex)
     (goto-char (point-min))
     (re-search-forward "@")
     (bibtex-beginning-of-entry)
     ,@body))

(ert-deftest org-literature-test-input-kind ()
  (should (equal (org-literature--input-kind "10.1145/3434327") '(doi "10.1145/3434327")))
  (should (equal (org-literature--input-kind "https://dl.acm.org/doi/epdf/10.1145/3434327")
                 '(doi "10.1145/3434327")))
  (should (equal (org-literature--input-kind "https://doi.org/10.1109/SP.2019.00002.")
                 '(doi "10.1109/SP.2019.00002")))
  (should (equal (org-literature--input-kind "arXiv:2401.01234v2") '(arxiv "2401.01234")))
  (should (equal (org-literature--input-kind "https://arxiv.org/abs/2401.01234") '(arxiv "2401.01234")))
  (should (eq (car (org-literature--input-kind "@article{x, title={a}}")) 'bibtex))
  (should (equal (org-literature--input-kind "  Spectre Attacks ") '(title "Spectre Attacks"))))

(ert-deftest org-literature-test-suggest-key ()
  (org-literature-test--with-entry
      "@inproceedings{x, title = {Branch {Target} {Reuse}: {Practical} Attacks}, author = {Wiebing, Sander}, year = {2026}}"
    (should (equal (org-literature--suggest-key) "wiebing2026branchtargetreuse")))
  (org-literature-test--with-entry
      "@article{x, title = {Remote Timing Attacks Are Practical}, author = {Brumley, David}, year = {2003}}"
    (should (equal (org-literature--suggest-key) "brumley2003remotetiming")))
  (org-literature-test--with-entry
      "@article{x, title = {Formally verified speculation in a JIT compiler}, author = {Barrière, Aurèle}, year = {2021}}"
    (should (equal (org-literature--suggest-key) "barriere2021formallyverified"))))

(ert-deftest org-literature-test-clean-url ()
  (org-literature-test--with-entry
      "@inproceedings{x, title = {T}, url = {Paper=https://a.org/p.pdf Web=https://a.org/btr Code=https://github.com/a/b}}"
    (should (equal (org-literature--clean-url) "https://a.org/p.pdf"))
    (should (equal (bibtex-text-in-field "url") "https://a.org/btr")))
  (org-literature-test--with-entry
      "@article{x, title = {T}, url = {http://dx.doi.org/10.1145/3434327}}"
    (should-not (org-literature--clean-url))
    (should (equal (bibtex-text-in-field "url") "https://doi.org/10.1145/3434327"))))

(ert-deftest org-literature-test-rewrite-entry ()
  (org-literature-test--with-entry
      " @inproceedings{Kocher_2019, title={Spectre Attacks}, DOI={10.1109/sp.2019.00002}, year={2019}, month=may }"
    (org-literature--set-field "month" nil)
    (org-literature--rewrite-entry "kocher2019spectre")
    (should (equal (string-trim (buffer-string))
                   "@inproceedings{kocher2019spectre,
  title = {Spectre Attacks},
  doi = {10.1109/sp.2019.00002},
  year = {2019}
}"))))

(ert-deftest org-literature-test-write-tags ()
  (org-literature-test--with-entry
      "@article{x,\n  title = {T},\n  keywords = {old}}"
    (org-literature--write-tags '("spectre" "jit"))
    (should (equal (bibtex-text-in-field "keywords") "spectre, jit"))
    (org-literature--write-tags nil)
    (should-not (bibtex-text-in-field "keywords"))))

(ert-deftest org-literature-test-rename-tag ()
  (org-literature-test--with-library
    (org-literature-test--refresh)
    (org-literature-rename-tag "side-channel" "")
    (org-literature-test--refresh)
    (should (equal (org-literature--all-tags) '("crypto" "spectre")))))

(ert-deftest org-literature-test-add-paper-from-bibtex ()
  (org-literature-test--with-library
    (org-literature-test--refresh)
    (let ((bibtex "@inproceedings{wiebingBranchTargetReuse2026,
    title = {Branch {Target} {Reuse}: {Practical} Attacks},
    url = {Paper=https://a.org/btr.pdf Web=https://a.org/btr},
    author = {Wiebing, Sander},
    month = nov,
    year = {2026},
    keywords = {class\\_sidechannels, type\\_paper},
}")
          downloads)
      (cl-letf (((symbol-function 'read-string) (lambda (_prompt &optional _initial &rest _) "wiebing2026btr"))
                ((symbol-function 'completing-read-multiple) (lambda (&rest _) '("spectre" "jit")))
                ((symbol-function 'y-or-n-p) (lambda (_) t))
                ((symbol-function 'org-literature--download-pdf)
                 (lambda (key links) (push (cons key links) downloads) nil))
                ((symbol-function 'browse-url) #'ignore))
        (org-literature-add-paper bibtex)
        (org-literature-test--refresh)
        (should (equal (citar-get-value "keywords" "wiebing2026btr") "spectre, jit"))
        (should (equal (citar-get-value "url" "wiebing2026btr") "https://a.org/btr"))
        (should-not (citar-get-value "month" "wiebing2026btr"))
        (should (equal downloads '(("wiebing2026btr" "https://a.org/btr.pdf"))))
        (should (equal (cadr (should-error (org-literature-add-paper bibtex) :type 'user-error))
                       "Already in the library as wiebing2026btr"))))))

(ert-deftest org-literature-test-download-pdf ()
  (org-literature-test--with-library
    (let* ((page (org-literature-test--write (expand-file-name "page.pdf" org-literature-directory) "<html>"))
           (paper (org-literature-test--write (expand-file-name "paper.pdf" org-literature-directory) "%PDF-1.4\n")))
      (should-not (org-literature--download-pdf "k" (list (concat "file://" page))))
      (should (equal (org-literature--download-pdf "k" (list (concat "file://" page) (concat "file://" paper)))
                     (expand-file-name "k.pdf" org-literature-pdf-directory))))))

(ert-deftest org-literature-test-add-pdf-choice ()
  (org-literature-test--with-library
    (org-literature-test--refresh)
    (org-literature-test--write (expand-file-name "yarom2016cachebleed.pdf" org-literature-pdf-directory) "%PDF-1.4\n")
    (org-literature-mode 1)
    (unwind-protect
        (cl-flet ((types (key)
                    (mapcar (lambda (cand) (get-text-property 0 'citar--resource cand))
                            (cdr (citar--get-resource-candidates (list key) :files t :links t :notes t :create-notes t)))))
          (should (equal (car (types "kocher2019spectre")) 'add-pdf))
          (should-not (memq 'add-pdf (types "yarom2016cachebleed")))
          (let (attached)
            (cl-letf (((symbol-function 'org-literature-attach-pdf) (lambda (key) (setq attached key))))
              (citar--open-resource (citar--prepend-candidate-citekey "kocher2019spectre" "Add a PDF") 'add-pdf))
            (should (equal attached "kocher2019spectre"))))
      (org-literature-mode -1))
    (should-not (advice-member-p #'org-literature--offer-add-pdf 'citar--get-resource-candidates))))

(defun org-literature-test--fake-json (url)
  "Answer URL like Crossref with no match and Semantic Scholar with three papers."
  (if (string-match-p "crossref" url)
      '((message (items ((title "Something Else Entirely") (DOI . "10.1/x")))))
    `((data ((title . "Mohabi: Disaggregating and Sandboxing the Firefox JavaScript Engine")
             (venue . "USENIX Security") (year . 2026)
             (authors ((name . "Shravan Narayan")))
             (externalIds (CorpusId . 1))
             (citationStyles (bibtex . "@['Conference']{Narayan2026Mohabi,\n title = {Mohabi: Disaggregating and Sandboxing the Firefox JavaScript Engine},\n author = {Shravan Narayan},\n year = {2026}\n}"))
             (openAccessPdf (url . "https://example.org/mohabi.pdf")))
            ((title . "Mohabi: Disaggregating and Sandboxing the Firefox JavaScript Engine (extended)")
             (externalIds (ArXiv . "2601.01234")))
            ((title . "Mohabi: Disaggregating and Sandboxing the Firefox JavaScript Engine, journal")
             (externalIds (DOI . "10.1145/9999")))
            ((title . "Unrelated Paper") (externalIds (DOI . "10.1/y")))))))

(ert-deftest org-literature-test-title-search-fallback ()
  (cl-letf (((symbol-function 'org-literature--fetch-json) #'org-literature-test--fake-json))
    (let ((choices (org-literature--semantic-scholar-choices
                    "Mohabi: Disaggregating and Sandboxing the Firefox JavaScript Engine")))
      (should (= (length choices) 3))
      (should (equal (cdr (nth 1 choices)) '(arxiv "2601.01234")))
      (should (equal (cdr (nth 2 choices)) '(doi "10.1145/9999")))
      (should (string-prefix-p "@inproceedings{Narayan2026Mohabi," (nth 2 (nth 0 choices))))
      (should (equal (nth 3 (nth 0 choices)) "https://example.org/mohabi.pdf")))
    (cl-letf (((symbol-function 'completing-read) (lambda (_prompt table &rest _) (car (all-completions "" table)))))
      (should (equal (car (org-literature--pick-by-title
                           "Mohabi: Disaggregating and Sandboxing the Firefox JavaScript Engine"))
                     'bibtex)))
    (should (equal (cadr (should-error (org-literature--pick-by-title "No Such Paper Title At All")
                                       :type 'user-error))
                   "No close match on Crossref or Semantic Scholar for \"No Such Paper Title At All\".  Paste its BibTeX instead"))))

(ert-deftest org-literature-test-add-paper-by-title ()
  (org-literature-test--with-library
    (org-literature-test--refresh)
    (let (downloads)
      (cl-letf (((symbol-function 'org-literature--fetch-json) #'org-literature-test--fake-json)
                ((symbol-function 'completing-read) (lambda (_prompt table &rest _) (car (all-completions "" table))))
                ((symbol-function 'read-string) (lambda (_prompt &optional initial &rest _) initial))
                ((symbol-function 'completing-read-multiple) (lambda (&rest _) '("isolation")))
                ((symbol-function 'y-or-n-p) (lambda (_) t))
                ((symbol-function 'org-literature--download-pdf)
                 (lambda (key links) (push (cons key links) downloads) t)))
        (org-literature-add-paper "Mohabi: Disaggregating and Sandboxing the Firefox JavaScript Engine")
        (org-literature-test--refresh)
        (should (citar-get-entry "narayan2026mohabi"))
        (should (equal (citar-get-value "keywords" "narayan2026mohabi") "isolation"))
        (should (equal downloads '(("narayan2026mohabi" "https://example.org/mohabi.pdf"))))))))

(ert-deftest org-literature-test-mode-setup ()
  (should (featurep 'org-pdftools))
  (should (featurep 'org-noter-pdftools))
  (org-literature-mode 1)
  (unwind-protect
      (progn
        (should (org-link-get-parameter "pdf" :follow))
        (should (memq #'org-noter-pdftools-jump-to-note pdf-annot-activate-handler-functions)))
    (org-literature-mode -1))
  (should-not (memq #'org-noter-pdftools-jump-to-note pdf-annot-activate-handler-functions)))

(ert-deftest org-literature-test-note-for-document ()
  (org-literature-test--with-library
    (org-literature-test--refresh)
    (let ((note (org-literature-note-for-document
                 (expand-file-name "kocher2019spectre.pdf" org-literature-pdf-directory))))
      (should (equal (file-name-nondirectory note) "kocher2019spectre.org"))
      (should (string-prefix-p "#+title: Spectre Attacks"
                               (with-temp-buffer (insert-file-contents note) (buffer-string))))
      (kill-buffer (get-file-buffer note)))
    (should-not (org-literature-note-for-document "/tmp/unknown-paper.pdf"))))

(provide 'org-literature-test)
;;; org-literature-test.el ends here
