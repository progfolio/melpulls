;;; melpulls.el --- Elpaca Menu for outstanding MELPA pull requests  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Nicholas Vollmer

;; Author:  Nicholas Vollmer
;; URL: https://github.com/progfolio/melpulls
;; Created: March 10, 2022
;; Keywords: convenience
;; Package-Requires: ((emacs "27.1") (elpaca "0.0.0"))
;; Version: 0.0.0

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

;; This package implements a menu which provides recipes for outstanding MELPA pull requests.
;; Useful for those who whish to quickly browse/install/review/contribute to such packages.

;;; Code:
(require 'url)
(require 'button)
(require 'cl-lib)
(require 'elpaca)

(defvar url-http-end-of-headers)

(defgroup melpulls nil
  "Elpaca menu for outstanding MELPA pull requests."
  :group 'elpaca
  :prefix "melpulls")

(defcustom melpulls-cache-file (expand-file-name "melpulls.eld" elpaca-cache-directory)
  "Name of the melpulls cache file."
  :type 'path)

(defcustom melpulls-poll-timeout 60 "Number of seconds to poll for recipes before timing out."
  :group 'melpulls
  :type  'number)

(defvar melpulls-url "https://api.github.com/repos/melpa/melpa/pulls?per_page=100"
  "Pull request end point for github's API.")
(defvar melpulls-accept-string "application/vnd.github.v3+json")
(defvar melpulls--cache (elpaca--read-file melpulls-cache-file) "Cache for items.")

(defalias 'melpulls--buttonize
  (with-no-warnings
    (if (version< emacs-version "29.1") #'button-buttonize #'buttonize)))

(defun melpulls--json ()
  "Return list of pulls from github's API."
  (let ((url-mime-accept-string melpulls-accept-string))
    (with-current-buffer (url-retrieve-synchronously melpulls-url 'silent)
      (json-parse-string
       (decode-coding-region url-http-end-of-headers (point-max) 'utf-8 t)
       :array-type 'list :object-type 'alist))))

(defun melpulls--diff-url (url)
  "Convert pull URL to diff URL."
  (replace-regexp-in-string "github\\.com" "patch-diff.githubusercontent.com/raw" url))

(defun melpulls--recipe ()
  "Return diff's recipe or nil if unparsable."
  (narrow-to-region url-http-end-of-headers (point-max))
  (goto-char (point-max))
  (re-search-backward "@@" nil 'noerror)
  (forward-line)
  (narrow-to-region (point) (point-max))
  ;; lines removed in diff and/or comments e.g. "\\ No newline at end of file"
  (flush-lines "^\\(?:-\\|\\\\\\)")
  (goto-char (point-min))
  (while (re-search-forward "^\\+" nil 'noerror)
    (replace-match ""))
  (when-let ((recipe (ignore-errors (read (buffer-string))))
             ((listp recipe))
             (package (pop recipe))
             ((member (plist-get recipe :fetcher) '(git github gitlab codeberg sourcehut))))
    (setq recipe (append (list :package (symbol-name package)) recipe))
    (unless (plist-member recipe :files)
      (setq recipe (plist-put recipe :files '(:defaults))))
    recipe))

(defvar melpulls--summary-regexp
  "\\(?:[^z-a]*?### Brief summary[^z-a]*?[\n]+?\\([^z-a]*?\\)###[^z-a]*\\)"
  "Regexp for pull request summary heading.")
(defvar melpulls--markdown-link-regexp "\\(?:\\[\\([^z-a]*?\\)](\\([^z-a]*?\\))\\)")

(defun melpulls--md-link-to-button (match)
  "Return button from MATCH."
  (let* ((data (match-data t))
         (description (substring match (nth 2 data) (nth 3 data)))
         (target (substring match (nth 4 data) (nth 5 data))))
    (melpulls--buttonize description #'browse-url target target)))

(defun melpulls--md-links-to-buttons (string)
  "Convert STRING's markdown links to buttons."
  (replace-regexp-in-string melpulls--markdown-link-regexp #'melpulls--md-link-to-button string))

(defun melpulls--item-description (pull)
  "Return first sentence of PULL's description or nil if unparsable."
  (when-let ((body (alist-get 'body pull))
             ((setq body (replace-regexp-in-string
                          "\\[Please write a quick summary of the package\\.\\]"
                          "" body nil 'literal)))
             (summary (replace-regexp-in-string melpulls--summary-regexp "\\1" body)))
    (cl-some (lambda (line)
               (when-let ((trimmed (string-trim line))
                          ((> (length trimmed) 0)))
                 trimmed))
             (split-string summary "\r\n" 'omit-nulls "[[:space:]]"))))

(defun melpulls--item-url (recipe pull)
  "Return URL for RECIPE or PULL URL if none found."
  (if-let ((url (plist-get recipe :url)))
      url
    (if-let ((fetcher (plist-get recipe :fetcher))
             ((memq fetcher '(github gitlab))))
        (format "https://www.%s.com/%s"
                fetcher (plist-get recipe :repo))
      (alist-get 'html_url pull))))

(defun melpulls--item (pull)
  "Return a menu item from PULL."
  (when-let ((recipe (melpulls--recipe)))
    (list (intern (plist-get recipe :package))
          :source "MELPA Pulls"
          :date (ignore-errors (date-to-time (alist-get 'created_at pull)))
          :description
          (let ((description (or (melpulls--item-description pull) "n/a"))
                (issue (alist-get 'issue_url pull)))
            (concat (when issue
                      (setq issue (replace-regexp-in-string "api" "www" issue))
                      (setq issue (replace-regexp-in-string "repos/" "" issue))
                      (melpulls--buttonize (file-name-base issue) #'browse-url issue issue))
                    (when issue " ")
                    (melpulls--md-links-to-buttons description)))
          :url (melpulls--item-url recipe pull)
          :recipe recipe)))

(defun melpulls--recipes (pulls)
  "Asynchronously parse recipes from PULLS JSON."
  (cl-loop
   with completed
   with requests = 0
   for pull in pulls
   do (when-let ((url (alist-get 'diff_url pull))
                 (diff (melpulls--diff-url url)))
        (cl-incf requests)
        (url-retrieve diff (lambda (_status pull) (push (melpulls--item pull) completed))
                      (list pull) 'silent))
   finally return (progn
                    (with-timeout (melpulls-poll-timeout
                                   (message "Melpulls recipe fetching timed out"))
                      (while (not (eq (length completed) requests))
                        (sleep-for 0.001)))
                    (nreverse (delq nil completed)))))

;;;###autoload
(defun melpulls (request &optional item)
  "Menu function which provides MELPA pull request recipes.
Supports `index` and `update` ITEM REQUESTs."
  (let ((cache (if (or (eq request 'update) (null melpulls--cache))
                   (prog2
                       (message "Updating Melpulls menu...")
                       (setq melpulls--cache (melpulls--recipes (melpulls--json)))
                     (elpaca--write-file melpulls-cache-file (prin1 melpulls--cache))
                     (message "Updating Melpulls menu...100%%")))))
    (if item (elpaca-alist-get item cache) cache)))

(provide 'melpulls)
;;; melpulls.el ends here
