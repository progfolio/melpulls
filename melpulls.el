;;; melpulls.el --- Elpaca Menu for outstanding MELPA pull requests  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Nicholas Vollmer

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

(defcustom melpulls-cache-file (expand-file-name "cache/melpulls.eld" elpaca-directory)
  "Name of the melpulls cache file."
  :type 'path)

(defvar melpulls-url "https://api.github.com/repos/melpa/melpa/pulls"
  "Pull request end point for github's API.")
(defvar melpulls-accept-string "application/vnd.github.v3+json")
(defvar melpulls--cache (elpaca--read-file melpulls-cache-file) "Cache for items.")

(defun melpulls--json ()
  "Return list of pulls from github's API."
  (let ((url-mime-accept-string melpulls-accept-string))
    (with-current-buffer (url-retrieve-synchronously melpulls-url)
      (json-parse-string
       (decode-coding-region url-http-end-of-headers (point-max) 'utf-8 t)
       :array-type 'list :object-type 'alist))))

(defun melpulls--diff-url (url)
  "Convert pull URL to diff URL."
  (replace-regexp-in-string "github\\.com" "patch-diff.githubusercontent.com/raw" url))

(defun melpulls--recipe (diff)
  "Return DIFF url's recipe or nil if unparsable."
  (with-current-buffer (url-retrieve-synchronously diff)
    (re-search-backward "@@" nil 'noerror)
    (forward-line)
    (narrow-to-region (point) (point-max))
    (flush-lines "^-")
    (goto-char (point-min))
    (while (re-search-forward "^\\+" nil 'noerror)
      (replace-match ""))
    (when-let ((recipe (ignore-errors (read (buffer-string))))
               ((listp recipe))
               (package (pop recipe))
               ((member (plist-get recipe :fetcher) '(git github gitlab))))
      (setq recipe (append (list :package (symbol-name package)) recipe))
      (unless (plist-member recipe :files)
        (setq recipe (plist-put recipe :files '(:defaults))))
      recipe)))

(defvar melpulls--summary-regexp
  "\\(?:[^z-a]*?### Brief summary[^z-a]*?[\n]+?\\([^z-a]*?\\)###[^z-a]*\\)"
  "Regexp for pull request summary heading.")
(defvar melpulls--markdown-link-regexp "\\(?:\\[\\([^z-a]*?\\)](\\([^z-a]*?\\))\\)")

(defun melpulls--md-links-to-buttons (string)
  "Convert STRING's markdown links to buttons."
  (replace-regexp-in-string
   melpulls--markdown-link-regexp
   (lambda (match)
     (let* ((data (match-data t))
            (description (substring match (nth 2 data) (nth 3 data)))
            (target (substring match (nth 4 data) (nth 5 data))))
       (buttonize description (lambda (_) (browse-url target)))))
   string))

(defun melpulls--item-description (pull)
  "Return first sentence of PULL's description or nil if unparsable."
  (when-let ((body (alist-get 'body pull))
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

(defun melpulls--items (&optional refresh)
  "Return list of menu items.
If REFRESH is non-nil, recompute the cache."
  (or (and (not refresh) melpulls--cache)
      (prog2
          (message "Updating Melpulls menu.")
          (setq melpulls--cache
                (cl-loop for pull in (melpulls--json)
                         for recipe = (when-let ((url (alist-get 'diff_url pull))
                                                 (diff (melpulls--diff-url url)))
                                        (melpulls--recipe diff))
                         when recipe collect
                         (list (intern (plist-get recipe :package))
                               :source      (buttonize
                                             "MELPA Pulls"
                                             (lambda (_) (browse-url
                                                          "https://www.github.com/melpa/melpa/pulls")))
                               :date        (ignore-errors (date-to-time (alist-get 'created_at pull)))
                               :description (melpulls--md-links-to-buttons
                                             (melpulls--item-description pull))
                               :url         (melpulls--item-url recipe pull)
                               :recipe      recipe)))
        (elpaca--write-file melpulls-cache-file (prin1 melpulls--cache))
        (message "Melpulls menu updated."))))

;;;###autoload
(defun melpulls (request)
  "Menu function which provides MELPA pull request recipes.
Supports `index` and `update` REQUEST."
  (melpulls--items (eq request 'update)))

(provide 'melpulls)
;;; melpulls.el ends here
