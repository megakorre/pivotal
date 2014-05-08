;;; pivotal.el --- Utils for pivital tracker

;; Copyright (C) 2013 Patrik Kårlin

;; Author: Patrik Kårlin <patrik.karlin@gmail.com>
;; Version: 1.0.0
;; Keywords: pivotal

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'json)
(require 'dash)
(require 'maps)

(defun pivotal-read-file (path)
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun pivotal-project ()
  (string-to-int (pivotal-read-file "~/.pivotal-project")))

(defun pivotal-api-key ()
  (pivotal-read-file "~/.pivotal-api-key"))

(defun pivotal-build-url (path arguments)
  (concat
   "https://www.pivotaltracker.com/services/v5/"
   path "?" (s-join "&" arguments)))

(defun pivotal-fetch (request)
  (m/letm ((method path filter) request)
    (let* ((url-request-method method)
           (url-request-extra-headers `(("X-TrackerToken" . ,(pivotal-api-key))))
           (arguments (if filter (list (format "filter=%s" filter))))
           (buffer (url-retrieve-synchronously (pivotal-build-url path arguments))))
      (with-current-buffer buffer
        (buffer-string)))))

(defun pivotal-parse-r (content)
  (-map 'm/alist->plist
	(json-read-from-string
	 (s-join "\n"
		 (-> content
		   (split-string "\n\n")
		   (rest))))))

(defun pivotal-stories-r (&rest args)
  (list
   :method "GET"
   :path (format "projects/%i/stories" (pivotal-project))
   :filter "state:started,finished,delivered,rejected"))

(defun pivotal-active-stories ()
  (-> (pivotal-stories-r) (pivotal-fetch) (pivotal-parse-r)))

(defun pivotal-active-stories-names ()
  (--map
   (m/letm ((id name) it)
     (format "%i|%s" id (decode-coding-string name 'utf-8)))
   (pivotal-active-stories)))

(defun pivotal-story-name->story-id (story-name)
  (first (split-string story-name "|")))

(defun pivotal-completing-read-story-id ()
  "Prompts for a pivotal story"
  (let ((stories (pivotal-active-stories-names)))
    (pivotal-story-name->story-id
     ;; Choose among available stories, defaults to first one
     (completing-read "Story: " stories nil nil nil nil (first stories)))))

(defun pivotal-make-ref (story-id)
  "Inserts pivotal ref for given story"
  (interactive
   (list (pivotal-completing-read-story-id)))
  (insert (format "[%s]" story-id)))

(defun pivotal-make-branch (story-id)
  "Creates a new branch with proper pivotal prefix"
  (interactive
   (list (pivotal-completing-read-story-id)))
  (magit-create-branch
   (read-string "Create branch: " (concat story-id "_"))
   (magit-read-rev "Parent" (or (magit-guess-branch)
                                (magit-get-current-branch)))))
(defun pivotal--magit-remote-update ()
  "Update all remotes synchronously."
  (or (run-hook-with-args-until-success 'magit-remote-update-hook)
      (magit-run-git "remote" "update" magit-custom-options)))

(defun pivotal--magit-find-branch (story-id)
  "Returns the first branch that matches the given story"
  (--first (string-match story-id (car it))
           (magit-list-interesting-refs)))

(defun pivotal-checkout-branch (story-id)
  "Checkouts or create a branch for the given id"
  (interactive
   (list (pivotal-completing-read-story-id)))
  (let ((existing (or (pivotal--magit-find-branch story-id)
                      (progn
                        (pivotal--magit-remote-update)
                        (pivotal--magit-find-branch story-id)))))
    (if existing
        (or
         (magit-maybe-create-local-tracking-branch (cdr existing))
         (magit-checkout (car existing)))
      (pivotal-make-branch story-id))))

(provide 'pivotal)
