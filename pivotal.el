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

(defun pivotal-fetch (request)
  (m/letm ((method path filter) request)
    (let ((url-request-method method)
	  (url-request-extra-headers `(("Content-Type" . "application/x-www-form-urlencoded")
				       ("X-TrackerToken" . ,(pivotal-api-key))))
	  (url-request-data (if filter (format "filter=%s" filter))))
      (with-current-buffer (url-retrieve-synchronously
			    (concat "https://www.pivotaltracker.com/services/v5/" path))
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
   :filter "state:started"))

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
  (magit-branch-and-checkout
   (read-string "Create branch: " (concat story-id "_"))
   (magit-read-branch "Parent" (magit-get-current-branch))))

(defun pivotal--magit-remote-update ()
  "Update all remotes synchronously."
  (or (run-hook-with-args-until-success 'magit-remote-update-hook)
      (magit-run-git "remote" "update")))

(defun pivotal--magit-find-local-branch (story-id)
  "Returns the first branch that matches the given story"
  (--first (string-match story-id it)
           (magit-list-local-branch-names)))

(defun pivotal--magit-find-branch (story-id)
  "Returns the first branch that matches the given story"
  (--first (string-match story-id it)
           (magit-list-refnames)))

(defun pivotal-checkout-branch (story-id)
  "Checkouts or create a branch for the given id"
  (interactive
   (list (pivotal-completing-read-story-id)))
  (let ((existing-local (pivotal--magit-find-local-branch story-id))
        (existing-remote (or (pivotal--magit-find-branch story-id)
                             (progn
                               (pivotal--magit-remote-update)
                               (pivotal--magit-find-branch story-id)))))
    (if existing-local
        (magit-checkout existing-local)
      (if existing-remote
          (magit-run-git "checkout" "-t" existing-remote)
        (pivotal-make-branch story-id)))))

(provide 'pivotal)
