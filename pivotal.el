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

(defun pivotal-make-ref (story-name)
  (interactive
   (list (completing-read "Story: " (pivotal-active-stories-names))))
  (insert
   (format "[%s]" (first (split-string story-name "|")))))

(provide 'pivotal)
