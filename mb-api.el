;;; mb-emacs-search.el --- Emacs MusicBrainz searching       -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author:  Oliwier Czerwiński <oliwier.czerwi@proton.me>
;; Keywords: convenience

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

;; So far, artist, release group, and work search is implemented, and it's limited.
;; NOTE non latin characters are not diplayed correctly

;;; Code:

(require 'url)
(require 'json)

(defun mb-api-search (type query)
  "Searches for QUERY of TYPE, and returns raw lisp data."
  (with-current-buffer
      (url-retrieve-synchronously (format "https://musicbrainz.org/ws/2/%s?query=%s&fmt=json" type query))
    (goto-char url-http-end-of-headers)
    (json-read))
  )

(defun mb-api-open (mbid)
  "Opens MBID in a MusicBrainz webpage."
  (browse-url (concat "https://musicbrainz.org/mbid/" mbid)))

(defun mb-api-search-artist (artist)
  "Searches for ARTIST, and returns raw lisp data."
  (mb-api-search "artist" artist)
  )

(defun mb-api-search-artist-tidy (artist)
  "Searches for ARTIST and returns a vector."
  (cdr (assoc 'artists (cdddr (mb-api-search-artist artist))))
  )

(defun mb-api-search-artist-exact (artist)
  "Searches for ARTIST, and returns an alist of names, disambiguations and IDs.
If there is no disambiguation, it puts (disambiguation . \"\")."
  (mapcar (lambda (x)
            (append (list
                     (assoc 'name x)
                     (assoc 'sort-name x)
                     (if (assoc 'disambiguation x)
                         (assoc 'disambiguation x)
                       '(disambiguation . ""))
                     ;; `car' is id, and it's faster than `assoc'
                     (car x))))
          (append (mb-api-search-artist-tidy artist) nil))
  )

(defun mb-api-artist-format (item)
  "Formats item into a string."
  (concat
   (propertize (cdr (assoc 'name item)) 'face 'underline)
   " (" (propertize (cdr (assoc 'sort-name item)) 'face 'italic)
   ;; if there is disambiguation, add it
   (unless (string= (cdr (assoc 'disambiguation item)) "")
     (format ", %s" (cdr (assoc 'disambiguation item))))
   ")"
   ))

(defun mb-api-artist-select (data)
  "Prompt the user to select a name from the list DATA and return the corresponding ID.
The DATA is meant to be the output of `mb-api-search-artist-exact'."
  (let* ((name-list (mapcar #'mb-api-artist-format data))
         (selected-name (completing-read "Artist: " name-list)))
    (cdr (assoc 'id (cl-find-if
                     (lambda (item)
                       (string= (mb-api-artist-format item) selected-name))
                     data)))))

(defun mb-api-artist (artist)
  (interactive "sArtist: ")
  (mb-api-open (mb-api-artist-select (mb-api-search-artist-exact artist)))
  )

(defun mb-api-search-release-group (release-group)
  "Searches for a RELEASE-GROUP, and returns raw lisp data."
  (mb-api-search "release-group" release-group))

(defun mb-api-search-release-group-tidy (release-group)
  "Searches for RELEASE-GROUP and returns a vector."
  (cdr (assoc 'release-groups (cdddr (mb-api-search-release-group release-group))))
  )

(defun mb-api-search-release-group-exact (release-group)
  "Searches for RELEASE-GROUP, and returns an alist of titles, first
release dates, and IDs. If there is no disambiguation, it
puts (disambiguation . \"\"). NOTE that non latin characters will
not be displayed correctly."
  (mapcar (lambda (x)
            (append (list
                     (assoc 'title x)
                     (assoc 'primary-type x)
                     (assoc 'first-release-date x)
                     (car x)
                     (cons 'artist-name (cdaar (append (cdr (assoc 'artist-credit x)) nil)))
                     )))
          (append (mb-api-search-release-group-tidy release-group) nil))
  )

(defun mb-api-release-group-format (x)
  (concat
   (cdr (assoc 'first-release-date x)) " - "
   (cdr (assoc 'primary-type x)) " - "
   (propertize (cdr (assoc 'title x)) 'face 'underline) " - "
   (cdr (assoc 'artist-name x))
   ))

(defun mb-api-release-group-select (data)
  "Prompt for a name from the list DATA and return the corresponding ID.
The DATA is meant to be the output of `mb-api-search-release-group-exact'."
  (let* ((name-list (mapcar #'mb-api-release-group-format data))
         (selected-name (completing-read "Release group: " name-list)))
    (cdr (assoc 'id (cl-find-if
                     (lambda (item)
                       (string= (mb-api-release-group-format item) selected-name))
                     data)))))

(defun mb-api-release-group (release-group)
  (interactive "sRelease group: ")
  (mb-api-open (mb-api-release-group-select (mb-api-search-release-group-exact release-group)))
  )

(defun mb-api-search-work (work)
  "Searches for WORK, and returns raw lisp data."
  (mb-api-search "work" work)
  )

(defun mb-api-search-work-tidy (work)
  "Searches for WORK and returns a vector."
  (cdr (assoc 'works (cdddr (mb-api-search-work work))))
  )

;; what to get out of work
;; name
;; disambiguation if there is one
;; composers, there might be multiple
;; id obviously

(defun mb-api-search-work-get-composers (relation)
  "Input should be the output of
(append (cdr (assoc 'relations (append (mb-api-search-work-tidy \"foo\") nil))) nil)."
  (when (string= (cdr (assoc 'type relation)) "composer")
    `(composers . ,(append (list
                            (assoc 'name (assoc 'artist relation))
                            (assoc 'sort-name (assoc 'artist relation))
                            (if (assoc 'disambiguation (assoc 'artist relation))
                                (assoc 'disambiguation (assoc 'artist relation))
                              '(disambiguation . ""))))))
  )

(defun mb-api-search-work-exact (work)
  "Searches for WORK, and returns an alist of names, disambiguations and IDs.
If there is no disambiguation, it puts (disambiguation . \"\")."
  (mapcar (lambda (x)
            (append (list
                     (assoc 'title x)
                     (if (assoc 'disambiguation x)
                         (assoc 'disambiguation x)
                       '(disambiguation . ""))
                     (car x))
                    ;; listing composers
                    (delq nil (mapcar #'mb-api-search-work-get-composers
                                      (append (cdr (assoc 'relations x)) nil)))))
          (append (mb-api-search-work-tidy work) nil))
  )

(defun mb-api-work-format (item)
  "Formats item into a string."
  (concat
   (propertize (cdr (assoc 'title item)) 'face 'underline)
   ;; (propertize (cdr (assoc 'sort-name item)) 'face 'italic)
   ;; if there is disambiguation, add it
   (unless (string= (cdr (assoc 'disambiguation item)) "")
     (concat " (" (cdr (assoc 'disambiguation item)) ")"))
   ;;   " (" (cdr (assoc 'disambiguation item)) ")")
   ))

(defun mb-api-work-select (data)
  "Prompt the user to select a name from the list DATA and return the corresponding ID.
The DATA is meant to be the output of `mb-api-search-work-exact'."
  (let* ((name-list (mapcar #'mb-api-work-format data))
         (selected-name (completing-read "Work: " name-list)))
    (cdr (assoc 'id (cl-find-if
                     (lambda (item)
                       (string= (mb-api-work-format item) selected-name))
                     data)))))

(defun mb-api-work (work)
  (interactive "sWork: ")
  (mb-api-open (mb-api-work-select (mb-api-search-work-exact work)))
  )

(provide 'mb-api)
;;; mb-api.el ends here
