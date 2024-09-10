;;; mb-search.el --- Emacs MusicBrainz searching       -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author:  Oliwier Czerwiński <oliwier.czerwi@proton.me>
;; Keywords: convenience
;; Version: 20240910

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

;; NOTE non latin characters are not displayed correctly

;;; Code:

(require 'url)
(require 'url-http)
(require 'json)

(defconst mb-search-version "20240910")

(defun mb-search-user-agent ()
  "Returns a valid User-Agent string."
  (format "emacs-mb-search/%s (https://github.com/deadendpl/emacs-mb-search)" mb-search-version)
  )

(defun mb-api-search (type query)
  "Searches for QUERY of TYPE, and returns raw lisp data."
  (with-current-buffer
      (let ((url-request-extra-headers `(("User-Agent" . ,(mb-search-user-agent)))))
        (url-retrieve-synchronously (format "https://musicbrainz.org/ws/2/%s?query=%s&fmt=json" type query)))
    (goto-char url-http-end-of-headers)
    (let ((output (json-read)))
      (if (assoc 'error output)
          (error (cdr (assoc 'error output)))
        output))))

(defun mb-api-select (data format-func prompt)
  "Prompt the user to select a name from the list DATA and return the corresponding ID.
The DATA should be the output of exact searching funcion like `mb-api-search-artist-exact'.
FORMAT-FUNC is the formatting function
PROMPT is a string that's used as comepltion prompt."
  (let* ((name-list (mapcar format-func data))
         (selected-name (completing-read prompt name-list)))
    (cdr (assoc 'id (cl-find-if
                     (lambda (item)
                       (string= (funcall format-func item) selected-name))
                     data)))))

(defun mb-api-open (mbid)
  "Opens MBID in a MusicBrainz webpage."
  (browse-url (concat "https://musicbrainz.org/mbid/" mbid)))

;;; Artist

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
  "Formats ITEM into a string.
The ITEM should be an alist returned by `mb-api-search-artist-exact'."
  (concat
   (propertize (cdr (assoc 'name item)) 'face 'underline)
   " (" (propertize (cdr (assoc 'sort-name item)) 'face 'italic)
   ;; if there is disambiguation, add it
   (unless (string= (cdr (assoc 'disambiguation item)) "")
     (format ", %s" (cdr (assoc 'disambiguation item))))
   ")"
   ))

(defun mb-api-artist-select (artist)
  (mb-api-select (mb-api-search-artist-exact artist) #'mb-api-artist-format "Artist: "))

;;;###autoload
(defun mb-search-artist (artist)
  (interactive "sArtist: ")
  (mb-api-open (mb-api-artist-select artist))
  )

;;; Release group

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
                     (car x) ; id
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

(defun mb-api-release-group-select (release-group)
  (mb-api-select (mb-api-search-release-group-exact release-group) #'mb-api-release-group-format "Release group: "))

;;;###autoload
(defun mb-search-release-group (release-group)
  (interactive "sRelease group: ")
  (mb-api-open (mb-api-release-group-select release-group))
  )

;;; Work

(defun mb-api-search-work (work)
  "Searches for WORK, and returns raw lisp data."
  (mb-api-search "work" work)
  )

(defun mb-api-search-work-tidy (work)
  "Searches for WORK and returns a vector."
  (cdr (assoc 'works (cdddr (mb-api-search-work work))))
  )

;; (defun mb-api-search-work-get-composer (relation)
;;   "Input should be the output of
;; (`car' (`append' (`cdr' (`assoc' 'relations (`car' (`append' (`mb-api-search-work-tidy' \"foo\") nil)))) nil))."
;;   (if (string= (cdr (assoc 'type relation)) "composer")
;;       `(composers . ,(append (list
;;                               (assoc 'name (assoc 'artist relation))
;;                               (assoc 'sort-name (assoc 'artist relation))
;;                               (if (assoc 'disambiguation (assoc 'artist relation))
;;                                   (assoc 'disambiguation (assoc 'artist relation))
;;                                 '(disambiguation . "")))))
;;     "")
;;   )

(defun mb-api-search-work-exact (work)
  "Searches for WORK, and returns an alist of names, disambiguations and IDs.
If there is no disambiguation, it puts (disambiguation . \"\")."
  (mapcar (lambda (x)
            (append (list
                     (assoc 'title x)
                     (if (assoc 'disambiguation x)
                         (assoc 'disambiguation x)
                       '(disambiguation . ""))
                     ;; (cons 'composers (mapcar #'mb-api-search-work-get-composer
                     ;;                          (append (cdr (assoc 'relations x)) nil)))
                     (car x))
                    ))
          (append (mb-api-search-work-tidy work) nil))
  )

(defun mb-api-work-format (item)
  "Formats item into a string."
  (concat
   (propertize (cdr (assoc 'title item)) 'face 'underline)
   ;; if there is disambiguation, add it
   (unless (string= (cdr (assoc 'disambiguation item)) "")
     (concat " (" (cdr (assoc 'disambiguation item)) ")"))
   ))

(defun mb-api-work-select (work)
  (mb-api-select (mb-api-search-work-exact work) #'mb-api-work-format "Work: "))

;;;###autoload
(defun mb-search-work (work)
  (interactive "sWork: ")
  (mb-api-open (mb-api-work-select work))
  )

;;; Release

(defun mb-api-search-release (release)
  (mb-api-search "release" release))

(defun mb-api-search-release-tidy (release)
  (cdr (assoc 'releases (cdddr (mb-api-search-release release)))))

(defun mb-api-search-release-exact (release)
  "Searches for RELEASE, and returns an alist of titles, first
release dates, and IDs. If there is no disambiguation, it
puts (disambiguation . \"\"). NOTE that non latin characters will
not be displayed correctly."
  (mapcar (lambda (x)
            (append (list
                     (assoc 'title x)
                     (if (assoc 'date x)
                         (assoc 'date x)
                       '(date . ""))
                     (car x) ; id
                     ;; (cons 'artist-name (cdr (assoc 'name (car (append (cdr (assoc 'artist-credit (car x))) nil)))))
                     ;; (cons 'artist-sort-name
                     ;;       (cdr (assoc 'sort-name (cadar (append (cdr (assoc 'artist-credit x)) nil)))))
                     )))
          (append (mb-api-search-release-tidy release) nil))
  )

(defun mb-api-release-format (x)
  (concat
   (cdr (assoc 'date x)) " - "
   (propertize (cdr (assoc 'title x)) 'face 'underline)
   ))

(defun mb-api-release-select (release)
  (mb-api-select (mb-api-search-release-exact release) #'mb-api-release-format "Release: "))

;;;###autoload
(defun mb-search-release (release)
  (interactive "sRelease: ")
  (mb-api-open (mb-api-release-select release))
  )

;;; Series

(defun mb-api-search-series (series)
  (mb-api-search "series" series))

(defun mb-api-search-series-tidy (series)
  (cdr (assoc 'series (cdddr (mb-api-search-series series)))))

(defun mb-api-search-series-exact (series)
  "Searches for SERIES, and returns an alist of basic info. If there is no disambiguation, it
puts (disambiguation . \"\"). NOTE that non latin characters will
not be displayed correctly."
  (mapcar (lambda (x)
            (append (list
                     (assoc 'name x)
                     (assoc 'type x)
                     (assoc 'type x)
                     (assoc 'disambiguation x)
                     (car x) ; id
                     )))
          (append (mb-api-search-series-tidy series) nil))
  )

(defun mb-api-series-format (x)
  (concat
   (cdr (assoc 'name x))
   " (" (cdr (assoc 'type x))
   (if (assoc 'disambiguation x)
       (concat ", " (cdr (assoc 'disambiguation x))))
   ")"
   ))

(defun mb-api-series-select (series)
  (mb-api-select (mb-api-search-series-exact series) #'mb-api-series-format "Series: "))

;;;###autoload
(defun mb-search-series (series)
  (interactive "sSeries: ")
  (mb-api-open (mb-api-series-select series))
  )

(provide 'mb-search)
;;; mb-search.el ends here
