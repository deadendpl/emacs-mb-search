;;; mb-search.el --- Emacs MusicBrainz searching       -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author:  Oliwier Czerwiński <oliwier.czerwi@proton.me>
;; Keywords: convenience
;; Version: 20241030

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

;; NOTE non latin characters are not displayed correctly unless there
;; is curl installed

;;; Code:

(require 'url)
(require 'url-http)
(require 'json)

(defconst mb-search-version "20241029")

(defcustom mb-search-limit 25
  "The maximum number of entries returned.
Only values between 1 and 100 (both inclusive) are allowed."
  :type 'integer)

(defvar mb-search-curl-p (executable-find "curl")
  "Non-nil means curl is in path.")

(defun mb-search-user-agent ()
  "Returns a valid User-Agent string."
  (format "emacs-mb-search/%s (https://github.com/deadendpl/emacs-mb-search)" mb-search-version)
  )

(defun mb-search-api--url (type query)
  "Searches for QUERY of TYPE, and returns raw lisp data.
It uses built-in url package."
  (with-current-buffer
      (let ((url-request-extra-headers `(("User-Agent" . ,(mb-search-user-agent)))))
        (url-retrieve-synchronously (format "https://musicbrainz.org/ws/2/%s?query=%s&fmt=json&limit=%s" type query mb-search-limit)))
    (goto-char url-http-end-of-headers)
    (let ((output (json-read)))
      (if (assoc 'error output)
          (error (cdr (assoc 'error output)))
        output))))

(defun mb-search-api--curl (type query)
  "Searches for QUERY of TYPE, and returns raw lisp data.
It uses curl."
  (let ((query (url-hexify-string query)))
    (with-temp-buffer
      (call-process "curl" nil t nil "-s" "-A" (mb-search-user-agent) (format "https://musicbrainz.org/ws/2/%s?query=%s&fmt=json&limit=%s" type query mb-search-limit))
      (goto-char (point-min))
      (let ((output (json-read)))
        (if (assoc 'error output)
            (error (cdr (assoc 'error output)))
          output))))
  )

(defun mb-search-api (type query)
  "Searches for QUERY of TYPE, and returns raw lisp data."
  (if mb-search-curl-p
      (mb-search-api--curl type query)
    (mb-search-api--url type query)))

(defun mb-search--tidy (func query)
  "Returns a data ready to be used.
It checks for results, and errors if there are none."
  (let ((data (append (cdr (cadddr (funcall func query))) nil)))
    (if (eq (length data) 0)
        (error "No results were found.")
      data)
    )
  )

(defun mb-search-select (data format-func prompt result)
  "Prompt the user to select a name from the list DATA and return the corresponding ID.
The DATA should be the output of exact searching funcion like `mb-search--artist-exact'.
FORMAT-FUNC is the formatting function.
PROMPT is a string that's used as comepletion prompt.
RESULT should be id symbol in most cases."
  (let* ((name-list (mapcar format-func data))
         (selected-name (completing-read prompt name-list nil t)))
    (cdr (assoc result (cl-find-if
                        (lambda (item)
                          (string= (funcall format-func item) selected-name))
                        data)))))

(defun mb-search-open (mbid)
  "Opens MBID in a MusicBrainz website."
  (browse-url (concat "https://musicbrainz.org/mbid/" mbid)))

;;; Artist

(defun mb-search--artist (artist)
  "Searches for ARTIST, and returns raw lisp data."
  (mb-search-api "artist" artist)
  )

(defun mb-search--artist-tidy (artist)
  "Searches for ARTIST and returns a vector."
  (mb-search--tidy #'mb-search--artist artist)
  )

(defun mb-search--artist-exact (artist)
  "Searches for ARTIST, and returns an alist of names, disambiguations and IDs."
  (mapcar (lambda (x)
            (append (list
                     (assoc 'name x)
                     (assoc 'sort-name x)
                     (assoc 'disambiguation x)
                     ;; `car' is id, and it's faster than `assoc'
                     (car x))))
          (mb-search--artist-tidy artist))
  )

(defun mb-search--artist-format (item)
  "Formats ITEM into a string.
The ITEM should be an alist returned by `mb-search--artist-exact'."
  (concat
   (propertize (cdr (assoc 'name item)) 'face 'underline)
   (when (assoc 'disambiguation item)
     (concat " ("
             (if (string= (cdr (assoc 'sort-name item)) (cdr (assoc 'name item)))
                 (concat (cdr (assoc 'disambiguation item)))
               (concat
                (propertize (cdr (assoc 'sort-name item)) 'face 'italic) ", "
                (cdr (assoc 'disambiguation item)))
               )
             ")"
             )
     )
   ))

(defun mb-search--artist-select (artist)
  (mb-search-select (mb-search--artist-exact artist) #'mb-search--artist-format "Artist: " 'id))

;;;###autoload
(defun mb-search-artist (artist)
  (interactive "sArtist: ")
  (mb-search-open (mb-search--artist-select artist))
  )

;;; Release group

(defun mb-search--release-group (release-group)
  "Searches for a RELEASE-GROUP, and returns raw lisp data."
  (mb-search-api "release-group" release-group))

(defun mb-search--release-group-tidy (release-group)
  "Searches for RELEASE-GROUP and returns a vector."
  (mb-search--tidy #'mb-search--release-group release-group)
  )

(defun mb-search--release-group-exact (release-group)
  "Searches for RELEASE-GROUP, and returns an alist of titles, first
release dates, and IDs."
  (mapcar (lambda (x)
            (append (list
                     (assoc 'title x)
                     (assoc 'primary-type x)
                     (assoc 'first-release-date x)
                     (car x) ; id
                     (cons 'artist-name (cdaar (append (cdr (assoc 'artist-credit x)) nil)))
                     )))
          (mb-search--release-group-tidy release-group))
  )

(defun mb-search--release-group-format (item)
  "Formats ITEM into a string.
The ITEM should be an alist returned by `mb-search--release-group-exact'."
  (concat
   (cdr (assoc 'first-release-date item)) " - "
   (cdr (assoc 'primary-type item)) " - "
   (propertize (cdr (assoc 'title item)) 'face 'underline) " - "
   (cdr (assoc 'artist-name item))
   ))

(defun mb-search--release-group-select (release-group)
  (mb-search-select (mb-search--release-group-exact release-group) #'mb-search--release-group-format "Releae group: " 'id))

;;;###autoload
(defun mb-search-release-group (release-group)
  (interactive "sRelease group: ")
  (mb-search-open (mb-search--release-group-select release-group))
  )

;;; Work

(defun mb-search--work (work)
  "Searches for WORK, and returns raw lisp data."
  (mb-search-api "work" work)
  )

(defun mb-search--work-tidy (work)
  "Searches for WORK and returns a vector."
  (mb-search--tidy #'mb-search--work work)
  )

;; (defun mb-search--work-get-composer (relation)
;;   "Input should be the output of
;; (`car' (`append' (`cdr' (`assoc' 'relations (`car' (`append' (`mb-search--work-tidy' \"foo\") nil)))) nil))."
;;   (if (string= (cdr (assoc 'type relation)) "composer")
;;       `(composers . ,(append (list
;;                               (assoc 'name (assoc 'artist relation))
;;                               (assoc 'sort-name (assoc 'artist relation))
;;                               (if (assoc 'disambiguation (assoc 'artist relation))
;;                                   (assoc 'disambiguation (assoc 'artist relation))
;;                                 '(disambiguation . "")))))
;;     "")
;;   )

(defun mb-search--work-exact (work)
  "Searches for WORK, and returns an alist of names, disambiguations and IDs."
  (mapcar (lambda (x)
            (append (list
                     (assoc 'title x)
                     (assoc 'disambiguation x)
                     ;; (cons 'composers (mapcar #'mb-search--work-get-composer
                     ;;                          (append (cdr (assoc 'relations x)) nil)))
                     (car x))
                    ))
          (mb-search--work-tidy work))
  )

(defun mb-search--work-format (item)
  "Formats ITEM into a string.
The ITEM should be an alist returned by `mb-search--work-exact'."
  (concat
   (propertize (cdr (assoc 'title item)) 'face 'underline)
   ;; if there is disambiguation, add it
   (if (assoc 'disambiguation item)
       (concat " (" (cdr (assoc 'disambiguation item)) ")"))
   ))

(defun mb-search--work-select (work)
  (mb-search-select (mb-search--work-exact work) #'mb-search--work-format "Work: " 'id))

;;;###autoload
(defun mb-search-work (work)
  (interactive "sWork: ")
  (mb-search-open (mb-search--work-select work))
  )

;;; Release

(defun mb-search--release (release)
  (mb-search-api "release" release))

(defun mb-search--release-tidy (release)
  (mb-search--tidy #'mb-search--release release))

(defun mb-search--release-exact (release)
  "Searches for RELEASE, and returns an alist of titles, first
release dates, and IDs."
  (mapcar (lambda (x)
            (append (list
                     (assoc 'title x)
                     (assoc 'date x)
                     (car x) ; id
                     ;; (cons 'artist-name (cdr (assoc 'name (car (append (cdr (assoc 'artist-credit (car x))) nil)))))
                     ;; (cons 'artist-sort-name
                     ;;       (cdr (assoc 'sort-name (cadar (append (cdr (assoc 'artist-credit x)) nil)))))
                     )))
          (mb-search--release-tidy release))
  )

(defun mb-search--release-format (item)
  "Formats ITEM into a string.
The ITEM should be an alist returned by `mb-search--release-exact'."
  (concat
   (if (assoc 'date item)
       (concat (cdr (assoc 'date item)) " - "))
   (propertize (cdr (assoc 'title item)) 'face 'underline)
   ))

(defun mb-search--release-select (release)
  (mb-search-select (mb-search--release-exact release) #'mb-search--release-format "Release: " 'id))

;;;###autoload
(defun mb-search-release (release)
  (interactive "sRelease: ")
  (mb-search-open (mb-search--release-select release))
  )

;;; Series

(defun mb-search--series (series)
  (mb-search-api "series" series))

(defun mb-search--series-tidy (series)
  (mb-search--tidy #'mb-search--series series))

(defun mb-search--series-exact (series)
  "Searches for SERIES, and returns an alist of basic info."
  (mapcar (lambda (x)
            (append (list
                     (assoc 'name x)
                     (assoc 'type x)
                     (assoc 'disambiguation x)
                     (car x) ; id
                     )))
          (mb-search--series-tidy series))
  )

(defun mb-search--series-format (item)
  "Formats ITEM into a string.
The ITEM should be an alist returned by `mb-search--series-exact'."
  (concat
   (cdr (assoc 'name item))
   " (" (cdr (assoc 'type item))
   (if (assoc 'disambiguation item)
       (concat ", " (cdr (assoc 'disambiguation item))))
   ")"
   ))

(defun mb-search--series-select (series)
  (mb-search-select (mb-search--series-exact series) #'mb-search--series-format "Series: " 'id))

;;;###autoload
(defun mb-search-series (series)
  (interactive "sSeries: ")
  (mb-search-open (mb-search--series-select series))
  )

;;; Tag

(defun mb-search--tag (tag)
  (mb-search-api "tag" tag))

(defun mb-search--tag-tidy (tag)
  (mb-search--tidy #'mb-search--tag tag))

(defun mb-search--tag-exact (tag)
  "Searches for TAG, and returns an list of names."
  (mapcar (lambda (x)
            (append (cdr (assoc 'name x))))
          (mb-search--tag-tidy tag))
  )

(defun mb-search--tag-format (item)
  "Formats ITEM into a string.
The ITEM should be an alist returned by `mb-search--tag-exact'."
  (propertize item 'face 'underline))

(defun mb-search--tag-select (tag)
  (completing-read "Tag: " (mb-search--tag-exact tag)))

;;;###autoload
(defun mb-search-tag (tag)
  (interactive "sTag: ")
  (browse-url (concat "https://musicbrainz.org/tag/" (mb-search--tag-select tag)))
  )

;;; Annotation

(defun mb-search--annotation (annotation)
  (mb-search-api "annotation" annotation))

(defun mb-search--annotation-tidy (annotation)
  (mb-search--tidy #'mb-search--annotation annotation))

(defun mb-search--annotation-exact (annotation)
  "Searches for ANNOTATION, and returns an alist of basic info."
  (mapcar (lambda (x)
            (append (list
                     (assoc 'text x)
                     (assoc 'type x)
                     (assoc 'name x)
                     (assoc 'entity x) ; id
                     )))
          (mb-search--annotation-tidy annotation))
  )

(defun mb-search--annotation-format (item)
  "Formats ITEM into a string.
The ITEM should be an alist returned by `mb-search--annotation-exact'."
  (concat
   (propertize (cdr (assoc 'text item)) 'face 'underline)
   " (" (cdr (assoc 'type item)) ": "
   (propertize (cdr (assoc 'name item)) 'face 'italic) ")"
   ))

(defun mb-search--annotation-select (annotation)
  (mb-search-select (mb-search--annotation-exact annotation) #'mb-search--annotation-format "Artist: " 'entity))

;;;###autoload
(defun mb-search-annotation (annotation)
  (interactive "sAnnotation: ")
  (mb-search-open (mb-search--annotation-select (mb-search--annotation-exact annotation)))
  )

;;; Area

(defun mb-search--area (area)
  (mb-search-api "area" area))

(defun mb-search--area-tidy (area)
  (mb-search--tidy #'mb-search--area area))

(defun mb-search--area-exact (area)
  "Searches for AREA, and returns an alist of basic info."
  (mapcar (lambda (x)
            (append (list
                     (assoc 'name x)
                     (assoc 'type x)
                     (car x) ; id
                     )))
          (mb-search--area-tidy area))
  )

(defun mb-search--area-format (item)
  "Formats ITEM into a string.
The ITEM should be an alist returned by `mb-search--area-exact'."
  (concat
   (propertize (cdr (assoc 'name item)) 'face 'underline)
   " (" (cdr (assoc 'type item)) ")"
   ))

(defun mb-search--area-select (area)
  (mb-search-select (mb-search--area-exact area) #'mb-search--area-format "Area: " 'id))

;;;###autoload
(defun mb-search-area (area)
  (interactive "sArea: ")
  (mb-search-open (mb-search--area-select area))
  )

;;; CDstub

(defun mb-search--cdstub (cdstub)
  (mb-search-api "cdstub" cdstub))

(defun mb-search--cdstub-tidy (cdstub)
  (mb-search--tidy #'mb-search--cdstub cdstub))

(defun mb-search--cdstub-exact (cdstub)
  "Searches for CDSTUB, and returns an alist of basic info."
  (mapcar (lambda (x)
            (append (list
                     (assoc 'title x)
                     (assoc 'artist x)
                     (car x) ; id
                     )))
          (mb-search--cdstub-tidy cdstub))
  )

(defun mb-search--cdstub-format (item)
  "Formats ITEM into a string.
The ITEM should be an alist returned by `mb-search--cdstub-exact'."
  (concat
   (propertize (cdr (assoc 'title item)) 'face 'underline)
   (if (assoc 'artist item)
       (concat " (" (cdr (assoc 'artist item)) ")")
     )
   ))

(defun mb-search--cdstub-select (cdstub)
  (mb-search-select (mb-search--cdstub-exact cdstub) #'mb-search--cdstub-format "Cdstub: " 'id))

;;;###autoload
(defun mb-search-cdstub (cdstub)
  (interactive "sCDstub: ")
  (browse-url (concat "https://musicbrainz.org/cdstub/" (mb-search--cdstub-select cdstub)))
  )

;;; Event

(defun mb-search--event (event)
  (mb-search-api "event" event))

(defun mb-search--event-tidy (event)
  (mb-search--tidy #'mb-search--event event))

(defun mb-search--event-exact (event)
  "Searches for EVENT, and returns an alist of basic info."
  (mapcar (lambda (x)
            (append (list
                     (assoc 'name x)
                     (assoc 'type x)
                     (assoc 'disambiguation x)
                     (assoc 'life-span x)
                     (car x) ; id
                     )))
          (mb-search--event-tidy event))
  )

(defun mb-search--event-format (item)
  "Formats ITEM into a string.
The ITEM should be an alist returned by `mb-search--event-exact'."
  (concat
   (propertize (cdr (assoc 'name item)) 'face 'underline)
   (let ((year (cdadr (assoc 'life-span item)))
         (disambiguation (cdr (assoc 'disambiguation item))))
     (when (or year disambiguation)
       (concat " ("
               (when year year)
               (when (and year disambiguation) ", ")
               (when disambiguation disambiguation)
               ")")))
   ))

(defun mb-search--event-select (event)
  (mb-search-select (mb-search--event-exact event) #'mb-search--event-format "Event: " 'id))

;;;###autoload
(defun mb-search-event (event)
  (interactive "sEvent: ")
  (mb-search-open (mb-search--event-select event))
  )

;;; Recording

(defun mb-search--recording (recording)
  (mb-search-api "recording" recording))

(defun mb-search--recording-tidy (recording)
  (mb-search--tidy #'mb-search--recording recording))

(defun mb-search--recording-exact (recording)
  "Searches for RECORDING, and returns an alist of basic info."
  (mapcar (lambda (x)
            (append (list
                     (assoc 'title x)
                     ;; calculating time as it's in ms
                     (if (assoc 'length x)
                         (let* ((total-seconds (/ (cdr (assoc 'length x)) 1000))
                                (minutes (/ total-seconds 60))
                                (seconds (% total-seconds 60)))
                           (cons 'length (format "%d:%02d" minutes seconds))))
                     (assoc 'disambiguation x)
                     (car x) ; id
                     )))
          (mb-search--recording-tidy recording))
  )

(defun mb-search--recording-format (item)
  "Formats ITEM into a string.
The ITEM should be an alist returned by `mb-search--recording-exact'."
  (concat
   (propertize (cdr (assoc 'title item)) 'face 'underline)
   (let ((length (cdr (assoc 'length item)))
         (disambiguation (cdr (assoc 'disambiguation item))))
     (when (or length disambiguation)
       (concat " ("
               (when length length)
               (when (and length disambiguation) ", ")
               (when disambiguation disambiguation)
               ")")))))

(defun mb-search--recording-select (recording)
  (mb-search-select (mb-search--recording-exact recording) #'mb-search--recording-format "Recording: " 'id))

;;;###autoload
(defun mb-search-recording (recording)
  (interactive "sRecording: ")
  (mb-search-open (mb-search--recording-select recording))
  )

;;; Instrument

(defun mb-search--instrument (instrument)
  "Searches for INSTRUMENT, and returns raw lisp data."
  (mb-search-api "instrument" instrument)
  )

(defun mb-search--instrument-tidy (instrument)
  "Searches for INSTRUMENT and returns a vector."
  (mb-search--tidy #'mb-search--instrument instrument))

(defun mb-search--instrument-exact (instrument)
  "Searches for INSTRUMENT, and returns an alist of names, disambiguations and IDs."
  (mapcar (lambda (x)
            (append (list
                     (assoc 'name x)
                     (assoc 'type x)
                     (assoc 'disambiguation x)
                     ;; `car' is id, and it's faster than `assoc'
                     (car x))))
          (mb-search--instrument-tidy instrument))
  )

(defun mb-search--instrument-format (item)
  "Formats ITEM into a string.
The ITEM should be an alist returned by `mb-search--instrument-exact'."
  (concat
   (propertize (cdr (assoc 'name item)) 'face 'underline)
   " (" (propertize (cdr (assoc 'type item)) 'face 'italic)
   ;; if there is disambiguation, add it
   (unless (string= (cdr (assoc 'disambiguation item)) "")
     (format ", %s" (cdr (assoc 'disambiguation item))))
   ")"
   ))

(defun mb-search--instrument-select (instrument)
  (mb-search-select (mb-search--instrument-exact instrument) #'mb-search--instrument-format "Instrument: " 'id))

;;;###autoload
(defun mb-search-instrument (instrument)
  (interactive "sInstrument: ")
  (mb-search-open (mb-search--instrument-select instrument))
  )

;;; Label

(defun mb-search--label (label)
  "Searches for LABEL, and returns raw lisp data."
  (mb-search-api "label" label)
  )

(defun mb-search--label-tidy (label)
  "Searches for LABEL and returns a vector."
  (mb-search--tidy #'mb-search--label label))

(defun mb-search--label-exact (label)
  "Searches for LABEL, and returns an alist of names, disambiguations and IDs."
  (mapcar (lambda (x)
            (append (list
                     (assoc 'name x)
                     ;; (assoc 'type x)
                     (assoc 'disambiguation x)
                     ;; `car' is id, and it's faster than `assoc'
                     (car x))))
          (mb-search--label-tidy label))
  )

(defun mb-search--label-format (item)
  "Formats ITEM into a string.
The ITEM should be an alist returned by `mb-search--label-exact'."
  (concat
   (propertize (cdr (assoc 'name item)) 'face 'underline)
   ;; if there is disambiguation, add it
   (if (assoc 'disambiguation item)
       (concat " (" (cdr (assoc 'disambiguation item)) ")")
     )))

(defun mb-search--label-select (label)
  (mb-search-select (mb-search--label-exact label) #'mb-search--label-format "Label: " 'id))

;;;###autoload
(defun mb-search-label (label)
  (interactive "sLabel: ")
  (mb-search-open (mb-search--label-select label))
  )

;;; Place

(defun mb-search--place (place)
  "Searches for PLACE, and returns raw lisp data."
  (mb-search-api "place" place)
  )

(defun mb-search--place-tidy (place)
  "Searches for PLACE and returns a vector."
  (mb-search--tidy #'mb-search--place place))

(defun mb-search--place-exact (place)
  "Searches for PLACE, and returns an alist of names, disambiguations and IDs."
  (mapcar (lambda (x)
            (append (list
                     (assoc 'name x)
                     (assoc 'type x)
                     (assoc 'disambiguation x)
                     ;; `car' is id, and it's faster than `assoc'
                     (car x))))
          (mb-search--place-tidy place))
  )

(defun mb-search--place-format (item)
  "Formats ITEM into a string.
The ITEM should be an alist returned by `mb-search--place-exact'."
  (concat
   (propertize (cdr (assoc 'name item)) 'face 'underline)
   (if (assoc 'type item)
       (concat " (" (cdr (assoc 'type item)) ")")
     )
   ))

(defun mb-search--place-select (place)
  (mb-search-select (mb-search--place-exact place) #'mb-search--place-format "Place: " 'id))

;;;###autoload
(defun mb-search-place (place)
  (interactive "sPlace: ")
  (mb-search-open (mb-search--place-select place))
  )

;;; Url

(defun mb-search--url (url)
  "Searches for URL, and returns raw lisp data."
  (mb-search-api "url" url)
  )

(defun mb-search--url-tidy (url)
  "Searches for URL and returns a vector."
  (mb-search--tidy #'mb-search--url url)
  )

(defun mb-search--url-exact (url)
  "Searches for URL, and returns an alist of names, disambiguations and IDs."
  (mapcar (lambda (x)
            (append (list
                     (assoc 'resource x)
                     ;; `car' is id, and it's faster than `assoc'
                     (car x))))
          (mb-search--url-tidy url))
  )

(defun mb-search--url-format (item)
  "Formats ITEM into a string.
The ITEM should be an alist returned by `mb-search--url-exact'."
  ;; (concat
  (propertize (cdr (assoc 'resource item)) 'face 'underline)
  ;; )
  )

(defun mb-search--url-select (url)
  (mb-search-select (mb-search--url-exact url) #'mb-search--url-format "Url: " 'id))

;;;###autoload
(defun mb-search-url (url)
  (interactive "sUrl: ")
  (mb-search-open (mb-search--url-select url))
  )

(provide 'mb-search)
;;; mb-search.el ends here
