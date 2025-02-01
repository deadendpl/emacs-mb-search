;;; mb-search.el --- Emacs MusicBrainz searching       -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author:  Oliwier Czerwiński <oliwier.czerwi@proton.me>
;; Keywords: convenience, music
;; Version: 20250201
;; URL: https://github.com/deadendpl/emacs-mb-search

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

;; This package allows doing MusicBrainz searches by using their [API].

;; It uses built-in `completing-read' to choose candidates.

;; It will use curl by default for doing API requests, because using
;; built-in url library resulted in characters not rendering correctly.
;; If you still want to use url library for API requests, you can change
;; the value of `mb-search-curl-p' to nil.

;; Usage

;; Run mb-search-[type of entity] to do a search.

;; Tips

;; - You can set the limit of candidates in the `mb-search-limit'
;;   variable. Values between 1 and 100 are allowed.

;; [API] <https://musicbrainz.org/doc/MusicBrainz_API>

;;; Code:

(require 'url)
(require 'url-http)
(require 'json)

(defconst mb-search-version "20250201")

(defcustom mb-search-limit 25
  "The maximum number of entries returned.
Only values between 1 and 100 (both inclusive) are allowed."
  :type 'integer)

(defvar mb-search-curl-p (executable-find "curl")
  "Non-nil means curl is in path.")

(defvar mb-search-user-agent
  (concat "emacs-mb-search/" mb-search-version
          " (https://github.com/deadendpl/emacs-mb-search)")
  "A User agent string for mb-search.")

(defun mb-search-api--url (type query)
  "Search for QUERY of TYPE, and return raw Lisp data.
It uses built-in url package."
  (with-current-buffer
      (let ((url-request-extra-headers `(("User-Agent" . ,mb-search-user-agent))))
        (url-retrieve-synchronously
         (format "https://musicbrainz.org/ws/2/%s?query=%s&fmt=json&limit=%s"
                 type query mb-search-limit)))
    (goto-char url-http-end-of-headers)
    (let ((output (json-read)))
      (if (assoc 'error output)
          (error (cdr (assoc 'error output)))
        output))))

(defun mb-search-api--curl (type query)
  "Search for QUERY of TYPE, and return raw Lisp data.
It uses curl."
  (let ((query (url-hexify-string query)))
    (with-temp-buffer
      (call-process
       "curl" nil t nil "-s" "-A" mb-search-user-agent
       (format
        "https://musicbrainz.org/ws/2/%s?query=%s&fmt=json&limit=%s"
        type query mb-search-limit))
      (goto-char (point-min))
      (let ((output (json-read)))
        (if (assoc 'error output)
            (error (cdr (assoc 'error output)))
          output)))))

(defun mb-search-api (type query)
  "Search for QUERY of TYPE, and return raw Lisp data."
  (if mb-search-curl-p
      (mb-search-api--curl type query)
    (mb-search-api--url type query)))

(defun mb-search--tidy (func query)
  "Return a data ready to be used.
It checks for results, and errors if there are none.
Applies QUERY to FUNC which should be mb-search--[type]."
  (let ((data (append (cdr (cadddr (funcall func query))) nil)))
    (if (eq (length data) 0)
        (error "No results were found")
      data)))

(defmacro mb-search-define-exact (type &rest args)
  "Define a exact retrieving function.
TYPE is a unquoted symbol of entity type.
ARGS are expressions used to retrieve info from element of tidy
function's output."
  `(defun ,(intern (format "mb-search--%s-exact" type)) (,type)
     (mapcar (lambda (x)
               (append (list ,@args)))
             (,(intern (format "mb-search--%s-tidy" type)) ,type))))

(defun mb-search-get-artists (artists)
  "Return a artist string.
ARTISTS is entity's artist-credit item."
  (let ((string))
    (seq-map (lambda (item)
               (setq string (concat string
                                    (concat (cdr (assoc 'name item))
                                            (cdr (assoc 'joinphrase item))))))
             artists)
    string))

(defun mb-search-select (data format-func prompt result)
  "Select a name from the list DATA and return the corresponding ID.
The DATA should be the output of exact searching
function like `mb-search--artist-exact'.
FORMAT-FUNC is the formatting function.
PROMPT is a string that's used as completion prompt.
RESULT should be id symbol in most cases."
  (let* ((name-list (mapcar format-func data))
         (selected-name (completing-read prompt name-list nil t)))
    (cdr (assoc result (cl-find-if
                        (lambda (item)
                          (string= (funcall format-func item) selected-name))
                        data)))))

(defun mb-search-open (mbid)
  "Open MBID in a MusicBrainz website."
  (interactive "sMBID: ")
  (browse-url (concat "https://musicbrainz.org/mbid/" mbid)))

;;; Artist

(defun mb-search--artist (artist)
  "Search for ARTIST, and return raw Lisp data."
  (mb-search-api "artist" artist))

(defun mb-search--artist-tidy (artist)
  "Search for ARTIST and return a vector."
  (mb-search--tidy #'mb-search--artist artist))

(mb-search-define-exact artist
                        (assoc 'name x)
                        (assoc 'sort-name x)
                        (assoc 'disambiguation x)
                        ;; `car' is id, and it's faster than `assoc'
                        (car x))

(defun mb-search--artist-format (item)
  "Format ITEM into a string.
The ITEM should be an alist returned by `mb-search--artist-exact'."
  (concat
   (propertize (cdr (assoc 'name item)) 'face 'underline)
   (let ((disambiguation (cdr (assoc 'disambiguation item)))
         (sort-name (cdr (assoc 'sort-name item))))
     (cond
      ((and disambiguation sort-name)
       (if (not (string= sort-name (cdr (assoc 'name item))))
           (concat " (" (propertize sort-name 'face 'italic) ", "
                   disambiguation ")")
         (concat " (" disambiguation ")")))
      (sort-name
       (unless (string= sort-name (cdr (assoc 'name item)))
         (concat " (" (propertize sort-name 'face 'italic) ")")))
      (disambiguation
       (concat " (" disambiguation ")"))))))

(defun mb-search--artist-select (artist)
  (mb-search-select (mb-search--artist-exact artist) #'mb-search--artist-format "Artist: " 'id))

;;;###autoload
(defun mb-search-artist (artist)
  (interactive "sArtist: ")
  (mb-search-open (mb-search--artist-select artist)))

;;; Release group

(defun mb-search--release-group (release-group)
  "Search for a RELEASE-GROUP, and return raw Lisp data."
  (mb-search-api "release-group" release-group))

(defun mb-search--release-group-tidy (release-group)
  "Search for RELEASE-GROUP and return a vector."
  (mb-search--tidy #'mb-search--release-group release-group))

(mb-search-define-exact release-group
                        (assoc 'title x)
                        (assoc 'primary-type x)
                        (assoc 'first-release-date x)
                        (assoc 'artist-credit x)
                        (car x)) ; id

(defun mb-search--release-group-format (item)
  "Format ITEM into a string.
The ITEM should be an alist returned by `mb-search--release-group-exact'."
  (concat
   (if (assoc 'first-release-date item)
       (concat (cdr (assoc 'first-release-date item)) " - "))
   (if (assoc 'primary-type item)
       (concat (cdr (assoc 'primary-type item)) " - "))
   (propertize (cdr (assoc 'title item)) 'face 'underline) " - "
   (mb-search-get-artists
    (append (cdr (assoc 'artist-credit item)) nil))))

(defun mb-search--release-group-select (release-group)
  (mb-search-select (mb-search--release-group-exact release-group) #'mb-search--release-group-format "Releae group: " 'id))

;;;###autoload
(defun mb-search-release-group (release-group)
  (interactive "sRelease group: ")
  (mb-search-open (mb-search--release-group-select release-group)))

;;; Work

(defun mb-search--work (work)
  "Search for WORK, and return raw Lisp data."
  (mb-search-api "work" work))

(defun mb-search--work-tidy (work)
  "Search for WORK and return a vector."
  (mb-search--tidy #'mb-search--work work))

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

(mb-search-define-exact work
                        (assoc 'title x)
                        (assoc 'disambiguation x)
                        (car x))

(defun mb-search--work-format (item)
  "Format ITEM into a string.
The ITEM should be an alist returned by `mb-search--work-exact'."
  (concat
   (propertize (cdr (assoc 'title item)) 'face 'underline)
   ;; if there is disambiguation, add it
   (if (assoc 'disambiguation item)
       (concat " (" (cdr (assoc 'disambiguation item)) ")"))))

(defun mb-search--work-select (work)
  (mb-search-select (mb-search--work-exact work) #'mb-search--work-format "Work: " 'id))

;;;###autoload
(defun mb-search-work (work)
  (interactive "sWork: ")
  (mb-search-open (mb-search--work-select work)))

;;; Release

(defun mb-search--release (release)
  (mb-search-api "release" release))

(defun mb-search--release-tidy (release)
  "Search for RELEASE and return a vector."
  (mb-search--tidy #'mb-search--release release))

(mb-search-define-exact release
                        (assoc 'title x)
                        (assoc 'date x)
                        (assoc 'disambiguation x)
                        (assoc 'artist-credit x)
                        (car x))

(defun mb-search--release-format (item)
  "Format ITEM into a string.
The ITEM should be an alist returned by `mb-search--release-exact'."
  (concat
   (if (and (assoc 'date item)
            ;; there may be empty dates
            (not (string= (cdr (assoc 'date item)) "")))
       (concat (cdr (assoc 'date item)) " - "))
   (propertize (cdr (assoc 'title item)) 'face 'underline)
   (if (assoc 'disambiguation item)
       (concat " (" (cdr (assoc 'disambiguation item)) ")"))
   " - " (mb-search-get-artists
          (append (cdr (assoc 'artist-credit item)) nil))))

(defun mb-search--release-select (release)
  (mb-search-select (mb-search--release-exact release) #'mb-search--release-format "Release: " 'id))

;;;###autoload
(defun mb-search-release (release)
  (interactive "sRelease: ")
  (mb-search-open (mb-search--release-select release)))

;;; Series

(defun mb-search--series (series)
  (mb-search-api "series" series))

(defun mb-search--series-tidy (series)
  "Search for SERIES and return a vector."
  (mb-search--tidy #'mb-search--series series))

(mb-search-define-exact series
                        (assoc 'name x)
                        (assoc 'type x)
                        (assoc 'disambiguation x)
                        (car x))

(defun mb-search--series-format (item)
  "Format ITEM into a string.
The ITEM should be an alist returned by `mb-search--series-exact'."
  (concat
   (cdr (assoc 'name item))
   " (" (cdr (assoc 'type item))
   (if (assoc 'disambiguation item)
       (concat ", " (cdr (assoc 'disambiguation item))))
   ")"))

(defun mb-search--series-select (series)
  (mb-search-select (mb-search--series-exact series) #'mb-search--series-format "Series: " 'id))

;;;###autoload
(defun mb-search-series (series)
  (interactive "sSeries: ")
  (mb-search-open (mb-search--series-select series)))

;;; Tag

(defun mb-search--tag (tag)
  (mb-search-api "tag" tag))

(defun mb-search--tag-tidy (tag)
  "Search for TAG and return a vector."
  (mb-search--tidy #'mb-search--tag tag))

(mb-search-define-exact tag (append (cdr (assoc 'name x))))

(defun mb-search--tag-format (item)
  "Format ITEM into a string.
The ITEM should be an alist returned by `mb-search--tag-exact'."
  (propertize item 'face 'underline))

(defun mb-search--tag-select (tag)
  (completing-read "Tag: " (mb-search--tag-exact tag)))

;;;###autoload
(defun mb-search-tag (tag)
  (interactive "sTag: ")
  (browse-url (concat "https://musicbrainz.org/tag/" (mb-search--tag-select tag))))

;;; Annotation

(defun mb-search--annotation (annotation)
  (mb-search-api "annotation" annotation))

(defun mb-search--annotation-tidy (annotation)
  "Search for ANNOTATION and return a vector."
  (mb-search--tidy #'mb-search--annotation annotation))

(mb-search-define-exact annotation
                        (assoc 'text x)
                        (assoc 'type x)
                        (assoc 'name x)
                        (assoc 'entity x))

(defun mb-search--annotation-format (item)
  "Format ITEM into a string.
The ITEM should be an alist returned by `mb-search--annotation-exact'."
  (concat
   (propertize (cdr (assoc 'text item)) 'face 'underline)
   " (" (cdr (assoc 'type item)) ": "
   (propertize (cdr (assoc 'name item)) 'face 'italic) ")"))

(defun mb-search--annotation-select (annotation)
  (mb-search-select (mb-search--annotation-exact annotation) #'mb-search--annotation-format "Artist: " 'entity))

;;;###autoload
(defun mb-search-annotation (annotation)
  (interactive "sAnnotation: ")
  (mb-search-open (mb-search--annotation-select annotation)))

;;; Area

(defun mb-search--area (area)
  (mb-search-api "area" area))

(defun mb-search--area-tidy (area)
  "Search for AREA and return a vector."
  (mb-search--tidy #'mb-search--area area))

(mb-search-define-exact area
                        (assoc 'name x)
                        (assoc 'type x)
                        (car x))

(defun mb-search--area-format (item)
  "Format ITEM into a string.
The ITEM should be an alist returned by `mb-search--area-exact'."
  (concat
   (propertize (cdr (assoc 'name item)) 'face 'underline)
   " (" (cdr (assoc 'type item)) ")"))

(defun mb-search--area-select (area)
  (mb-search-select (mb-search--area-exact area) #'mb-search--area-format "Area: " 'id))

;;;###autoload
(defun mb-search-area (area)
  (interactive "sArea: ")
  (mb-search-open (mb-search--area-select area)))

;;; CDstub

(defun mb-search--cdstub (cdstub)
  (mb-search-api "cdstub" cdstub))

(defun mb-search--cdstub-tidy (cdstub)
  "Search for CDSTUB and return a vector."
  (mb-search--tidy #'mb-search--cdstub cdstub))

(mb-search-define-exact cdstub
                        (assoc 'title x)
                        (assoc 'artist x)
                        (car x))

(defun mb-search--cdstub-format (item)
  "Format ITEM into a string.
The ITEM should be an alist returned by `mb-search--cdstub-exact'."
  (concat
   (propertize (cdr (assoc 'title item)) 'face 'underline)
   (if (assoc 'artist item)
       (concat " (" (cdr (assoc 'artist item)) ")"))))

(defun mb-search--cdstub-select (cdstub)
  (mb-search-select (mb-search--cdstub-exact cdstub) #'mb-search--cdstub-format "Cdstub: " 'id))

;;;###autoload
(defun mb-search-cdstub (cdstub)
  (interactive "sCDstub: ")
  (browse-url (concat "https://musicbrainz.org/cdstub/" (mb-search--cdstub-select cdstub))))

;;; Event

(defun mb-search--event (event)
  (mb-search-api "event" event))

(defun mb-search--event-tidy (event)
  "Search for EVENT and return a vector."
  (mb-search--tidy #'mb-search--event event))

(mb-search-define-exact event
                        (assoc 'name x)
                        (assoc 'type x)
                        (assoc 'disambiguation x)
                        (assoc 'life-span x)
                        (car x))

(defun mb-search--event-format (item)
  "Format ITEM into a string.
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
               ")")))))

(defun mb-search--event-select (event)
  (mb-search-select (mb-search--event-exact event) #'mb-search--event-format "Event: " 'id))

;;;###autoload
(defun mb-search-event (event)
  (interactive "sEvent: ")
  (mb-search-open (mb-search--event-select event)))

;;; Recording

(defun mb-search--recording (recording)
  (mb-search-api "recording" recording))

(defun mb-search--recording-tidy (recording)
  "Search for RECORDING and return a vector."
  (mb-search--tidy #'mb-search--recording recording))

(mb-search-define-exact recording
                        (assoc 'title x)
                        ;; calculating time as it's in ms
                        (if (assoc 'length x)
                            (let* ((total-seconds (/ (cdr (assoc 'length x)) 1000))
                                   (minutes (/ total-seconds 60))
                                   (seconds (% total-seconds 60)))
                              (cons 'length (format "%d:%02d" minutes seconds))))
                        (assoc 'disambiguation x)
                        (assoc 'artist-credit x)
                        (car x))

(defun mb-search--recording-format (item)
  "Format ITEM into a string.
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
               ")")))
   " - " (mb-search-get-artists
          (append (cdr (assoc 'artist-credit item)) nil))))

(defun mb-search--recording-select (recording)
  (mb-search-select (mb-search--recording-exact recording) #'mb-search--recording-format "Recording: " 'id))

;;;###autoload
(defun mb-search-recording (recording)
  (interactive "sRecording: ")
  (mb-search-open (mb-search--recording-select recording)))

;;; Instrument

(defun mb-search--instrument (instrument)
  "Search for INSTRUMENT, and return raw Lisp data."
  (mb-search-api "instrument" instrument))

(defun mb-search--instrument-tidy (instrument)
  "Search for INSTRUMENT and return a vector."
  (mb-search--tidy #'mb-search--instrument instrument))

(mb-search-define-exact instrument
                        (assoc 'name x)
                        (assoc 'type x)
                        (assoc 'disambiguation x)
                        (car x))

(defun mb-search--instrument-format (item)
  "Format ITEM into a string.
The ITEM should be an alist returned by `mb-search--instrument-exact'."
  (concat
   (propertize (cdr (assoc 'name item)) 'face 'underline)
   " (" (propertize (cdr (assoc 'type item)) 'face 'italic)
   ;; if there is disambiguation, add it
   (if (assoc 'disambiguation item)
       (concat ", " (cdr (assoc 'disambiguation item))))
   ")"))

(defun mb-search--instrument-select (instrument)
  (mb-search-select (mb-search--instrument-exact instrument) #'mb-search--instrument-format "Instrument: " 'id))

;;;###autoload
(defun mb-search-instrument (instrument)
  (interactive "sInstrument: ")
  (mb-search-open (mb-search--instrument-select instrument)))

;;; Label

(defun mb-search--label (label)
  "Search for LABEL, and return raw Lisp data."
  (mb-search-api "label" label))

(defun mb-search--label-tidy (label)
  "Search for LABEL and return a vector."
  (mb-search--tidy #'mb-search--label label))

(mb-search-define-exact label
                        (assoc 'name x)
                        (assoc 'disambiguation x)
                        (car x))

(defun mb-search--label-format (item)
  "Format ITEM into a string.
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
  (mb-search-open (mb-search--label-select label)))

;;; Place

(defun mb-search--place (place)
  "Search for PLACE, and return raw Lisp data."
  (mb-search-api "place" place))

(defun mb-search--place-tidy (place)
  "Search for PLACE and return a vector."
  (mb-search--tidy #'mb-search--place place))

(mb-search-define-exact place
                        (assoc 'name x)
                        (assoc 'type x)
                        (assoc 'disambiguation x)
                        (car x))

(defun mb-search--place-format (item)
  "Format ITEM into a string.
The ITEM should be an alist returned by `mb-search--place-exact'."
  (concat
   (propertize (cdr (assoc 'name item)) 'face 'underline)
   (if (assoc 'type item)
       (concat " (" (cdr (assoc 'type item)) ")"))))

(defun mb-search--place-select (place)
  (mb-search-select (mb-search--place-exact place) #'mb-search--place-format "Place: " 'id))

;;;###autoload
(defun mb-search-place (place)
  (interactive "sPlace: ")
  (mb-search-open (mb-search--place-select place)))

;;; URL

(defun mb-search--url (url)
  "Search for URL, and return raw Lisp data."
  (mb-search-api "url" url))

(defun mb-search--url-tidy (url)
  "Search for URL and return a vector."
  (mb-search--tidy #'mb-search--url url))

(mb-search-define-exact url
                        (assoc 'resource x)
                        (car x))

(defun mb-search--url-format (item)
  "Format ITEM into a string.
The ITEM should be an alist returned by `mb-search--url-exact'."
  (propertize (cdr (assoc 'resource item)) 'face 'underline))

(defun mb-search--url-select (url)
  (mb-search-select (mb-search--url-exact url) #'mb-search--url-format "URL: " 'id))

;;;###autoload
(defun mb-search-url (url)
  (interactive "sURL: ")
  (mb-search-open (mb-search--url-select url)))

(provide 'mb-search)
;;; mb-search.el ends here
