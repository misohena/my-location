;;; my-location.el --- My Location DB             -*- lexical-binding: t; -*-

;; Copyright (C) 2021  AKIYAMA Kouhei

;; Author: AKIYAMA Kouhei <misohena@gmail.com>
;; Keywords: 

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

;; Find out where you were at the specified time.

;;; Code:

(require 'dom)
(require 'cl-lib)
(require 'subr-x)

(defgroup my-location nil
  "Find out where you were at the specified time"
  :prefix "my-location-")

;;;; Math

(defun my-location-latlng-to-vec3 (ll)
  (let* ((lat (degrees-to-radians (car ll)))
         (lng (degrees-to-radians (cdr ll)))
         (cos-lat (cos lat)))
    (vector (* (cos lng) cos-lat) ;;x=lng0
            (* (sin lng) cos-lat) ;;y=lng90E
            (sin lat)))) ;;z=North

(defun my-location-vec3-to-latlng (v)
  (cons
   (radians-to-degrees
    (atan (aref v 2)
          (sqrt (+ (* (aref v 0) (aref v 0))
                   (* (aref v 1) (aref v 1))))))
   (radians-to-degrees
    (atan (aref v 1)
          (aref v 0)))))

(defun my-location-vec3-cross (a b)
  (vector (- (* (aref a 1) (aref b 2)) (* (aref a 2) (aref b 1)))
          (- (* (aref a 2) (aref b 0)) (* (aref a 0) (aref b 2)))
          (- (* (aref a 0) (aref b 1)) (* (aref a 1) (aref b 0)))))

(defun my-location-vec3-dot (a b)
  (+ (* (aref a 0) (aref b 0))
     (* (aref a 1) (aref b 1))
     (* (aref a 2) (aref b 2))))

(defun my-location-vec3-length-sq (v)
  (let ((x (aref v 0))
        (y (aref v 1))
        (z (aref v 2)))
    (+ (* x x) (* y y) (* z z))))

(defun my-location-vec3-length (v)
  (sqrt (my-location-vec3-length-sq v)))

(defun my-location-vec3-angle (a b)
  (atan (my-location-vec3-length (my-location-vec3-cross a b))
        (my-location-vec3-dot a b)))

(defun my-location-vec3-mul-scalar (v s)
  (vector (* (aref v 0) s)
          (* (aref v 1) s)
          (* (aref v 2) s)))

(defun my-location-vec3-add (v1 v2)
  (vector (+ (aref v1 0) (aref v2 0))
          (+ (aref v1 1) (aref v2 1))
          (+ (aref v1 2) (aref v2 2))))

(defun my-location-vec3-normalize (v)
  (my-location-vec3-mul-scalar
   v
   (/ 1.0 (my-location-vec3-length v))))

(defun my-location-vec3-rotate-z-90 (v)
  (vector (- (aref v 1))
          (aref v 0)
          (aref v 2)))

(defun my-location-vec3-slerp (v1 v2 T)
  (let* ((angle (* T (my-location-vec3-angle v1 v2)))
         (axis (my-location-vec3-cross v1 v2))
         (err 10e-16)
         (axis (if (< (my-location-vec3-length-sq axis) (* err err))
                   (my-location-vec3-rotate-z-90 v1)
                 axis))
         (dir (my-location-vec3-normalize (my-location-vec3-cross axis v1))))
    (my-location-vec3-add
     (my-location-vec3-mul-scalar v1 (cos angle))
     (my-location-vec3-mul-scalar dir (sin angle)))))

(defun my-location-interpolate (ll1 ll2 T)
  (my-location-vec3-to-latlng
   (my-location-vec3-slerp
    (my-location-latlng-to-vec3 ll1)
    (my-location-latlng-to-vec3 ll2)
    T)))

;;;; Date/Time

(defun my-location-string-to-number (str)
  (when str
    (string-to-number str)))

(defun my-location-string-to-time (str &optional curr-time)
  (unless (string-match "\\(?:\\(?:\\(?:\\([0-9]+\\)[/-]\\)?\\([0-9]+\\)[/-]\\)?\\([0-9]+\\) +\\)?\\([0-9]+\\):\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?" str)
    (error "Invalid date/time format"))
  (let* ((curr-time (or curr-time (decode-time (current-time))))
         (curr-second (decoded-time-second curr-time))
         (curr-minute (decoded-time-minute curr-time))
         (curr-hour (decoded-time-hour curr-time))
         (curr-day (decoded-time-day curr-time))
         (curr-month (decoded-time-month curr-time))
         (curr-year (decoded-time-year curr-time))
         (input-second (or (my-location-string-to-number (match-string 6 str))
                           0))
         (input-minute (my-location-string-to-number (match-string 5 str)))
         (input-hour (my-location-string-to-number (match-string 4 str)))
         (input-day (or (my-location-string-to-number (match-string 3 str))
                        (if (<= (+ (* 3600 input-hour)
                                   (* 60 input-minute)
                                   input-second)
                                (+ (* 3600 curr-hour)
                                   (* 60 curr-minute)
                                   curr-second))
                            curr-day
                          (1- curr-day))))
         (input-month (or (my-location-string-to-number (match-string 2 str))
                          (if (<= input-day curr-day)
                              curr-month
                            (1- curr-month))))
         (input-year (or (my-location-string-to-number (match-string 1 str))
                         (if (or (and (= input-month curr-month)
                                      (<= input-day curr-day))
                                 (< input-month curr-month))
                             curr-year
                           (1- curr-year)))))
    (encode-time
     (make-decoded-time :second input-second
                        :minute input-minute
                        :hour input-hour
                        :day input-day
                        :month input-month
                        :year input-year))))

(defun my-location-guess-time-from-file-name (file)
  (when (and (stringp file)
             (string-match "\\(20[0-9][0-9]\\|19[0-9][0-9]\\)-?\\(0[1-9]\\|1[0-2]\\)-?\\([0-3][0-9]\\)[ _]?\\([01][0-9]\\|2[0-3]\\)\\([0-5][0-9]\\)\\([0-5][0-9]\\)?" file))
    (encode-time
     (make-decoded-time
      :year (string-to-number (match-string 1 file))
      :month (string-to-number (match-string 2 file))
      :day (string-to-number (match-string 3 file))
      :hour (string-to-number (match-string 4 file))
      :minute (string-to-number (match-string 5 file))
      :second (string-to-number (or (match-string 6 file) "0"))))))

(defun my-location-read-date-time (prompt)
  (let* ((str (read-string prompt))
         (time (or (ignore-errors (my-location-string-to-time str))
                   (my-location-guess-time-from-file-name str))))
    (unless time
      (error "Invalid date/time format"))
    time))

;;;; Track Point

(cl-defstruct (my-location-point
               (:constructor my-location-point-create (lat lng time)))
  lat
  lng
  time)

(defun my-location-point-latlng (point)
  (when point
    (cons
     (my-location-point-lat point)
     (my-location-point-lng point))))

;;;; Track Segment

(cl-defstruct (my-location-segment
               (:constructor my-location-segment-create (points track)))
  points
  track)

(defun my-location-segment-min-time (segment)
  "SEGMENTに記録されている点の最も早い時刻を返します。"
  (when segment
    (my-location-point-time (car (my-location-segment-points segment)))))

(defun my-location-segment-max-time (segment)
  "SEGMENTに記録されている点の最も遅い時刻を返します。"
  (when segment
    (my-location-point-time (car (last (my-location-segment-points segment))))))

(defun my-location-segment-find-point-by-time (segment time)
  "SEGMENTに記録されている点のリストのTIMEで指定されている時刻以降を返します。

返すリストの要素が一つだけの場合、SEGMENTが持つ一番最後の点とTIMEの時刻は一致しています。

返すリストの要素が二つ以上の場合、先頭と二番目の要素の間に時刻TIMEがあります。二つ目の要素がある場合、二つ目の時刻は必ずTIMEより大きくなります(等しいということはありません)。一つ目の要素はTIMEと同じか小さい時刻になります。

TIMEがSEGMENTが持つ範囲の外である場合、nilを返します。
"
  (let ((points (my-location-segment-points segment)))

    (unless (time-less-p time (my-location-point-time (car points)))
      (while (and (cdr points)
                  (not
                   (time-less-p time (my-location-point-time (cadr points)))))
        (setq points (cdr points)))
      (if (or (cdr points)
              (time-equal-p (my-location-point-time (car points)) time))
          points))))

(defun my-location-segment-latlng-at-time (segment time)
  "SEGMENTに記録されている情報を使って、TIMEで指定した時刻の位置を推定し緯度経度で返します。

記録されていない時刻の場合、前後の記録から補間して求めます。

TIMEがSEGMENTが持つ範囲の外である場合、nilを返します。"
  (let ((points (my-location-segment-find-point-by-time segment time)))
    (cond
     ((null points)
      nil)
     ((null (cdr points))
      (my-location-point-latlng (car points)))
     (t
      (let* ((p0 (car points))
             (p1 (cadr points))
             (time0 (my-location-point-time p0))
             (time1 (my-location-point-time p1))
             (dt01 (float-time (time-subtract time1 time0)))
             (dt0t (float-time (time-subtract time time0))))
        (if (= dt01 0)
            (my-location-point-latlng p1)
          (my-location-interpolate
           (my-location-point-latlng p0)
           (my-location-point-latlng p1)
           (/ dt0t dt01))))))))

;;;; Track

(cl-defstruct (my-location-track
               (:constructor my-location-track-create (name)))
  name
  segments)

;;;; Global Location Data

(defun my-location-clear ()
  (interactive)
  (setq my-location-loaded-files nil)
  (setq my-location-tracks nil)
  (setq my-location-segments nil))

;;;;; Global Segment List

(defvar my-location-segments nil) ;; a list of segment

(defun my-location-segment-on-time (time-lower &optional time-upper)
  (unless time-upper
    (setq time-upper time-lower))
  (seq-find
   (lambda (segment)
     (and
      (not (time-less-p time-upper (my-location-segment-min-time segment)))
      (not (time-less-p (my-location-segment-max-time segment) time-lower))))
   my-location-segments))

(defun my-location-add-segment (segment)
  ;; @todo Prohibit duplication of ranges?
  ;; (unless (my-location-segment-on-time
  ;;          (my-location-segment-min-time segment)
  ;;          (my-location-segment-max-time segment)) )
  (push segment my-location-segments))

(defun my-location-points-on-time (time &optional no-auto-load-p)
  (unless no-auto-load-p
    (my-location-load-files-on-time time))
  (when-let ((segment (my-location-segment-on-time time)))
    (my-location-segment-find-point-by-time segment time)))

(defun my-location-latlng-at-time (time &optional no-auto-load-p)
  (unless no-auto-load-p
    (my-location-load-files-on-time time))
  (when-let ((segment (my-location-segment-on-time time)))
    (my-location-segment-latlng-at-time segment time)))

;;;;; Global Track List

(defvar my-location-tracks nil) ;;a list of track

(defun my-location-add-track (track)
  (push track my-location-tracks)
  (mapc
   #'my-location-add-segment
   (my-location-track-segments track)))

(defun my-location-add-tracks (tracks)
  (mapc
   #'my-location-add-track
   tracks))

;;;;; Global File Management

(defvar my-location-loaded-files nil)

(defcustom my-location-sources
  '((:dir "~/my-location/%Y%m" :file-pattern "\\`%Y%m%d.*\\.gpx"))
  "gpx file locations."
  :group 'my-location
  :type '(repeat
          (list
           (const :format "" :dir)
           (string :format "Directory: %v" "~/my-location/%Y%m")
           (const :format "" :file-pattern)
           (string :format "File Regexp: %v" "\\`%Y%m%d.*\\.gpx"))))

(defun my-location-source-files-on-time (time)
  (apply
   #'nconc
   (mapcar
    (lambda (source)
      (when-let ((dir-format (plist-get source :dir))
                 (file-pattern-format (plist-get source :file-pattern)))
        (let ((dir (format-time-string dir-format time))
              (file-pattern (format-time-string file-pattern-format time)))
          (when (and (file-exists-p dir)
                     (directory-name-p dir))
            (directory-files dir t file-pattern)))))
    my-location-sources)))

(defun my-location-load-files-on-time (time)
  (mapc #'my-location-load-file (my-location-source-files-on-time time)))

(defun my-location-load-file (file)
  (let ((abs-file (expand-file-name file)))
    (unless (assoc abs-file my-location-loaded-files #'string=)
      (message "loading %s..." abs-file)
      (let ((tracks (my-location-load-gpx abs-file)))
        (push (cons abs-file tracks) my-location-loaded-files)))))

(defun my-location-unload-file (file)
  (let* ((abs-file (expand-file-name file))
         (file-tracks (assoc abs-file my-location-loaded-files #'string=)))
    (when file-tracks
      ;; Remove track & segments
      ;;@todo

      ;; Remove the entry
      (setf
       (alist-get abs-file my-location-loaded-files nil 'remove #'string=)
       nil))))

;;;; GPX File

;; References:
;; - https://www.topografix.com/gpx.asp

(defun my-location-load-gpx (file)
  "GPXファイルを読み取ります。読み取ったデータはグローバルなトラックリストやセグメントリストに格納して問い合わせに備えます。"
  (let* ((gpx (with-temp-buffer
                (insert-file-contents file)
                (libxml-parse-xml-region (point-min) (point-max))))
         (tracks
          (cl-loop
           for trk in (dom-children gpx)
           when (eq (dom-tag trk) 'trk)
           collect
           (let* ((track (my-location-track-create
                          (dom-text (car (dom-by-tag trk 'name)))))
                  (segments (cl-loop
                             for trkseg in (dom-children trk)
                             when (eq (dom-tag trkseg) 'trkseg)
                             collect
                             (my-location-segment-create
                              (cl-loop
                               for trkpt in (dom-children trkseg)
                               when (eq (dom-tag trkpt) 'trkpt)
                               collect
                               (when-let ((lat-str (dom-attr trkpt 'lat))
                                          (lng-str (dom-attr trkpt 'lon))
                                          (time-elem (car (dom-by-tag trkpt 'time))))
                                 (my-location-point-create
                                  (string-to-number lat-str)
                                  (string-to-number lng-str)
                                  (parse-iso8601-time-string
                                   (dom-text time-elem)))))
                              track))))
             (setf (my-location-track-segments track) segments)
             track))))
    (my-location-add-tracks tracks)
    tracks))


;;;; Map

(defun my-location-expand-template (template alist)
  (let ((regexp "{{{\\([^:}]+\\)\\(?::\\([^:}]+\\)\\)?}}}"))
    (replace-regexp-in-string
     regexp
     (lambda (str)
       (save-match-data
         (if (string-match regexp str)
             (let ((key (match-string 1 str))
                   (fmt (or (match-string 2 str) "%s")))
               (format fmt (alist-get (intern key) alist "")))
           "")))
     template
     t)))

(defcustom my-location-map-url
  "https://www.openstreetmap.org/#map=17/{{{lat:%.6f}}}/{{{lng:%.6f}}}"
  "Map Service URL."
  :group 'my-location
  :type '(choice
          (const nil)
          (const :tag "Open Street Maps" "https://www.openstreetmap.org/#map=17/{{{lat:%.6f}}}/{{{lng:%.6f}}}")
          (const :tag "Apple Maps" "http://maps.apple.com/?ll={{{lat:%.6f}}},{{{lng:%.6f}}}&z=17")
          (const :tag "Google Maps" "https://www.google.com/maps?ll={{{lat:%.6f}}},{{{lng:%.6f}}}&z=17")
          (const :tag "Japan GSI" "https://maps.gsi.go.jp/#17/{{{lat:%.6f}}}/{{{lng:%.6f}}}/")
          string))

(defun my-location-browse-map (ll)
  (message "url=%s" (my-location-expand-template
               my-location-map-url
               (list (cons 'lat (car ll))
                     (cons 'lng (cdr ll)))))
  (browse-url (my-location-expand-template
               my-location-map-url
               (list (cons 'lat (car ll))
                     (cons 'lng (cdr ll))))))

(defun my-location-at-time (time &optional arg)
  "TIMEで指定した時刻の位置を表示します。

ARGを指定した場合結果をバッファに挿入します。指定しなかった場合マップを開きます。
"
  (interactive
   (list
    (my-location-read-date-time
     "Past date and time ([[[YYYY-]MM-]DD ]HH:MM[:SS]): ")
    current-prefix-arg))
  (let ((ll (my-location-latlng-at-time time)))
    (if ll
        (let ((str (format "%.6f,%.6f" (car ll) (cdr ll))))
          (if arg
              (insert str)
            (when my-location-map-url
              (my-location-browse-map ll)))
          (message "%s" str)
          ll)
      (message "No data")
      nil)))


(provide 'my-location)
;;; my-location.el ends here
