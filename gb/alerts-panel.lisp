;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-
;;;
;;; Copyright (C) 2020, 2021, 2022, 2024  Anthony Green <green@moxielogic.com>
;;;
;;; gb is free software; you can redistribute it
;;; and/or modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 3, or
;;; (at your option) any later version.
;;;
;;; gb is distributed in the hope that it will be
;;; useful, but WITHOUT ANY WARRANTY; without even the implied
;;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;;; See the GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with gb; see the file COPYING3.  If not
;;; see <http://www.gnu.org/licenses/>.

(in-package :gb)

(defclass alerts-panel (panel)
  ((db-connection :reader db-connection :initform nil)
   (content :initform "")
   (query :reader query)))

(defun connect-to-db (alerts-panel)
  (unless (slot-value alerts-panel 'db-connection)
    (log:info (getenv "TIMESCALEDB_HOST"))
    (log:info (getenv "TIMESCALEDB_PASSWORD"))
    (handler-case
        (progn
          (log:info "Trying to connect...")
          (setf (slot-value alerts-panel 'db-connection)
                (let* ((db-host (getenv "TIMESCALEDB_HOST"))
                       (dbc (dbi:connect-cached :postgres :database-name "gdash"
                                                :host db-host
                                                :port 5432
                                                :username "gdash" :password (getenv "TIMESCALEDB_PASSWORD"))))
                  (log:info "Connected to timescaledb at ~A:5432 (~A)" db-host dbc)
                  (dolist (cmd '("CREATE TABLE IF NOT EXISTS tower_notifications (name character(40), status character(10), url character(128), unixtimestamp integer);"
                                 "CREATE TABLE IF NOT EXISTS alerts (message character(128), unixtimestamp integer);"))
                    (dbi:do-sql dbc cmd))
                  (setf (slot-value alerts-panel 'query)
                        (dbi:prepare (db-connection alerts-panel)
                                     "select message, unixtimestamp from alerts order by unixtimestamp desc limit 10;"))
                  dbc)))
      (error (e)
             (log:error "Error connecting to database" e)
             nil))))

(defmethod initialize-instance :after ((alerts-panel alerts-panel) &key)
  (log:info "initialize-instance alerts-panel")
  (connect-to-db alerts-panel)
  (log:info "finished initialize-instance alerts-panel")
  alerts-panel)

(defmethod process ((alerts-panel alerts-panel) data)
  (log:info "PROCESS ~A" data)
  (when (connect-to-db alerts-panel)
    (let ((message (cl-base64:base64-string-to-string (sb-ext:octets-to-string data))))
      (dbi:do-sql (slot-value alerts-panel 'db-connection)
                  (format nil "insert into alerts(message, unixtimestamp) values ('~A', round(extract(epoch from now())));"
	                        (cl-ppcre:regex-replace-all "'" message "''"))))
    (pull-alerts-log)))

(defun-push push-alerts (alerts-html) (+ajax-pusher+)
  ((@ console log) "push-alerts")
  (setf *alerts-html* alerts-html))

(defmethod render ((jfp alerts-panel))
  (pull-alerts-log)
  (spinneret:with-html-string
      (:div :class "panel-alerts gbpanel gbpanel-graybg"
            (:div :class "title mb-0 mt-4" "Alerts")
            (:table :class "table center"
                    :id "alerts-table"))))

(defmethod inject-js ((jfp alerts-panel))
  (ps:ps
    (defvar *alerts-html* (lisp (slot-value jfp 'content)))
    (defun update-alerts-panel ()
      (let ((table ((@ document get-element-by-id) "alerts-table")))
        (flet ((update-alerts ()
                 ((@ console log) "update-alerts")
                 (setf (inner-html table)
                       (concatenate 'string
                                    "<thead><tr><th>Time</th><th>Message</th></tr></thead>"
                                    *alerts-html*))
                 ((@ ($ window) trigger) "resize")))
	        (update-alerts)
	        (set-interval update-alerts 5000))))
    (update-alerts-panel)))

(defun pull-alerts-log ()
  (when (connect-to-db alerts-panel)
    (let* ((result (dbi:execute (query *alerts-panel*)))
	         (fstr (make-array '(0) :element-type 'base-char
                             :fill-pointer 0 :adjustable t)))
      (log:info "pull-alerts-log!")
      (with-output-to-string (s fstr)
        (loop for row = (dbi:fetch result)
              while row
              do (destructuring-bind (j1 message j2 unixtimestamp)
                     row
                   (format s "<tr><td>~A</td><td>~A</td></tr>"
                           (local-time:format-timestring
                            nil
                            (local-time:unix-to-timestamp unixtimestamp)
                            :format local-time:+asctime-format+)
                           message)))
        (log:info fstr)
        (setf (slot-value *alerts-panel* 'content) fstr)
        (let ((hunchentoot:*acceptor* (acceptor *alerts-panel*)))
          (push-alerts fstr))))))
