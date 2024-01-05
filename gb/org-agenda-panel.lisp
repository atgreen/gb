;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-
;;;
;;; Copyright (C) 2021, 2022  Anthony Green <green@moxielogic.com>
;;;
;;; gb is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3, or (at your
;;; option) any later version.
;;;
;;; gb is distributed in the hope that it will be useful, but WITHOUT
;;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;;; License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with gb; see the file COPYING3.  If not see
;;; <http://www.gnu.org/licenses/>.

(in-package :gb)

(defparameter +org-agenda+ "/topic/org-agenda")

(defclass org-agenda-panel (panel)
  ((hunchentoot-server :initarg :hunchentoot-server :initform (error "Missing HUNCHENTOOT-SERVER arg"))
   (agenda-text :initform "<pre>Waiting for update...</pre>")))

(defun-push push-org-agenda (org-agenda-html) (+ajax-pusher+)
  ((@ console log) "push-org-agenda")
  (setf *org-agenda-html* org-agenda-html))

(defmethod render ((ap org-agenda-panel))
  (log:info "render")
  (push-org-agenda (slot-value ap 'agenda-text))
  (spinneret:with-html-string
      (:div :class "panel-org-agenda gbpanel-bluebg" :id "org-agenda-panel"
            (:div :class "mb-0 mt-4" :id "org-agenda" "Agenda!"))))

(defmethod inject-js ((ap org-agenda-panel))
  (log:info "inject-js")
  (ps:ps
    (defvar *org-agenda-html* (lisp (slot-value ap 'agenda-text)))
    (defun update-org-agenda-panel ()
      (let ((org-agenda ((@ document get-element-by-id) "org-agenda-panel")))
        (flet ((update-org-agenda ()
                 ((@ console log) "update-org-agenda")
                 (setf (inner-html org-agenda)
                       *org-agenda-html*)
                 ((@ ($ window) trigger) "resize")))
	  (update-org-agenda)
	  (set-interval update-org-agenda 5000))))
    (update-org-agenda-panel)))
