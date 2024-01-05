;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: GREENBOARD; Base: 10 -*-
;;;
;;; Copyright (C) 2021  Anthony Green <green@moxielogic.com>
;;;
;;; greenboard is free software; you can redistribute it
;;; and/or modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 3, or
;;; (at your option) any later version.
;;;
;;; greenboard is distributed in the hope that it will be
;;; useful, but WITHOUT ANY WARRANTY; without even the implied
;;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;;; See the GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with greenboard; see the file COPYING3.  If not
;;; see <http://www.gnu.org/licenses/>.

(in-package :gb)

;; The SmackJack package enables AJAX-like calls from this server into
;; parenscript code running in the browser.  SmackJack polls our
;; server at this URI.

(defparameter +ajax-pusher+
  (make-instance 'smackjack:ajax-pusher :server-uri "/ajax-push"))
