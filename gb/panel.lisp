;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-
;;;
;;; Copyright (C) 2020, 2021, 2022, 2024  Anthony Green <green@moxielogic.com>
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

(defun getenv (var)
  (let ((val (uiop:getenv var)))
    (when (null val)
      (error "Environment variable ~A is not set." var))
    val))

(defclass panel ()
  ((acceptor :initarg :acceptor :reader acceptor)))
