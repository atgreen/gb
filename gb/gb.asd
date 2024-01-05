;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: GB; Base: 10 -*-
#|
 Copyright (C) 2021, 2022, 2024  Anthony Green <anthony@moxielogic.com>

 This program is free software: you can redistribute it and/or
 modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of
 the License, or (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Affero General Public License for more details.

 You should have received a copy of the GNU Affero General Public
 License along with this program.  If not, see
 <http://www.gnu.org/licenses/>.
|#

(asdf:defsystem #:gb
  :description "gb - a personal dashboard"
  :author "Anthony Green <anthony@moxielogic.com>"
  :version "0"
  :serial t
  :components ((:file "package")
               (:file "ajax")
               (:file "panel")
               (:file "alerts-panel")
               (:file "org-agenda-panel")
	       (:file "gb"))
  :depends-on (:cl-fad
               :cl-json
               :cl-toml
               :cl-dbi
               :smackjack
               :parenscript
               :cl-base64
               :postmodern
               :ironclad
               :uax-15
               :easy-routes
               :hunchentoot
               :spinneret
               :inferior-shell
               :log4cl
               :markup
               :prometheus
               :prometheus.collectors.process
               :prometheus.collectors.sbcl
               :prometheus.exposers.hunchentoot
               :prometheus.formats.text
               :str))
