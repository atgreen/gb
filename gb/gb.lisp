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

;; Top level for gb

(markup:enable-reader)

(in-package :gb)

;; ----------------------------------------------------------------------------
;; Get the version number at compile time.  This comes from
;; APP_VERSION (set on the linux container build commandline), or from
;; git at compile-time.  Use UNKNOWN if all else fails.

;; This can come from build time...
(eval-when (:compile-toplevel :execute :load-toplevel)
  (defparameter +gb-git-version+
    (inferior-shell:run/ss
     "git describe --tags --dirty=+ || git rev-parse --short HEAD || echo UNKNOWN")))

;; But this must come from runtime...
(defparameter +gb-version+
  (let ((v +gb-git-version+))
    (if (equal v "UNKNOWN")
 	(or (uiop:getenv "APP_VERSION") v)
 	v)))

;; ----------------------------------------------------------------------------
;; Webhook credentials.  Set them as environment variables.

(defvar *webhook-username* nil)
(defvar *webhook-password* nil)

;; Hostname for our ActiveMQ broker.  We normally run in kubernetes,
;; so this is just the service name.
(defparameter +amq-host+ "localhost")

;; Remember the last matching event.  This is used by new connections.
(defvar *timestring* 0)

;; Create some panels
(defvar *alerts-panel* nil)
(defvar *org-agenda-panel* nil)

;; Web server
(defvar *hunchentoot-server* nil)

(defun getenv (var)
  "Getenv with an error if the variable is not defined."
  (let ((val (uiop:getenv var)))
    (if (null val)
	"America/Toronto"
	val)))

;; ----------------------------------------------------------------------------
;; Find the directory in which we are installed.  This is used to
;; serve up static content.

(defun gb-root ()
  (fad:pathname-as-directory
   (make-pathname :name nil
                  :type nil
                  :defaults #.(or *compile-file-truename* *load-truename*))))

(defun-push push-next-meeting (datestring) (+ajax-pusher+)
  "Parenscript code we call from the server when we have an agenda
   update.  This is injected into the browser as javascript."
  ((@ console log) "push-next-meeting")
  (setf *deadline* (if (equal 0 datestring)
		       nil
		       (ps:chain -Date (parse datestring)))))

;; ----------------------------------------------------------------------------
;; Machinery for managing the execution of the server.

(defvar *shutdown-cv* (bt:make-condition-variable))
(defvar *server-lock* (bt:make-lock))

;; ----------------------------------------------------------------------------
;; Default configuration.  Overridden by external config file.
;; Config files are required to be in TOML format.

(defvar *config* nil)
(defvar *default-config* nil)
(defparameter +default-config-text+
"server-uri = \"http://localhost:8080\"
security-token = \"test\"
")

;; ----------------------------------------------------------------------------
;; The URI of the server.  Define this in your config.ini files.  Use
;; this is you are generating responses that point back to this
;; application.

(defvar *server-uri* nil)

;; ----------------------------------------------------------------------------
;; The security token for accessing the board

(defvar *security-token* nil)

;; ----------------------------------------------------------------------------
;; Initialize prometheus values.

(defparameter *http-requests-counter* nil)
(defparameter *http-request-duration* nil)

(defun initialize-metrics ()
  (unless *gb-registry*
    (setf *gb-registry* (prom:make-registry))
    (let ((prom:*default-registry* *gb-registry*))
      (setf *http-requests-counter*
            (prom:make-counter :name "http_requests_total"
                               :help "Counts http request by type"
                               :labels '("method" "app")))
      (setf *http-request-duration*
	    (prom:make-histogram :name "http_request_duration_milliseconds"
                                 :help "HTTP requests duration[ms]"
                                 :labels '("method" "app")
                                 :buckets '(10 25 50 75 100 250 500 750 1000 1500 2000 3000)))
      #+sbcl
      (prom.sbcl:make-memory-collector)
      #+sbcl
      (prom.sbcl:make-threads-collector)
      (prom.process:make-process-collector))))

;; ----------------------------------------------------------------------------
;; API routes

(defparameter *gb-registry* nil)

;; Readiness probe.  Always ready by default, but this can be as
;; complex as required.
(easy-routes:defroute health ("/health") ()
  "ready")

;; ----------------------------------------------------------------------------
;; HTTP server control

(defparameter *handler* nil)

(defparameter +gb-dispatch-table+
  (list
   (hunchentoot:create-folder-dispatcher-and-handler
    "/images/" (fad:pathname-as-directory
                (make-pathname :name "static/images"
                               :defaults (gb-root))))
   (hunchentoot:create-folder-dispatcher-and-handler
    "/js/" (fad:pathname-as-directory
            (make-pathname :name "static/js"
                           :defaults (gb-root))))
   (hunchentoot:create-folder-dispatcher-and-handler
    "/css/" (fad:pathname-as-directory
             (make-pathname :name "static/css"
                            :defaults (gb-root))))
   (create-ajax-dispatcher +ajax-pusher+)))

(defclass exposer-acceptor (prom.tbnl:exposer hunchentoot:acceptor)
  ())

(defmacro with-http-authentication (&rest body)
  `(multiple-value-bind (username password) (hunchentoot:authorization)
     (format t "Expecting: ~A:~A~%" *webhook-username* *webhook-password*)
     (format t "Received : ~A:~A~%" username password)
     (cond ((and (string= username *webhook-username*) (string= password *webhook-password*))
            ,@body)
           (t (hunchentoot:require-authorization "greenboard-webhook")))))

(defclass application (easy-routes:easy-routes-acceptor)
  ((exposer :initarg :exposer :reader application-metrics-exposer)
   (mute-access-logs :initform t :initarg :mute-access-logs :reader mute-access-logs)
   (mute-messages-logs :initform t :initarg :mute-error-logs :reader mute-messages-logs)))

(defmacro stop-server (&key (handler '*handler*))
  "Shutdown the HTTP handler"
  `(hunchentoot:stop ,handler))

(defun start-server (&optional (config-ini "/etc/gb/config.ini"))

  (log:info (fad:pathname-as-directory
             (make-pathname :name "static/css"
                            :defaults (gb-root))))

  (setf *webhook-username* (getenv "WEBHOOK_USERNAME"))
  (setf *webhook-password* (getenv "WEBHOOK_PASSWORD"))

  ;; Set the default timezone based on ${TZ} (eg. America/Toronto)
  (local-time:reread-timezone-repository)

  (setf local-time:*default-timezone* (local-time:find-timezone-by-location-name (getenv "TZ"))
        hunchentoot:*catch-errors-p* t
	hunchentoot:*show-lisp-errors-p* t
	hunchentoot:*show-lisp-backtraces-p* t)

  (log:info "Starting gb version ~A" +gb-version+)

  (setf *dispatch-table* +gb-dispatch-table+)
  (setf prom:*default-registry* *gb-registry*)

  (let ((hunchentoot-server (let ((exposer (make-instance 'exposer-acceptor :registry *gb-registry* :port 9101)))
                              (hunchentoot:start (make-instance 'application
                                                                :document-root #p"./"
                                                                :port 8080
                                                                :exposer exposer)))))
    (setf *hunchentoot-server* hunchentoot-server)

    (log:info "...started")

    (reset-session-secret)

    (log:info "...session secret reset")

    (setf *alerts-panel* (make-instance 'alerts-panel :acceptor hunchentoot-server))
    (log:info *alerts-panel*)
    (setf *org-agenda-panel* (make-instance 'org-agenda-panel :acceptor hunchentoot-server :hunchentoot-server hunchentoot-server))
    (log:info *org-agenda-panel*)

    (log:info "All Set!")

    ;; Read the built-in configuration settings.
    (setf *default-config* (cl-toml:parse +default-config-text+))

    ;; Set the default timezone based on ${TZ} (eg. America/Toronto)
    (local-time:reread-timezone-repository)

    ;; Read the user configuration settings.
    (setf *config*
  	  (if (fad:file-exists-p config-ini)
	      (cl-toml:parse
	       (alexandria:read-file-into-string config-ini
					         :external-format :latin-1))
	      (make-hash-table)))

    (log:info "config.ini at ~A" config-ini)
    ;; dump the contents of *config*
    (maphash (lambda (key value)
         (log:info "~A = ~A" key value))
       *config*)

    (flet ((get-config-value (key)
	     (let ((value (or (gethash key *config*)
			      (gethash key *default-config*)
			      (error "config does not contain key '~A'" key))))
	       ;; Some of the users of these values are very strict
	       ;; when it comes to string types... I'm looking at you,
	       ;; SB-BSD-SOCKETS:GET-HOST-BY-NAME.
	       (if (subtypep (type-of value) 'vector)
		   (coerce value 'simple-string)
		   value))))

      ;; Extract any config.ini settings here.
      (setf *server-uri* (get-config-value "server-uri"))
      (setf *security-token* (get-config-value "security-token"))

      (log:info *security-token*)

      ;; Initialize prometheus
      (initialize-metrics)

      (log:info "Starting server")

      ;; This is a blocking call. To stop the server,
      ;; call (stomp:stop *stomp-server*).
      (loop
        do (sleep 3000))

      (hunchentoot:stop hunchentoot-server))))

(defmethod hunchentoot:start ((app application))
  (hunchentoot:start (application-metrics-exposer app))
  (call-next-method))

(defmethod hunchentoot:stop ((app application) &key soft)
  (call-next-method)
  (hunchentoot:stop (application-metrics-exposer app) :soft soft))

(defmethod hunchentoot:acceptor-dispatch-request ((app application) request)
  (let ((labels (list (string-downcase (string (hunchentoot:request-method request)))
		      "gb_app")))
    (log:info *http-requests-counter*)
    (prom:counter.inc *http-requests-counter* :labels labels)
    (prom:histogram.time
     (prom:get-metric *http-request-duration* labels)
     (call-next-method))))

(defun countdown-js ()
  "Parenscript code that is injected into the HTML page as javascript.
  This implements the countdown timer.  CSS changes based on 5min and
  2min warnings are implemented by changing the document body class,
  and providing CSS content keyed on that."
  (ps
    (defparameter +two-minutes+ (* 1000 60 2))
    (defparameter +five-minutes+ (* 1000 60 5))
    (defvar *deadline* (ps:chain -Date (parse (ps:lisp *timestring*))))

    (defun get-time-remaining ()
      (let* ((total (- *deadline* (ps:chain -Date (parse (ps:new (-Date))))))
	     (seconds (floor (mod (/ total 1000) 60)))
	     (minutes (floor (mod (/ (/ total 1000) 60) 60)))
	     (hours (floor (/ total (* 1000 60 60)))))
	(values total seconds minutes hours)))

    (defun initialize-clock (id endtime)
      (let* ((clock ((@ document get-element-by-id) id))
	     (hour-span ((@ clock query-selector) ".hours"))
	     (minute-span ((@ clock query-selector) ".minutes"))
	     (second-span ((@ clock query-selector) ".seconds"))
             (countdown-dashboard-piece ((@ document get-element-by-id) "gbpanel-countdown")))
	(flet ((update-clock ()
		 (if *deadline*
		     (multiple-value-bind (total seconds minutes hours)
			 (get-time-remaining)
	       (if (< total +two-minutes+)
			   (setf (@ countdown-dashboard-piece class-name) "WithinTwoMinutes")
			   (if (< total +five-minutes+)
			       (setf (@ countdown-dashboard-piece class-name) "WithinFiveMinutes")
			       (setf (@ countdown-dashboard-piece class-name) "")))
		       (setf (inner-html hour-span) ((@ (+ "0" hours) slice) -2))
		       (setf (inner-html minute-span) ((@ (+ "0" minutes) slice) -2))
		       (setf (inner-html second-span) ((@ (+ "0" seconds) slice) -2)))
		     (dolist (span (list hour-span minute-span second-span))
		       (setf (inner-html span) "--")))))
	  (update-clock)
	  (set-interval update-clock 1000))))

    (initialize-clock "clockdiv" *deadline*)))

(easy-routes:defroute index ("/") (token)
  (log:info *alerts-panel*)
  (log:info *org-agenda-panel*)

  (if (not (string= token *security-token*))
      "AUTH ERROR"
      (markup:write-html
       <html>

         <head>
           <meta charset="UTF-8" />
           <title>greenboard</title>
           <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/bulma@0.9.1/css/bulma.min.css" />
           <meta name="viewport" content="width=device-width, initial-scale=1" />
           <link rel="stylesheet" href="css/gb.css" />
           <script type="text/javascript"> ,@(list (generate-prologue +ajax-pusher+ :wrapper nil)) </script>
           <script language="JavaScript" type="text/javascript" src="https://code.jquery.com/jquery-3.5.1.min.js" />
           <script language="JavaScript" type="text/javascript" src="js/fix-height.js" />
         </head>

         <body onload="javascript:smackpusher.startPoll()">
           <div class="tile is-ancestor is-vertical">
             <div class="tile is-12 gbpanel gbpanel-greenbg">
               <div id="gbpanel-countdown">
                 <div class="title mb-0 mt-4">
                   Next Meeting
                 </div>
                 <div id="clockdiv">
                   <div>
                     <span class="hours"></span>
                     <div class="smalltext">
                       Hours
                     </div>
                   </div>
                   <div>
                     <span class="minutes"></span>
                     <div class="smalltext">
                       Minutes
                     </div>
                   </div>
                   <div>
                     <span class="seconds"></span>
                     <div class="smalltext">
                       Seconds
                     </div>
                   </div>
                 </div>
               </div>
             </div>

             <div class="tile is-parent is-12">
               <div class="tile is-child is-4"
                    style="min-height: 458.083px;">
                 <div class="panel-alerts gbpanel gbpanel-graybg">
                   <div class="title mb-0 mt-4">Alerts</div>
                   <table class="table center"
                          id="alerts-table"></table>
                 </div>
               </div>
               <div class="tile is-child is-8" style="min-height: 458.083px;">
                 <div class="panel-org-agenda gbpanel-bluebg" id="org-agenda-panel">
                   <div class="mb-0 mt-4" id="org-agenda">Agenda!</div>
                 </div>
               </div>
             </div>
             <footer class="footer">
               <p/><span class="footer__copyright">copyright</span> 2021 Anthony Green
             </footer>
           </div>
         </body>
         <script type="text/javascript">
           ,@(list
              (countdown-js)
              (inject-js *org-agenda-panel*)
              (inject-js *alerts-panel*))
         </script>
       </html>
       )
      )
  )

(easy-routes:defroute org-agenda ("/org-agenda" :method :post) ()
  (with-http-authentication
      (setf (hunchentoot:content-type*) "text/plain")
    (log:info (hunchentoot:post-parameters*))
    (let ((data (hunchentoot:raw-post-data :request hunchentoot:*request*)))
      (log:info "/org-agenda")
      (log:info data)
      (when data
        (let ((atext (concatenate 'string "<pre>" (cl-base64:base64-string-to-string (sb-ext:octets-to-string data)) "</pre>")))
          (format t "About to push-org-agenda: ~A~%" atext)
          (let ((hunchentoot:*acceptor* *hunchentoot-server*))
            (push-org-agenda atext))
          (setf (slot-value *org-agenda-panel* 'agenda-text) atext))))))

(easy-routes:defroute alert ("/alert" :method :post) ()
  (log:info "/alert")
  (setf (hunchentoot:content-type*) "text/plain")
  (process *alerts-panel* (hunchentoot:raw-post-data :request hunchentoot:*request*)))

(easy-routes:defroute tower-alert ("/tower-alert" :method :post) ()
  (log:info "/tower-alert")
  (log:info (json:decode-json-from-string (sb-ext:octets-to-string (hunchentoot:raw-post-data :request hunchentoot:*request*))))
  (setf (hunchentoot:content-type*) "text/plain")
  (let* ((json (json:decode-json-from-string (sb-ext:octets-to-string (hunchentoot:raw-post-data :request hunchentoot:*request*))))
         (url (cdr (assoc :URL json)))
         (status (cdr (assoc :STATUS json)))
         (msg (or (cdr (assoc :BODY json))
                  (cdr (assoc :NAME json))))
         (fmsg (if url
                   (format nil "<a href=~S target=\"_blank\">~A</a>" url msg)
                   msg))
         (sfmsg (if status
                    (format nil "~A: ~A" status fmsg)
                    fmsg)))
    (process *alerts-panel* (sb-ext:string-to-octets (cl-base64:string-to-base64-string sfmsg)))))

(easy-routes:defroute gcal-agenda ("/gcal-agenda" :method :post) ()
  (with-http-authentication
      (setf (hunchentoot:content-type*) "text/plain")
    (print (hunchentoot:post-parameters*))
    (let ((data (hunchentoot:raw-post-data :request hunchentoot:*request*)))
      (log:info "/gcal-agenda")
      (when data
        (let ((in (make-string-input-stream (cl-base64:base64-string-to-string (sb-ext:octets-to-string data))))
	      (now (local-time:now)))
          (loop for line = (read-line in nil)
	        while line
	        until (let* ((data (ppcre:split #\tab line))
		             (timestring-gmt (format nil "~AT~A:00.000000" (car data) (cadr data)))
		             (timestamp-gmt (local-time:parse-timestring timestring-gmt))
		             (timestring (format nil "~A~A"
					         timestring-gmt
					         (local-time:format-timestring nil timestamp-gmt
									       :format '(:gmt-offset))))
		             (timestamp (local-time:parse-timestring timestring)))
		        (if (local-time:timestamp>= timestamp now)
		            (let ((hunchentoot:*acceptor* *hunchentoot-server*))
			      (push-next-meeting (setf *timestring* timestring))
			      t)
		            nil))
	        finally (unless line
		          ;; There's no next meeting.
		          (let ((hunchentoot:*acceptor* *hunchentoot-server*))
		            (push-next-meeting (setf *timestring* 0))))))))))
