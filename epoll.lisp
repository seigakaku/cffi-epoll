(in-package #:cffi-epoll)

;;; C functions

(defcfun (epoll-create "epoll_create") :fd
  (size :int))

(defcfun (epoll-create1 "epoll_create1") :fd
  (flags :int))

(defcfun (epoll-ctl "epoll_ctl") :int
  (epoll-fd :fd)
  (operation operation)
  (fd :fd)
  (event (:pointer (:struct epoll-event))))

(defcfun (epoll-wait "epoll_wait") :int
  (epoll-fd :fd)
  (events (:pointer (:struct epoll-event)))
  (max-events :int)
  (timeout :int))

(defcfun (epoll-pwait "epoll_pwait") :int
  (epoll-fd :fd)
  (events (:pointer (:struct epoll-event)))
  (max-events :int)
  (signal-mask sigset))

(defcfun (epoll-pwait2 "epoll_pwait2") :int
  (epoll-fd :fd)
  (events (:pointer (:struct epoll-event)))
  (max-events :int)
  (timeout (:pointer (:struct timespec)))
  (signal-mask sigset))

;;; Utilities

(defcfun (%strerror "strerror") :string
  (errno :int))

(defun strerror (&optional (errno errno))
  (%strerror (cffi:foreign-enum-value 'errno-values errno)))

(defun simple-perror (prefix &optional (errno errno))
  (error 'simple-error
         :format-control "~@<~A: ~2I~_~A~:>"
         :format-arguments (list prefix (strerror errno))))

;;; Low level wrappers

(deftype fd () `(signed-byte 32))
(deftype epoll () `fd)
(deftype operation () `(member ,@(foreign-enum-keyword-list 'operation)))
(deftype event () `(member ,@(foreign-enum-keyword-list 'event)))

(declaim (ftype (function (&optional boolean) epoll) make-epoll))
(defun make-epoll (&optional (close-on-exec t))
  (let ((epoll-fd (epoll-create1 (if close-on-exec cloexec 0))))
    (if (minusp epoll-fd)
        (simple-perror "epoll-create1")
        epoll-fd)))

(declaim (ftype (function (epoll operation fd &rest (or null event foreign-pointer)) *) control))
(defun control (epoll-fd operation fd &rest events)
  (let ((ret
          (cond
            ((pointerp (car events)) (epoll-ctl epoll-fd operation fd (car events)))
            ((and (not (eq operation :del)) events)
             (with-foreign-object (event '(:struct epoll-event))
               (let ((events (foreign-bitfield-value 'event events))
                     (data (foreign-slot-value event '(:struct epoll-event) 'data)))
                 (setf (foreign-slot-value event '(:struct epoll-event) 'events) events
                       (foreign-slot-value data '(:union epoll-data) 'fd) fd))
               (epoll-ctl epoll-fd operation fd event)))
            (t (epoll-ctl epoll-fd operation fd nil)))))
    (when (minusp ret)
      (simple-perror "epoll-ctl"))))

(declaim (ftype (function (epoll
                           foreign-pointer
                           fixnum
                           &key (:timeout-ms fixnum) (:catch-eintr boolean))
                          *)
                wait))
(defun wait (epoll events max-events &key (timeout-ms -1) (catch-eintr t))
  (if catch-eintr
      (loop
        for retval = (epoll-wait epoll events max-events timeout-ms)
        while (and (minusp retval) (= errno eintr))
        finally (if (minusp retval)
                    (simple-perror "epoll-wait")
                    (return retval)))
      (let ((retval (epoll-wait epoll events max-events timeout-ms)))
        (if (minusp retval)
            (simple-perror "epoll-wait")
            retval))))
