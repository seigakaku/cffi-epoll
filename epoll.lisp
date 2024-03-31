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

(declaim (ftype (function (&optional boolean) epoll) make-epoll))
(defun make-epoll (&optional (close-on-exec t))
  (let ((epoll-fd (epoll-create1 (if close-on-exec cloexec 0))))
    (if (minusp epoll-fd)
        (simple-perror "epoll-create1")
        epoll-fd)))

(declaim (ftype (function (epoll operation fd &optional t) *) control))
(defun control (epoll-fd operation fd &optional event)
  (when (minusp (epoll-ctl epoll-fd operation fd (if (eq operation :del) nil event)))
    (simple-perror "epoll-ctl")))

(declaim (ftype (function (epoll foreign-pointer fixnum &optional fixnum) *) wait))
(defun wait (epoll events max-events &optional (timeout -1))
  (when (minusp (epoll-wait epoll events max-events timeout))
    (simple-perror "epoll-wait")))
