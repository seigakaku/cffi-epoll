(in-package #:cffi-epoll)

(defcfun (create "epoll_create") :fd
  (size :int))

(defcfun (create1 "epoll_create1") :fd
  (flags :int))

(defcfun (ctl "epoll_ctl") :int
  (epoll-fd :fd)
  (operation operation)
  (fd :fd)
  (event (:pointer (:struct epoll-event))))

(defcfun (wait "epoll_wait") :int
  (epoll-fd :fd)
  (events (:pointer (:struct epoll-event)))
  (max-events :int)
  (timeout :int))

(defcfun (pwait "epoll_pwait") :int
  (epoll-fd :fd)
  (events (:pointer (:struct epoll-event)))
  (max-events :int)
  (signal-mask sigset))

(defcfun (pwait2 "epoll_pwait2") :int
  (epoll-fd :fd)
  (events (:pointer (:struct epoll-event)))
  (max-events :int)
  (timeout (:pointer (:struct timespec)))
  (signal-mask sigset))
