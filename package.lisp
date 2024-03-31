;;;; package.lisp

(defpackage #:cffi-epoll
  (:use #:cl #:cffi)
  (:nicknames #:epoll)
  (:export #:cloexec
           ;; CFFI Types
           #:sigset #:time #:timespec #:operation #:event #:flags #:epoll-data #:epoll-event
           ;; CFFI C Functions
           #:epoll-create #:epoll-create1 #:epoll-ctl #:epoll-wait #:epoll-pwait #:epoll-pwait2

           ;; Lisp types
           #:fd #:epoll #:operation
           ;; Low level lisp wrappers
           #:make-epoll #:control #:wait))
