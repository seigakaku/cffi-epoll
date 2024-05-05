;;;; package.lisp

(defpackage #:cffi-epoll
  (:use #:cl #:cffi)
  (:nicknames #:epoll)
  (:export #:cloexec
           ;; CFFI Types
           #:sigset #:time #:timespec #:operation #:event #:flags #:epoll-data #:epoll-event
           ;; CFFI Foreign Slots
           #:events #:data #:ptr #:fd #:u32 #:u64
           ;; CFFI C Functions
           #:epoll-create #:epoll-create1 #:epoll-ctl #:epoll-wait #:epoll-pwait #:epoll-pwait2
           ;; CFFI C variables
           #:errno

           ;; Lisp types
           #:fd #:epoll #:operation #:event #:timeout #:seconds
           ;; Low level lisp wrappers
           #:make-epoll #:control #:wait
           ;; Macros
           #:with-epoll))
