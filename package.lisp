;;;; package.lisp

(defpackage #:cffi-epoll
  (:use #:cl #:cffi)
  (:nicknames #:epoll)
  (:export #:cloexec

           #:operation #:event #:flags

           #:epoll-data #:epoll-event

           #:einval #:emfile #:enfile #:enomem #:ebadf #:eexist
           #:eloop #:enoent #:enospc #:eperm #:efault #:eintr

           #:timespec #:time #:sigset))
