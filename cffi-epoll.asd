;;;; cffi-epoll.asd

#-linux
(error "Epoll is only available on Linux")

(defsystem #:cffi-epoll
  :version "0.0.1"
  :serial t
  :defsystem-depends-on (#:cffi-grovel)
  :depends-on (#:cffi)
  :components ((:file "package")
               (:cffi-grovel-file "epoll-constants")
               (:file "epoll")))
