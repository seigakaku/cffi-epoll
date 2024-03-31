(in-package #:cffi-epoll)

(include "sys/epoll.h" "signal.h" "time.h" "errno.h")

;;;; epoll create flags
(constant (cloexec "EPOLL_CLOEXEC"))

(ctype :fd "int")
(ctype sigset "sigset_t *")
(ctype time "time_t")

(cstruct timespec "struct timespec"
         (seconds "tv_sec" :type time)
         (nano-seconds "tv_nsec" :type :int32))

;;;; epoll_ctl op
(constantenum (operation :base-type :int
                         :define-constants nil)
              ((:add "EPOLL_CTL_ADD"))
              ((:modify "EPOLL_CTL_MOD"))
              ((:delele "EPOLL_CTL_DEL")))

;;;; epoll_ctl events bitmask
(bitfield event
          ((:in "EPOLLIN"))
          ((:out "EPOLLOUT"))
          ((:read-hangup "EPOLLRDHUP"))
          ((:hangup "EPOLLHUP"))
          ((:priority "EPOLLPRI"))
          ((:err "EPOLLERR")))

;;;; epoll_ctl input flags
(bitfield (flags :base-type :uint)
          ((:edge-triggered "EPOLLET"))
          ((:level-triggered "EPOLLONESHOT"))
          ((:wakeup "EPOLLWAKEUP"))
          ((:exclusive "EPOLLEXCLUSIVE")))

(cunion epoll-data "union epoll_data"
        (ptr "ptr" :type :pointer)
        (fd "fd" :type :int)
        (u32 "u32" :type :uint32)
        (u64 "u64" :type :uint64))

(cstruct epoll-event "struct epoll_event"
         (events "events" :type :uint32)
         (data "data" :type epoll-data))

 ;;;; Errors
(constant (einval "EINVAL"))
(constant (emfile "EMFILE"))
(constant (enfile "ENFILE"))
(constant (enomem "ENOMEM"))
(constant (ebadf "EBADF"))
(constant (eexist "EEXIST"))
(constant (eloop "ELOOP"))
(constant (enoent "ENOENT"))
(constant (enospc "ENOSPC"))
(constant (eperm "EPERM"))
(constant (efault "EFAULT"))
(constant (eintr "EINTR"))