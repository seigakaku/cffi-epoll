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
              ((:delete "EPOLL_CTL_DEL")))

;;;; epoll_ctl events bitmask
(bitfield (event :base-type :uint32)
          ((:input "EPOLLIN"))
          ((:output "EPOLLOUT"))
          ((:read-hangup "EPOLLRDHUP"))
          ((:hangup "EPOLLHUP"))
          ((:priority "EPOLLPRI"))
          ((:error "EPOLLERR"))
          ((:edge-triggered "EPOLLET"))
          ((:level-triggered "EPOLLONESHOT"))
          ((:one-shot "EPOLLONESHOT"))
          ((:wakeup "EPOLLWAKEUP"))
          ((:exclusive "EPOLLEXCLUSIVE")))

(cunion epoll-data "union epoll_data"
        (ptr "ptr" :type :pointer)
        (fd "fd" :type :int)
        (u32 "u32" :type :uint32)
        (u64 "u64" :type :uint64))

(cstruct epoll-event "struct epoll_event"
         (events "events" :type event)
         (data "data" :type (:union epoll-data)))

 ;;;; Errors
(constant (eintr "EINTR"))
(constantenum (errno-values :base-type :int
                            :define-constants nil)

              ((:einval "EINVAL"))
              ((:emfile "EMFILE"))
              ((:enfile "ENFILE"))
              ((:enomem "ENOMEM"))
              ((:ebadf "EBADF"))
              ((:eexist "EEXIST"))
              ((:eloop "ELOOP"))
              ((:enoent "ENOENT"))
              ((:enospc "ENOSPC"))
              ((:eperm "EPERM"))
              ((:efault "EFAULT"))
              ((:eintr "EINTR")))

(cvar ("errno" errno) :int)
