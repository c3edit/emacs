(defconst c3edit-directory
  (file-name-directory
   (cond
    (load-in-progress load-file-name)
    (buffer-file-name))))

(add-to-list 'load-path c3edit-directory)
(require 'c3edit)

(setopt c3edit-backend-path "~/git/c3edit-backend/target/debug/c3edit"
        c3edit-port
        (if (bound-and-true-p server-process)
            ;; This is the server process
            7000
          7001))

(when c3edit--process
  (c3edit-stop))

(c3edit-start)
(unless (bound-and-true-p server-process)
  ;; Add the server as a peer
  (c3edit-add-peer "localhost:7000"))
