;;; c3edit.el --- Real-time cross-editor collaborative editing -*- lexical-binding: t -*-

;; Author: Adam Zhang <898544@lcps.org>
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.1"))
;; Homepage: https://github.com/adam-zhang-lcps/c3edit

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; WIP

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'map)
(require 'ansi-color)

(defgroup c3edit nil
  "Real-time cross-editor collaborative editing."
  :group 'editing)

(defcustom c3edit-backend-path (executable-find "c3edit")
  "Path to c3edit backend binary."
  :type '(file :must-match t))

(defcustom c3edit-port nil
  "Port to listen for incoming connections on."
  :type 'natnum)

(defconst c3edit-peer-faces
  '(ansi-color-red
    ansi-color-cyan
    ansi-color-blue
    ansi-color-green)
  "List of faces to use for peer cursor/selection colors.")

(defvar c3edit--hooks nil
  "Alist of hooks to add and remove for document buffers.
Populated by `c3edit--defhook' macro.")

(defvar c3edit--process nil
  "Process for c3edit backend.")

(defvar c3edit--buffers nil
  "Alist mapping active buffers to their backend IDs.")

(defvar c3edit--cursors-alist nil
  "Alist mapping buffer IDs to their cursor overlays.
The cdr of each element is an alist mapping peer IDs to their respective
cursor overlay.")

;; TODO Replace this with a callback-based approach with a macro in which a
;; function can be registered to handle the next message.
(defvar c3edit--currently-creating-buffer nil
  "Buffer currently in the process of being created on the backend.")

(defvar c3edit--pre-command-point nil
  "Point before the last command was executed.")

(defvar c3edit--synced-changes-p nil
  "Whether current changes being inserted are from backend.
Dynamically-scoped variable to prevent infinitely-recursing changes.")

(defun c3edit-start ()
  "Start the c3edit backend.
Start as server if SERVER is non-nil."
  (interactive)
  (when c3edit--process
    (user-error "Backend for c3edit is already running"))
  (let ((command (list c3edit-backend-path)))
    (when c3edit-port
      (setq command (nconc command `("--port" ,(number-to-string c3edit-port)))))
    (setq c3edit--process (make-process
                           :name "c3edit"
                           :command command
                           :connection-type 'pipe
                           :filter #'c3edit--process-filter
                           :stderr (get-buffer-create "*c3edit log*")))))

(defun c3edit-stop ()
  "Kill c3edit backend."
  (interactive)
  (unless c3edit--process
    (user-error "Backend for c3edit is not running"))
  ;; In case the process crashed, ignore errors.
  ;; TODO Ignore the specific error only.
  (ignore-errors
    (kill-process c3edit--process))
  (cl-loop for (buffer . _id) in c3edit--buffers
           do (cl-loop for (hook . function) in c3edit--hooks
                       do (remove-hook hook function 'local)))
  (setq c3edit--process nil
        c3edit--buffers nil
        c3edit--cursors-alist nil))

(defun c3edit-add-peer (address)
  "Add a peer at ADDRESS."
  (interactive "sAddress: ")
  (c3edit--send-message `((type . "add_peer")
                          (address . ,address))))

(defun c3edit-create-document (buffer)
  "Create a new c3edit document with BUFFER's contents.
When called interactively, BUFFER is the current buffer."
  (interactive (list (current-buffer)))
  (c3edit--send-message `((type . "create_document")
                          (name . ,(buffer-name buffer))
                          (initial_content . ,(with-current-buffer buffer
                                                (buffer-string)))))
  (setq c3edit--currently-creating-buffer buffer))

(defun c3edit-join-document (id)
  "Join document with ID."
  (interactive "sDocument ID: ")
  (c3edit--send-message `((type . "join_document")
                          (id . ,id))))

(defsubst c3edit--send-message (message)
  "Serialize MESSAGE into JSON and send it to the c3edit backend."
  (process-send-string
   c3edit--process (concat (json-serialize message) "\n")))

(defun c3edit--json-read-all (string)
  "Read all JSON objects from STRING.
Returns list of read objects."
  (let (data)
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (condition-case _err
          (while t
            (push (json-parse-buffer
                   :object-type 'alist
                   :array-type 'list)
                  data))
        (json-end-of-file)))
    (nreverse data)))

(defun c3edit--process-filter (_process text)
  "Process filter for c3edit backend messages.
Processes message from TEXT."
  ;; Emacs process handling may return many lines at once, we have to make sure
  ;; to read them all in order.
  (let* ((data (c3edit--json-read-all text))
         (c3edit--synced-changes-p t))
    (dolist (message data)
      ;; NOTE: Is this performant?
      (let* ((type (cdr (assoc 'type message)))
             (handler (intern-soft (concat "c3edit--handle-"
                                           (string-replace "_" "-" type)))))
        (if (functionp handler)
            (funcall handler message)
          (display-warning
           'c3edit (format "Unknown message type: %s" type) :warning))))))

(defun c3edit--get-peer-face (document-id)
  "Return a face for a new peer in DOCUMENT-ID."
  (let* ((peers (length (cdr (assoc document-id c3edit--cursors-alist))))
         (face (nth (mod peers (length c3edit-peer-faces)) c3edit-peer-faces)))
    `(:inverse-video t :inherit ,face)))

(defmacro c3edit--defhandler
    (name args &rest body)
  "Define a handler for message type NAME.
The resulting function will be named `c3edit--handle-NAME'. The elements of ARGS will be automatically bound within BODY with the corresponding values from the message. Dashes in ARGS will be replaced with underscores when extracting."
  (declare (indent defun))
  (let ((args-bind (mapcar
                    (lambda (arg)
                      `(,arg (cdr
                              (assoc
                               ',(intern
                                  (string-replace "-" "_" (symbol-name arg)))
                               message))))
                    args)))
    `(defun ,(intern (format "c3edit--handle-%s" name))
         (message)
       ,(format "Handle `%s' type MESSAGE.\nGenerated by `c3edit--defhandler'."
                name)
       (let (,@args-bind)
         ,@body))))

(c3edit--defhandler create-document-response (id)
  (push `(,c3edit--currently-creating-buffer . ,id)
        c3edit--buffers)
  (push `(,id . nil) c3edit--cursors-alist)
  (with-current-buffer c3edit--currently-creating-buffer
    (cl-loop for (hook . function) in c3edit--hooks
             do (add-hook hook function nil 'local)))
  (message "Document created with ID %s" id))

(c3edit--defhandler join-document-response (id current-content)
  (let ((buffer (get-buffer-create id)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert current-content)
      (cl-loop for (hook . function) in c3edit--hooks
               do (add-hook hook function nil 'local)))
    (push `(,buffer . ,id) c3edit--buffers)
    (push `(,id . nil) c3edit--cursors-alist)
    (pop-to-buffer buffer)
    (message "Joined document with ID %s" id)))

(c3edit--defhandler add-peer-response (address)
  (message "Successfully added peer at %s" address))

(c3edit--defhandler change (document-id change)
  (let-alist change
    (with-current-buffer (car (rassoc document-id c3edit--buffers))
      (save-excursion
        (pcase .type
          ("insert"
           (goto-char (1+ .index))
           (insert .text))
          ("delete"
           (delete-region
            (1+ .index)
            (+ 1 .index .len))))))))

(c3edit--defhandler set-cursor (document-id location peer-id)
  (let* ((data (rassoc document-id c3edit--buffers))
         (buffer (car data))
         (id (cdr data))
         (overlay (cdr (assoc id c3edit--cursors-alist))))
    (with-current-buffer buffer
      (cond
       ;; Our cursor
       ((not peer-id)
        (goto-char (1+ location)))
       ;; Create new cursor for peer
       ((not overlay)
        (setq overlay (make-overlay (1+ location) (+ 2 location)))
        (overlay-put overlay 'face (c3edit--get-peer-face id))
        (push `(,id . ,overlay) c3edit--cursors-alist))
       (t
        (move-overlay overlay (1+ location) (+ 2 location)))))))

(c3edit--defhandler set-selection (document-id peer-id point mark)
  (let* ((data (rassoc document-id c3edit--buffers))
         (buffer (car data))
         (id (cdr data))
         (overlay (cdr (assoc id c3edit--cursors-alist))))
    (with-current-buffer buffer
      (cond
       ;; Our selection
       ((not peer-id)
        (goto-char (1+ point))
        (set-mark (1+ mark)))
       ;; Create new selection for peer
       ((not overlay)
        (setq overlay (make-overlay (1+ point) (1+ mark)))
        (overlay-put overlay 'face (c3edit--get-peer-face id))
        (push `(,id . ,overlay) c3edit--cursors-alist))
       (t
        (move-overlay overlay (1+ point) (1+ mark)))))))

(defmacro c3edit--defhook
    (hook args &rest body)
  "Define a hook function for HOOK.
The resulting function will be named `c3edit--hook-HOOK', take ARGS as
arguments, and have body BODY.

BODY will additionally be evaluated with `document-id' bound to the
backend ID of the current buffer's document, and will not be run if
`c3edit--synced-changes-p' is non-nil."
  (declare (indent defun))
  (let ((name (intern (format "c3edit--hook-%s" hook))))
    `(progn
       (defun ,name
           ,args
         ,(format "Handle `%s' for updating backend.\nGenerated by `c3edit--defhook'."
                  hook)
         (when-let (((not c3edit--synced-changes-p))
                    (document-id (cdr (assoc (current-buffer) c3edit--buffers))))
           ,@body))
       (push '(,hook . ,name) c3edit--hooks))))

(c3edit--defhook after-change-functions (beg end len)
  (let (change)
    (if (= beg end)
        (setq change `((type . "delete")
                       (index . ,(1- beg))
                       (len . ,len)))
      (setq change `((type . "insert")
                     (index . ,(1- beg))
                     (text . ,(buffer-substring-no-properties beg end)))))
    (c3edit--send-message `((type . "change")
                            (document_id . ,document-id)
                            (change . ,change)))))

(c3edit--defhook pre-command-hook ()
  (setq c3edit--pre-command-point (point)))

(c3edit--defhook post-command-hook ()
  (when (not (= c3edit--pre-command-point (point)))
    (c3edit--send-message
     (if mark-active
         `((type . "set_selection")
           (document_id . ,document-id)
           (point . ,(1- (point)))
           (mark . ,(1- (mark))))
       `((type . "set_cursor")
         (document_id . ,document-id)
         (location . ,(1- (point))))))))

(c3edit--defhook activate-mark-hook ()
  (c3edit--send-message `((type . "set_selection")
                          (document_id . ,document-id)
                          (point . ,(1- (point)))
                          (mark . ,(1- (mark))))))

(c3edit--defhook deactivate-mark-hook ()
  ;; TODO Refactor cursor update into separate function (duplicate from
  ;; `c3edit--post-command-function' right now).
  (c3edit--send-message `((type . "set_cursor")
                          (document_id . ,document-id)
                          (location . ,(1- (point))))))

(provide 'c3edit)

;;; c3edit.el ends here
