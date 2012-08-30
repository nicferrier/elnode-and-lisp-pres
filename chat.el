;;; chat example - very simple webchat -*- lexical-binding: t -*-
(require 'esxml)
(require 'cl)
(require 'elnode)
(defconst chat-dir (file-name-directory
                    (or (buffer-file-name)
                        load-file-name
                        default-directory)))

(defun chat-main-templater ()
  "Return the `chat-list' as rows for initial chat display."
  (list
   (cons
    "messages"
    (chat-list-to-html))))

(defun chat-main-handler (httpcon)
  "The main handler."
  (let ((chat-js (concat chat-dir "chat.js"))
        (chat-html (concat chat-dir "chat.html"))
        (chat-css (concat chat-dir "styles.css")))
    (elnode-hostpath-dispatcher
     httpcon
     `(("^.*//chat/poll/" . chat-comet-handler)
       ("^.*//chat/send/" . chat-send-handler)
       ("^.*//chat.js" . ,(elnode-make-send-file chat-js))
       ("^.*//styles.css" . ,(elnode-make-send-file chat-css))
       ("^.*//" . ,(elnode-make-send-file
                    chat-html
                    :replacements 'chat-main-templater))))))

(defvar chat-list '())

(defun chat-add (user text)
  (add-to-list
   'chat-list
    (list (current-time) user text)))

(defun chat-list-since (since)
  (loop for rec in chat-list
       if (time-less-p since (car rec))
       collect rec))

;; chat-comet-handler - defer until there is some chat to send
(defun chat-comet-handler (httpcon)
  "Defer until there is new chat."
  (let ((entered (current-time)))
    (elnode-defer-until (chat-list-since entered)
        (elnode-send-json
         httpcon elnode-defer-guard-it :jsonp t))))

;; chat-send-handler - add the POSTed chat to the chat-list
;; the username is in a cookie: chatusername
;; the message parameter is "msg"
(defun chat-send-handler (httpcon)
  (let ((username (elnode-http-cookie httpcon "chatusername" t))
        (msg (elnode-http-param httpcon "msg")))
    (chat-add username msg)
    (elnode-send-json httpcon (json-encode '("thanks")))))

;; chat-list-to-html - make a table rows
;; each cell should have a class, username or message
;; the username row could have the username as a class as well
(defun chat-list-to-html ()
  (loop for entry in chat-list
     concat
       (esxml-to-xml
        `(tr
          ()
          (td
           ((class . "username"))
           ,(elt entry 1))
          (td
           ((class . "message"))
           ,(elt entry 2))))))


;;; chat.el ends here
