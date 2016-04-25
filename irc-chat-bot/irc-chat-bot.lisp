(ql:quickload "usocket")
(ql:quickload :cl-ppcre)

(defparameter +nickname+ "the-lispbot")
(defparameter *state* nil)
(defparameter *channel* "#aomx")
(defparameter *served-nicks* '())

(defmacro with-connected-irc-socket ((server &optional port) &body body)
  `(let* ((socket (connect-to-irc-server ,server ,port)) 
	  (stream (usocket:socket-stream socket)))
     ,@body
     (usocket:socket-close socket)))

(defmacro with-regex-matches ((regex target-string) &body body)
  `(let ((matches (nth-value 1 (ppcre:scan-to-strings ,regex ,target-string))))
     (when (not (null matches))
	 ,@body)))

(defun connect-to-irc-server (server &optional (port 6667))
  (if (null port)
      (usocket:socket-connect server 6667)
      (usocket:socket-connect server port)))

(defun disconnect-from-irc-server (stream)
  (setf *served-nicks* '())
  (write-irc-line stream (concatenate 'string "QUIT :quit")))

(defun irc-privmsg-regex ()
  ":(.*)!(.*)@(.*)\\sPRIVMSG\\s(.*):(.*)")

(defun is-served-nick (nick)
  (member nick *served-nicks* :test #'string-equal))

(defun handle-privmsg (stream nick msg)
  (cond ((not (is-served-nick nick))
	 (push nick *served-nicks*)
	 (write-privmsg stream nick (concatenate 'string "hello " nick " this is the lisp bot, i'm now serving you...")))
	(T
	 (if (string-equal msg "quit")
	     (disconnect-from-irc-server stream)
	     (write-privmsg stream nick (concatenate 'string "are you sure that you " msg "?"))))))

(defun handle-irc-line-connected-state (stream line)
  (with-regex-matches ((irc-privmsg-regex) line)
    (handle-privmsg stream (aref matches 0) (aref matches 4)))
  (format t "connected: ~a~%" line))

(defun handle-irc-line-do-nothing-state (stream line))

(defun handle-irc-line-connecting-state (stream line)
  (with-regex-matches ("004" line)
    (defparameter *state* #'handle-irc-line-connected-state)
    (write-irc-line stream (concatenate 'string "JOIN " *channel*))
    (format t "we're connected yeay!~%"))

  (with-regex-matches ("433" line)
    (format t "got disconnected message!~%")
    (disconnect-from-irc-server stream)
    (defparameter *state* #'handle-irc-line-do-nothing-state))

  (format t "connecting: ~a~%" line))

(defun eval-irc-lines (stream)
  (let* ((line (read-line stream nil))
	 (filtered-line (string-trim '(#\Space #\Tab #\return #\linefeed) line)))
    (when line
      (with-regex-matches ("PING(.*)\\s(.*)" line)
	(write-irc-line stream (concatenate 'string "PONG " (aref matches 1))))
     
      (funcall *state* stream filtered-line)
      (eval-irc-lines stream))))

(defun write-irc-line (stream line)
  (write-string (concatenate 'string line '(#\return #\linefeed)) stream)
  (force-output stream))

(defun write-privmsg (stream nick msg)
  (write-irc-line stream (concatenate 'string "PRIVMSG " nick " :" msg)))

(defun irc-tryout ()
  (with-connected-irc-socket ("irc.inet.tele.dk")
    (write-irc-line stream (concatenate 'string "NICK " +nickname+))
    (write-irc-line stream (concatenate 'string "USER " +nickname+ " 8 * " +nickname+))
    (defparameter *state* #'handle-irc-line-connecting-state)
    (eval-irc-lines stream)))

