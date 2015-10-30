(ql:quickload :cl-ppcre)

(defstruct irc-file-listing bot channel pack-id num-gets size filename timefound)

(defun read-in-irc-logfile (irc-logfile)
  (let ((lines '()))
    (with-open-file (stream irc-logfile)
      (do ((line (read-line stream nil)
		 (read-line stream nil)))
	  ((null line))
	(push line lines)))
    lines))
(defun create-irc-line-scanner ()
  (ppcre:create-scanner ":(.*)!(.*)@(.*)\\sPRIVMSG\\s(.*):(.*)#([0-9]*)([^0-9]*)([0-9]*)x\\s\\[(.{4})\\]\\s(.*)\\-\\-\\-\\s([0-9]*)"))

(defun parse-irc-lines (lines)
  (let ((scanner (create-irc-line-scanner)))
    (remove-if #'null (mapcar (lambda (line) (parse-irc-line scanner line)) lines))))

(defun convert-array-to-file-listing-struct (matches)
  (if (not (null matches))
      (make-irc-file-listing :bot (aref matches 0) :channel (aref matches 3) :pack-id (aref matches 5) :num-gets (aref matches 7) 
			     :size (aref matches 8) :filename (remove-irc-color-codes (aref matches 9)) :timefound (aref matches 10))))

(defun parse-irc-line (scanner line)
  (multiple-value-bind (matched-string matches) (ppcre:scan-to-strings scanner line)
    (convert-array-to-file-listing-struct matches)))

(defun get-irc-color-codes()
  '(#\Stx #\Return #\Si #\Us #\Syn))

(defun is-irc-color-code (x)
  (member x (get-irc-color-codes)))

(defun remove-irc-color-codes (filename)
  (coerce (remove-if #'is-irc-color-code (coerce (string-trim " " filename) 'list)) 'string))

(defun main-irc-parser (file)
  (let ((parsed-lines (parse-irc-lines (read-in-irc-logfile file))))
    (loop for x in parsed-lines do
	 (print (irc-file-listing-filename x)))))


