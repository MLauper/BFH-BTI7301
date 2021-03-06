;;;; FILES

;; -> OPEN returns character-based input stream
;; -> READ-CHAR reads a single character
;; -> READ-LINE returns a line of texts
;; -> CLOSE closes a file

(open "/tmp/test.txt")
(let ((in (open "/tmp/test.txt")))
  (format t "~a;~%" (read-line in))
  (close in))

;; -> Specify behaviour if file does not exist
;; -> Available options: :error (abort script), :create (create empty file), :nil (return empty stream, do not create file)
(let ((in (open "/tmp/test2.txt" :if-does-not-exist :create)))
  (when in
    (format t "~a~%" (read-line in))
    (close in)))

;; -> By default, all Read-commands return an error when called at the end of file
;; -> Can be altered by defining an optional parameter

(let ((in (open "/tmp/test3.txt" :if-does-not-exist :create)))
  (when in
    (loop for line = (read-line in nil) ; parameter nil changes error at end of file
       while line do (format t "~a~%" line))
    (close in)))

;;; WRITE
;; -> :direction keyword has to be passed to OPEN
;; -> For Write-Operations :output
;; -> :if-exists supports :supersede (replace file), :append, :overwrite (start writing at the beginning of the file), NIL (Open returns nil if file already exist)
(open "/tmp/test4.txt" :direction :output :if-exists :supersede)

;; -> Supported Write-Methods include WRITE-CHAR, WRITE-LINE, WRITE-STRING, FRESH-LINE
;; -> s-expressions can be printed with PRINT, PRINT1, PPRINT

;;; CLOSE
;; Easier way to open files, without explicit close
(with-open-file (stream "/tmp/test1.txt" :if-does-not-exist :create)
  (format t "~a~%" (read-line stream nil)))

;;; File System Interaction
;; PROBE-FILE: Test if file exists
;; DELETE-FILE, RENAME-FILE, ENSURE-DIRECTORIES-EXIST (Create directory if not existing
;; FILE-WRITE-DATE, FILE-AUTHOR, FILE-LENGHT

