(ql:quickload :cl-ppcre)

(defparameter *device* nil)

(defun read-response ()
  ;; Hang till there's at least something to read
  (peek-char nil *device*)
  (with-output-to-string (response)
    (let (c)
      (loop while (setf c (read-char-no-hang *device*))
	    do (write-char c response)))))

(defmacro with-subseqs-bound ((template-string target-string) &body body)
  (let (str vars)
    (setf vars (list (loop for reg in (cl-ppcre:all-matches-as-strings "(?<=<!-- TMPL_VAR )[^ ]+?(?= -->)" template-string)
			   collect (intern (string-upcase reg)))))
    ;; Quote all parts of the template string we are not interested in
    (let ((split
	    (loop for match in (cl-ppcre:split "(<!-- TMPL_VAR [^ ]+ -->)" template-string :with-registers-p t)
		  collect (if (cl-ppcre:scan "(<!-- TMPL_VAR [^ ]+ -->)" match)
			      match
			      (cl-ppcre:quote-meta-chars match)))))
      (setf str (apply #'concatenate (cons 'string split))))
    (setf str (cl-ppcre:regex-replace-all "<!-- TMPL_VAR [^ ]+ -->" str "(?s)(.+)"))
    `(cl-ppcre:register-groups-bind ,@vars
				    (,str ,target-string)
				    ,@body)))

(defun prompt-read-line (&optional prompt)
  (if prompt
      (format t "~&~A > " prompt)
      (format t "~&> "))
  (finish-output)
  (read-line))

(defun handle-ussd-response ()
  (with-subseqs-bound
      ("+CUSD: <!-- TMPL_VAR m -->,\"<!-- TMPL_VAR str -->\",<!-- TMPL_VAR dcs -->"
       (read-response))
    (cond
      ((and m str dcs)
       (format t "~A" str) (finish-output)
       (return-from handle-ussd-response m))
      (t
       nil))))

(defun ussd (code)
  "Run a ussd session, end with \q"
  (format *device* "AT+CUSD=1,\"~A\",15~C" code #\return)
  (finish-output *device*)
  (handle-ussd-response)
  (let (input)
    (loop while (setf input (prompt-read-line "ussd"))
	  if (equalp input "\\q") return nil
	    do
	       (cond ((zerop (length input))
		      ;; Just return
		      )
		     ((> (length input) 1)
		      (if (and (char= (elt input 0) #\/)
			       (not (char= (elt input 1) #\/)))
			  (progn
			    ;; Execute command
			    )
			  (progn
			    (format *device* "AT+CUSD=1,\"~A\",15~C" input #\return)
			    (finish-output *device*))))
		     (t
		      (format *device* "AT+CUSD=1,\"~A\",15~C" input #\return)
		      (finish-output *device*)))
	       (handle-ussd-response))))

(defun main ()
  (setf *device* (open "/dev/ttyUSB0" :direction :io :element-type 'character
				      :if-exists :overwrite))
  (let (input)
    (loop do
      (setf input (cl-ppcre:split "\\s+" (prompt-read-line)))
      (cond ((equalp (first input) "ussd")
	     (ussd (second input)))
	    ;; Other commands
	    )
	  until (equalp (first input) "quit"))))
