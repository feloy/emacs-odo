(global-set-key (kbd "C-x C-j") 'odo-init)

(defun odo-init ()
  "Initialize a new project with odo"
  (interactive)
  (setq files (directory-files "."))
  (if (member "devfile.yaml" files)
      (message "devfile already exists")
    (if (equal files '("." ".."))
	(odo-interactive)
      (odo-alizer))
    )
  )

(defun odo-interactive ()
  "Select devfile interactively"
  (interactive)
  (setq temp-buffer "#temp-odo")
  (setq result (call-process "odo" nil temp-buffer nil "registry" "-o" "json"))
  (if (= result 0)
      (progn
	(set-buffer temp-buffer)
	(goto-char (point-min))
	(setq res (json-parse-buffer))
	(setq lang (completing-read "Select a language: " (hash-table-keys res) nil t))
	(setq typ (completing-read "Select a project type: " (hash-table-keys (gethash lang res)) nil t))
	(setq choice (gethash typ (gethash lang res)))
	(setq devfile (gethash "name" choice))
	(setq registry (gethash "name" (gethash "registry" choice)))
	(download-devfile devfile registry)
	(kill-buffer temp-buffer)
	)
    )
  )

(defun odo-alizer ()
  "Run odo alizer command"
  (interactive)
  (setq temp-buffer "#temp-odo")
  (setq result (call-process "odo" nil temp-buffer nil "alizer" "-o" "json"))
  (if (= result 0)
      (progn
	(set-buffer temp-buffer)
	(goto-char (point-min))
	(setq res (json-parse-buffer))
	(print res)
	(setq devfile (gethash "devfile" res))
	(setq registry (gethash "devfile-registry" res))
	(download-devfile devfile registry)
	(kill-buffer temp-buffer)
	)
    (message "error")
    )
  )

(defun hash-table-keys (hash-table)
  (let ((keys ()))
    (maphash (lambda (k v) (push k keys)) hash-table)
    keys)
  )

(defun download-devfile (devfile registry)
  (progn
    (setq msg (format "The devfile '%s' from the registry '%s' will be downloaded. Continue? " devfile registry))
    (if (yes-or-no-p msg)
	(progn
	  (setq name (read-string "Component name: " (format "my-%s-app" devfile)))
	  (erase-buffer)
	  (if (= 0 (call-process "odo" nil temp-buffer nil "init" "--name" name "--devfile" devfile "--devfile-registry" registry "-o" "json"))
	      (progn
		(goto-char (point-min))
		(setq res (json-parse-buffer))
		(message "devfile downloaded in %s" (gethash "devfile-path" res))
		)
	    (message "error initializing project")
	    ))
      )
    )
  )
