(require 'seq)
(global-set-key (kbd "C-x C-j C-i") 'odo-init)

(defun odo-init ()
  "Initialize a new project with odo"
  (interactive)
  (let ((temp-buffer "#temp-odo"))
    (generate-new-buffer temp-buffer)
    (set-buffer temp-buffer)
    (let ((files (directory-files ".")))
      (if (member "devfile.yaml" files)
	  (message "devfile already exists")
	(if (equal files '("." ".."))
	    (if (odo-interactive)
		(pull-starter)
	      )
	  (odo-alizer))
	)
      )
    (kill-buffer temp-buffer)
    )
  )

(defun odo-interactive ()
  "Select devfile interactively"
  (erase-buffer)
  (let ((result (call-process "odo" nil temp-buffer nil "registry" "-o" "json")))    
    (if (= result 0)
	(progn
	  (goto-char (point-min))
	  (let* ((res (json-parse-buffer))
		 (lang (completing-read "Select a language: " (hash-table-keys res) nil t))	      
		 (typ (completing-read "Select a project type: " (hash-table-keys (gethash lang res)) nil t))
		 (choice (gethash typ (gethash lang res)))
		 (devfile (gethash "name" choice))
		 (registry (gethash "name" (gethash "registry" choice)))
		 (msg (format "The devfile '%s' from the registry '%s' will be downloaded. Continue? " devfile registry))
		 )
	    (if (yes-or-no-p msg)
		(progn
		  (download-devfile devfile registry)
		  't)
	      nil
	      )
	    )
	  )
      )
    )
  )

(defun pull-starter ()
  "Pull a starter project"
  (erase-buffer)
  (if (= 0 (call-process "odo" nil temp-buffer nil "devfile" "info" "-o" "json"))
      (progn
	(goto-char (point-min))
	(let* ((res (gethash "starterProjects" (json-parse-buffer)))
	       (projects (starter-project-names res))
	       (projects (push "NONE" projects))
	       (project (completing-read "Select a starter project: " projects nil t))
	       )
	  (if (not (equal "NONE" project))
	      (progn
		(erase-buffer)
		(if (= 0 (call-process "odo" nil nil nil "devfile" "pull" project "-o" "json"))
		    (message "Starter project downloaded")
		  (message "Error downloading starter project")
		  )
		)
	    )
	  )
	)
    (message "error getting starter projects list")
    )
  )

(defun odo-alizer ()
  "Run odo alizer command"
  (erase-buffer)
  (if (= 0 (call-process "odo" nil temp-buffer nil "alizer" "-o" "json"))
      (progn
	(goto-char (point-min))
	(let* ((res (json-parse-buffer))
	       (devfile (gethash "devfile" res))
	       (registry (gethash "devfile-registry" res))
	       (msg (format "The devfile '%s' from the registry '%s' will be downloaded. Continue? " devfile registry))
	       )
	  (if (yes-or-no-p msg)
	      (download-devfile devfile registry)
	    (odo-interactive)
	    )
	  )
	)
    (message "alizer error")
    )
  )

(defun download-devfile (devfile registry)
  (let ((name (read-string "Component name: " (format "my-%s-app" devfile))))
    (erase-buffer)
    (if (= 0 (call-process "odo" nil temp-buffer nil "init" "--name" name "--devfile" devfile "--devfile-registry" registry "-o" "json"))
	(progn
	  (goto-char (point-min))
	  (let ((res (json-parse-buffer)))
	    (message "devfile downloaded in %s" (gethash "devfile-path" res))
	    )
	  )
      (message "error initializing project")
      )
    )
  )

(defun starter-project-names (list)
  "Extract the names of starter projects from a list of starter projects"
  (let ((names ()))
    (seq-doseq (starter list)
      (push (gethash "name" starter) names)
      )
    names)
  )

(defun hash-table-keys (hash-table)
  "Extract the list of keys of a hashmap"
  (let ((keys ()))
    (maphash (lambda (k v) (push k keys)) hash-table)
    keys)
  )

