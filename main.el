(require 'seq)
(global-set-key (kbd "C-x C-j") 'odo-init)

(defun odo-init ()
  "Initialize a new project with odo"
  (interactive)
  (setq temp-buffer "#temp-odo")
  (generate-new-buffer temp-buffer)
  (set-buffer temp-buffer)
  (setq files (directory-files "."))
  (if (member "devfile.yaml" files)
      (message "devfile already exists")
    (if (equal files '("." ".."))
	(if (odo-interactive)
	    (pull-starter)
	  )
      (odo-alizer))
    )
  (kill-buffer temp-buffer)
  )

(defun odo-interactive ()
  "Select devfile interactively"
  (interactive)
  (erase-buffer)
  (setq result (call-process "odo" nil temp-buffer nil "registry" "-o" "json"))
  (if (= result 0)
      (progn
	(goto-char (point-min))
	(setq res (json-parse-buffer))
	(setq lang (completing-read "Select a language: " (hash-table-keys res) nil t))
	(setq typ (completing-read "Select a project type: " (hash-table-keys (gethash lang res)) nil t))
	(setq choice (gethash typ (gethash lang res)))
	(setq devfile (gethash "name" choice))
	(setq registry (gethash "name" (gethash "registry" choice)))
	(setq msg (format "The devfile '%s' from the registry '%s' will be downloaded. Continue? " devfile registry))
	(if (yes-or-no-p msg)
	    (progn
	      (download-devfile devfile registry)
	      't)
	  nil
	  )
	)
    )
  )

(defun pull-starter ()
  "Pull a starter project"
  (interactive)
  (erase-buffer)
  (if (= 0 (call-process "odo" nil temp-buffer nil "devfile" "info" "-o" "json"))
      (progn
	(goto-char (point-min))
	(setq res (gethash "starterProjects" (json-parse-buffer)))
	(setq projects (starter-project-names res))
	(push "NONE" projects)
	(setq project (completing-read "Select a starter project: " projects nil t))
	(if (not (equal "NONE" project))
	    (progn
	      (erase-buffer)
	      (setq result (call-process "odo" nil nil nil "devfile" "pull" project "-o" "json"))
	      (if (= 0 result)
		  (message "Starter project downloaded")
		(message "Error downloading starter project")
		)
	      )
	  )
	)
    (message "error getting starter projects list")
    )
  )

(defun odo-alizer ()
  "Run odo alizer command"
  (interactive)
  (setq temp-buffer "#temp-odo")
  (erase-buffer)
  (setq result (call-process "odo" nil temp-buffer nil "alizer" "-o" "json"))
  (if (= result 0)
      (progn
	(goto-char (point-min))
	(setq res (json-parse-buffer))
	(setq devfile (gethash "devfile" res))
	(setq registry (gethash "devfile-registry" res))
	(if (yes-or-no-p msg)
	    (download-devfile devfile registry)
	  (odo-interactive)
	  )
	)
    (message "error")
    )
  )

(defun starter-project-names (list)
  (let ((names ()))
    (seq-doseq (starter list)
      (push (gethash "name" starter) names)
      )
    names)
  )

(defun hash-table-keys (hash-table)
  (let ((keys ()))
    (maphash (lambda (k v) (push k keys)) hash-table)
    keys)
  )

(defun download-devfile (devfile registry)
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
