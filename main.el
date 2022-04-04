(require 'seq)
(require 'ctable)
(global-set-key (kbd "C-x C-j C-i") 'odo-init)
(global-set-key (kbd "C-x C-j C-l") 'odo-list)

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
		 (lang (completing-read "Select a language: " (get-languages-from-registry-list res) nil t))
		 (typ (completing-read "Select a project type: "
				       (get-project-types-from-registry-list-for-language res lang) nil t))
		 (choice (get-devfile-by-unique-id res typ))
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

(defun get-languages-from-registry-list (list)
  "List the unique languages in the list of devfiles"
  (let ((langs ()))
    (seq-doseq (devfile list)
      (push (gethash "language" devfile) langs)
      )
    (delete-dups langs)
    )
  )

(defun get-project-types-from-registry-list-for-language (list lang)
  "List the unique project types in the list of devfile using the specified language"
  (let ((types ()))
    (seq-doseq (devfile list)
      (if (equal lang (gethash "language" devfile))
	  (push (get-devfile-unique-id devfile) types)
	)
      )
    (delete-dups types)
    )
  )

(defun get-devfile-unique-id (devfile)
  "Returns a unique identifier for a devfile"
  (concat (gethash "display-name" devfile) " (" (gethash "name" (gethash "registry" devfile)) ")")
  )

(defun get-devfile-by-unique-id (list id)
  "Returns a devfile description from the list given its unique id built with get-devfile-unique-id"
  (let (value)
    (seq-doseq (devfile list)
      (if (equal id (get-devfile-unique-id devfile))
	  (setq value devfile)
	)
      )
    value
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

(defun odo-list ()
  "List odo components"
  (interactive)
  (let ((temp-buffer "#temp-odo"))
    (generate-new-buffer temp-buffer)
    (set-buffer temp-buffer)
    (display-odo-list (call-odo-list))
    )
  )

(defun call-odo-list ()
  "Call odo list -o json command"
  (erase-buffer)
  (if (= 0 (call-process "odo" nil temp-buffer nil "list" "-o" "json"))
      (progn
	(goto-char (point-min))
	(json-parse-buffer)
	)
    )
  )

(defun display-odo-list (list)
  ""
  (let (
	(odo-list-buffer "*odo-list*")
	(all-components)
	)
    (generate-new-buffer odo-list-buffer)
    (switch-to-buffer odo-list-buffer)
    (erase-buffer)
    (insert "odo components in namespace: " (gethash "namespace" list) "\n\n")
    (seq-doseq (component (gethash "components" list))
      (let* (
	    (name (gethash "name" component))
	    (managedby (gethash "managed-by" component))
	    (modes (get-modes-as-string (gethash "modes" component)))
	    (type (gethash "type" component))
	    (component (list name managedby modes type))
	    )
	(push component all-components)
	)
      )
    (ctbl:create-table-component-region
     :model (ctbl:make-model-from-list all-components (list "NAME" "MANAGED BY" "MODES" "TYPE")))
    )
  )

(defun get-modes-as-string (modes)
  "Returns the list of modes as string"
  (let ((list ()))
    (seq-doseq (mode (hash-table-keys modes))
      (push mode list)
      )
    (mapconcat 'identity list ",")
    )
  )

(defun hash-table-keys (hash-table)
  "Extract the list of keys of a hashmap"
  (let ((keys ()))
    (maphash (lambda (k v) (push k keys)) hash-table)
    keys)
  )
