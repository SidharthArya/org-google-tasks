;;; org-google-tasks.el -- Does something cool


(require 'json)
(require 'request)

(defcustom org-google-tasks-credential nil
  "")

(defcustom org-google-tasks-auth-scope "https://www.googleapis.com/auth/tasks"
  "")

(defcustom org-google-tasks-api-url "https://tasks.googleapis.com/tasks/v1"
  "")
(defcustom org-google-tasks-auth-token-url "https://oauth2.googleapis.com/token"
  "")

(defcustom org-google-tasks-credential-file "/home/arya/Documents/Org/Bots/Org/google-tasks"
  "")
(defvar org-google-tasks-temp-tasklist nil)
(defvar org-google-tasks-temp-task nil)
(defun org-google-tasks-init()
  ""
  (interactive)
  (if (not (file-exists-p org-google-tasks-credential-file))
      (with-current-buffer (find-file-noselect org-google-tasks-credential-file)
        (insert "{}")
        (save-buffer)))
  (org-google-tasks-load-credentials)
  (or (gethash "client_id" org-google-tasks-credential) (puthash "client_id" (read-string "Client ID: ") org-google-tasks-credential))
  (or (gethash "client_secret" org-google-tasks-credential) (puthash "client_secret" (read-string "Client Secret: ") org-google-tasks-credential))
  (org-google-tasks-save-credentials)
      )
(defun org-google-tasks-load-credentials()
  ""
  (interactive)
  (setq org-google-tasks-credential (json-parse-string
 (with-current-buffer (find-file-noselect org-google-tasks-credential-file)
   (buffer-string)
   )))
  )
(defun org-google-tasks-oauth-url()
  ""
  (concat "https://accounts.google.com/o/oauth2/auth?scope=" org-google-tasks-auth-scope "&response_type=code&access_type=offline&redirect_uri=urn:ietf:wg:oauth:2.0:oob&client_id=" (gethash "client_id" org-google-tasks-credential)))
(defun org-google-tasks-get-tokens ()
  ""
  (interactive)
  (let* ((client_id (gethash "client_id" org-google-tasks-credential))
        (client_secret (gethash "client_secret" org-google-tasks-credential))
        (refresh_token (gethash "refresh_token" org-google-tasks-credential))
        (authorization_code (gethash "autorization_code" org-google-tasks-credential)))
    
    (if refresh_token
        (request org-google-tasks-auth-token-url
          :type "POST"
          :parser (lambda() (puthash "access_token" (gethash "access_token" (json-parse-string (buffer-string))) org-google-tasks-credential))
          :data (list 
                 (cons "grant_type"  "refresh_token")
                 (cons "client_id"  client_id)
                 (cons "client_secret" client_secret)
                 (cons "redirect_uri" "urn:ietf:wg:oauth:2.0:oob")
                 (cons "refresh_token"  refresh_token)
              ))
      (request org-google-tasks-auth-token-url
        :type "POST"
        :parser 'json-parse-buffer
        :data '(("client_id" . client_id)
                ("client_secret" . client_secret)
                ("redirect_uri" . "urn:ietf:wg:oauth:2.0:oob")
                ("grant_type" . "authorization_code")
                ("code" . authorization_code)
                )))))

  (defun org-google-tasks-get-remote-list()
    "Get list of tasks from remote"
    (interactive)
    (let ((access_token (gethash "access_token" org-google-tasks-credential)))
      (request (concat org-google-tasks-api-url "/users/@me/lists")
        :type "GET"
        :params (list (cons "access_token" access_token))
        :parser  (lambda () (message "%s" (buffer-string))))))
  (defun org-google-tasks-insert-remote-list(name)
    ""
    (interactive "sTitle: ")
    (let ((access_token (gethash "access_token" org-google-tasks-credential)))
      (request (concat org-google-tasks-api-url "/users/@me/lists")
        :type "POST"
        :parser (lambda () (gethash "id" (json-parse-string (buffer-string))))
        :params (list (cons "access_token" access_token))
        :headers '(("Content-Type" . "application/json"))
        :data (json-encode (list (cons "title" name)))
        :complete
        (cl-function (lambda (&key data &allow-other-keys)
                       (setq org-google-tasks-temp-tasklist data))))))
                       
        
  (defun org-google-tasks-insert-remote-task(tasklist name &optional entry)
    ""
    (interactive "sTitle: ")
    (let ((access_token (gethash "access_token" org-google-tasks-credential)))
      (request (concat org-google-tasks-api-url "/lists/" tasklist "/tasks")
        :type "POST"
        :parser (lambda () (gethash "id" (json-parse-string (buffer-string))))
        :params (list (cons "access_token" access_token))
        :headers '(("Content-Type" . "application/json"))
        :data (json-encode (list (cons "title" name) (cons "notes" entry)))
        :complete
        (cl-function (lambda (&key data &allow-other-keys)
                       (setq org-google-tasks-temp-task data))))))
                       
        
    
(defun org-google-tasks-get-oauth-code ()
  ""
  (interactive)
  (browse-url (org-google-tasks-oauth-url))
  (puthash "authorization_code" (read-string "Authorization Code: ") org-google-tasks-credential)
  (org-google-tasks-save-credentials)
  )

(defun org-google-tasks-save-credentials()
  ""
  (interactive)
  (with-current-buffer (find-file-noselect org-google-tasks-credential-file)
    (kill-region (point-min) (point-max))
    (insert (json-encode org-google-tasks-credential))
    (save-buffer)))
(defun org-google-tasks-sync-file(&optional file)
  ""
  (interactive)
  (if (equal file nil)
      (setq file (buffer-file-name)))
  (with-current-buffer (find-file-noselect file)
    (org-map-entries 'org-google-tasks-sync-entry)))
    

    (defun org-google-tasks-sync-entry()
      ""
      (interactive)
      (let* (
             (access-token (gethash "access_token" org-google-tasks-credential))
             (google-task-id (org-entry-get nil "GOOGLE_TASKS"))
             (heading (org-get-heading t t t t))
             (todo (org-get-todo-state))
             (tags (org-get-tags)) 
             (entry (org-get-entry))
             (tasklist (upcase-initials (car (split-string (buffer-name) "\\."))))
             (google-tasklist-id (car (cdr (car (org-collect-keywords '("GOOGLE_TASKLIST"))))))
             (priority (org-get-priority (org-get-heading)))
             (scheduled (org-get-priority (org-get-heading)))
             )
        (unless google-tasklist-id
          (message "H: %s" tasklist)
          (org-google-tasks-insert-remote-list tasklist)
          (save-excursion
            (beginning-of-buffer)
            (re-search-forward "^* ")
            (previous-line)
            (end-of-line)
            (insert (concat "\n#+GOOGLE_TASKLIST: " org-google-tasks-temp-tasklist))))
        (setq google-tasklist-id org-google-tasks-temp-tasklist)
        (unless (and google-task-id (equal todo "TODO"))
          (message "Inserting: %s" heading)
          (org-google-tasks-insert-remote-task google-tasklist-id heading entry)
          (org-set-property "GOOGLE_TASKS" org-google-tasks-temp-task)
        )
      ))
   
         
