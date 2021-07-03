;;; org-google-tasks.el --- Does something cool  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021 Sidharth Arya

;; Author: Sidharth Arya <sidhartharya10@gmail.com>
;; Maintainer: Someone Else <someone@example.com>
;; Keywords: languages
;; URL: https://example.com/foo

;; This file is not part of GNU Emacs.

;; This file is free software…
…
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
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

(defcustom org-google-tasks-credential-file "~/.emacs.d/google-tasks"
  "")
(defcustom org-google-tasks-create-new-tasklist nil
  "")

(defcustom org-google-tasks-use-inheritance nil
  "")
(defvar org-google-tasks-temp-tasklist nil)
(defvar org-google-tasks-temp-task nil)
(defvar org-google-tasks-remote-list nil)
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

  (puthash "refresh_token" nil org-google-tasks-credential)
  (concat "https://accounts.google.com/o/oauth2/auth?scope=" org-google-tasks-auth-scope "&response_type=code&access_type=offline&redirect_uri=urn:ietf:wg:oauth:2.0:oob&client_id=" (gethash "client_id" org-google-tasks-credential))
  )
(defun org-google-tasks-get-tokens ()
  ""
  (interactive)
  (let* ((client_id (gethash "client_id" org-google-tasks-credential))
         (client_secret (gethash "client_secret" org-google-tasks-credential))
         (refresh_token (gethash "refresh_token" org-google-tasks-credential))
         (authorization_code (gethash "authorization_code" org-google-tasks-credential)))
    (when (or (equal refresh_token "null") (equal refresh_token ':null))
      (org-google-tasks-get-oauth-code)
      (setq refresh_token (gethash "refresh_token" org-google-tasks-credential)))
    (if refresh_token
        (request org-google-tasks-auth-token-url
          :type "POST"
          :sync t
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
        :sync t
        :parser (lambda() (puthash "access_token" (gethash "access_token" (json-parse-string (buffer-string))) org-google-tasks-credential)(puthash "refresh_token" (gethash "refresh_token" (json-parse-string (buffer-string))) org-google-tasks-credential) (message "%s" (buffer-string)))
        :data (list
               (cons "client_id" client_id)
                (cons "client_secret" client_secret)
                (cons "redirect_uri" "urn:ietf:wg:oauth:2.0:oob")
                (cons "grant_type" "authorization_code")
                (cons "code" authorization_code)
                ))))
  (puthash "date" (format-time-string "%s" (current-time)) org-google-tasks-credential)
  (org-google-tasks-save-credentials))

(defun org-google-tasks-get-remote-list(&optional sync force)
  "Get list of tasks from remote"
  (interactive)
  (if (and (not force) org-google-tasks-remote-list)
      org-google-tasks-remote-list
    (progn
      (let ((access_token (gethash "access_token" org-google-tasks-credential)))
        (request (concat org-google-tasks-api-url "/users/@me/lists")
          :type "GET"
          :sync sync
          :params (list (cons "access_token" access_token))
          :parser  (lambda () (setq org-google-tasks-response (message "%s" (buffer-string))))))
    (setq org-google-tasks-remote-list org-google-tasks-response))))

(defun org-google-tasks-get-remote-tasks(tasklist &optional sync)
  "Get list of tasks from remote"
  (interactive)
  (let ((access_token (gethash "access_token" org-google-tasks-credential)))
    (request (concat org-google-tasks-api-url "/lists/"  tasklist "/tasks")
      :type "GET"
      :sync sync
      :params (list (cons "access_token" access_token))
      :parser  (lambda () (setq org-google-tasks-response (message "%s" (buffer-string)))))))
(defun org-google-tasks-insert-remote-tasklist(name &optional sync)
  ""
  (interactive "sTitle: ")
  (let ((access_token (gethash "access_token" org-google-tasks-credential)))
    (request (concat org-google-tasks-api-url "/users/@me/lists")
      :type "POST"
      :parser (lambda () (gethash "id" (json-parse-string (buffer-string))))
      :params (list (cons "access_token" access_token))
      :headers '(("Content-Type" . "application/json"))
      :data (json-encode (list (cons "title" name)))
      :sync sync
      :complete
      (cl-function (lambda (&key data &allow-other-keys)
                     (setq org-google-tasks-temp-tasklist data))))))


(defun org-google-tasks-insert-remote-task(tasklist name &optional entry due sync parent)
  ""
  (interactive "sTitle: ")
  (let ((access_token (gethash "access_token" org-google-tasks-credential)))
    (request (concat org-google-tasks-api-url "/lists/" tasklist "/tasks")
      :type "POST"
      :parser (lambda () (gethash "id" (json-parse-string (buffer-string))))
      :params (list (cons "access_token" access_token)
              (and parent (cons "parent" parent))
                    )
      :headers '(("Content-Type" . "application/json"))
      :sync sync
      :data (json-encode
             (list
              (cons "title" name)
              (cons "notes" entry)
              (cons "due" due)
              ))
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
    (delete-region (point-min) (point-max))
    (insert (json-encode org-google-tasks-credential))
    (save-buffer)))
(defun org-google-tasks-sync-file(&optional file)
  ""
  (interactive)
  (if (equal file nil)
      (setq file (buffer-file-name)))
  (with-current-buffer (find-file-noselect file)
    (let ((size (point-max)))
      (org-map-entries 'org-google-tasks-sync-entry)
      (unless (equal size (point-max))
        (org-google-tasks-get-remote-list t t)))))
  


(defun org-google-tasks-sync-entry(&optional sync_tasklist)
  ""
  (interactive)
  (let* (
         (access-token (gethash "access_token" org-google-tasks-credential))
         (google-task-id (org-entry-get nil "GOOGLE_TASK_ID"))
         (heading (org-get-heading t t t t))
         (todo (org-get-todo-state))
         (tags (org-get-tags)) 
         (uentry (split-string (car (split-string (org-get-entry) "* ")) ":END:"))
         (entry (if (equal (length uentry) 2)
                    (car (cdr uentry))
                  (car uentry)))
         (inproperty (org-entry-get nil "GOOGLE_TASKLIST" org-google-tasks-use-inheritance))
         (tasklist (or (org-entry-get nil "GOOGLE_TASKLIST" org-google-tasks-use-inheritance) (car (cdr (car (org-collect-keywords '("GOOGLE_TASKLIST"))))) (upcase-initials (car (split-string (buffer-name) "\\.")))))
         (google-tasklist-id (if inproperty (org-entry-get nil "GOOGLE_TASKLIST_ID" org-google-tasks-use-inheritance) (car (cdr (car (org-collect-keywords '("GOOGLE_TASKLIST_ID")))))))
         (priority (org-get-priority (org-get-heading)))
         (due (format-time-string "%Y-%m-%dT%H:%M:%S.000Z" (or (org-get-scheduled-time (point)) (org-get-deadline-time (point)))))
         )
    (unless google-tasklist-id
      (setq google-tasklist-id (car (org-google-tasks-get-tasklist-id-by-name tasklist)))
      (if google-tasklist-id
      (message "Exist Tasklist: %s" tasklist))
      (unless google-tasklist-id
        (message "Insert Tasklist: %s" tasklist)
        (org-google-tasks-insert-remote-tasklist tasklist t)
        (setq google-tasklist-id org-google-tasks-temp-tasklist))
      (if inproperty
          (org-set-property "GOOGLE_TASKLIST_ID" google-tasklist-id)
        (save-excursion
          (beginning-of-buffer)
          (re-search-forward "^* ")
          (previous-line)
          (end-of-line)
          (insert (concat "\n#+GOOGLE_TASKLIST_ID: " google-tasklist-id))
          (message "%s" google-tasklist-id))))
    
    (if sync_tasklist
        (org-google-tasks-get-remote-list t t))
    
    (when (and (not google-task-id) (equal todo "TODO"))
          (message "Inserting Task: %s" heading)
          (org-google-tasks-insert-remote-task google-tasklist-id heading entry due t)
          (setq google-task-id org-google-tasks-temp-task)
          (message "%s" google-task-id)
          (org-set-property "GOOGLE_TASK_ID" google-task-id)
        )))
    
         
(defun org-google-tasks-get-tasklist-id-by-name(name)
  ""
  (interactive)
  (org-google-tasks-get-remote-list t)
  (delete 'nil (mapcar (lambda (a) (if (equal name (cdr (assoc 'title a))) (cdr (assoc 'id a)))) (cdr (assoc 'items (json-read-from-string (format org-google-tasks-remote-list)))))))

(defun org-google-tasks-insert-list(list tasklist-name)
  ""
  (interactive)
  (let*
      ((tasklist-id (car (org-google-tasks-get-tasklist-id-by-name tasklist-name)))
       (due (format-time-string "%Y-%m-%dT%H:%M:%S.000Z" (current-time))))
    (mapcar (lambda (a) (org-google-tasks-insert-remote-task tasklist-id a "" due t)) list)
    ))

(defun org-google-tasks-check-tasklist-today(tasklist-name)
  ""
  (interactive)
  (let*
      ((tasklist-id (car (org-google-tasks-get-tasklist-id-by-name tasklist-name)))
       (due (format-time-string "%Y-%m-%dT00:00:00.000Z" (current-time))))
    (org-google-tasks-get-remote-tasks tasklist-id t)
    (if (assoc 'items (json-read-from-string org-google-tasks-response))
        (delete 'nil (mapcar (lambda(a) (equal due (cdr (assoc 'due a)))) (cdr (assoc 'items (json-read-from-string org-google-tasks-response)))))
      nil)
    ))

(defun org-google-tasks-check-date()
  ""
  (> (- (string-to-number (format-time-string "%s" (current-time))) (string-to-number (gethash "date" org-google-tasks-credential))) 3600)
  )
(defun org-google-tasks-convert-time (time)
  (format-time-string "%Y-%m-%dT%H:%M:%S.000Z" time))
  
(provide 'org-google-tasks)
;; org-google-tasks.el e
