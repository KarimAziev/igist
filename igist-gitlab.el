;;; igist-gitlab.el --- GitLab snippets support for igist -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/igist
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; GitLab snippets support for `igist'.

;; This file adapts the GitLab snippets API
;; (https://docs.gitlab.com/api/snippets/) to igist.  GitLab API
;; responses are converted (normalized) into the same internal
;; representation that GitHub gists use, which allows reusing the
;; tabulated list, editing and commenting machinery of `igist'.
;; Normalized snippets carry a `(forge . gitlab)' marker that
;; commands use to select the appropriate endpoints and credentials.

;; Commands:

;; M-x `igist-gitlab-list-snippets' - Display your GitLab snippets as table.
;; M-x `igist-gitlab-explore-public-snippets' - List public GitLab snippets.
;; M-x `igist-gitlab-create-new-snippet' - Create a new GitLab snippet.
;; M-x `igist-gitlab-new-snippet-from-buffer' - Create a snippet from buffer.

;;; Code:

(require 'url-util)
(require 'igist)

(defcustom igist-gitlab-host "gitlab.com/api/v4"
  "The GitLab API host used for snippet requests.

For GitLab.com this should be \"gitlab.com/api/v4\".  For self-hosted
GitLab instances use \"HOST/api/v4\", e.g. \"gitlab.example.com/api/v4\"."
  :group 'igist
  :type 'string)

(defcustom igist-gitlab-user-name ""
  "The GitLab username for the current user.

This is the username used by `igist' for interactions with the GitLab
snippets API, analogously to how `igist-current-user-name' is used for
GitHub gists."
  :group 'igist
  :type 'string)

(defcustom igist-gitlab-auth-marker 'igist
  "GitLab personal access token or suffix added to USERNAME^MARKER in auth sources.

This variable works like `igist-auth-marker', but for GitLab.  It can
either hold an explicit personal access token as a string, or a symbol
that is used as a suffix for the `login' field in `auth-sources'.

For example, if you set this to the symbol `igist' (the default
setting), you would add an entry like this to your auth-sources:

\"machine gitlab.com/api/v4 login GITLAB_USERNAME^igist password GITLAB_TOKEN\".

The token needs at least the `api' (or `read_api' for read-only usage)
scope."
  :group 'igist
  :type '(radio
          (string :tag "Personal Access Token")
          (symbol :tag "Suffix" igist)))

(defvar igist-gitlab-explore-buffer-name "*igist-gitlab-explore*"
  "Buffer name for tabulated display of public GitLab snippets.")

(defun igist-gitlab-get-user-name ()
  "Return the current GitLab user's name if it's a non-empty string."
  (when (and igist-gitlab-user-name
             (stringp igist-gitlab-user-name)
             (not (string-empty-p igist-gitlab-user-name)))
    igist-gitlab-user-name))

;;; GitLab snippets

;; The functions in this section adapt the GitLab snippets API
;; (https://docs.gitlab.com/api/snippets/) to igist.  GitLab API
;; responses are converted (normalized) into the same internal
;; representation that GitHub gists use, which allows reusing the
;; tabulated list, editing and commenting machinery.  Normalized
;; snippets carry a `(forge . gitlab)' marker that commands use to
;; select the appropriate endpoints and credentials.

(defun igist-gitlab--normalize-id (id)
  "Normalize GitLab snippet or note ID to a string."
  (cond ((stringp id) id)
        ((numberp id) (number-to-string id))
        (t (format "%s" id))))

(defun igist-gitlab--file-language (filename)
  "Guess a human-readable language name for FILENAME.

Uses `auto-mode-alist' to detect the mode associated with FILENAME."
  (when filename
    (when-let* ((cell (ignore-errors
                        (seq-find (lambda (it)
                                    (and (stringp (car it))
                                         (string-match-p (car it) filename)))
                                  auto-mode-alist)))
                (mode-name (cdr cell)))
      (when (symbolp mode-name)
        (string-join (mapcar #'capitalize
                              (split-string
                               (string-remove-suffix
                                "-mode"
                                (symbol-name mode-name))
                               "-" t))
                     " ")))))

(defun igist-gitlab--api-raw-url (id file)
  "Return the GitLab API URL serving raw content of FILE in snippet ID.

FILE is an alist with `path' and `raw_url' cells, as returned by the
GitLab snippets API.  The web raw URL stored in `raw_url' requires a
browser session for private snippets, while the API URL constructed
here works with the `PRIVATE-TOKEN' header."
  (let ((path (igist-alist-get-symb 'path file))
        (web-url (igist-alist-get-symb 'raw_url file)))
    (when path
      (format "https://%s/snippets/%s/files/%s/%s/raw"
              igist-gitlab-host
              id
              (if (and web-url
                       (string-match "/raw/\\([^/]+\\)/" web-url))
                  (match-string 1 web-url)
                "main")
              (url-hexify-string path)))))

(defun igist-gitlab--normalize-snippet (snippet)
  "Convert SNIPPET from the GitLab API to igist's internal gist format.

GitLab snippet titles are mapped to the gist `description', snippet
visibility to the boolean `public' flag and the snippet author to the
gist `owner'.  The original `visibility' and `description' values are
preserved in the `gitlab_visibility' and `gitlab_description' cells,
and the returned alist contains a `(forge . gitlab)' marker."
  (let ((id (igist-gitlab--normalize-id (igist-alist-get-symb 'id snippet)))
        (visibility (igist-alist-get-symb 'visibility snippet)))
    (list
     (cons 'id id)
     (cons 'description (or (igist-alist-get-symb 'title snippet) ""))
     (cons 'public (equal visibility "public"))
     (cons 'html_url (igist-alist-get-symb 'web_url snippet))
     (cons 'url (format "https://%s/snippets/%s" igist-gitlab-host id))
     (cons 'owner
           (list (cons 'login
                       (igist-alist-get-symb
                        'username
                        (igist-alist-get-symb 'author snippet)))))
     (cons 'created_at (igist-alist-get-symb 'created_at snippet))
     (cons 'updated_at (igist-alist-get-symb 'updated_at snippet))
     (cons 'comments (or (igist-alist-get-symb 'user_notes_count snippet) 0))
     (cons 'forge 'gitlab)
     (cons 'gitlab_visibility visibility)
     (cons 'gitlab_description (igist-alist-get-symb 'description snippet))
     (cons 'git_pull_url (igist-alist-get-symb 'http_url_to_repo snippet))
     (cons 'git_push_url (igist-alist-get-symb 'ssh_url_to_repo snippet))
     (cons 'files
           (delq nil
                 (mapcar
                  (lambda (file)
                    (when-let* ((path (igist-alist-get-symb 'path file)))
                      (list (intern path)
                            (cons 'filename path)
                            (cons 'raw_url (igist-gitlab--api-raw-url id file))
                            (cons 'language (igist-gitlab--file-language path))
                            (cons 'web_raw_url
                                  (igist-alist-get-symb 'raw_url file)))))
                  (igist-alist-get-symb 'files snippet)))))))

(defun igist-gitlab--normalize-snippets (snippets)
  "Convert a list of GitLab SNIPPETS to igist's internal gist format."
  (mapcar #'igist-gitlab--normalize-snippet snippets))

(defun igist-gitlab--normalize-note (note)
  "Convert GitLab snippet NOTE to igist's internal comment format.

System notes (e.g. \"changed title\") are skipped, returning nil."
  (unless (eq (igist-alist-get-symb 'system note) t)
    (cons (cons 'user
                (list (cons 'login
                            (or (igist-alist-get-symb
                                 'username
                                 (igist-alist-get-symb 'author note))
                                ""))))
          note)))

(defun igist-gitlab--normalize-notes (notes)
  "Convert GitLab NOTES to igist comments, dropping system notes."
  (delq nil (mapcar #'igist-gitlab--normalize-note notes)))

(defun igist-gitlab--auth-secret (host user)
  "Return the auth-source secret for HOST and USER, or nil."
  (when-let* ((plist (car-safe (auth-source-search :host host
                                                   :user user
                                                   :require '(:secret)
                                                   :max 1)))
              (secret (plist-get plist :secret)))
    (if (functionp secret) (funcall secret) secret)))

(defun igist-gitlab--token ()
  "Return the GitLab personal access token for `igist-gitlab-host', or nil.

The token is read either from `igist-gitlab-auth-marker' when it holds
a string, or from `auth-sources' using the marker symbol."
  (cond ((and (stringp igist-gitlab-auth-marker)
              (not (string-empty-p igist-gitlab-auth-marker)))
         igist-gitlab-auth-marker)
        ((and (symbolp igist-gitlab-auth-marker)
              (igist-gitlab-get-user-name))
         (let ((user (concat (igist-gitlab-get-user-name)
                             "^"
                             (symbol-name igist-gitlab-auth-marker))))
           (or (igist-gitlab--auth-secret igist-gitlab-host user)
               (igist-gitlab--auth-secret
                (car (split-string igist-gitlab-host "/" t))
                user))))))

(defun igist-gitlab--download-headers (gist)
  "Return extra HTTP headers for fetching raw files of GIST from GitLab."
  (when (eq (igist-gist-forge gist) 'gitlab)
    (when-let* ((token (igist-gitlab--token)))
      (list (cons "PRIVATE-TOKEN" token)))))

(defun igist-gitlab-get-user-buffer-name (user)
  "Return a buffer name for GitLab USER's snippets list."
  (when user
    (concat "*igist-gitlab-" user "*")))

(defun igist-gitlab-list-request (resource &optional explore background callback callback-args)
  "Fetch and possibly display a list of GitLab snippets from RESOURCE.

RESOURCE is the API path for the request, e.g. \"/snippets\" for the
authenticated user's snippets or \"/snippets/public\" for public snippets.

If EXPLORE is non-nil, the buffer uses `igist-explore-mode', otherwise
`igist-list-mode' is used.

If optional argument BACKGROUND is non-nil, the buffer will not be shown.

Optional argument CALLBACK is a function that will be called when the request is
completed.

Optional argument CALLBACK-ARGS is a list of arguments that will be passed to
the CALLBACK function.

Loading next pages can be stopped by the command `igist-list-cancel-load'."
  (pcase-let ((`(,buffer . ,mode)
               (if explore
                   (cons (get-buffer-create igist-gitlab-explore-buffer-name)
                         'igist-explore-mode)
                 (cons (get-buffer-create
                        (igist-gitlab-get-user-buffer-name
                         (igist-gitlab-get-user-name)))
                       'igist-list-mode))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'igist-list-mode)
        (funcall mode)
        (igist-tabulated-list-init-header))
      (setq igist-buffer-forge 'gitlab)
      (if igist-list-loading
          (progn
            (setq igist-list-loading nil)
            (igist-spinner-stop)
            (setq igist-list-cancelled
                  (lambda ()
                    (igist-gitlab-list-request resource explore
                                               background
                                               callback
                                               callback-args))))
        (setq igist-list-loading t)
        (setq igist-list-page 1)
        (igist-spinner-show)
        (let ((per-page (igist-list-get-per-page-query buffer))
              (normalized-acc '()))
          (igist--request "GET" resource
                          nil
                          :forge 'gitlab
                          :host igist-gitlab-host
                          :username (igist-gitlab-get-user-name)
                          :auth (if (igist-gitlab-get-user-name)
                                    igist-gitlab-auth-marker
                                  'none)
                          :reader #'igist--read-json-payload
                          :query `((per_page . ,per-page))
                          :errorback
                          (lambda (err &rest _args)
                            (igist-show-request-error err)
                            (when buffer
                              (igist-with-exisiting-buffer
                                  buffer
                                (setq igist-list-loading nil)
                                (setq igist-list-cancelled nil)
                                (igist-spinner-stop))))
                          :callback
                          (lambda (value _headers _status req)
                            (condition-case nil
                                (progn
                                  (let ((new-raw
                                         (nthcdr (length normalized-acc)
                                                 value)))
                                    (setq normalized-acc
                                          (append normalized-acc
                                                  (igist-gitlab--normalize-snippets
                                                   new-raw))))
                                  (setf (ghub-req-extra req) per-page)
                                  (igist-list-loaded-callback buffer normalized-acc
                                                              req
                                                              callback
                                                              callback-args))
                              (error (setq igist-list-cancelled nil)
                                     (setq igist-list-loading nil)
                                     (igist-spinner-stop))))))))
    (unless background
      (igist-ensure-buffer-visible buffer))
    buffer))



;;;###autoload
(defun igist-gitlab-list-snippets (&optional background)
  "List the GitLab snippets of `igist-gitlab-user-name'.

Loading of subsequent pages may be halted by the command
`igist-list-cancel-load'.  If optional argument BACKGROUND is non-nil,
the buffer is not shown."
  (interactive)
  (while (not (igist-gitlab-get-user-name))
    (setq igist-gitlab-user-name (read-string "GitLab user: ")))
  (igist-gitlab-list-request "/snippets" nil background))

;;;###autoload
(defun igist-gitlab-explore-public-snippets (&optional background)
  "List public GitLab snippets, sorted by the most recently updated.

Loading of subsequent pages may be halted by the command
`igist-list-cancel-load'.  If optional argument BACKGROUND is non-nil,
the buffer is not shown."
  (interactive)
  (igist-gitlab-list-request "/snippets/public" t background))

(defun igist-gitlab--visibility (gist current-public)
  "Return the GitLab visibility string to save GIST with.

CURRENT-PUBLIC is the buffer-local visibility flag.  If it was not
changed relative to GIST's `public' flag, preserve the original
visibility string of GIST, so that \"internal\" snippets stay
internal."
  (let ((orig-public (and (igist-alist-get-symb 'public gist) t))
        (orig-visibility (igist-alist-get-symb 'gitlab_visibility gist)))
    (if (equal orig-public (and current-public t))
        (or orig-visibility
            (if current-public "public" "private"))
      (if current-public "public" "private"))))

(defun igist-gitlab--make-file-payload (orig-filename new-filename content)
  "Return a list of GitLab file actions for saving a snippet file.

ORIG-FILENAME is the file's current name in the snippet (nil for new
files), NEW-FILENAME is the name it should be saved as and CONTENT is
the file content.  Renames use a `move' action followed by an `update'
action, since GitLab requires them separately."
  (cond ((not orig-filename)
         (list
          (list
           (cons 'action "create")
           (cons 'file_path new-filename)
           (cons 'content content))))
        ((equal orig-filename new-filename)
         (list
          (list
           (cons 'action "update")
           (cons 'file_path orig-filename)
           (cons 'content content))))
        (t
         (list
          (list
           (cons 'action "move")
           (cons 'previous_path orig-filename)
           (cons 'file_path new-filename))
          (list
           (cons 'action "update")
           (cons 'file_path new-filename)
           (cons 'content content))))))

(defun igist-gitlab-save-existing-snippet (buffer &optional callback)
  "Save the GitLab snippet edited in BUFFER.
If CALLBACK is non-nil, call it without arguments."
  (let* ((gist (buffer-local-value 'igist-current-gist buffer))
         (orig-filename (igist-alist-get-symb 'filename gist))
         (new-filename (buffer-local-value 'igist-current-filename buffer))
         (content (with-current-buffer buffer
                    (save-excursion
                      (save-restriction
                        (widen)
                        (buffer-substring-no-properties
                         (point-min)
                         (point-max)))))))
    (igist-put (concat "/snippets/" (igist-alist-get-symb 'id gist))
               nil
               :forge 'gitlab
               :payload `((title . ,(or (buffer-local-value
                                         'igist-current-description buffer)
                                        (igist-alist-get-symb
                                         'description
                                         gist)
                                        ""))
                          (visibility . ,(igist-gitlab--visibility
                                          gist
                                          (buffer-local-value
                                           'igist-current-public buffer)))
                          (files . ,(vconcat
                                     (igist-gitlab--make-file-payload
                                      orig-filename
                                      new-filename
                                      content))))
               :buffer buffer
               :callback
               (lambda (val &rest _ignored)
                 (if-let* ((id (igist-alist-get-symb 'id val)))
                     (igist-with-exisiting-buffer
                         buffer
                       (let* ((normalized (igist-gitlab--normalize-snippet val))
                              (new-gist (igist-normalize-gist-file
                                         normalized
                                         new-filename)))
                         (unless (equal orig-filename new-filename)
                           (rename-buffer (concat id "-" new-filename))
                           (setq buffer-file-name (concat
                                                   (temporary-file-directory)
                                                   (buffer-name)))
                           (set-auto-mode t)
                           (igist-edit-mode))
                         (igist-setup-local-vars new-gist new-filename)
                         (setq igist-current-gist-loading nil
                               igist-current-gist-error nil)
                         (igist-update-gist-header-line)
                         (set-buffer-modified-p nil)
                         (when (memq igist-enable-copy-gist-url-p
                                     '(t after-update))
                           (when-let* ((url (igist-get-current-gist-url)))
                             (kill-new url)
                             (igist-message "Copied %s" url)))
                         (when callback
                           (funcall callback))))
                   (igist-message "igist: Couldn't save snippet."))
                 (igist-with-exisiting-buffer
                     (igist-gitlab-get-user-buffer-name
                      (igist-gitlab-get-user-name))
                   (igist-gitlab-list-request "/snippets" nil t))))))

(defun igist-gitlab-save-new-snippet (buffer &optional callback)
  "Save the new GitLab snippet in BUFFER, then execute CALLBACK."
  (let ((file (buffer-local-value 'igist-current-filename buffer)))
    (igist-post "/snippets" nil
                :forge 'gitlab
                :payload
                `((title . ,(or (buffer-local-value
                                 'igist-current-description buffer)
                                (when (eq igist-ask-for-description
                                          'before-save)
                                  (read-string "Title: "
                                               (and igist-current-filename
                                                    (igist--generate-description
                                                     igist-current-filename))))
                                ""))
                  (visibility . ,(if (buffer-local-value
                                      'igist-current-public buffer)
                                     "public"
                                   "private"))
                  (files . [((file_path . ,file)
                             (content .
                              ,(with-current-buffer buffer
                                 (save-excursion
                                   (save-restriction
                                     (widen)
                                     (buffer-substring-no-properties
                                      (point-min)
                                      (point-max)))))))]))
                :buffer buffer
                :callback
                (lambda (value &rest _)
                  (when value
                    (igist-with-exisiting-buffer
                        buffer
                      (set-buffer-modified-p nil)
                      (let* ((normalized
                              (igist-gitlab--normalize-snippet value))
                             (new-gist (igist-normalize-gist-file normalized
                                                                  file)))
                        (rename-buffer (concat (igist-alist-get-symb 'id value)
                                               "-"
                                               file))
                        (igist-setup-local-vars new-gist file)
                        (setq igist-current-gist-loading nil
                              igist-current-gist-error nil)
                        (igist-update-gist-header-line))
                      (when (memq igist-enable-copy-gist-url-p '(t after-new))
                        (when-let* ((url (igist-get-current-gist-url)))
                          (kill-new url)
                          (igist-message "Copied %s" url)))
                      (when callback
                        (funcall callback)))
                    (igist-gitlab-list-snippets t))))))

;;;###autoload
(defun igist-gitlab-create-new-snippet ()
  "Set up and switch to a buffer for creating a new GitLab snippet.

If Transient Mark mode is enabled and the mark is active, insert it as
initial content."
  (interactive)
  (while (not (igist-gitlab-get-user-name))
    (setq igist-gitlab-user-name (read-string "GitLab user: ")))
  (let ((region-content (igist-get-region-content)))
    (let ((filename (read-string "Filename: "
                                 (when region-content
                                   (igist-suggest-filename)))))
      (pop-to-buffer (igist-setup-new-gist-buffer filename
                                                  (or region-content "")
                                                  'gitlab)))))

;;;###autoload
(defun igist-gitlab-new-snippet-from-buffer ()
  "Create a new GitLab snippet from the current buffer's contents."
  (interactive)
  (while (not (igist-gitlab-get-user-name))
    (setq igist-gitlab-user-name (read-string "GitLab user: ")))
  (when-let* ((content (or (igist-get-region-content)
                           (buffer-substring-no-properties (point-min)
                                                           (point-max)))))
    (let ((filename (read-string "Filename: "
                                 (if buffer-file-name
                                     (file-name-nondirectory buffer-file-name)
                                   (buffer-name)))))
      (pop-to-buffer
       (igist-setup-new-gist-buffer filename content 'gitlab)
       nil
       t))))

(defun igist-gitlab--remove-deleted-entry (id)
  "Mark snippet with ID as deleted in all GitLab snippet list buffers."
  (dolist (buff (buffer-list))
    (when (buffer-live-p buff)
      (with-current-buffer buff
        (when (and (derived-mode-p 'igist-list-mode)
                   (eq igist-buffer-forge 'gitlab))
          (when-let* ((data (and igist-rendered-hash
                                 (gethash id igist-rendered-hash))))
            (unless (assq 'deleted data)
              (setq data (push '(deleted . t) data))
              (puthash id data igist-rendered-hash)
              (save-excursion
                (igist-remove-entry id)))))))))

(defun igist-gitlab--clone-url (gist protocol)
  "Return the git clone URL for a normalized GitLab snippet GIST.

PROTOCOL is either \"git@\" or \"https://\".  The snippet's
`git_push_url' (SSH) or `git_pull_url' (HTTP) cells are used when
available, falling back to deriving the URL from the snippet's web URL."
  (let ((http-url (igist-alist-get-symb 'git_pull_url gist))
        (ssh-url (igist-alist-get-symb 'git_push_url gist))
        (web-url (igist-alist-get-symb 'html_url gist)))
    (if (and protocol (string-prefix-p "http" protocol))
        (or http-url
            (and web-url (concat web-url ".git")))
      (or ssh-url
          (when web-url
            (require 'url-parse)
            (let ((parsed (url-generic-parse-url web-url)))
              (concat "git@" (url-host parsed) ":"
                      (string-remove-prefix "/" (url-filename parsed))
                      ".git")))
          (user-error "Cannot determine clone URL for this snippet")))))


(provide 'igist-gitlab)
;;; igist-gitlab.el ends here
