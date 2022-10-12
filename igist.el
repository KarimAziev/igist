;;; igist.el --- List, create, update and delete GitHub gists -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/igist
;; Version: 0.8.0
;; Keywords: tools
;; Package-Requires: ((emacs "28.1") (ghub "3.5.1"))

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

;; Edit, create and view your github gists.

;; Usage

;;  `igist-dispatch' - to invoke transient popup with the list of available commands

;; Display commands:

;; M-x `igist-list-gists' - to display your gists as table.
;; M-x `igist-list-other-user-gists' - to display public gists of any user.
;; M-x `igist-edit-list' - Read user gists in minibuffer and open it in edit buffer.

;;; Create commands:

;; M-x `igist-create-new-gist'
;;      Setup new gist buffer with currently active region content or empty.

;; M-x `igist-new-gist-from-buffer' (&rest _ignore)
;;      Setup new gist buffer whole buffer contents.

;; M-x `igist-list-add-file'
;;      Add new file name to gist at point.

;; M-x `igist-fork-gist'
;;      Fork gist at point in `igist-list-mode' or currently opened.

;; Delete commands:

;; M-x `igist-delete-current-gist'
;;      Delete current gist with all files.

;; M-x `igist-delete-current-filename'
;;      Delete current file from gist.

;; M-x `igist-delete-other-gist-or-file' (gist)
;;      Delete GIST with id.

;; M-x `igist-kill-all-gists-buffers'
;;      Delete all gists buffers.

;;; Edit commands:

;; M-x `igist-list-view-current'
;;      Fetch tabulated gist entry at point.

;; M-x `igist-list-edit-gist-at-point' (&optional _entry)
;;      Open tabulated GIST-ITEM at point in edit buffer.

;; M-x `igist-browse-gist'
;;      Browse gist at point or currently open.

;; M-x `igist-save-current-gist-and-exit'
;;      Save current gist.

;; M-x `igist-save-current-gist'
;;      Save current gist.

;; M-x `igist-read-filename' (&rest _args)
;;      Update filename for current gist without saving.

;; M-x `igist-read-description' (&rest _args)
;;      Update description for current gist without saving.

;; M-x `igist-add-file-to-gist'
;;      Add new file to existing gist.

;; M-x `igist-toggle-public' (&rest _)
;;      Toggle value of variable `igist-current-public'.

;; M-x `igist-list-edit-description' (&rest _)
;;      Edit description for current gist at point in tabulated list mode.


;; Comments commands:

;; M-x `igist-post-comment'
;;      Post current comment.

;; M-x `igist-delete-comment-at-point' (&rest _)
;;      Add or edit comment for gist at point or edit buffer.

;; M-x `igist-add-or-edit-comment' (&rest _)
;;      Add or edit comment for gist at point or edit buffer.

;; M-x `igist-add-comment' (&rest _)
;;      Add new comment for gist.

;; M-x `igist-load-comments' (&rest _)
;;      Load comments for gist at point or edit buffer.

;; User commands:

;; M-x `igist-change-user' (&rest _)
;;      Change user for retrieving gist.

;;; Customization

;; `igist-auth-marker' - default valie is `igist'.

;;  The auth marker in Auth-Sources appended to username and divided with ^.

;;  For example, if the value of marker is `igist',
;;  you need to add such entry:

;;  machine api.github.com login GITHUB_USERNAME^igist password GITHUB_TOKEN.

;; `igist-per-page-limit'
;;  The number of results per page (max 100).

;; `igist-ask-for-description'
;;   When to prompt for description before posting new gists.

;; `igist-mode-for-comments'
;;  Major mode when editing and viewing comments.

;;  Program `pandoc' should be installed for `org-mode' (currently not implemented).

;; `igist-list-format'
;;    Format for gist list.

;;; Keymaps

;; `igist-list-mode-map'
;;   Keymap for display gists as table.

;; `igist-edit-mode-map'
;;    Keymap for edit gist buffer.

;; `igist-comments-edit-mode-map'
;;      Keymap for posting and editing comments.

;; `igist-comments-list-mode-map'
;;      A keymap used for displaying comments.

;;; Code:



(require 'transient)
(require 'timezone)
(require 'ghub)

(eval-when-compile
  (require 'subr-x))

(defvar igist-default-list-format
  '((id "Id" 10 nil (lambda (id gist)
                      (cons
                       (format "%s" id)
                       (list
                        'action
                        #'igist-list-edit-gist-at-point
                        'help-echo
                        (format "Edit gist %s" id)
                        'gist
                        gist))))
    (description "Description" 30 t (lambda (description gist)
                                      (if (equal igist-current-user-name
                                                 (igist-get-owner gist))
                                          (cons
                                           (format "%s" description)
                                           (list
                                            'action
                                            #'igist-list-edit-description
                                            'help-echo
                                            description
                                            'gist
                                            gist))
                                        description)))
    (visibility "Visibility" 10 t
                (lambda (public &rest _ )
                  (or
                   (and public
                        "public")
                   "private")))
    (updated_at "Updated" 20 t "%D %R")
    (comments "Comments" 10 t (lambda (comments &rest _)
                                (cons
                                 (format "%s" comments)
                                 (list
                                  'action
                                  #'igist-load-comments
                                  'help-echo
                                  "Show comments"))))
    (files "Files" 0 t
           (lambda (files gist)
             (let ((one-or-none (<= (length files) 1)))
               (concat
                (if one-or-none "" "\n")
                (mapconcat
                 (lambda (file)
                   (concat (if one-or-none
                               ""
                             "\t\t" )
                           (propertize
                            file
                            'filename file
                            'face 'link
                            'gist gist)))
                 files
                 "\n"))))))
  "Default format for tabulated gist display.")

(defcustom igist-list-format igist-default-list-format
  "Format for gist list."
  :type '(alist
          :key-type
          (choice
           (const :tag "Id" id)
           (const :tag "Visibility" visibility)
           (const :tag "Updated" updated_at)
           (const :tag "Description" description)
           (const :tag "Comments" comments)
           (const :tag "Files" files))
          :value-type
          (list
           (string :tag "Label")
           (integer :tag "Field length")
           (boolean :tag "Sortable")
           (choice
            (string :tag "Format")
            (function :tag "Formatter"))))
  :group 'igist)

(defcustom igist-mode-for-comments 'markdown-mode
  "Major mode when editing and viewing comments.
Program `pandoc' should be installed for `org-mode'."
  :type '(radio
          (const :tag "Org mode" org-mode)
          (const :tag "Markdown" markdown-mode)
          (function :tag "Other"))
  :group 'igist)

(defcustom igist-ask-for-description 'before-save
  "When to prompt for description before posting new gists."
  :type '(radio
          (const :tag "Never" nil)
          (const :tag "Immediately" immediately)
          (const :tag "Before posting" before-save))
  :group 'igist)

(defcustom igist-per-page-limit 30
  "The number of results per page (max 100)."
  :type 'integer
  :group 'igist)

(defvar igist-before-save-hook '()
  "A list of hooks run before posting gist.")

(defvar igist-current-user-name nil
  "Current github user.")

(defvar igist-other-username nil
  "Name of the user to get gists from.")

(defcustom igist-auth-marker 'igist
  "The auth marker in Auth-Sources appended to username and divided with \"^\".

For example, if the value of marker is `igist' (which is default value),
you need to add such entry:

\"machine api.github.com login GITHUB_USERNAME^igist password GITHUB_TOKEN\"."
  :group 'igist
  :type 'symbol)

(defvar igist-github-token-scopes '(gist)
  "The required Github API scopes.

You need the gist OAuth scope and a token.

You have to manually create or update the token at
https://github.com/settings/tokens.  This variable
only serves as documentation.")

(defvar-local igist-current-gist nil
  "Current gist in edit buffer.")

(defvar-local igist-current-description nil
  "Current gist description in edit buffer.")

(defvar-local igist-current-public nil
  "Whether the current gist in edit buffer is public or private.")

(defvar-local igist-current-filename nil
  "Current gist filename.")

(defvar-local igist-comment-gist-id nil
  "Current gist id in comment buffer.")

(defvar-local igist-comment-id nil
  "Current comment id.")

(defvar igist-gists-list-buffer-name "igists-"
  "Buffer name for tabulated gists display.")

(defvar-local igist-gists-response nil)
(defvar igist-normalized-gists nil)

(defvar igist-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'igist-save-current-gist-and-exit)
    (define-key map (kbd "C-c '") #'igist-save-current-gist-and-exit)
    (define-key map (kbd "C-c C-k") #'kill-current-buffer)
    (define-key map (kbd "M-o") #'igist-dispatch)
    (define-key map [remap save-buffer] #'igist-save-current-gist)
    map)
  "Keymap used in edit gist buffers.")

(defvar igist-comments-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'igist-post-comment)
    (define-key map (kbd "C-c C-k") #'kill-current-buffer)
    (define-key map (kbd "M-o") #'igist-dispatch)
    map)
  "Keymap for posting and editing comments.")

(defvar igist-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "+") #'igist-list-add-file)
    (define-key map (kbd "-") #'igist-delete-current-filename)
    (define-key map (kbd "g") #'igist-list-refresh)
    (define-key map (kbd "c") #'igist-load-comments)
    (define-key map (kbd "a") #'igist-add-comment)
    (define-key map (kbd "f") #'igist-fork-gist)
    (define-key map (kbd "e") #'igist-list-edit-description)
    (define-key map (kbd "b") #'igist-browse-gist)
    (define-key map (kbd "D") #'igist-delete-current-gist)
    (define-key map (kbd "RET") #'igist-list-edit-gist-at-point)
    (define-key map (kbd "C-j") #'igist-list-view-current)
    (define-key map (kbd "v") #'igist-list-view-current)
    (define-key map (kbd "K") #'igist-list-cancel-load)
    map)
  "Keymap used in tabulated gists views.")

(defvar igist-comments-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'igist-load-comments)
    (define-key map (kbd "D") #'igist-delete-comment-at-point)
    (define-key map (kbd "e") #'igist-add-or-edit-comment)
    (define-key map (kbd "+") #'igist-add-comment)
    (define-key map (kbd "-") #'igist-delete-comment-at-point)
    (define-key map (kbd "q") #'kill-current-buffer)
    map)
  "A keymap used for displaying comments.")

;; Macros
(defmacro igist-or (&rest functions)
  "Return an unary function which invoke FUNCTIONS until first non-nil result."
  (declare (debug t)
           (pure t)
           (side-effect-free t))
  `(lambda (it)
     (or
      ,@(mapcar (lambda (v)
                  (if (symbolp v)
                      `(,v it)
                    `(funcall ,v it)))
                functions))))

(defmacro igist-and (&rest functions)
  "Return an unary function which invoke FUNCTIONS until first nil result."
  (declare (debug t)
           (pure t)
           (side-effect-free t))
  `(lambda (it)
     (and
      ,@(mapcar (lambda (v)
                  (if (symbolp v)
                      `(,v it)
                    `(funcall ,v it)))
                functions))))

(defmacro igist-rpartial (fn &rest args)
  "Return a partial application of FN to right-hand ARGS.

ARGS is a list of the last N arguments to pass to FN. The result is a new
function which does the same as FN, except that the last N arguments are fixed
at the values with which this function was called."
  (declare (side-effect-free t))
  `(lambda (&rest pre-args)
     ,(car (list (if (symbolp fn)
                     `(apply #',fn (append pre-args (list ,@args)))
                   `(apply ,fn (append pre-args (list ,@args))))))))

(defmacro igist-converge (combine-fn &rest functions)
  "Return a function that apply COMBINE-FN with results of branching FUNCTIONS.
If first element of FUNCTIONS is vector, it will be used instead.

Example:

\(funcall (igist-converge concat [upcase downcase]) \"John\").
\(funcall (igist-converge concat upcase downcase) \"John\")

Result: \"JOHNjohn\"."
  `(lambda (&rest args)
     (apply
      ,@(if (symbolp combine-fn)
            `(#',combine-fn)
          (list combine-fn))
      (list
       ,@(mapcar (lambda (v)
                   (setq v (macroexpand v))
                   (if (symbolp v)
                       `(apply #',v args)
                     `(apply ,v args)))
                 (if (vectorp (car functions))
                     (append (car functions) nil)
                   functions))))))

(defmacro igist-pipe (&rest functions)
  "Return left-to-right composition from FUNCTIONS."
  (declare (debug t) (pure t) (side-effect-free t))
  `(lambda (&rest args)
     ,@(let ((init-fn (pop functions)))
         (list
          (seq-reduce
           (lambda (acc fn)
             (if (symbolp fn)
                 `(funcall #',fn ,acc)
               `(funcall ,fn ,acc)))
           functions
           (if (symbolp init-fn)
               `(apply #',init-fn args)
             `(apply ,init-fn args)))))))

(defmacro igist-compose (&rest functions)
  "Return right-to-left composition from FUNCTIONS."
  (declare (debug t) (pure t) (side-effect-free t))
  `(igist-pipe ,@(reverse functions)))

(defmacro igist-with-exisiting-buffer (buffer-or-name &rest body)
  "Expand BODY in buffer BUFFER-OR-NAME if it is exists and visible."
  (declare (indent 1)
           (debug t))
  `(when (and
          (get-buffer ,buffer-or-name)
          (buffer-live-p (get-buffer ,buffer-or-name)))
     (with-current-buffer (get-buffer ,buffer-or-name)
       (progn ,@body))))

(defun igist-get-user-buffer-name (user)
  "Return the name of buffer with USER's gists."
  (when user (concat "*igist-" user "*")))

(defun igist-get-user-buffer (user)
  "Return buffer with USER's gists."
  (when user
    (get-buffer (igist-get-user-buffer-name user))))

(defun igist-get-gist-buffer (id filename)
  "Return gist's FILENAME buffer with ID."
  (get-buffer (concat "*" id "-" filename "*")))

(defun igist-ensure-gist-list-mode ()
  "Turn on `igist-list-mode' if it is active in current buffer."
  (unless (eq major-mode 'igist-list-mode)
    (igist-list-mode)))

(defun igist-edit-ensure-edit-mode ()
  "Set `igist-edit-mode' if it is not enabled."
  (unless (and (boundp 'igist-edit-mode)
               (symbol-value 'igist-edit-mode))
    (igist-edit-mode)))

(defmacro igist-with-user-gists-buffer (user &rest body)
  "Evaluate BODY in buffer with USER's gists.
If buffer doesn't exists, create it and turn on `igist-list-mode'."
  (declare (indent 1)
           (debug t))
  `(with-current-buffer (get-buffer-create
                         ,(igist-get-user-buffer-name user))
     (igist-ensure-gist-list-mode)
     (progn ,@body)))

;; Request api
(cl-defun igist-request (method resource &optional params &key query payload
                                headers silent unpaginate noerror reader auth
                                username host forge callback errorback value
                                buffer extra)
  "Make a METHOD request for RESOURCE with `ghub-request'.

With argument BUFFER show spinner in those buffer.

Arguments PARAMS, QUERY, PAYLOAD, HEADERS, SILENT, UNPAGINATE, NOERROR, READER,
USERNAME, AUTH, HOST, FORGE, CALLBACK, ERRORBACK, VALUE and EXTRA have the same
 meaning, as in `ghub-request'."
  (when buffer
    (igist-set-loading t buffer))
  (ghub-request method
                resource
                params
                :username (or username igist-current-user-name)
                :query query
                :auth (or auth
                          (when igist-current-user-name
                            igist-auth-marker)
                          'none)
                :forge (or forge 'github)
                :host (or host "api.github.com")
                :callback
                (lambda (value headers status req)
                  (unless (ghub-continue req)
                    (when buffer
                      (igist-set-loading nil buffer))
                    (when callback
                      (funcall callback value headers status
                               req))))
                :payload payload
                :headers headers
                :silent silent
                :unpaginate unpaginate
                :noerror noerror
                :reader reader
                :errorback
                (lambda (&rest args)
                  (when buffer
                    (igist-set-loading nil buffer))
                  (igist-show-request-error (car args))
                  (when errorback
                    (apply errorback args)))
                :value value
                :extra extra))

(cl-defun igist-get (resource &optional params &key buffer query payload headers
                              silent unpaginate noerror reader username auth
                              host callback errorback extra)
  "Make a `GET' request for RESOURCE, with optional query PARAMS.

With argument BUFFER show spinner in those buffer.

Arguments PARAMS, QUERY, PAYLOAD, HEADERS, SILENT, UNPAGINATE, NOERROR, READER,
USERNAME, AUTH, HOST, FORGE, CALLBACK, ERRORBACK, VALUE and EXTRA have the same
 meaning, as in `ghub-request'."
  (igist-request "GET" resource params
                 :buffer buffer
                 :query query
                 :payload payload
                 :headers headers
                 :silent silent
                 :unpaginate unpaginate
                 :noerror noerror
                 :reader reader
                 :username username
                 :auth auth
                 :host host
                 :callback callback
                 :errorback errorback
                 :extra extra))

(cl-defun igist-post (resource &optional params &key query payload headers
                               buffer silent unpaginate noerror reader username
                               auth host callback errorback extra)
  "Make a `POST' request for RESOURCE, with optional payload PARAMS.

With argument BUFFER show spinner in those buffer.

Arguments PARAMS, QUERY, PAYLOAD, HEADERS, SILENT, UNPAGINATE, NOERROR, READER,
USERNAME, AUTH, HOST, FORGE, CALLBACK, ERRORBACK, VALUE and EXTRA have the same
 meaning, as in `ghub-request'."
  (igist-request "POST" resource params
                 :query query
                 :buffer buffer
                 :payload payload
                 :headers headers
                 :silent silent
                 :unpaginate unpaginate
                 :noerror noerror
                 :reader reader
                 :username username
                 :auth auth
                 :host host
                 :callback callback
                 :errorback errorback
                 :extra extra))

(cl-defun igist-patch (resource &optional params &key buffer query payload
                                headers silent unpaginate noerror reader
                                username auth host callback errorback extra)
  "Make a `PATCH' request for RESOURCE, with optional payload PARAMS.

With argument BUFFER show spinner in those buffer.

Arguments PARAMS, QUERY, PAYLOAD, HEADERS, SILENT, UNPAGINATE, NOERROR, READER,
USERNAME, AUTH, HOST, FORGE, CALLBACK, ERRORBACK, VALUE and EXTRA have the same
 meaning, as in `ghub-request'."
  (igist-request "PATCH" resource params
                 :query query
                 :payload payload
                 :headers headers
                 :silent silent
                 :buffer buffer
                 :unpaginate unpaginate
                 :noerror noerror
                 :reader reader
                 :username username
                 :auth auth
                 :host host
                 :callback callback
                 :errorback errorback
                 :extra extra))

(cl-defun igist-delete (resource &optional params &key buffer query payload
                                 headers silent unpaginate noerror reader
                                 username auth host callback errorback extra)
  "Make a `DELETE' request for RESOURCE, with optional payload PARAMS.

With argument BUFFER show spinner in those buffer.

Arguments PARAMS, QUERY, PAYLOAD, HEADERS, SILENT, UNPAGINATE, NOERROR, READER,
USERNAME, AUTH, HOST, FORGE, CALLBACK, ERRORBACK, VALUE and EXTRA
have the same meaning, as in `ghub-request'."
  (igist-request "DELETE" resource params
                 :buffer buffer
                 :query query
                 :payload payload
                 :headers headers
                 :silent silent
                 :unpaginate unpaginate
                 :noerror noerror
                 :reader reader
                 :username username
                 :auth auth
                 :host host
                 :callback callback
                 :errorback errorback
                 :extra extra))

;; Common utils
(defun igist-pick-from-alist (props alist)
  "Pick PROPS from ALIST."
  (mapcar (igist-converge cons
                          [identity
                           (igist-rpartial igist-alist-get alist)])
          props))

(defun igist-visible-windows ()
  "Return a list of the visible, non-popup (dedicated) windows."
  (seq-filter (igist-or
               (igist-rpartial window-parameter 'visible)
               (igist-compose not window-dedicated-p))
              (window-list)))

(defun igist-get-all-edit-buffers ()
  "Return all gists buffer with content of gists."
  (seq-filter
   (igist-and buffer-live-p
              (apply-partially #'buffer-local-value 'igist-current-gist))
   (buffer-list)))

(defun igist-get-all-list-buffers ()
  "Return all buffers with `igist-list-mode'."
  (seq-filter
   (igist-and buffer-live-p
              (igist-compose
               (apply-partially #'eq 'igist-list-mode)
               (apply-partially #'buffer-local-value 'major-mode)))
   (buffer-list)))

(defun igist-buffer-window (buffer)
  "Return visible and non-dedicated window BUFFER or nil."
  (seq-find
   (igist-compose
    (apply-partially #'eq (if (stringp buffer)
                              (get-buffer buffer)
                            buffer))
    window-buffer)
   (igist-visible-windows)))

(defun igist-alist-get (key alist)
  "Find the first element of ALIST whose car equals KEY and return its cdr."
  (cdr (assoc key alist)))

(defun igist-download-url (url)
  "Download URL and return stirng."
  (let ((download-buffer (url-retrieve-synchronously url)))
    (prog1
        (with-current-buffer download-buffer
          (set-buffer download-buffer)
          (goto-char (point-min))
          (re-search-forward "^$" nil 'move)
          (forward-char)
          (delete-region (point-min)
                         (point))
          (decode-coding-region (point-min)
                                (point-max)
                                'dos)
          (buffer-string))
      (kill-buffer download-buffer))))

(defun igist-convert-region-pad-right (str width)
  "Pad STR with spaces on the right to increase the length to WIDTH."
  (unless str (setq str ""))
  (let ((exceeds (> (length str) width))
        (separator "..."))
    (cond ((and exceeds
                (> width
                   (length separator)))
           (concat (substring str 0 (- width (length separator))) separator))
          ((and exceeds)
           str)
          (t (concat str (make-string (- width (length str)) ?\ ))))))

(defun igist-find-by-id-and-file (id filename response)
  "Return gists with ID and FILENAME in RESPONSE."
  (let ((gist (seq-find (lambda (cell)
                          (and (equal id (igist-alist-get 'id cell))))
                        response)))
    (igist-normalize-gist-file gist filename)))

(defun igist-set-major-mode (filename)
  "Guess major mode for FILENAME."
  (let ((buffer-file-name (expand-file-name filename default-directory)))
    (delay-mode-hooks
      (set-auto-mode)
      (font-lock-ensure))))

(defun igist--get-time (gist key)
  "Return timestamp from value of KEY in GIST."
  (let* ((date (timezone-parse-date (igist-alist-get key gist)))
         (time (timezone-parse-time (aref date 3))))
    (encode-time (string-to-number (aref time 2))
                 (string-to-number (aref time 1))
                 (string-to-number (aref time 0))
                 (string-to-number (aref date 2))
                 (string-to-number (aref date 1))
                 (string-to-number (aref date 0))
                 (aref date 4))))

(defun igist-tabulated-gist-at-point ()
  "Get tabulated gist at point."
  (get-text-property (point) 'gist))

(defun igist-tabulated-gist-file-at-point ()
  "Get tabulated gist with file at point."
  (when-let ((parent (igist-tabulated-gist-at-point))
             (filename (get-text-property (point) 'filename)))
    (cdr (igist-normalize-gist-file parent filename))))

(defun igist-read-gist-file (prompt gist)
  "Read file in GIST with PROMPT in minibuffer.
GIST should be raw GitHub item."
  (let ((filename (completing-read
                   prompt
                   (igist-alist-get 'files gist))))
    (cdr (igist-normalize-gist-file gist filename))))

;;;###autoload
(defun igist-browse-gist ()
  "Browse gist at point or currently open."
  (interactive)
  (when-let ((gist-url
              (igist-alist-get 'html_url
                               (or (igist-tabulated-gist-at-point)
                                   igist-current-gist))))
    (browse-url gist-url)))

(defun igist-list-gist-to-fetch ()
  "Get tabulated gist with file at point."
  (or (igist-tabulated-gist-file-at-point)
      (when-let ((parent (get-text-property (point) 'gist)))
        (if (= 1 (length (igist-alist-get 'files parent)))
            (cdr (igist-normalize-gist-file parent
                                            (igist-alist-get 'filename
                                                             (cdar
                                                              (igist-alist-get
                                                               'files
                                                               parent)))))
          (igist-read-gist-file "Filename: " parent)))))

;;;###autoload
(defun igist-list-edit-gist-at-point (&optional _entry)
  "Open tabulated GIST-ITEM at point in edit buffer."
  (interactive)
  (when-let ((gist (igist-list-gist-to-fetch)))
    (let ((buff (igist-setup-edit-buffer gist)))
      (switch-to-buffer-other-window buff))))

;;;###autoload
(defun igist-list-view-current ()
  "Fetch tabulated gist entry at point."
  (interactive)
  (when-let ((current-window (selected-window))
             (gist (igist-list-gist-to-fetch)))
    (with-selected-window current-window
      (let ((buff (igist-setup-edit-buffer gist)))
        (switch-to-buffer-other-window buff)))))

;;;###autoload
(defun igist-list-edit-description (&rest _)
  "Edit description for current gist at point in tabulated list mode."
  (interactive)
  (when-let* ((gist (igist-tabulated-gist-at-point))
              (description (read-string "Description: " (igist-alist-get
                                                         'description gist))))
    (igist-patch (concat "/gists/" (igist-alist-get 'id gist))
                 nil
                 :payload `((description . ,description))
                 :callback (lambda (&rest _)
                             (igist-load-logged-user-gists)))))

(defun igist-read-filename-new (gist)
  "Read new filename for GIST."
  (let* ((filenames (mapcar (apply-partially #'igist-alist-get 'filename)
                            (igist-alist-get 'files gist)))
         (id (igist-alist-get 'id
                              gist))
         (file
          (read-string (concat "New file in " (if id (format "%s " id) "")))))
    (while (member file filenames)
      (setq file
            (read-string
             (concat (format "File %s already exists in %s. Type new filename: "
                             id
                             file)
                     (or
                      id
                      "")))))
    file))

(defun igist-parse-gist (gist)
  "Return a list of the GIST's attributes for display."
  (mapcar
   (lambda (it)
     (let* ((key (car it))
            (value
             (pcase key
               ('id (igist-alist-get 'id gist))
               ('visibility (eq (igist-alist-get 'public gist) t))
               ('updated_at (igist--get-time gist key))
               ('description (or (igist-alist-get 'description
                                                  gist)
                                 ""))
               ('files (mapcar (lambda (f)
                                 (when f (igist-alist-get
                                          'filename f)))
                               (igist-alist-get 'files gist)))
               ('comments (igist-alist-get 'comments gist))
               (_ (igist-alist-get 'id gist))))
            (format-val (car (last it)))
            (format-fn (if (memq key '(created_at updated_at))
                           'format-time-string
                         'format)))
       (if (stringp format-val)
           (funcall format-fn format-val value)
         (funcall format-val value gist))))
   igist-list-format))

(defun igist-tabulated-entry (gist)
  "Return vector with GIST props in tabulated format."
  (let* ((data (igist-parse-gist gist))
         (repo (igist-alist-get 'id gist)))
    (list repo (apply #'vector data))))

(defun igist-list-render (gists &optional _background)
  "Render list of GISTS."
  (let ((entries (mapcar #'igist-tabulated-entry gists)))
    (setq tabulated-list-entries entries)
    (when (not (equal (length gists)
                      (length entries)))
      (setq mode-name (format "Gists[%d/%d]" (length entries)
                              (length gists)))))
  (tabulated-list-print t))

(defun igist-ensure-buffer-visible (buffer &optional select)
  "Select BUFFER window. If SELECT is non-nil select window."
  (let ((buff-wind (igist-buffer-window buffer)))
    (cond ((and select
                buff-wind
                (not (eq (selected-window) buff-wind)))
           (select-window buff-wind))
          ((and (not buff-wind)
                (minibuffer-window-active-p (selected-window)))
           (with-minibuffer-selected-window
             (pop-to-buffer-same-window buffer t)))
          ((not buff-wind)
           (set-window-buffer nil buffer)))))

(defun igist-spinner-show ()
  "Show spinner."
  (require 'spinner nil t)
  (when (fboundp 'spinner-start)
    (spinner-start)))

(defun igist-spinner-stop ()
  "Stop spinner."
  (require 'spinner nil t)
  (when (fboundp 'spinner-stop)
    (spinner-stop)))

(defun igist-set-loading (loading &optional buffer)
  "Update LOADING status in BUFFER.
IF BUFFER is nil use current buffer.
If LOADING is non nil show spinner, otherwise hide."
  (when-let ((fn
              (when (or (not buffer)
                        (and (bufferp buffer)
                             (buffer-live-p buffer)))
                (if loading
                    #'igist-spinner-show
                  #'igist-spinner-stop))))
    (if (or
         (not buffer)
         (eq buffer (current-buffer)))
        (funcall fn)
      (with-current-buffer buffer (funcall fn)))))

(defun igist-normalize-gist (gist)
  "Return alist of files in GIST where car is key and cdr is merged gist."
  (let ((filtered-cell (remove (assoc 'files gist) gist))
        (files (mapcar #'cdr (cdr (assoc 'files gist))))
        (len))
    (setq len (length files))
    (seq-map-indexed
     (lambda (it idx)
       (let* ((value (append filtered-cell
                             it
                             `((idx . ,idx)
                               (total . ,len)
                               (files . ,files)))))
         (cons (igist-make-gist-key value)
               value)))
     files)))

(defun igist-normalize-gist-file (gist filename)
  "Return normalized GIST with FILENAME."
  (seq-find
   (igist-compose
    (apply-partially #'equal filename)
    (apply-partially #'igist-alist-get 'filename))
   (igist-normalize-gist gist)))

(defun igist-normalize-gists (gists)
  "Normalize GISTS."
  (seq-reduce
   (lambda (acc cell)
     (setq acc (append acc (igist-normalize-gist cell))))
   gists '()))

(defun igist-make-file-counter (gist-alist)
  "Format GIST-ALIST props idx and total."
  (if-let ((idx (igist-alist-get 'idx gist-alist)))
      (format "(%s/%s)" (1+ idx)
              (igist-alist-get 'total gist-alist))
    "New"))

(defun igist-make-gist-key (gist)
  "Create KEY from GIST's props."
  (let ((vals
         (seq-remove (igist-or null string-empty-p)
                     (mapcar (igist-rpartial
                              igist-alist-get gist)
                             '(id
                               filename)))))
    (string-join (list (car vals)
                       (car (reverse vals)))
                 "-")))

(defun igist-get-region-content ()
  "Return string without props with active region in current buffer or nil."
  (when (and
         (region-active-p)
         (use-region-p))
    (buffer-substring-no-properties (region-beginning)
                                    (region-end))))

(defun igist-refresh-buffer-vars (gist)
  "Setup buffer gist variables from GIST."
  (setq-local igist-current-filename (igist-alist-get 'filename gist))
  (setq-local header-line-format (format
                                  "Gist %s %s"
                                  igist-current-filename
                                  (igist-make-file-counter gist)))
  (setq-local igist-current-gist gist)
  (setq-local igist-current-description
              (igist-alist-get
               'description
               gist))
  (setq-local igist-current-public
              (igist-alist-get
               'public
               gist)))

(defun igist-sync-gists-lists (response)
  "Synchronize gists buffers with RESPONSE."
  (let ((buffers (igist-get-all-edit-buffers)))
    (dolist (buff buffers)
      (with-current-buffer buff
        (when-let ((gist
                    (and
                     (alist-get 'created_at
                                igist-current-gist)
                     (igist-find-by-id-and-file
                      (igist-alist-get
                       'id
                       igist-current-gist)
                      (or (igist-alist-get
                           'filename
                           igist-current-gist)
                          igist-current-filename)
                      response))))
          (igist-refresh-buffer-vars gist))))))

(defun igist-get-gist-buffers-by-id (id)
  "Return all live gist buffers with ID."
  (seq-filter (lambda (b)
                (and (buffer-live-p b)
                     (buffer-local-value 'igist-current-gist
                                         b)
                     (equal
                      id
                      (igist-alist-get 'id (buffer-local-value
                                            'igist-current-gist
                                            b)))))
              (buffer-list)))

(defun igist-request-delete-filename (gist)
  "Remove filename in GIST."
  (let ((filename (igist-alist-get 'filename gist))
        (id (igist-alist-get 'id gist)))
    (when-let ((buff (get-buffer (igist-get-gist-buffer id filename))))
      (when (buffer-live-p buff)
        (kill-buffer buff)))
    (igist-patch (concat "/gists/" id)
                 nil
                 :payload `((files (,(intern filename) .
                                    ((content . "")))))
                 :callback (lambda (&rest _)
                             (igist-load-logged-user-gists)))))

(defun igist-delete-gists-buffers-by-id (id)
  "Delete all gist's buffer with ID."
  (let ((buffers (igist-get-gist-buffers-by-id id)))
    (dolist (buff buffers)
      (when (buffer-live-p buff)
        (kill-buffer buff)))))

(defun igist-request-delete (id)
  "Delete GIST with ID."
  (igist-delete (concat "/gists/" id)
                nil
                :callback (lambda (&rest _)
                            (igist-load-logged-user-gists))))

(defun igist-get-github-users ()
  "Return list of users in auth sources with host `api.github.com'."
  (let ((all-users (delq nil (mapcar (igist-rpartial plist-get :user)
                                     (auth-source-search
                                      :host "api.github.com"
                                      :require
                                      '(:user :secret)
                                      :max
                                      most-positive-fixnum))))
        (suffix (regexp-quote (concat "^" (symbol-name igist-auth-marker)))))
    (or (seq-filter (apply-partially #'string-match-p suffix) all-users)
        all-users)))

(defun igist-popup-minibuffer-select-window ()
  "Select minibuffer window if it is active."
  (when-let ((wind (active-minibuffer-window)))
    (select-window wind)))

(defun igist-make-gist-payload (filename new-filename description content)
  "Make payload from FILENAME, NEW-FILENAME, DESCRIPTION and CONTENT."
  (let ((data `((files
                 (,(intern filename) .
                  ((filename . ,(or new-filename filename))
                   (content . ,content)))))))
    (if description
        (push `(description . ,description) data)
      data)))

(defun igist-show-request-error (value)
  "Pluck error message and status from VALUE and display it."
  (if-let ((status (seq-find #'numberp value)))
      (let ((msg (igist-alist-get 'message (car (last value)))))
        (minibuffer-message "Gist request failed with %s status%s"
                            status
                            (if msg (concat ":\s" msg) "")))
    (minibuffer-message "Gist request error: %s" value)))

;;;###autoload
(defun igist-fork-gist ()
  "Fork gist at point in `igist-list-mode' or currently opened."
  (interactive)
  (if-let ((id (or (igist-alist-get 'id igist-current-gist)
                   (tabulated-list-get-id))))
      (igist-post (concat "/gists/" id
                          "/forks")
                  nil
                  :buffer (current-buffer)
                  :callback
                  (lambda (&rest _)
                    (when igist-current-user-name
                      (igist-load-logged-user-gists)
                      (message "Gist forked"))))
    (user-error "No gist for forking")))

(defun igist-save-existing-gist (buffer &optional cb)
  "Save gist in BUFFER asynchronously with CB."
  (let* ((gist (buffer-local-value 'igist-current-gist buffer))
         (id (igist-alist-get 'id gist))
         (orig-filename (igist-alist-get 'filename gist))
         (new-filename (buffer-local-value
                        'igist-current-filename
                        buffer)))
    (igist-patch (concat "/gists/" id)
                 nil
                 :payload (igist-make-gist-payload
                           (or orig-filename
                               new-filename)
                           new-filename
                           (or
                            (buffer-local-value
                             'igist-current-description
                             buffer)
                            (igist-alist-get
                             'description
                             igist-current-gist))
                           (with-current-buffer
                               buffer
                             (buffer-substring-no-properties
                              (point-min)
                              (point-max))))
                 :buffer buffer
                 :callback
                 (lambda (val &rest _ignored)
                   (if (igist-alist-get 'id val)
                       (igist-with-exisiting-buffer
                           buffer
                         (let ((new-gist
                                (igist-normalize-gist-file val
                                                           new-filename)))
                           (unless (equal orig-filename new-filename)
                             (rename-buffer
                              (concat "*" id "-" new-filename "*"))
                             (igist-set-major-mode
                              new-filename)
                             (igist-edit-mode))
                           (igist-setup-local-vars new-gist new-filename)
                           (set-buffer-modified-p nil)
                           (igist-load-logged-user-gists)
                           (when cb
                             (funcall cb))))
                     (message "Couldn't save gist."))))))

(defun igist-update-created-gist (filename buffer response)
  "Update BUFFER with RESPONSE data for freshly created gist with FILENAME.
If callback is non nil, call it without args."
  (igist-with-exisiting-buffer buffer
    (let* ((new-gist (igist-normalize-gist-file response
                                                filename))
           (content (igist-alist-get
                     'content
                     (igist-alist-get
                      (intern
                       filename)
                      (igist-alist-get
                       'files
                       response)))))
      (rename-buffer
       (concat
        "*" (igist-alist-get 'id response) "-" filename "*"))
      (igist-setup-local-vars
       new-gist filename)
      (replace-region-contents (point-min)
                               (point-max)
                               (lambda () content))
      (set-buffer-modified-p nil))))

(defun igist-save-new-gist (buffer &optional callback)
  "Save new gist in BUFFER, refresh gists and execute CALLBACK without args."
  (let ((file (buffer-local-value 'igist-current-filename
                                  buffer)))
    (igist-post "/gists" nil
                :payload
                `((description . ,(or (buffer-local-value
                                       'igist-current-description
                                       buffer)
                                      (when (eq
                                             igist-ask-for-description
                                             'before-save)
                                        (read-string "Description: "))
                                      ""))
                  (public . ,(buffer-local-value 'igist-current-public
                                                 buffer))
                  (files
                   (,(intern file)
                    .
                    ((content .
                              ,(with-current-buffer
                                   buffer
                                 (buffer-substring-no-properties
                                  (point-min)
                                  (point-max))))))))
                :buffer buffer
                :callback
                (lambda (value &rest _)
                  (igist-with-exisiting-buffer buffer
                    (when callback
                      (funcall callback)))
                  (igist-load-logged-user-gists
                   #'igist-update-created-gist
                   file buffer value)))))

(defun igist-setup-local-vars (gist filename)
  "Setup local variables for GIST with FILENAME."
  (let ((gist-id (igist-alist-get 'id gist)))
    (setq-local header-line-format (format
                                    "Gist %s %s"
                                    (or filename
                                        (igist-alist-get 'filename gist))
                                    (igist-make-file-counter gist)))
    (setq-local igist-current-gist gist)
    (setq-local igist-current-filename (if gist-id
                                           (or igist-current-filename
                                               filename)
                                         filename))
    (setq-local igist-current-description (or igist-current-description
                                              (igist-alist-get
                                               'description
                                               gist)
                                              ""))
    (setq-local igist-current-public (if gist-id
                                         (or igist-current-public
                                             (igist-alist-get
                                              'public
                                              gist))
                                       (yes-or-no-p "Public?")))))

(defun igist-get-owner (gist)
  "Return login name of owner in GIST."
  (igist-alist-get 'login (igist-alist-get 'owner gist)))

(defun igist-setup-edit-buffer (gist &optional setup-fn)
  "Setup edit buffer for GIST in popup window.

If SETUP-FN is a non nil, it will be called without args."
  (let* ((filename (or (igist-alist-get 'filename gist)
                       (read-string "Filename: ")))
         (gist-id (igist-alist-get 'id gist))
         (buffer (and filename
                      gist-id
                      (igist-get-gist-buffer gist-id filename))))
    (if (and buffer
             (buffer-live-p buffer)
             (buffer-modified-p buffer))
        (with-current-buffer buffer
          (setq buffer-read-only nil)
          (igist-set-major-mode filename)
          (igist-setup-local-vars gist filename)
          (igist-edit-ensure-edit-mode)
          (current-buffer))
      (setq buffer (if gist-id
                       (get-buffer-create
                        (concat "*" (igist-make-gist-key gist) "*"))
                     (get-buffer-create
                      (concat "*" "newgist" "-" filename "*"))))
      (let ((content
             (if
                 (igist-alist-get 'raw_url gist)
                 (igist-download-url (igist-alist-get 'raw_url gist))
               "")))
        (with-current-buffer buffer
          (erase-buffer)
          (setq buffer-read-only nil)
          (progn
            (save-excursion
              (insert content))
            (igist-set-major-mode filename)
            (when setup-fn
              (funcall setup-fn)))
          (igist-setup-local-vars gist filename)
          (igist-edit-ensure-edit-mode)
          (setq buffer-undo-list nil)
          (set-buffer-modified-p nil))
        buffer))))

(defun igist-setup-new-gist-buffer (filename content)
  "Setup edit buffer for new gist with FILENAME and CONTENT."
  (let ((buffer (get-buffer-create
                 (concat "*" "newgist" "-" filename "*"))))
    (with-current-buffer buffer
      (erase-buffer)
      (setq buffer-read-only nil)
      (progn
        (set-buffer-modified-p nil)
        (setq buffer-undo-list nil)
        (save-excursion
          (igist-set-major-mode filename)
          (when content
            (insert content)))
        (igist-edit-mode))
      (setq header-line-format (or (concat "New gist: " filename)))
      (setq-local igist-current-gist nil)
      (setq-local igist-current-filename filename)
      (setq-local igist-current-description
                  (when (eq igist-ask-for-description 'immediately)
                    (read-string "Description: ")))
      (setq-local igist-current-public (yes-or-no-p "Public?")))
    buffer))

(defun igist-setup-comment-buffer (gist-id &optional comment-id comment-body)
  "Setup and return buffer for editing COMMENT-ID or new comment for GIST-ID."
  (let* ((buffer-name (if comment-id
                          (format "*%s-%s-comment*" gist-id comment-id)
                        (format "*%s-comment*" gist-id)))
         (buffer (and gist-id
                      (get-buffer buffer-name))))
    (if (and buffer
             (buffer-live-p buffer)
             (buffer-modified-p buffer))
        (with-current-buffer buffer
          (igist-comments-edit-mode)
          (setq-local igist-comment-gist-id gist-id)
          (setq-local igist-comment-id comment-id)
          (current-buffer))
      (setq buffer (get-buffer-create buffer-name))
      (with-current-buffer buffer
        (erase-buffer)
        (when comment-body
          (insert comment-body))
        (igist-comments-edit-mode)
        (setq buffer-undo-list nil)
        (set-buffer-modified-p nil)
        (setq-local igist-comment-gist-id gist-id)
        (setq-local igist-comment-id comment-id))
      buffer)))

(defun igist-edit-buffer (gist &optional setup-fn)
  "Display GIST in popup window.
If SETUP-FN is a non nil, it will be called without args."
  (pop-to-buffer (apply #'igist-setup-edit-buffer (list gist setup-fn)))
  (igist-popup-minibuffer-select-window))

(defun igist-edit-gist (gist-cell)
  "Edit GIST-CELL."
  (igist-edit-buffer
   (if (stringp gist-cell)
       (cdr (assoc gist-cell igist-normalized-gists))
     (cdr gist-cell))))

(defun igist-display-to-real (gist-cell)
  "Transform GIST-CELL to gist alist."
  (if (stringp gist-cell)
      (cdr (assoc gist-cell igist-normalized-gists))
    (cdr gist-cell)))

(defun igist-annotate-transformer (gist-key &optional max)
  "A function for annotating GIST-KEY in minibuffer completions.
MAX is length of most longest key."
  (let* ((cell (cdr (assoc gist-key igist-normalized-gists)))
         (extra (format "(%s/%s)" (1+ (igist-alist-get 'idx cell))
                        (igist-alist-get 'total cell)))
         (description (igist-convert-region-pad-right  (igist-alist-get
                                                        'description
                                                        cell)
                                                       50))
         (len (- (1+ max)
                 (length gist-key)))
         (annotation (string-join (list
                                   description
                                   extra
                                   (if
                                       (igist-alist-get 'public cell)
                                       "Public"
                                     "Private"))
                                  ": ")))
    (concat (make-string len ?\ )
            (if (igist-alist-get 'public cell)
                (propertize annotation 'face 'success)
              annotation))))

(defun igist-suggest-filename ()
  "Suggest filename for current buffer."
  (if-let ((ext
            (when buffer-file-name (file-name-extension
                                    buffer-file-name))))
      (concat (file-name-base buffer-file-name) "." ext)
    (let ((case-fold-search t))
      (string-join (split-string (buffer-name) "[^-a-z0-9.]" t) ""))))

(define-derived-mode igist-list-mode tabulated-list-mode "Gists"
  "Major mode for browsing gists.
\\<igist-list-mode-map>
\\{igist-list-mode-map}"
  (setq tabulated-list-format
        (apply #'vector
               (mapcar
                (igist-compose (igist-rpartial seq-take 3)
                               (igist-rpartial seq-drop 1))
                igist-list-format))
        tabulated-list-padding 2
        tabulated-list-sort-key nil)
  (tabulated-list-init-header)
  (use-local-map igist-list-mode-map))

(defun igist-pandoc-from-string (string input-type output-type &rest options)
  "Execute `pandoc' on STRING in INPUT-TYPE to OUTPUT-TYPE additional OPTIONS."
  (setq options (delete nil (flatten-list options)))
  (let ((args (append
               (list "pandoc" t t nil)
               (list "-f" input-type "-t"
                     output-type)
               options)))
    (with-temp-buffer
      (insert string)
      (let ((status (apply #'call-process-region (append (list (point-min)
                                                               (point-max))
                                                         args))))
        (when (eq 0 status)
          (buffer-string))))))

(defun igist-render-comment-to-md (gist-id comment-alist)
  "Render and comment COMMENT-ALIST for gist with GIST-ID in markdown format."
  (let ((comment (alist-get 'body comment-alist))
        (comment-id (alist-get 'id comment-alist))
        (updated
         (format-time-string "%D %R"
                             (igist--get-time comment-alist 'updated_at)))
        (author (alist-get 'login (alist-get 'user comment-alist))))
    (propertize (format "# Comment (%s at %s)\n%s" author updated comment)
                'igist-comment-id comment-id
                'igist-comment-gist-id gist-id
                'igist-gist-author author)))

;;;###autoload
(define-minor-mode igist-comments-list-mode
  "Minor mode for viewing and rendering gists comments.

This minor mode is turned on after command `igist-load-comments'.

\\<igist-comments-list-mode-map>
\\{igist-comments-list-mode-map}."
  :lighter " igists"
  :keymap igist-comments-list-mode-map
  :global nil
  (when igist-comments-list-mode
    (use-local-map
     (let ((map (copy-keymap
                 igist-comments-list-mode-map)))
       (set-keymap-parent map (current-local-map))
       map))))

(defun igist-render-comments (comments gist-id)
  "Render COMMENTS for gist with GIST-ID.
GIST-ID is used to create comments buffer."
  (if (not comments)
      (minibuffer-message "No comments in gist.")
    (when-let* ((md-comments (mapconcat (apply-partially
                                         #'igist-render-comment-to-md
                                         gist-id)
                                        comments
                                        "\n\n"))
                (args (if (and
                           (eq igist-mode-for-comments 'org-mode)
                           (executable-find "pandoc"))
                          (cons (igist-pandoc-from-string md-comments "gfm"
                                                          "org")
                                'org-mode)
                        (cons md-comments igist-mode-for-comments)))
                (buffer (get-buffer-create (concat "*" gist-id "-comments*"))))
      (with-current-buffer buffer
        (setq buffer-read-only nil)
        (erase-buffer)
        (progn
          (save-excursion
            (insert (car args)))
          (funcall (cdr args)))
        (setq buffer-undo-list nil)
        (igist-comments-list-mode)
        (set-buffer-modified-p nil)
        (setq buffer-read-only t)
        (setq igist-comment-gist-id gist-id)
        (pop-to-buffer (current-buffer))))))

;;;###autoload
(defun igist-load-comments (&rest _)
  "Load comments for gist at point or edit buffer."
  (interactive)
  (when-let ((gist-id
              (or
               igist-comment-gist-id
               (igist-alist-get 'id igist-current-gist)
               (when (eq major-mode 'igist-list-mode)
                 (tabulated-list-get-id))))
             (buff (current-buffer)))
    (igist-with-exisiting-buffer
     (get-buffer-create
      (concat "*" gist-id "-comments*"))
     (igist-spinner-show))
    (igist-get (concat "/gists/" gist-id "/comments") nil
               :buffer buff
               :callback
               (lambda (val &rest _)
                 (igist-render-comments val gist-id)
                 (igist-with-exisiting-buffer
                  (concat "*" gist-id "-comments*")
                  (igist-spinner-stop))))))

(defun igist-property-boundaries (prop &optional pos)
  "Return property boundaries for PROP at POS."
  (unless pos (setq pos (point)))
  (goto-char pos)
  (when (get-text-property (point) prop)
    (let ((beg (previous-single-char-property-change (point) prop))
          (end (next-single-char-property-change (point) prop)))
      (cons beg end))))

(defun igist-get-comment-bounds (&optional with-heading)
  "Return substring with comment at point.
If WITH-HEADING is non nil, include also heading, otherwise only body."
  (when-let ((body-bounds
              (igist-property-boundaries 'igist-comment-id
                                         (point))))
    (if with-heading
        (cons (car body-bounds)
              (cdr body-bounds))
      (goto-char (car body-bounds))
      (forward-line 1)
      (cons (point)
            (cdr body-bounds)))))

(defun igist-get-comment-body ()
  "Return substring with comment body at point."
  (if-let ((bounds (igist-get-comment-bounds)))
      (buffer-substring-no-properties (car bounds)
                                      (cdr bounds))
    (user-error "Not on gist comment")))

(defun igist-overlay-prompt-region (beg end face fn &rest args)
  "Highlight region from BEG to END with FACE while invoking FN with ARGS."
  (let ((overlay (make-overlay beg end)))
    (unwind-protect
        (progn (overlay-put overlay 'face face)
               (apply fn args))
      (delete-overlay overlay))))

;;;###autoload
(defun igist-add-comment (&rest _)
  "Add new comment for gist."
  (interactive)
  (when-let ((gist-id (igist-get-gist-id)))
    (pop-to-buffer-same-window (igist-setup-comment-buffer
                                gist-id)
                               t)))

(defun igist-get-gist-id ()
  "Return id for gist from all sources."
  (or
   (when (eq major-mode 'igist-list-mode)
     (tabulated-list-get-id))
   igist-comment-gist-id
   (get-text-property (point) 'igist-comment-gist-id)
   (igist-alist-get 'id igist-current-gist)))

;;;###autoload
(defun igist-add-or-edit-comment (&rest _)
  "Add or edit comment for gist at point or edit buffer."
  (interactive)
  (if-let* ((comment-id (get-text-property
                         (point)
                         'igist-comment-id))
            (comment-body (igist-get-comment-body)))
      (pop-to-buffer (igist-setup-comment-buffer
                      (get-text-property (point)
                                         'igist-comment-gist-id)
                      comment-id
                      comment-body))
    (when-let ((gist-id
                (igist-get-gist-id)))
      (pop-to-buffer-same-window (igist-setup-comment-buffer
                                  gist-id)
                                 t))))

;;;###autoload
(defun igist-delete-comment-at-point (&rest _)
  "Add or edit comment for gist at point or edit buffer."
  (interactive)
  (if-let ((comment-id (or
                        igist-comment-id
                        (get-text-property
                         (point)
                         'igist-comment-id)))
           (gist-id (or igist-comment-gist-id
                        (get-text-property
                         (point) 'igist-comment-gist-id))))
      (when
          (if-let ((bounds (igist-get-comment-bounds t)))
              (igist-overlay-prompt-region (car bounds)
                                           (cdr bounds)
                                           'error
                                           'yes-or-no-p "Delete comment?")
            (yes-or-no-p "Delete comment?"))
        (igist-delete (format "/gists/%s/comments/%s" gist-id comment-id)
                      :callback (lambda (&rest _)
                                  (igist-with-exisiting-buffer
                                      (concat "*" gist-id
                                              "-comments*")
                                    (setq igist-comment-gist-id gist-id)
                                    (igist-load-comments))
                                  (igist-load-logged-user-gists))))
    (user-error "Not in gist comment")))

;;;###autoload
(defun igist-post-comment ()
  "Post current comment."
  (interactive)
  (when-let* ((buffer (current-buffer))
              (gist-id (buffer-local-value 'igist-comment-gist-id buffer))
              (content (with-current-buffer buffer
                         (buffer-substring-no-properties
                          (point-min)
                          (point-max))))
              (callback-fn (lambda (&rest _)
                             (when (buffer-live-p buffer)
                               (kill-buffer buffer))
                             (igist-load-logged-user-gists
                              (lambda ()
                                (igist-with-exisiting-buffer
                                    (concat "*" gist-id
                                            "-comments*")
                                  (setq igist-comment-gist-id gist-id)
                                  (igist-load-comments)))))))
    (if-let ((comment-id (buffer-local-value 'igist-comment-id buffer)))
        (igist-patch  (format "/gists/%s/comments/%s" gist-id comment-id)
                      nil
                      :payload `((body . ,content))
                      :buffer buffer
                      :callback callback-fn)
      (igist-post (format "/gists/%s/comments" gist-id)  nil
                  :callback callback-fn
                  :payload `((body . ,content))))))

;;;###autoload
(defun igist-list-add-file ()
  "Add new file name to gist at point."
  (interactive)
  (when-let* ((current-window (selected-window))
              (gist (or (cdar (igist-normalize-gist
                               (igist-tabulated-gist-at-point)))
                        igist-current-gist))
              (filename
               (when (igist-alist-get 'id gist)
                 (igist-read-filename-new gist))))
    (let ((buff (igist-setup-edit-buffer
                 (append `((filename . ,filename))
                         (igist-pick-from-alist
                          '(owner
                            id files
                            created-at
                            description)
                          gist)))))
      (switch-to-buffer-other-window buff))))

;;;###autoload
(defun igist-list-gists ()
  "Load and render gists in tabulated list mode."
  (interactive)
  (unless igist-current-user-name
    (igist-change-user))
  (igist-list-load-gists
   igist-current-user-name nil))

;;;###autoload
(defun igist-new-gist-from-buffer (&rest _ignore)
  "Setup new gist buffer whole buffer contents."
  (interactive)
  (when-let ((content (or (igist-get-region-content)
                          (buffer-substring-no-properties (point-min)
                                                          (point-max)))))
    (let ((filename (read-string "Filename: "
                                 (when buffer-file-name
                                   (if-let ((ext
                                             (file-name-extension
                                              buffer-file-name)))
                                       (concat
                                        (file-name-base
                                         buffer-file-name)
                                        "." ext))))))
      (pop-to-buffer
       (igist-setup-new-gist-buffer filename content)
       nil
       t))))

;;;###autoload
(defun igist-create-new-gist ()
  "Setup new gist buffer with currently active region content or empty."
  (interactive)
  (let ((region-content (igist-get-region-content)))
    (let ((filename (read-string "Filename: "
                                 (when region-content
                                   (igist-suggest-filename)))))
      (pop-to-buffer (igist-setup-new-gist-buffer filename
                                                  (or region-content ""))))))

;;;###autoload
(defun igist-kill-all-gists-buffers ()
  "Delete all gists buffers."
  (interactive)
  (dolist (buff (igist-get-all-edit-buffers))
    (kill-buffer buff)))

;;;###autoload
(defun igist-change-user (&optional prompt initial-input history)
  "Read a user in minubuffer with PROMPT, INITIAL-INPUT and HISTORY."
  (interactive)
  (let* ((alist (mapcar (lambda (it)
                          (let ((parts (split-string it "[\\^]" t)))
                            (cons (pop parts)
                                  (pop parts) )))
                        (igist-get-github-users)))
         (annotf (lambda (str)
                   (format "^%s" (cdr (assoc str alist)))))
         (login-name (completing-read (or prompt "Github user name: ")
                                      (lambda (str pred action)
                                        (if (eq action 'metadata)
                                            `(metadata
                                              (annotation-function . ,annotf))
                                          (complete-with-action action alist str
                                                                pred)))
                                      nil
                                      nil
                                      initial-input
                                      history))
         (marker (igist-alist-get login-name alist)))
    (when-let ((marker (and marker (intern marker))))
      (unless (eq marker igist-auth-marker)
        (setq igist-auth-marker marker)))
    (setq igist-current-user-name login-name)
    (if (string-empty-p login-name)
        nil
      login-name)))

(defun igist-list-get-per-page-query (buffer)
  "Return estimed gists count for BUFFER."
  (let ((estimed-gists-count
         (if-let ((prev-response (and (bufferp buffer)
                                      (buffer-live-p buffer)
                                      (buffer-local-value
                                       'igist-gists-response
                                       buffer))))
             (length prev-response)
           (or igist-per-page-limit 30))))
    (funcall
     (igist-compose number-to-string min)
     estimed-gists-count
     100)))

(defvar-local igist-list-loading nil)
(defvar-local igist-list-cancelled nil)

(defun igist-list-cancel-load ()
  "Cancel loading for current gists."
  (interactive)
  (setq igist-list-cancelled t)
  (igist-spinner-stop))

(defun igist-list-refresh ()
  "Refresh current gist buffer in `igist-list-mode'."
  (interactive)
  (if (equal (buffer-name) "*igist-explore*")
      (igist-load-public-gists)
    (when-let ((owner (igist-get-owner
                       (car igist-gists-response))))
      (igist-list-load-gists owner))))

(defun igist-list-loaded-callback (buffer value req callback callback-args)
  "Render VALUE in existing BUFFER and REQ.
REQ is a `ghub--req' struct, used for loading next page."
  (if (and (bufferp buffer)
           (buffer-live-p buffer)
           (buffer-local-value 'igist-list-cancelled buffer))
      (with-current-buffer buffer
        (setq igist-list-cancelled nil)
        (setq igist-list-loading nil))
    (let ((more (ghub-continue req)))
      (igist-with-exisiting-buffer buffer
        (when (or
               (not more)
               (>= (length value)
                   (length igist-gists-response)))
          (setq-local igist-gists-response value)
          (igist-list-render value))
        (unless more
          (setq igist-list-loading nil)
          (when callback callback-args
                (apply callback callback-args))
          (igist-spinner-stop)))
      (unless more (igist-sync-gists-lists value)))))

(defun igist-explore-public-gists (&optional background callback callback-args)
  "List public gists sorted by most recently updated to least recently updated.

Render and load up to 3000 gists with pagination.
Then execute CALLBACK with CALLBACK-ARGS.

To stop or pause loading use command `igist-list-cancel-load'.

If BACKGROUND is nil, don't show user's buffer."
  (interactive)
  (let* ((buffer (get-buffer-create "*igist-explore*"))
         (query `((per_page . ,(igist-list-get-per-page-query
                                buffer)))))
    (with-current-buffer buffer
      (igist-ensure-gist-list-mode)
      (if igist-list-loading
          (setq igist-list-cancelled t)
        (setq igist-list-loading t))
      (igist-spinner-show)
      (unless background
        (igist-ensure-buffer-visible buffer)))
    (ghub-request "GET" "/gists/public"
                  nil
                  :auth (if igist-current-user-name
                            igist-auth-marker
                          'none)
                  :username igist-current-user-name
                  :query query
                  :forge 'github
                  :host "api.github.com"
                  :errorback
                  (lambda (&rest args)
                    (when buffer
                      (igist-with-exisiting-buffer
                          buffer
                        (igist-spinner-stop)))
                    (igist-show-request-error (car args)))
                  :callback
                  (lambda (value _headers _status req)
                    (igist-list-loaded-callback buffer
                                                value
                                                req
                                                callback
                                                callback-args)))))

(defun igist-load-logged-user-gists (&optional cb &rest args)
  "Load gists asynchronously with callback CB and ARGS."
  (unless igist-current-user-name
    (igist-change-user))
  (igist-list-load-gists igist-current-user-name
                         t
                         cb args))

(defun igist-list-load-gists (user &optional background callback callback-args)
  "Load and render gists for USER with CALLBACK and CALLBACK-ARGS.
If BACKGROUND is nil, don't show user's buffer."
  (let ((buffer (get-buffer-create
                 (igist-get-user-buffer-name user))))
    (with-current-buffer buffer
      (igist-ensure-gist-list-mode)
      (if igist-list-loading
          (setq igist-list-cancelled t)
        (setq igist-list-loading t))
      (igist-spinner-show)
      (unless background
        (igist-ensure-buffer-visible buffer)))
    (ghub-request "GET" (concat "/users/"
                                user
                                "/gists")
                  nil
                  :auth (if igist-current-user-name
                            igist-auth-marker
                          'none)
                  :username igist-current-user-name
                  :query `((per_page . ,(igist-list-get-per-page-query
                                         buffer)))
                  :forge 'github
                  :host "api.github.com"
                  :errorback
                  (lambda (&rest args)
                    (when buffer
                      (igist-with-exisiting-buffer
                          buffer
                        (igist-spinner-stop)))
                    (igist-show-request-error (car args)))
                  :callback
                  (lambda (value _headers _status req)
                    (igist-list-loaded-callback buffer
                                                value
                                                req
                                                callback
                                                callback-args)))))

(defun igist-list-other-user-gists (user)
  "Load and render gists for USER."
  (interactive (read-string "User: "))
  (igist-list-load-gists user nil))

(defun igist-list-change-other-user (&optional prompt input history)
  "Read owner of gists to load in minibuffer with PROMPT, INPUT and HISTORY."
  (setq igist-other-username (read-string (or prompt "User: ") input history))
  (unless (string-empty-p igist-other-username)
    (igist-list-other-user-gists
     igist-other-username))
  igist-other-username)

;;;###autoload
(defun igist-delete-other-gist-or-file (gist)
  "Delete GIST with id."
  (interactive
   (list
    (igist-completing-read-gists "Delete gist: " (igist-alist-get
                                                  'id
                                                  igist-current-gist))))
  (let* ((id (igist-alist-get
              'id
              igist-current-gist))
         (actions `((?y "delete only filename")
                    (?Y
                     ,(if-let ((files (igist-alist-get 'files gist)))
                          (format "Remove gist with %s files"
                                  (length
                                   files))
                        "Remove whole gist."))
                    (?n "no")))
         (answer (read-multiple-choice "Window"
                                       actions)))
    (pcase (car answer)
      (?y (igist-request-delete-filename gist))
      (?Y
       (igist-delete-gists-buffers-by-id id)
       (igist-request-delete id)))))

;;;###autoload
(defun igist-delete-current-filename ()
  "Delete current file from gist."
  (interactive)
  (when-let ((confirmed-gist
              (if-let ((file (if (eq major-mode
                                     'igist-list-mode)
                                 (igist-tabulated-gist-file-at-point)
                               igist-current-gist)))
                  (when
                      (if-let ((bounds (igist-property-boundaries 'filename
                                                                  (point))))
                          (igist-overlay-prompt-region (car bounds)
                                                       (cdr bounds) 'error
                                                       'yes-or-no-p
                                                       (format
                                                        "Delete file %s from gist?"
                                                        (igist-alist-get
                                                         'filename
                                                         file)))
                        (yes-or-no-p (format
                                      "Delete file %s from gist?"
                                      (igist-alist-get
                                       'filename
                                       file))))
                    file)
                (when-let ((parent (igist-tabulated-gist-at-point)))
                  (igist-read-gist-file "Delete file from gist: "
                                        parent)))))
    (igist-request-delete-filename confirmed-gist)))

;;;###autoload
(defun igist-delete-current-gist ()
  "Delete current gist with all files."
  (interactive)
  (cond ((igist-alist-get 'id igist-current-gist)
         (igist-delete-other-gist-or-file
          igist-current-gist))
        ((and igist-current-gist)
         (kill-current-buffer))
        ((eq major-mode 'igist-list-mode)
         (when-let ((id (tabulated-list-get-id))
                    (bounds (igist-property-boundaries 'gist (point))))
           (when (igist-overlay-prompt-region (car bounds)
                                              (cdr bounds) 'error
                                              'yes-or-no-p
                                              "Delete gist?")
             (igist-delete-gists-buffers-by-id id)
             (igist-request-delete id))))))

;;;###autoload
(defun igist-toggle-public (&rest _)
  "Toggle value of variable `igist-current-public'."
  (interactive)
  (setq igist-current-public (not igist-current-public)))

;;;###autoload
(defun igist-add-file-to-gist ()
  "Add new file to existing gist."
  (interactive)
  (cond ((and igist-current-filename
              (not (igist-alist-get 'id igist-current-gist)))
         (let ((gist (igist-completing-read-gists
                      "Add file to gist ")))
           (let ((id (igist-alist-get 'id gist))
                 (description (or (igist-alist-get 'description gist) ""))
                 (files (igist-alist-get 'files gist)))
             (setq igist-current-description description)
             (setq igist-current-gist
                   `((id . ,id)
                     (description . ,(or
                                      description
                                      ""))
                     (files . ,files)
                     (owner . ,(igist-alist-get 'owner gist))
                     (idx . ,(1+ (length files)))
                     (total . ,(1+ (length files))))))))
        ((eq major-mode 'igist-list-mode)
         (igist-list-add-file))
        (t
         (let* ((gist (or igist-current-gist
                          (igist-completing-read-gists "Add file to gist ")))
                (data (igist-pick-from-alist '(owner id files
                                                     description)
                                             gist)))
           (pop-to-buffer
            (igist-setup-edit-buffer data))))))

;;;###autoload
(defun igist-read-description (&rest _args)
  "Update description for current gist without saving."
  (interactive)
  (if (igist-not-editable-p)
      (setq igist-current-description
            (or igist-current-description
                (igist-alist-get 'description
                                 igist-current-gist)))
    (let ((descr
           (read-string "Description: "
                        (or igist-current-description
                            (igist-alist-get 'description
                                             igist-current-gist)))))
      (setq igist-current-description descr))))

;;;###autoload
(defun igist-read-filename (&rest _args)
  "Update filename for current gist without saving."
  (interactive)
  (let ((file (read-string
               (format "Rename (%s) to "
                       (igist-alist-get 'filename
                                        igist-current-gist))
               (or igist-current-filename
                   (igist-alist-get 'filename
                                    igist-current-gist)))))
    (setq igist-current-filename file)))

(defun igist-gist-modified-p (buffer)
  "Return t current gist's BUFFER is modified."
  (or (buffer-modified-p buffer)
      (let ((gist (buffer-local-value 'igist-current-gist buffer))
            (description (buffer-local-value 'igist-current-description buffer))
            (filename (buffer-local-value 'igist-current-filename buffer)))
        (or (not (equal filename (igist-alist-get 'filename gist)))
            (not (equal description (igist-alist-get 'description gist)))))))

(defun igist-save-gist-buffer (buffer &optional callback)
  "Run hooks `igist-before-save-hook' and save gist in BUFFER if it is edited.
With CALLBACK call it without args after success request."
  (with-current-buffer buffer
    (run-hooks igist-before-save-hook))
  (if
      (not (igist-alist-get 'id (buffer-local-value
                                 'igist-current-gist buffer)))
      (igist-save-new-gist buffer callback)
    (when (igist-gist-modified-p buffer)
      (igist-save-existing-gist buffer callback))))

;;;###autoload
(defun igist-save-current-gist ()
  "Post current gist and stay in the buffer."
  (interactive)
  (igist-save-gist-buffer (current-buffer)
                          (lambda ()
                            (message "Gist saved"))))

;;;###autoload
(defun igist-save-current-gist-and-exit ()
  "Post current gist and exit."
  (interactive)
  (igist-save-gist-buffer (current-buffer)
                          (lambda ()
                            (kill-current-buffer)
                            (message "Gist created"))))

;;;###autoload
(define-minor-mode igist-comments-edit-mode
  "Minor mode for editing and creating gists comments.

This minor mode is turned on after commands `igist-add-comment'
and `igist-edit-comment'.

\\<igist-comments-edit-mode-map>
\\{igist-comments-edit-mode-map}."
  :lighter " Igist"
  :keymap igist-comments-edit-mode-map
  :global nil
  (when igist-comments-edit-mode
    (pcase igist-mode-for-comments
      ('org-mode
       (when (not (executable-find "pandoc"))
         (error "You must install pandoc to use org-mode for gists")
         (org-mode)))
      ((pred functionp)
       (funcall igist-mode-for-comments)))
    (use-local-map
     (let ((map (copy-keymap
                 igist-comments-edit-mode-map)))
       (set-keymap-parent map (current-local-map))
       map))))
  
;;;###autoload
(define-minor-mode igist-edit-mode
  "Minor mode for language major mode buffers generated by `igist'.

This minor mode is turned on after command `igist-edit-gist'.

\\<igist-edit-mode-map>
\\{igist-edit-mode-map}

See also `igist-before-save-hook'."
  :lighter " Igist"
  :keymap igist-edit-mode-map
  :global nil
  (when igist-edit-mode
    (progn
      (setq buffer-read-only nil)
      (set-buffer-modified-p nil)
      (use-local-map
       (let ((map (copy-keymap
                   igist-edit-mode-map)))
         (set-keymap-parent map (current-local-map))
         map)))))

;;;###autoload
(defun igist-completing-read-gists (&optional prompt action initial-input)
  "Read gist in minibuffer with PROMPT and INITIAL-INPUT.
If ACTION is non nil, call it with gist."
  (interactive)
  (setq igist-normalized-gists (igist-normalize-gists igist-gists-response))
  (let* ((enhanced-action (lambda (g)
                            (funcall (or action (if (active-minibuffer-window)
                                                    'igist-edit-buffer
                                                  'identity))
                                     (igist-display-to-real g))))
         (max-len (and igist-normalized-gists
                       (apply #'max (mapcar (igist-compose length car)
                                            igist-normalized-gists))))
         (collection-fn (lambda (str pred action)
                          (if
                              (eq action 'metadata)
                              `(metadata
                                (annotation-function
                                 .
                                 (lambda (it)
                                   (igist-annotate-transformer
                                    it
                                    ,max-len))))
                            (complete-with-action action
                                                  igist-normalized-gists
                                                  str
                                                  pred)))))
    (cond ((and (eq completing-read-function
                    'ivy-completing-read)
                (fboundp 'ivy-read))
           (let ((key (ivy-read (or prompt "Gists: ") collection-fn
                                :initial-input (or initial-input "")
                                :preselect (funcall
                                            (igist-compose
                                             (igist-and identity
                                                        igist-make-gist-key)
                                             igist-tabulated-gist-at-point))
                                :caller 'igist-completing-read-gists
                                :action enhanced-action)))
             (funcall enhanced-action key)))
          (t
           (let ((key
                  (completing-read
                   (or prompt "Gists: ")
                   collection-fn
                   nil t
                   initial-input)))
             (funcall enhanced-action key))))))

;;;###autoload
(defun igist-edit-list ()
  "Read user gists in minibuffer and open it in edit buffer."
  (interactive)
  (igist-load-logged-user-gists #'igist-completing-read-gists
                             "Edit gist\s"
                             #'igist-edit-gist))

;; Transient
(transient-define-argument igist-set-current-filename-variable ()
  "Set a Lisp variable, `igist-current-filename'."
  :description "Rename"
  :class 'transient-lisp-variable
  :if (lambda ()
        (and (or igist-current-filename
                 igist-current-gist)
             (igist-editable-p)))
  :shortarg "-f"
  :variable 'igist-current-filename
  :reader #'igist-read-filename
  :argument "--filename=")

(transient-define-argument igist-set-current-user ()
  "Read user name and assign it in the variable `igist-current-user-name'."
  :description "Login Name"
  :class 'transient-lisp-variable
  :shortarg "-u"
  :variable 'igist-current-user-name
  :reader #'igist-change-user
  :argument "--user=")

(transient-define-argument igist-transient-change-owner ()
  "Change user whose gists to fetch."
  :description "Other user"
  :class 'transient-lisp-variable
  :shortarg "-o"
  :reader #'igist-list-change-other-user
  :variable 'igist-other-username
  :argument "--owner")

(transient-define-argument igist-set-current-description-variable ()
  "Read description and assign it in the variable `igist-current-description'."
  :description "Description"
  :if (lambda ()
        (or igist-current-filename
            igist-current-gist))
  :class 'transient-lisp-variable
  :reader #'igist-read-description
  :shortarg "-f"
  :variable 'igist-current-description
  :argument "--description=")

(transient-define-argument igist-transient-toggle-public ()
  "Toggle gist visibility and assign it in the variable `igist-current-public'."
  :description "Public"
  :if (lambda ()
        (and igist-current-filename
             (not (igist-alist-get 'id igist-current-gist))))
  :class 'transient-lisp-variable
  :shortarg "-p"
  :variable 'igist-current-public
  :reader #'igist-toggle-public
  :argument "affirmative")

(defun igist-not-editable-p (&optional gist)
  "Check whether GIST cannot be edited by user `igist-current-user-name'."
  (if-let ((owner (igist-get-owner (or
                                    gist
                                    (igist-tabulated-gist-at-point)
                                    igist-current-gist))))
      (not (equal igist-current-user-name owner))
    (not igist-current-user-name)))

(defun igist-editable-p (&optional gist)
  "Check whether GIST can be edited by user `igist-current-user-name'."
  (not (igist-not-editable-p gist)))

(defun igist-forkable (&optional gist)
  "Return t if GIST can be forked by user `igist-current-user-name'."
  (and
   igist-current-user-name
   (when-let ((owner (igist-get-owner (or
                                       gist
                                       (igist-tabulated-gist-at-point)
                                       igist-current-gist))))
     (not (equal igist-current-user-name owner)))))

(transient-define-prefix igist-dispatch-transient ()
  "Invoke transient popup with available gists commands for current buffer."
  [:class transient-columns
          [:if (lambda () (or igist-current-gist
                         (when (eq major-mode 'igist-list-mode)
                           (tabulated-list-get-id))))
               :description (lambda () (or
                                   (igist-alist-get 'id igist-current-gist)
                                   (tabulated-list-get-id)))
               ("D" "Delete gist" igist-delete-current-gist
                :inapt-if igist-not-editable-p)
               ("-" "Delete file" igist-delete-current-filename
                :inapt-if igist-not-editable-p)
               ("+" "Add file" igist-add-file-to-gist :inapt-if
                igist-not-editable-p)
               ("f" "Fork" igist-fork-gist :inapt-if-not
                igist-forkable)
               ("RET" "Save" igist-save-current-gist
                :inapt-if igist-not-editable-p)
               ("b r" "Browse" igist-browse-gist
                :inapt-if-not (lambda ()
                                (or
                                 (alist-get 'html_url igist-current-gist)
                                 (igist-alist-get
                                  'html_url
                                  (igist-tabulated-gist-at-point)))))
               ("r" igist-set-current-filename-variable)
               ("d" igist-set-current-description-variable)
               ("p" igist-transient-toggle-public)]
          ["Comments"
           ("c l" "Load comments" igist-load-comments)
           ("c a" "Add comment" igist-add-comment)
           ("c e" "Edit comment" igist-add-or-edit-comment
            :if (lambda () (get-text-property
                       (point)
                       'igist-comment-id)))]
          ["User"
           ("u" igist-set-current-user)
           ("o" igist-transient-change-owner)]
          ["Public"
           ("E"  "Explore public gists" igist-explore-public-gists)]
          ["My gists"
           ("R" "Remove" igist-delete-other-gist-or-file)
           ("l" "Edit gist" igist-edit-list)
           ("L" "List gists" igist-list-gists)
           ("n" "New" igist-create-new-gist)
           ("a" "Add file to gist" igist-add-file-to-gist
            :if-not (lambda () (or igist-current-gist
                              (when (eq major-mode 'igist-list-mode)
                                (tabulated-list-get-id)))))
           ("b b" "New gist from buffer" igist-new-gist-from-buffer)
           ("B" "Kill all gists buffers" igist-kill-all-gists-buffers)
           ("q" "Quit" transient-quit-all)]])

;;;###autoload
(defun igist-dispatch ()
  "Dispatch transient api."
  (interactive)
  (funcall-interactively #'igist-dispatch-transient))

(provide 'igist)
;;; igist.el ends here