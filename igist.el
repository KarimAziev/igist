;;; igist.el --- Edit, create and view your github gists -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/igist
;; Version: 0.3.0
;; Keywords: tools
;; Package-Requires: ((emacs "28.1"))

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

;;; Code:

(require 'cl-lib)
(require 'ghub)
(require 'transient)
(require 'timezone)
(eval-when-compile
  (require 'subr-x))

(defcustom igist-list-format '((id "Id" 10 nil (lambda (id)
                                                 (cons
                                                  (format "%s" id)
                                                  (list
                                                   'action
                                                   #'igist-files-button-action
                                                   'help-echo
                                                   "Show gist"))))
                               (description "Description" 30 t identity)
                               (visibility "Visibility" 10 t
                                           (lambda (public)
                                             (or
                                              (and public
                                                   "public")
                                              "private")))
                               (updated_at "Updated" 20 t "%D %R")
                               (files "Files" 0 t
                                      (lambda (files)
                                        (when files
                                          (if (<= (length files) 1)
                                              (car files)
                                            (concat "\n" (mapconcat
                                                          (apply-partially
                                                           #'format
                                                           "\t\t%s")
                                                          files "\n")))))))
  "Format for gist list."
  :type '(alist
          :key-type
          (choice
           (const :tag "Id" id)
           (const :tag "Visibility" visibility)
           (const :tag "Updated" updated_at)
           (const :tag "Description" description)
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

(defcustom igist-current-user-auth 'gist
  "Suffix of user name in Auth-Sources.
For example, your github username is km, you have a token \"012345abcdef...\",
add such an entry in `auth-sources'.

machine api.github.com login km^gist password 012345abcdef...,
where `gist' is a suffix (default value)."
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

(defvar igist-gists-list-buffer-name "*igists*"
  "Buffer name for tabulated gists display.")

(defvar igist-batch-buffer nil)

(defvar igist-edit-buffer-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'igist-save-current-gist-and-exit)
    (define-key map (kbd "C-c '") 'igist-save-current-gist-and-exit)
    (define-key map (kbd "C-c C-k") 'kill-current-buffer)
    (define-key map (kbd "M-o") 'igist-dispatch)
    (define-key map [remap save-buffer] 'igist-save-current-gist)
    map)
  "Keymap for edit gist buffer.")

(defvar igist-gists-response nil)
(defvar igist-normalized-gists nil)
(defvar igist-loading nil)

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

(defmacro igist-with-visible-buffer (buffer-or-name &rest body)
  "Expand BODY in buffer BUFFER-OR-NAME if it is exists and visible."
  `(when (get-buffer ,buffer-or-name)
     (with-current-buffer (get-buffer ,buffer-or-name)
       (when (memq (current-buffer)
                   (igist-visible-buffers))
         (progn ,@body)))))

(defmacro igist-with-gists-buffer (&rest body)
  "Evaluate BODY in tabulated gists buffer."
  `(with-current-buffer (get-buffer-create igist-gists-list-buffer-name)
     (unless (eq major-mode 'igist-list-mode)
       (igist-list-mode))
     (progn ,@body)))

;; Request api
(cl-defun igist-request (method resource &optional params &key query payload
                                headers silent unpaginate noerror reader
                                username auth host forge callback errorback
                                value extra)
  "Make make a METHOD request for RESOURCE with `ghub-request'.

Arguments PARAMS, QUERY, PAYLOAD, HEADERS, SILENT, UNPAGINATE, NOERROR, READER,
USERNAME, AUTH, HOST, FORGE, CALLBACK, ERRORBACK, VALUE and EXTRA have the same
 meaning, as in `ghub-request'."
  (setq igist-current-user-name (or username
                                    igist-current-user-name
                                    (igist-git-user)))
  (if igist-current-user-name
      (setq igist-current-user-auth (or auth igist-current-user-auth))
    (igist-change-user))
  (igist-list-set-loading t)
  (let ((req (ghub-request method
                           resource
                           params
                           :username igist-current-user-name
                           :query query
                           :auth igist-current-user-auth
                           :forge (or forge 'github)
                           :host (or host "api.github.com")
                           :callback
                           (when callback
                             (lambda (value headers status req)
                               (unless (ghub-continue req)
                                 (igist-set-loading nil)
                                 (funcall callback value headers status req))))
                           :payload payload
                           :headers headers
                           :silent silent
                           :unpaginate unpaginate
                           :noerror noerror
                           :reader reader
                           :errorback errorback
                           :value value
                           :extra extra)))
    (unless callback
      (igist-set-loading nil))
    req))

(cl-defun igist-head (resource &optional params &key query payload headers
                               silent unpaginate noerror reader username auth
                               host callback errorback extra)
  "Make a `HEAD' request for RESOURCE, with optional query PARAMS.

Arguments PARAMS, QUERY, PAYLOAD, HEADERS, SILENT, UNPAGINATE, NOERROR, READER,
USERNAME, AUTH, HOST, FORGE, CALLBACK, ERRORBACK, VALUE and EXTRA have the same
 meaning, as in `ghub-request'."
  (igist-request "HEAD" resource params
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

(cl-defun igist-get (resource &optional params &key query payload headers silent
                              unpaginate noerror reader username auth host
                              callback errorback extra)
  "Make a `GET' request for RESOURCE, with optional query PARAMS.

Arguments PARAMS, QUERY, PAYLOAD, HEADERS, SILENT, UNPAGINATE, NOERROR, READER,
USERNAME, AUTH, HOST, FORGE, CALLBACK, ERRORBACK, VALUE and EXTRA have the same
 meaning, as in `ghub-request'."
  (igist-request "GET" resource params
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

(cl-defun igist-put (resource &optional params &key query payload headers silent
                              unpaginate noerror reader username auth host
                              callback errorback extra)
  "Make a `PUT' request for RESOURCE, with optional payload PARAMS.

Arguments PARAMS, QUERY, PAYLOAD, HEADERS, SILENT, UNPAGINATE, NOERROR, READER,
USERNAME, AUTH, HOST, FORGE, CALLBACK, ERRORBACK, VALUE and EXTRA have the same
 meaning, as in `ghub-request'."
  (igist-request "PUT" resource params
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
                               silent unpaginate noerror reader username auth
                               host callback errorback extra)
  "Make a `POST' request for RESOURCE, with optional payload PARAMS.

Arguments PARAMS, QUERY, PAYLOAD, HEADERS, SILENT, UNPAGINATE, NOERROR, READER,
USERNAME, AUTH, HOST, FORGE, CALLBACK, ERRORBACK, VALUE and EXTRA have the same
 meaning, as in `ghub-request'."
  (igist-request "POST" resource params
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

(cl-defun igist-patch (resource &optional params &key query payload headers
                                silent unpaginate noerror reader username auth
                                host callback errorback extra)
  "Make a `PATCH' request for RESOURCE, with optional payload PARAMS.

Arguments PARAMS, QUERY, PAYLOAD, HEADERS, SILENT, UNPAGINATE, NOERROR, READER,
USERNAME, AUTH, HOST, FORGE, CALLBACK, ERRORBACK, VALUE and EXTRA have the same
 meaning, as in `ghub-request'."
  (igist-request "PATCH" resource params
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

(cl-defun igist-delete (resource &optional params &key query payload headers
                                 silent unpaginate noerror reader username auth
                                 host callback errorback extra)
  "Make a `DELETE' request for RESOURCE, with optional payload PARAMS.

Arguments PARAMS, QUERY, PAYLOAD, HEADERS, SILENT, UNPAGINATE, NOERROR, READER,
USERNAME, AUTH, HOST, FORGE, CALLBACK, ERRORBACK, VALUE and EXTRA
have the same meaning, as in `ghub-request'."
  (igist-request "DELETE" resource params
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
               (igist-rpartial 'window-parameter 'visible)
               (igist-compose not window-dedicated-p))
              (window-list)))

(defun igist-visible-buffers (&optional buffer-list)
  "Return a list of visible buffers from BUFFER-LIST."
  (let ((buffers (delete-dups (mapcar #'window-buffer (window-list)))))
    (if buffer-list
        (seq-remove (lambda (b)
                      (memq b buffer-list))
                    buffers)
      (delete-dups buffers))))

(defun igist-get-all-gists-buffers ()
  "Return all gists buffer."
  (seq-filter
   (igist-and buffer-live-p
              (apply-partially #'buffer-local-value 'igist-current-gist))
   (buffer-list)))

(defun igist-with-every-gist-buffer (fn)
  "Call FN in every gist buffer."
  (when-let ((buffers (igist-get-all-gists-buffers)))
    (unwind-protect
        (progn  (add-hook 'minibuffer-setup-hook
                          'igist-batch-pop-current-buffer)
                (let ((max (length buffers)))
                  (dotimes (k max)
                    (let ((buff (nth k buffers)))
                      (when (buffer-live-p buff)
                        (with-current-buffer buff
                          (funcall fn)))))))
      (remove-hook 'minibuffer-setup-hook 'igist-batch-pop-current-buffer)
      (setq igist-batch-buffer nil))))

(defun igist-batch-pop-current-buffer ()
  "If buffer of `igist-batch-buffer' is not visible, show it."
  (when (and igist-batch-buffer
             (get-buffer igist-batch-buffer)
             (not (memq igist-batch-buffer (igist-visible-buffers))))
    (if (minibuffer-selected-window)
        (with-minibuffer-selected-window (pop-to-buffer
                                          igist-batch-buffer))
      (pop-to-buffer igist-batch-buffer))))

(defun igist-buffer-window (buffer)
  "Return visible and non-dedicated window BUFFER or nil."
  (seq-find
   (igist-compose
    (apply-partially #'eq
                     (if (stringp buffer)
                         (get-buffer buffer)
                       buffer))
    'window-buffer)
   (igist-visible-windows)))

(defun igist-alist-get (key alist)
  "Find the first element of ALIST whose car equals KEY and return its cdr."
  (cdr (assoc key alist)))

(defun igist-alist-set (key value alist)
  "Find the first element of ALIST whose car is KEY and set its cdr to VALUE."
  (if-let ((cell (assoc key alist)))
      (setcdr cell value)
    (let ((new-cell (cons key value)))
      (setf alist (push new-cell alist))))
  alist)

(defun igist-exec (command &rest args)
  "Execute COMMAND with ARGS synchronously.

Returns (STATUS . OUTPUT) when it is done, where STATUS is the returned error
code of the process and OUTPUT is its stdout output."
  (let ((buff (generate-new-buffer command)))
    (with-current-buffer buff
      (erase-buffer)
      (let ((status))
        (setq status (apply #'call-process command nil t nil
                            (delq nil (flatten-list args))))
        (let ((result (string-trim (buffer-string))))
          (if (= 0 status)
              (prog1 result (kill-current-buffer))
            (let ((command-with-args (concat command "\s" (string-join
                                                           (delq nil
                                                                 (flatten-list
                                                                  args))
                                                           "\s"))))
              (message "Error %s in %s: %s" command-with-args
                       (when default-directory
                         (abbreviate-file-name
                          default-directory))
                       result))
            nil))))))

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
          (buffer-string))
      (kill-buffer download-buffer))))

(defun igist-download-url-as-file (url file)
  "Download URL contents and write it to FILE."
  (let ((parent (file-name-directory file)))
    (unless (and parent (file-exists-p parent))
      (make-directory parent t))
    (let ((str (igist-download-url url)))
      (write-region str nil file))
    (when (file-exists-p file)
      file)))

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

(defun igist-git-command (&rest args)
  "Exec git config with ARGS."
  (apply #'igist-exec "git"
         args))

(defun igist-git-user ()
  "Return current github user."
  (igist-git-command "config" "github.user"))

(defun igist-set-gists (response)
  "Put RESPONSE  to the variable `igist-gists-response'.
Put normalized response to the variable `igist-normalized-gists'."
  (setq igist-gists-response response)
  (setq igist-normalized-gists (igists-normalize-gists igist-gists-response)))

(defun igist-get-gists-by-id (id)
  "Return gists with ID."
  (seq-filter (lambda (cell)
                (equal id
                       (igist-alist-get 'id (cdr cell))))
              igist-normalized-gists))

(defun igist-find-by-id-in-response (id)
  "Return gists with ID."
  (seq-find (lambda (cell)
              (equal id
                     (igist-alist-get 'id cell)))
            igist-gists-response))

(defun igist-find-by-id-and-file (id filename)
  "Return gists with ID and FILENAME."
  (seq-find (lambda (cell)
              (and (equal id
                          (igist-alist-get 'id (cdr cell)))
                   (equal filename
                          (igist-alist-get 'filename (cdr cell)))))
            igist-normalized-gists))

(defun igist-set-major-mode (filename)
  "Guess major mode for FILENAME."
  (let ((buffer-file-name (expand-file-name filename default-directory)))
    (set-auto-mode)))

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

(defun igist-bounds-by-chars (chars)
  "Return bounds of thing at point if it is match CHARS.
CHARS is like the inside of a [...] in a regular expression
except that ] is never special and \ quotes ^, - or \ (but
 not at the end of a range; quoting is never needed there)."
  (save-excursion
    (let* ((a (save-excursion
                (skip-chars-backward chars)
                (point)))
           (b (save-excursion
                (skip-chars-forward chars)
                (point))))
      (if (string-blank-p (buffer-substring-no-properties a b))
          nil
        (cons a b)))))

(defun igist-word-at-point ()
  "Return word at point."
  (when-let ((bounds (igist-bounds-by-chars "-*_~$A-Za-z0-9:.#\\+")))
    (buffer-substring-no-properties (car bounds)
                                    (cdr bounds))))

(defun igist-tabulated-gist-at-point ()
  "Run an action for BUTTON."
  (when (eq major-mode 'igist-list-mode)
    (let ((id (tabulated-list-get-id)))
      (let* ((files (mapcar #'cdr (igist-get-gists-by-id id)))
             (word-at-pos (igist-word-at-point))
             (file-at-pos
              (when word-at-pos
                (seq-find (igist-compose
                           (apply-partially #'equal word-at-pos)
                           (apply-partially #'igist-alist-get
                                            'filename))
                          files))))
        (if (= (length files) 1)
            (car files)
          file-at-pos)))))

(defun igist-files-button-action (_button)
  "Run an action for BUTTON."
  (let ((id (tabulated-list-get-id))
        (current-window (selected-window)))
    (let* ((files (mapcar #'cdr (igist-get-gists-by-id id)))
           (file-at-pos
            (car (member (igist-word-at-point)
                         (mapcar (apply-partially #'igist-alist-get
                                                  'filename)
                                 files))))
           (gist (if (or file-at-pos
                         (> (length files) 1))
                     (igist-find-by-id-and-file
                      id
                      (or file-at-pos
                          (completing-read "Files" (mapcar (apply-partially
                                                            #'igist-alist-get
                                                            'filename)
                                                           files))))
                   (car files))))
      (with-selected-window current-window
        (let ((buff (igist-setup-edit-buffer gist)))
          (switch-to-buffer-other-window buff))))))

(defun igist-read-filename-new (gist)
  "Read new filename for GIST."
  (let ((filenames (mapcar (apply-partially #'igist-alist-get 'filename)
                           (igist-alist-get 'files gist)))
        (file (read-string (concat "New file in " (or (igist-alist-get 'id gist)
                                                      ""))))
        (id (igist-alist-get 'id
                             gist)))
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
               (_ (igist-alist-get 'id gist))))
            (format-val (car (last it)))
            (format-fn (if (memq key '(created_at updated_at))
                           'format-time-string
                         'format)))
       (if (stringp format-val)
           (funcall format-fn format-val value)
         (funcall format-val value))))
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

(defun igist-show-spinner ()
  "Show spinner."
  (require 'spinner nil t)
  (when (fboundp 'spinner-start)
    (spinner-start)))

(defun igist-spinner-stop ()
  "Stop spinner."
  (require 'spinner nil t)
  (when (fboundp 'spinner-stop)
    (spinner-stop)))

(defun igist-list-set-loading (loading)
  "Update LOADING status for gist list buffer if exists.
If LOADING is non nil show spinner, otherwise hide."
  (when-let ((gists-buff (get-buffer igist-gists-list-buffer-name)))
    (when (buffer-live-p gists-buff)
      (with-current-buffer gists-buff
        (if loading
            (igist-show-spinner)
          (igist-spinner-stop)
          (tabulated-list-revert))))))

(defun igist-set-loading (loading)
  "Update LOADING status in gists buffers if exists.
If LOADING is non nil show spinner, otherwise hide."
  (igist-list-set-loading loading)
  (igist-with-every-gist-buffer (if loading
                                    #'igist-show-spinner
                                  #'igist-spinner-stop)))

(defun igist-request-gists-async (&optional cb &rest args)
  "Load gists asynchronously with callback CB and ARGS."
  (if igist-current-user-name
      (setq igist-current-user-auth igist-current-user-auth)
    (igist-change-user))
  (igist-request "GET" (concat "/users/"
                               igist-current-user-name
                               "/gists")
                 nil
                 :username igist-current-user-name
                 :query `((per_page . ,(if (not igist-gists-response)
                                           igist-per-page-limit
                                         (if
                                             (> (length igist-gists-response)
                                                100)
                                             100
                                           (length igist-gists-response)))))
                 :auth igist-current-user-auth
                 :forge 'github
                 :host "api.github.com"
                 :callback (lambda (value _headers _status req)
                             (if (and (ghub-continue req))
                                 (progn (igist-with-visible-buffer
                                         igist-gists-list-buffer-name
                                         (unless
                                             (eq major-mode 'igist-list-mode)
                                           (igist-list-mode))
                                         (igist-list-render value)))
                               (unwind-protect
                                   (progn
                                     (igist-set-gists value)
                                     (igist-with-visible-buffer
                                      igist-gists-list-buffer-name
                                      (unless (eq major-mode 'igist-list-mode)
                                        (igist-list-mode))
                                      (igist-list-render value))
                                     (igist-sync-gists-lists)
                                     (igist-set-loading nil)
                                     (when cb
                                       (apply cb args)))
                                 (igist-set-loading nil))))))

(defun igists-normalize-gists (gists)
  "Normalize GISTS."
  (seq-reduce
   (lambda (acc cell)
     (let ((filtered-cell (remove (assoc 'files cell) cell))
           (files (mapcar #'cdr (cdr (assoc 'files cell))))
           (len))
       (setq len (length files))
       (setq acc (append acc
                         (seq-map-indexed
                          (lambda (it idx)
                            (let* ((value (append filtered-cell
                                                  it
                                                  `((idx . ,idx)
                                                    (total . ,len)
                                                    (files . ,files)))))
                              (cons (igist-make-gist-key value)
                                    value)))
                          files)))))
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

(defun igist-get-gist-buffer (id filename)
  "Return gist's FILENAME buffer with ID."
  (get-buffer (concat "*" id "-" filename "*")))

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

(defun igist-sync-gists-lists ()
  "Synchronize gists buffer with `igist-normalized-gists'."
  (let ((buffers (igist-get-all-gists-buffers)))
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
                          igist-current-filename)))))
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

(defun igist-delete-gist-filename (gist)
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
                             (igist-request-gists-async)))))

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
                            (igist-request-gists-async))))

(defun igist-get-github-users ()
  "Return list of users in auth sources with host `api.github.com'."
  (remove nil (mapcar (igist-rpartial plist-get :user)
                      (auth-source-search
                       :host "api.github.com"
                       :require
                       '(:user :secret)
                       :max
                       most-positive-fixnum))))

(defun igist-popup-minibuffer-select-window ()
  "Select minibuffer window if it is active."
  (when-let ((wind (active-minibuffer-window)))
    (select-window wind)))

(defun igist-refresh-gist (gist)
  "Refresh GIST."
  (let ((buff (igist-get-gist-buffer (igist-alist-get 'id gist)
                                     (igist-alist-get 'filename gist)))
        (id (igist-alist-get 'id
                             gist)))
    (igist-request-gists-async
     (lambda ()
       (when (and (bufferp buff)
                  (buffer-live-p buff))
         (if-let* ((new-name (format "%s"
                                     (buffer-local-value
                                      'igist-current-filename
                                      buff)))
                   (new-gist (and new-name
                                  (not
                                   (equal
                                    new-name
                                    (igist-alist-get
                                     'filename
                                     gist)))
                                  (igist-find-by-id-and-file
                                   id
                                   new-name))))
             (if-let ((wind (igist-buffer-window
                             buff)))
                 (progn
                   (select-window wind)
                   (pop-to-buffer-same-window
                    (igist-setup-edit-buffer
                     new-gist))
                   (kill-buffer buff))
               (pop-to-buffer (igist-setup-edit-buffer
                               new-gist)))
           (with-current-buffer buff
             (when-let ((content
                         (igist-download-url
                          (igist-alist-get
                           'raw_url
                           gist))))
               (replace-region-contents
                (point-min)
                (point-max)
                (lambda ()
                  content))
               (set-buffer-modified-p nil)
               (message "Refreshed")))))))))

(defun igist-make-gist-payload (filename new-filename description content)
  "Make payload from FILENAME, NEW-FILENAME, DESCRIPTION and CONTENT."
  (let ((data `((files
                 (,(intern filename) .
                  ((filename . ,(or new-filename filename))
                   (content . ,content)))))))
    (if description
        (push `(description . ,description) data)
      data)))

(defun igist-save-existing-gist (buffer)
  "Save gist in BUFFER."
  (let* ((gist (buffer-local-value 'igist-current-gist buffer))
         (id (igist-alist-get 'id gist))
         (orig-filename (igist-alist-get 'filename gist))
         (new-file-name (buffer-local-value
                         'igist-current-filename
                         buffer))
         (payload (igist-make-gist-payload
                   (or orig-filename
                       new-file-name)
                   new-file-name
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
                      (point-max))))))
    (igist-patch (concat "/gists/" id)
                 nil
                 :unpaginate t
                 :payload payload
                 :callback (lambda (&rest _ignored)
                             (igist-request-gists-async
                              (lambda ()
                                (when-let* ((new-gist
                                             (cdr
                                              (igist-find-by-id-and-file
                                               id
                                               new-file-name)))
                                            (content
                                             (igist-alist-get
                                              'content
                                              new-gist)))
                                  (when (buffer-live-p buffer)
                                    (with-current-buffer
                                        buffer
                                      (unless (equal
                                               orig-filename
                                               new-file-name)
                                        (rename-buffer
                                         (concat
                                          "*"
                                          id
                                          "-"
                                          new-file-name
                                          "*")))
                                      (igist-setup-local-vars
                                       new-gist new-file-name)
                                      (replace-region-contents
                                       (point-min)
                                       (point-max)
                                       (lambda ()
                                         content))
                                      (set-buffer-modified-p
                                       nil))))))))))

(defun igist-save-new-gist (buffer)
  "Save new gist in BUFFER."
  (let* ((file (buffer-local-value 'igist-current-filename
                                   buffer))
         (payload
          `((description . ,(or (buffer-local-value 'igist-current-description
                                                    buffer)
                                (when
                                    (eq igist-ask-for-description 'before-save)
                                  (read-string "Description: "))
                                ""))
            (public . ,(buffer-local-value 'igist-current-public
                                           buffer))
            (files
             (,(intern file)
              . ((content .
                          ,(with-current-buffer
                               buffer
                             (buffer-substring-no-properties
                              (point-min)
                              (point-max))))))))))
    (igist-post "/gists"
                nil
                :payload payload
                :callback
                (lambda (value &rest _)
                  (igist-request-gists-async
                   (lambda ()
                     (when-let* ((new-gist
                                  (cdr
                                   (igist-find-by-id-and-file
                                    (igist-alist-get
                                     'id
                                     value)
                                    file)))
                                 (content
                                  (igist-alist-get
                                   'content
                                   new-gist)))
                       (when (buffer-live-p
                              buffer)
                         (with-current-buffer
                             buffer
                           (rename-buffer
                            (concat
                             "*"
                             (igist-alist-get
                              'id
                              value)
                             "-"
                             file
                             "*"))
                           (igist-setup-local-vars
                            new-gist file)
                           (replace-region-contents
                            (point-min)
                            (point-max)
                            (lambda
                              ()
                              content))
                           (set-buffer-modified-p
                            nil))))))))))

(defun igist-setup-local-vars (gist filename)
  "Setup local variables for GIST with FILENAME."
  (let ((gist-id (igist-alist-get 'id gist)))
    (setq header-line-format (or (concat "Gist " filename
                                         (igist-make-file-counter
                                          gist))))
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

(defun igist-setup-edit-buffer (gist &rest setup-args)
  "Setup edit buffer for GIST in popup window.

SETUP-ARGS can includes keymaps, syntax table, filename and function.
A filename can be opened with \\<igist-edit-buffer-default-keymap>\.
A function will be called without args inside quit function.

If SETUP-ARGS contains syntax table, it will be used in the inspect buffer."
  (let ((filename (or (igist-alist-get 'filename gist)
                      (read-string "Filename: ")))
        (gist-id (igist-alist-get 'id gist))
        (url (igist-alist-get 'raw_url gist)))
    (let ((content (if url (igist-download-url url) ""))
          (buffer (if gist-id
                      (get-buffer-create (concat "*" (igist-make-gist-key gist)
                                                 "*"))
                    (get-buffer-create
                     (concat "*" "newgist" "-" filename "*"))))
          (mode-fn (seq-find #'functionp setup-args)))
      (with-current-buffer buffer
        (erase-buffer)
        (setq buffer-read-only nil)
        (progn
          (set-buffer-modified-p nil)
          (save-excursion
            (delay-mode-hooks
              (igist-set-major-mode filename))
            (insert content))
          (igist-edit-mode)
          (when mode-fn
            (funcall mode-fn)))
        (igist-setup-local-vars gist filename)
        (setq buffer-undo-list nil)
        (set-buffer-modified-p nil))
      buffer)))

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
          (delay-mode-hooks
            (igist-set-major-mode filename))
          (when content
            (insert content)))
        (igist-edit-mode))
      (setq header-line-format (or (concat "New gist: " filename)))
      (setq-local igist-current-gist nil)
      (setq-local igist-current-filename filename)
      (setq-local igist-current-description
                  (when (eq igist-ask-for-description 'immediately)
                    (read-string "Description")))
      (setq-local igist-current-public (yes-or-no-p "Public?")))
    buffer))

(defun igist-edit-buffer (gist &rest setup-args)
  "Display GIST in popup window.

SETUP-ARGS can includes keymaps, syntax table, filename and function.
A filename can be opened with \\<igist-edit-buffer-default-keymap>\.
A function will be called without args inside quit function.

If SETUP-ARGS contains syntax table, it will be used in the inspect buffer."
  (pop-to-buffer (apply #'igist-setup-edit-buffer (list gist setup-args)))
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
    (string-join (split-string (buffer-name) "[^a-zZ-A0-9-]" t) "")))

(defvar igist-list-menu-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "+" 'igist-list-add-file)
    (define-key map "g" 'igist-list-gists)
    (define-key map (kbd "RET") 'igist-list-fetch-current)
    map))

(define-derived-mode igist-list-mode tabulated-list-mode "Gists"
  "Major mode for browsing gists.
\\<igist-list-menu-mode-map>
\\{igist-list-menu-mode-map}"
  (setq tabulated-list-format
        (apply #'vector
               (mapcar
                (igist-compose (igist-rpartial seq-take 3)
                               (igist-rpartial seq-drop 1))
                igist-list-format))
        tabulated-list-padding 2
        tabulated-list-sort-key nil)
  (tabulated-list-init-header)
  (use-local-map igist-list-menu-mode-map))

;;;###autoload
(defun igist-list-fetch-current ()
  "Fetch tabulated gist entry at point."
  (interactive)
  (igist-files-button-action nil))

;;;###autoload
(defun igist-list-add-file ()
  "Add new file name to gist at point."
  (interactive)
  (when-let* ((id (tabulated-list-get-id))
              (current-window (selected-window))
              (gist (cdr (car (igist-get-gists-by-id id))))
              (filename (igist-read-filename-new gist)))
    (let ((buff (igist-setup-edit-buffer
                 (igist-pick-from-alist
                  '(owner
                    id files
                    description)
                  gist))))
      (switch-to-buffer-other-window buff))))

;;;###autoload
(defun igist-list-gists ()
  "Show gists in tabulated list mode."
  (interactive)
  (igist-request-gists-async
   (lambda ()
     (if-let ((buff (get-buffer igist-gists-list-buffer-name)))
         (igist-ensure-buffer-visible buff)
       (igist-with-gists-buffer
        (igist-list-render igist-gists-response)
        (igist-ensure-buffer-visible (current-buffer)))))))

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
(defun igist-refresh-current-gist (&rest _ignore)
  "Refresh current GIST."
  (interactive)
  (let ((buff (current-buffer)))
    (igist-request-gists-async
     (lambda ()
       (igist-sync-gists-lists)
       (when (and (bufferp buff)
                  (buffer-live-p buff))
         (with-current-buffer buff
           (when-let ((gist (and (alist-get 'created_at
                                            igist-current-gist)
                                 (or (igist-find-by-id-and-file
                                      (igist-alist-get
                                       'id
                                       igist-current-gist)
                                      (igist-alist-get
                                       'filename
                                       igist-current-gist))
                                     (igist-find-by-id-and-file
                                      (igist-alist-get
                                       'id
                                       igist-current-gist)
                                      igist-current-filename)))))
             (when (or (not (buffer-modified-p buff))
                       (yes-or-no-p
                        (format
                         "Buffer %s modified. Discard your edits?"
                         buff)))
               (when-let ((content
                           (igist-download-url
                            (igist-alist-get
                             'raw_url
                             gist))))
                 (replace-region-contents (point-min)
                                          (point-max)
                                          (lambda ()
                                            content))
                 (set-buffer-modified-p nil))))))))))

;;;###autoload
(defun igist-kill-all-gists-buffers ()
  "Delete all gists buffers."
  (interactive)
  (dolist (buff (igist-get-all-gists-buffers))
    (kill-buffer buff)))

;;;###autoload
(defun igist-change-user (&rest _)
  "Change user for retrieving gist."
  (interactive)
  (when-let* ((users (igist-get-github-users))
              (gist-user
               (completing-read "User: " users nil nil
                                (car
                                 (seq-filter
                                  (apply-partially
                                   #'string-match-p
                                   "\\^gist$")
                                  (mapcar
                                   (apply-partially
                                    #'format
                                    "%s")
                                   (igist-get-github-users))))))
              (parts (split-string gist-user "\\^" t))
              (auth (car (reverse parts)))
              (user (string-join (nbutlast parts 1) "^")))
    (setq igist-current-user-auth (intern auth)
          igist-current-user-name user)))

;;;###autoload
(defun igist-delete-gist (gist)
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
                          (format "yes, remove gist with %s files"
                                  (length
                                   files))
                        "Remove whole gist."))
                    (?n "no")))
         (answer (read-multiple-choice "Window"
                                       actions)))
    (pcase (car answer)
      (?y (igist-delete-gist-filename gist))
      (?Y
       (igist-delete-gists-buffers-by-id id)
       (igist-request-delete id)))))

;;;###autoload
(defun igist-delete-current-gist ()
  "Delete current gist."
  (interactive)
  (if-let ((id (igist-alist-get 'id igist-current-gist)))
      (igist-delete-gist igist-current-gist)
    (message "Not in gist buffer")))

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
             (setq igist-current-gist `((id . ,id)
                                        (description . ,(or
                                                         description
                                                         ""))
                                        (files . ,files)
                                        (owner . ,(igist-alist-get 'owner gist))
                                        (idx . ,(1+ (length files)))
                                        (total . ,(1+ (length files))))))))
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
  (let ((descr
         (read-string "Description: "
                      (or igist-current-description
                          (igist-alist-get 'description
                                           igist-current-gist)))))
    (setq igist-current-description descr)))

;;;###autoload
(defun igist-read-filename (&rest _args)
  "Update filename for current gist without saving."
  (interactive)
  (let ((file
         (read-string
          (format "Rename (%s) to "
                  (igist-alist-get 'filename
                                   igist-current-gist))
          (or igist-current-filename
              (igist-alist-get 'filename
                               igist-current-gist)))))
    (setq igist-current-filename file)))

;;;###autoload
(defun igist-save-current-gist (&optional arg)
  "Save current gist and with argument ARG kill buffer."
  (interactive "P")
  (let ((buff (current-buffer)))
    (if (igist-alist-get 'id (buffer-local-value 'igist-current-gist buff))
        (igist-save-existing-gist buff)
      (igist-save-new-gist buff))
    (with-current-buffer buff
      (set-auto-mode)
      (run-hooks igist-before-save-hook))
    (when arg
      (kill-buffer buff)
      (message "Saved"))))

;;;###autoload
(defun igist-save-current-gist-and-exit ()
  "Save current gist."
  (interactive)
  (igist-save-current-gist 1))
  
;;;###autoload
(define-minor-mode igist-edit-mode
  "Minor mode for editable gists buffers."
  :lighter " Igist"
  :map 'igist-edit-buffer-keymap
  :global nil
  (if igist-edit-mode
      (progn
        (setq buffer-read-only nil)
        (set-buffer-modified-p nil)
        (add-hook 'kill-buffer-hook
                  'igist-popup-minibuffer-select-window
                  nil t)
        (use-local-map
         (let ((map (copy-keymap
                     igist-edit-buffer-keymap)))
           (set-keymap-parent map (current-local-map))
           map)))
    (progn
      (remove-hook 'kill-buffer-hook
                   'igist-popup-minibuffer-select-window
                   t))))

;;;###autoload
(defun igist-completing-read-gists (prompt &optional action initial-input)
  "Read gist in minibuffer with PROMPT and INITIAL-INPUT.
If ACTION is non nil, call it with gist."
  (interactive)
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
           (let ((key (ivy-read prompt collection-fn
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
                   prompt
                   collection-fn
                   nil t
                   initial-input)))
             (funcall enhanced-action key))))))

;;;###autoload
(defun igist-edit-gists ()
  "Read user gists in minibuffer and open it in edit buffer."
  (interactive)
  (igist-request-gists-async #'igist-completing-read-gists
                             "Edit gist\s"
                             #'igist-edit-gist))

;; Transient
(transient-define-argument igist-set-current-filename-variable ()
  "Set a Lisp variable, `igist-current-filename'."
  :description "Rename"
  :class 'transient-lisp-variable
  :if (lambda ()
        (or igist-current-filename
            igist-current-gist))
  :shortarg "-f"
  :variable 'igist-current-filename
  :reader #'igist-read-filename
  :argument "--filename=")

(transient-define-argument igist-set-current-user ()
  "Set a Lisp variable, `igist-current-user-name'."
  :description "User"
  :class 'transient-lisp-variable
  :shortarg "-u"
  :variable 'igist-current-user-name
  :reader #'igist-change-user
  :argument "--user=")

(transient-define-argument igist-set-current-description-variable ()
  "Set a Lisp variable, `igist-current-description'."
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
  "Are we affirmative?"
  :description "Public"
  :if (lambda ()
        (and igist-current-filename
             (not igist-current-gist)))
  :class 'transient-lisp-variable
  :shortarg "-p"
  :variable 'igist-current-public
  :reader #'igist-toggle-public
  :argument "affirmative")

(transient-define-prefix igist-dispatch-transient ()
  "Invoke transient popup with list of available commands."
  [:class transient-columns
          [:if (lambda () (or igist-current-gist
                         igist-current-filename))
               :description
               (lambda ()
                 (concat
                  (if (igist-alist-get 'id igist-current-gist)
                      (format "Gist %s %s"
                              (igist-alist-get 'id igist-current-gist)
                              (igist-make-file-counter igist-current-gist))
                    "New gist")))
               ("r" igist-set-current-filename-variable)
               ("d" igist-set-current-description-variable)
               ("p" igist-transient-toggle-public)
               ("g" "Refresh" igist-refresh-current-gist
                :if (lambda () igist-current-gist))
               ("RET" "Save" igist-save-current-gist
                :if (lambda () (or igist-current-filename
                              igist-current-gist)))
               ("f" "Add file" igist-add-file-to-gist)]
          ["User"
           ("u" igist-set-current-user)]
          ["Gists"
           ("D" "Delete" igist-delete-gist)
           ("l" "Edit gist" igist-edit-gists)
           ("L" "List gists" igist-list-gists)
           ("n" "New" igist-create-new-gist)
           ("f" "Add file to gist" igist-add-file-to-gist)
           ("b" "New gist from buffer" igist-new-gist-from-buffer)
           ("B" "Kill all gists buffers" igist-kill-all-gists-buffers)
           ("q" "Quit" transient-quit-all)]])

;;;###autoload
(defun igist-dispatch ()
  "Dispatch transient api."
  (interactive)
  (unless igist-current-user-name
    (igist-change-user))
  (funcall-interactively #'igist-dispatch-transient))

(provide 'igist)
;;; igist.el ends here