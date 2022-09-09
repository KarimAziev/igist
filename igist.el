;;; igist.el --- Edit, create and view your github gists -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/igist
;; Version: 0.1.0
;; Keywords: vc
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

(require 'ivy nil t)
(require 'ghub)
(require 'transient)

(defmacro igist-unless (pred fn)
  "Return an unary function that invoke FN if result of calling PRED is nil.
Both PRED and FN called with one argument.
If result of PRED is non nil return the argument as is."
  `(lambda (arg)
     (if ,(if (symbolp pred)
              `(,pred arg)
            `(funcall ,pred arg))
         arg
       ,(if (symbolp fn)
            `(,fn arg)
          `(funcall ,fn arg)))))

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

(defun igist-visible-windows ()
  "Return a list of the visible, non-popup (dedicated) windows."
  (seq-filter (igist-or
               (igist-rpartial 'window-parameter 'visible)
               (igist-compose not window-dedicated-p))
              (window-list)))

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

(defun igist-f-slash (directory)
  "Add slash to DIRECTORY if none."
  (when directory
    (if (string-match-p "/$" directory)
        directory
      (setq directory (concat directory "/")))))

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

(defun igist-f-parent-dir (path)
  "Return the parent directory to PATH with slash."
  (when-let ((path (igist-f-parent path)))
    (igist-f-slash path)))

(defun igist-f-parent (path)
  "Return the parent directory to PATH without slash."
  (let ((parent (file-name-directory
                 (directory-file-name
                  (expand-file-name path default-directory)))))
    (when (and (file-exists-p path)
               (file-exists-p parent)
               (not (equal
                     (file-truename (directory-file-name
                                     (expand-file-name path)))
                     (file-truename (directory-file-name
                                     (expand-file-name parent))))))
      (if (file-name-absolute-p path)
          (directory-file-name parent)
        (file-relative-name parent)))))

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

(defvar igist-current-user-name nil)
(defvar igist-current-user-auth nil)

(defun igist-git-command (&rest args)
  "Exec git config with ARGS."
  (apply #'igist-exec "git"
         args))

(defun igist-git-user ()
  "Return current github user."
  (igist-git-command "config" "github.user"))

(defvar igist-list nil)
(defvar igist-loading nil)

(defun igist-get-gists-by-id (id)
  "Return gists with ID."
  (seq-filter (lambda (cell)
                (equal id
                       (igist-alist-get 'id (cdr cell))))
              igist-list))

(defun igist-find-by-id-and-file (id filename)
  "Return gists with ID and FILENAME."
  (seq-find (lambda (cell)
              (and (equal id
                          (igist-alist-get 'id (cdr cell)))
                   (equal filename
                          (igist-alist-get 'filename (cdr cell)))))
            igist-list))

(defvar igist-users-gists-response nil)
(defun igist-request-users-gists (&optional user)
  "Load gists for USER (default to `igist-current-user-name') synchronously."
  (setq igist-current-user-name (or user igist-current-user-name
                                    (igist-git-user)))
  (setq igist-current-user-auth (or igist-current-user-auth 'gist))
  (setq igist-users-gists-response
        (ghub-request "GET" (concat "/users/"
                                    igist-current-user-name
                                    "/gists")
                      nil
                      :unpaginate t
                      :username igist-current-user-name
                      :auth igist-current-user-auth
                      :host "api.github.com"))
  (igists-normalize-gists igist-users-gists-response))

(defun igist-request-gists-async (&optional cb)
  "Load gists with callback CB asynchronously."
  (setq igist-current-user-name (or igist-current-user-name
                                    (igist-git-user)))
  (setq igist-loading t)
  (setq igist-current-user-auth (or igist-current-user-auth 'gist))
  (ghub-request "GET" (concat "/users/"
                              igist-current-user-name
                              "/gists")
                nil
                :unpaginate t
                :username igist-current-user-name
                :auth igist-current-user-auth
                :host "api.github.com"
                :callback (lambda (value &rest _)
                            (setq igist-loading nil)
                            (setq igist-users-gists-response value)
                            (setq igist-list
                                  (igists-normalize-gists
                                   igist-users-gists-response))
                            (when cb
                              (funcall cb)))))

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
                            (let* ((filename (cdr
                                              (assoc
                                               'filename
                                               it)))
                                   (value (append filtered-cell
                                                  it
                                                  `((idx . ,idx)
                                                    (total . ,len)
                                                    (files
                                                     ,(seq-remove
                                                       (lambda
                                                         (f)
                                                         (equal
                                                          filename
                                                          (cdr
                                                           (assoc
                                                            'filename
                                                            f))))
                                                       files))))))
                              (cons (igist-make-gist-key value)
                                    value)))
                          files)))))
   gists '()))

(defun igist-make-file-counter (gist-alist)
  "Format GIST-ALIST props idx and total."
  (if-let ((idx (igist-alist-get 'idx gist-alist)))
      (format "(%s/%s)" (1+ idx)
              (igist-alist-get 'total gist-alist))
    "(1/1)"))

(defun igist-make-gist-key (gist)
  "Create KEY from GIST's props."
  (let ((vals
         (seq-remove (igist-or null string-empty-p)
                     (mapcar (igist-rpartial
                              igist-alist-get gist)
                             '(id
                               filename)))))
    (string-join (list (car vals)
                       (igist-make-file-counter gist)
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

(defvar igist-edit-buffer-default-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'igist-save-current-gist)
    (define-key map (kbd "C-c C-k") 'kill-current-buffer)
    (define-key map (kbd "C-c '") 'igist-save-current-gist-and-exit)
    (define-key map (kbd "M-o") 'igist-transient-api)
    map))

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
          (if (active-minibuffer-window)
              (delay-mode-hooks
                (igist-get-major-mode filename))
            (igist-get-major-mode filename)))
        (when content
          (insert content))
        (add-hook 'kill-buffer-hook
                  'igist-popup-minibuffer-select-window
                  nil t)
        (use-local-map
         (let ((map (copy-keymap
                     igist-edit-buffer-default-keymap)))
           (set-keymap-parent map (current-local-map))
           map)))
      (setq header-line-format (or (concat "New gist: " filename)))
      (setq-local igist-current-gist nil)
      (setq-local igist-current-filename filename)
      (setq-local igist-current-description (read-string "Description"))
      (setq-local igist-current-public (yes-or-no-p "Public?")))
    buffer))

;;;###autoload
(defun igist-new-gist-from-buffer ()
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

(defun igist-suggest-filename ()
  "Suggest filename for current buffer."
  (if-let ((ext
            (when buffer-file-name (file-name-extension
                                    buffer-file-name))))
      (concat (file-name-base buffer-file-name) "." ext)
    (string-join (split-string (buffer-name) "[^a-zZ-A0-9-]" t) "")))

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

(defvar-local igist-current-gist nil
  "Current gist.")

(defvar-local igist-current-description nil
  "Current gist description.")

(defvar-local igist-current-public nil
  "Current gist visibility.")

(defvar-local igist-current-filename nil
  "Current gist filename.")

(defun igist-get-all-gist-buffers (id)
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

(defun igist-delete-gists-buffers (id)
  "Delete all gist's buffer with ID."
  (let ((buffers (igist-get-all-gist-buffers id)))
    (dolist (buff buffers)
      (when (buffer-live-p buff)
        (kill-buffer buff)))))

(defun igist-request-delete (id)
  "Delete GIST with ID."
  (ghub-delete (concat "/gists/" id)
               nil
               :username igist-current-user-name
               :auth igist-current-user-auth
               :host "api.github.com"))

(defun igist-get-github-users ()
  "Return list of users in auth sources with host `api.github.com'."
  (remove nil (mapcar (igist-rpartial plist-get :user)
                      (auth-source-search
                       :host "api.github.com"
                       :require
                       '(:user :secret)
                       :max
                       most-positive-fixnum))))

;;;###autoload
(defun igist-change-user (&rest _)
  "Change user for retrieving gist."
  (interactive)
  (if-let* ((users (igist-get-github-users))
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
            igist-current-user-name user)
    (message "Cannot find github users in %s" auth-sources)
    nil))

;;;###autoload
(defun igist-ivy-delete-current-gist ()
  "Delete current gist during `ivy-read'."
  (interactive)
  (when-let* ((curr (igist-alist-get (ivy-state-current ivy-last)
                                     igist-list))
              (id (igist-alist-get 'id (cdr curr)))
              (user (igist-alist-get 'login
                                     (igist-alist-get 'owner
                                                      (cdr curr)))))
    (igist-request-delete id)
    (igist-ivy-extra-update (mapcar #'car (igist-request-users-gists)))))

(defun igist-ivy-extra-update (candidates &optional prompt)
  "Update ivy collection to CANDIDATES without exiting minibuffer.
With optional argument PROMPT also update `ivy--prompt'."
  (ivy-update-candidates candidates)
  (let ((input ivy-text)
        (pos
         (when-let ((wind (active-minibuffer-window)))
           (with-selected-window wind
             (point))))
        (prompt-end (minibuffer-prompt-end))
        (diff))
    (delete-minibuffer-contents)
    (setq diff (- pos prompt-end))
    (setf (ivy-state-collection ivy-last)
          ivy--all-candidates)
    (setf (ivy-state-preselect ivy-last)
          ivy--index)
    (ivy--reset-state ivy-last)
    (when prompt
      (setq ivy--prompt prompt))
    (when-let ((wind (active-minibuffer-window)))
      (with-selected-window wind
        (insert input)
        (goto-char (minibuffer-prompt-end))
        (forward-char diff)))))

;;;###autoload
(defun igist-delete-current-gist ()
  "Delete current gist."
  (interactive)
  (if-let ((id (igist-alist-get 'id igist-current-gist)))
      (progn
        (when (yes-or-no-p
               (format "Remove gist with %s files?"
                       (length
                        (igist-alist-get 'files
                                         igist-current-gist))))
          (igist-request-delete id)
          (igist-delete-gists-buffers id)
          (setq igist-list (igist-request-users-gists))))
    (message "Not in gist buffer")))

;;;###autoload
(defun igist-delete-gist (id)
  "Delete gist with ID."
  (interactive
   (list
    (igist-alist-get 'id
                     (igist-completing-read-gists "Delete gist: "))))
  (let ((prompt
         (if-let ((files (igist-alist-get 'files (igist-get-gists-by-id id))))
             (format "Remove gist with %s files?"
                     (length
                      files))
           "Remove gist?")))
    (when (yes-or-no-p prompt)
      (igist-request-delete id)
      (igist-delete-gists-buffers id))))

;;;###autoload
(defun igist-toggle-public (&rest _)
  "Toggle value of variable `igist-current-public'."
  (interactive)
  (setq igist-current-public (not igist-current-public)))

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
  "Set a Lisp variable, `igist-current-current-user'."
  :description "User"
  :class 'transient-lisp-variable
  :shortarg "-u"
  :variable 'igist-current-user-name
  :reader #'igist-change-user
  :argument "--user=")

(transient-define-argument igist-set-current-description-variable ()
  "Set a Lisp variable, `igist-current-description'."
  :description "Edit description"
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

(transient-define-prefix igist-transient-api ()
  "Igist transient api."
  [:class transient-columns
          ["Gist"
           ("r" igist-set-current-filename-variable)
           ("d" igist-set-current-description-variable)
           ("p" igist-transient-toggle-public)
           ("g" "Refresh current gist" igist-refresh-current-gist
            :if (lambda () igist-current-gist))
           ("d" "Delete current gist" igist-delete-current-gist
            :if (lambda () igist-current-gist))
           ("RET" "Save current gist" igist-save-current-gist
            :if (lambda () (or igist-current-filename
                          igist-current-gist)))]
          ["User"
           ("u" igist-set-current-user)]
          ["Gists"
           ("d" "Delete" igist-delete-gist)
           ("l" "List" igist-edit-gists)
           ("n" "New" igist-create-new-gist)
           ("b" "New gist from buffer"
            igist-new-gist-from-buffer)
           ("q" "Quit" transient-quit-one)]])

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

(defun igist-popup-minibuffer-select-window ()
  "Select minibuffer window if it is active."
  (when-let ((wind (active-minibuffer-window)))
    (select-window wind)))

;;;###autoload
(defun igist-refresh-current-gist ()
  "Refresh current GIST."
  (interactive)
  (igist-refresh-gist igist-current-gist))

(defun igist-refresh-gist (gist)
  "Refresh GIST."
  (let ((buff (igist-get-gist-buffer (igist-alist-get 'id gist)
                                     (igist-alist-get 'filename gist)))
        (id (igist-alist-get 'id
                             gist))
        (user (igist-alist-get 'login
                               (igist-alist-get 'owner
                                                gist))))
    (setq igist-list (igist-request-users-gists user))
    (when (and (bufferp buff)
               (buffer-live-p buff))
      (if-let* ((new-name (format "%s"
                                  (buffer-local-value
                                   'igist-current-filename
                                   buff)))
                (new-gist (and new-name
                               (not (equal new-name
                                           (igist-alist-get
                                            'filename
                                            gist)))
                               (igist-find-by-id-and-file
                                id
                                new-name))))
          (if-let ((wind (igist-buffer-window buff)))
              (progn
                (select-window wind)
                (pop-to-buffer-same-window
                 (igist-setup-edit-buffer
                  new-gist))
                (kill-buffer buff))
            (pop-to-buffer (igist-setup-edit-buffer new-gist)))
        (with-current-buffer buff
          (when-let ((content (igist-download-url (igist-alist-get 'raw_url
                                                                   gist))))
            (replace-region-contents (point-min)
                                     (point-max)
                                     (lambda () content))
            (message "Refreshed")))))))

(defun igist-make-gist-payload (filename new-filename description content
                                         public)
  "Make payload from FILENAME, NEW-FILENAME, DESCRIPTION, CONTENT and PUBLIC."
  `((description . ,description)
    (files
     (,(intern filename) .
      ((filename . ,(or new-filename filename))
       (content . ,content))))
    (public ,public)))

(defun igist-save-existing-gist (buffer)
  "Save gist in BUFFER."
  (let* ((gist (buffer-local-value 'igist-current-gist buffer))
         (id (igist-alist-get 'id gist))
         (old-parent-gist (seq-find (lambda (it)
                                      (equal id
                                             (alist-get 'id it)))
                                    igist-users-gists-response))
         (orig-filename (igist-alist-get 'filename gist))
         (new-file-name (buffer-local-value
                         'igist-current-filename
                         buffer))
         (payload (igist-make-gist-payload
                   orig-filename
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
                      (point-max)))
                   (buffer-local-value
                    'igist-current-public
                    buffer)))
         (response (ghub-patch (concat "/gists/" id)
                               nil
                               :unpaginate t
                               :payload payload
                               :auth 'gist)))
    (setq igist-users-gists-response (append
                                      (list response)
                                      (delete old-parent-gist
                                              igist-users-gists-response)))
    (setq igist-list (igists-normalize-gists igist-users-gists-response))
    (when-let* ((new-gist (cdr (igist-find-by-id-and-file id new-file-name)))
                (content (igist-alist-get 'content new-gist)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (unless (equal orig-filename new-file-name)
            (rename-buffer (concat "*" id "-" new-file-name "*")))
          (igist-setup-local-vars new-gist new-file-name)
          (replace-region-contents (point-min)
                                   (point-max)
                                   (lambda () content)))))))

(defun igist-save-new-gist (buffer)
  "Save new gist in BUFFER."
  (let* ((file (buffer-local-value 'igist-current-filename
                                   buffer))
         (payload
          `((description . ,file)
            (public . ,(buffer-local-value 'igist-current-public
                                           buffer))
            (files
             (,(intern file)
              . ((content .
                          ,(with-current-buffer
                               buffer
                             (buffer-substring-no-properties
                              (point-min)
                              (point-max)))))))))
         (response (ghub-post "/gists"
                              nil
                              :payload payload
                              :host "api.github.com"
                              :username (igist-git-command "config"
                                                           "github.user")
                              :auth 'gist)))
    (setq igist-users-gists-response (append
                                      (list response)
                                      igist-users-gists-response))
    (setq igist-list (igists-normalize-gists igist-users-gists-response))
    (when-let* ((new-gist (cdr (igist-find-by-id-and-file
                                (igist-alist-get 'id
                                                 response)
                                file)))
                (content (igist-alist-get 'content new-gist)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (rename-buffer (concat "*" (igist-alist-get 'id
                                                      response)
                                 "-"
                                 file "*"))
          (igist-setup-local-vars new-gist file)
          (replace-region-contents (point-min)
                                   (point-max)
                                   (lambda () content)))))))

;;;###autoload
(defun igist-save-current-gist (&optional arg)
  "Save current gist and with argument ARG kill buffer."
  (interactive "P")
  (let ((buff (current-buffer)))
    (if (igist-alist-get 'id (buffer-local-value 'igist-current-gist buff))
        (igist-save-existing-gist buff)
      (igist-save-new-gist buff))
    (when arg
      (kill-buffer buff)
      (message "Saved"))))

;;;###autoload
(defun igist-save-current-gist-and-exit ()
  "Save current gist."
  (interactive)
  (igist-save-current-gist 1))

(defun igist-setup-local-vars (gist filename)
  "Setup local variables for GIST with FILENAME."
  (let ((gist-id (igist-alist-get 'id gist)))
    (setq header-line-format (or (concat "Editing " filename)))
    (setq-local igist-current-gist gist)
    (setq-local igist-current-filename (if gist-id
                                           (or igist-current-filename
                                               filename)
                                         filename))
    (setq-local igist-current-description (if (not gist-id)
                                              (read-string "Description")
                                            (or igist-current-description
                                                (igist-alist-get
                                                 'description
                                                 gist)
                                                "")))
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
          (keymaps (seq-filter #'keymapp setup-args))
          (stx-table (seq-find #'syntax-table-p setup-args))
          (mode-fn (seq-find #'functionp setup-args)))
      (with-current-buffer buffer
        (erase-buffer)
        (setq buffer-read-only nil)
        (progn
          (set-buffer-modified-p nil)
          (setq buffer-undo-list nil)
          (save-excursion
            (if (active-minibuffer-window)
                (delay-mode-hooks
                  (igist-get-major-mode filename))
              (igist-get-major-mode filename)))
          (insert content)
          (add-hook 'kill-buffer-hook
                    'igist-popup-minibuffer-select-window
                    nil t)
          (when mode-fn
            (funcall mode-fn))
          (use-local-map
           (let ((map (if keymaps
                          (make-composed-keymap
                           keymaps
                           igist-edit-buffer-default-keymap)
                        (copy-keymap
                         igist-edit-buffer-default-keymap))))
             (set-keymap-parent map (current-local-map))
             map)))
        (when stx-table
          (set-syntax-table stx-table))
        (igist-setup-local-vars gist filename))
      buffer)))

(defun igist-edit-buffer (gist &rest setup-args)
  "Display GIST in popup window.

SETUP-ARGS can includes keymaps, syntax table, filename and function.
A filename can be opened with \\<igist-edit-buffer-default-keymap>\.
A function will be called without args inside quit function.

If SETUP-ARGS contains syntax table, it will be used in the inspect buffer."
  (pop-to-buffer (apply #'igist-setup-edit-buffer (list gist setup-args)))
  (igist-popup-minibuffer-select-window))

(defun igist-get-major-mode (filename)
  "Guess major mode for FILENAME."
  (let ((buffer-file-name (expand-file-name filename default-directory)))
    (delay-mode-hooks
      (set-auto-mode))))

(defun igist-edit-gist (gist-cell)
  "Edit GIST-CELL."
  (igist-edit-buffer
   (if (stringp gist-cell)
       (cdr (assoc gist-cell igist-list))
     (cdr gist-cell))))

(defun igist-display-to-real (gist-cell)
  "Transform GIST-CELL to gist alist."
  (if (stringp gist-cell)
      (cdr (assoc gist-cell igist-list))
    (cdr gist-cell)))

(defvar igist-ivy-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-k") 'igist-ivy-delete-current-gist)
    map)
  "Keymap for `igist-completing-read-gists' with `ivy-read'.")

(defun igist-completing-read-gists (&optional prompt action)
  "Read gist in minibuffer with PROMPT.
If ACTION is non nil, call it with gist."
  (interactive)
  (setq igist-list (igist-request-users-gists))
  (let ((enhanced-action (lambda (g)
                           (funcall (or action (if (active-minibuffer-window)
                                                   'igist-edit-buffer
                                                 'identity))
                                    (igist-display-to-real g)))))
    (cond ((and (eq completing-read-function 'ivy-completing-read)
                (fboundp 'ivy-read))
           (let ((key (ivy-read (or prompt "Gist: ")
                                (mapcar #'car igist-list)
                                :keymap igist-ivy-minibuffer-map
                                :caller 'igist-completing-read-gists
                                :action enhanced-action)))
             (message "%s" key)
             (funcall enhanced-action key)))
          (t
           (let ((key (completing-read (or prompt "Gist: ") igist-list)))
             (funcall enhanced-action key))))))

;;;###autoload
(defun igist-edit-gists ()
  "Read user gists in minibuffer and open it in edit buffer."
  (interactive)
  (transient--do-quit-all)
  (igist-completing-read-gists "Edit gist\s" #'igist-edit-gist))

(defun igist-ivy-display-transformer (gist-key)
  "A function for annotating GIST-KEY in ivy completions."
  (let* ((cell (cdr (assoc gist-key igist-list)))
         (id (igist-alist-get 'id cell))
         (filename (igist-convert-region-pad-right
                    (igist-alist-get 'filename cell)
                    50))
         (extra (format "(%s/%s)" (1+ (igist-alist-get 'idx cell))
                        (igist-alist-get 'total cell)))
         (description (igist-convert-region-pad-right  (igist-alist-get
                                                        'description
                                                        cell)
                                                       50)))
    (string-join (list
                  (propertize id 'face 'font-lock-keyword-face)
                  (propertize filename 'face 'font-lock-builtin-face)
                  description
                  extra
                  (if (igist-alist-get 'public cell)
                      "Public"
                    "Private"))
                 ": ")))

(when (fboundp 'ivy-configure)
  (ivy-configure 'igist-completing-read-gists
    :display-transformer-fn 'igist-ivy-display-transformer))

(provide 'igist)
;;; igist.el ends here