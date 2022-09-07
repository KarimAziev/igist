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

(defun igist-request-users-gists (user)
  "Load USER gists synchronously."
  (let ((response  (ghub-get (concat "/users/"
                                     (or user (read-string "User: " user))
                                     "/gists")
                             nil
                             :unpaginate t
                             :auth 'gist
                             :host "api.github.com")))
    (igists-normalize-gists response)))

(defun igists-normalize-gists (gists)
  "Normalize GISTS."
  (seq-reduce
   (lambda (acc cell)
     (let ((id (cdr (assoc 'id cell)))
           (description (cdr (assoc 'description cell)))
           (filtered-cell (remove (assoc 'files cell) cell))
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
                                   (key
                                    (string-join (seq-remove #'string-empty-p
                                                             (list
                                                              id
                                                              filename
                                                              description))
                                                 "\s")))
                              (cons key
                                    (append filtered-cell
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
                                                 files)))))))
                          files)))))
   gists '()))

(defun igist-make-gist-key (gist idx)
  "Create KEY from GIST's props and IDX."
  (let ((vals
         (seq-remove 'string-empty-p
                     (mapcar (igist-rpartial
                              igist-alist-get gist)
                             '(filename
                               id
                               description)))))
    (string-join (append vals (list (number-to-string idx))) "\s")))

(defun igist-get-gist-buffer (id filename)
  "Return gist's FILENAME buffer with ID."
  (get-buffer (concat "*" id "-" filename "*")))

(defun igist-request-update-gist (id files &optional description callback)
  "Update gist's with ID FILES and DESCRIPTION and execute CALLBACK."
  (let ((payload `((description . ,(or description
                                       (igist-alist-get
                                        'description
                                        (car (igist-get-gists-by-id
                                              id)))))
                   (files . ,files))))
    (message "Saving gist")
    (ghub-patch (concat "/gists/" id)
                nil
                :unpaginate t
                :payload payload
                :auth 'gist
                :callback
                (lambda (_value _headers _status _req)
                  (message "Saved gist %s" id)
                  (when callback
                    (run-with-timer 0.5 nil callback igist-list))))))

(defun igist-request-create-gist (files &optional description public callback)
  "Create new gist with FILES and DESCRIPTION and execute CALLBACK.
If PUBLIC is non nil, create PUBLIC gist, othervise private."
  (let ((payload `((description . ,(or description ""))
                   (files . ,files)
                   (public . ,(if public t nil)))))
    (message "Creating gist")
    (ghub-post "/gists/"
               nil
               :unpaginate t
               :payload payload
               :auth 'gist
               :callback
               (lambda (value _headers _status _req)
                 (message "Created gist %s " value)
                 (when callback
                   (run-with-timer 0.5 nil callback igist-list))))))

(defun igist-create-new-gist ()
  "Create new gist without saving and open it in edit buffer."
  (interactive)
  (igist-edit-buffer nil))

(defvar igist-edit-buffer-default-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'igist-save-current-gist)
    (define-key map (kbd "C-c C-k") 'kill-current-buffer)
    (define-key map (kbd "M-o") 'igist-transient-api)
    map))

(defvar-local igist-current-gist nil
  "Current gist.")

(defvar-local igist-current-description nil
  "Current gist description.")

(defvar-local igist-current-public nil
  "Current gist visibility.")

(defvar-local igist-current-filename nil
  "Current gist filename.")

(defun igist-toggle-public (&rest _)
  "Toggle value of variable `igist-current-public'."
  (interactive)
  (setq igist-current-public (not igist-current-public)))

(transient-define-argument igist-set-current-filename-variable ()
  "Set a Lisp variable, `igist-current-filename'."
  :description "Rename"
  :class 'transient-lisp-variable
  :shortarg "-f"
  :variable 'igist-current-filename
  :reader #'igist-read-filename
  :argument "--filename=")

(transient-define-argument igist-set-current-description-variable ()
  "Set a Lisp variable, `igist-current-description'."
  :description "Edit description"
  :class 'transient-lisp-variable
  :reader #'igist-read-description
  :shortarg "-f"
  :variable 'igist-current-description
  :argument "--description=")

(transient-define-argument igist-transient-toggle-public ()
  "Are we affirmative?"
  :description "Public"
  :class 'transient-lisp-variable
  :shortarg "-p"
  :variable 'igist-current-public
  :reader #'igist-toggle-public
  :argument "affirmative")

(transient-define-prefix igist-transient-api ()
  "Igist transient api."
  [:class transient-columns
          ["Current gist"
           ("r" igist-set-current-filename-variable)
           ("d" igist-set-current-description-variable)
           ("p" igist-transient-toggle-public)
           ("g" "Refresh" igist-refresh-current-gist)
           ("RET" "Save" igist-save-current-gist)]
          ["Others"
           ("l" "Others gists" igist-edit-gists)
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
                               gist)))
    (setq igist-list (igist-request-users-gists user))
    (when (and (bufferp buff)
               (buffer-live-p buff))
      (if-let* ((new-name (format "%s"
                                  (buffer-local-value
                                   'igist-current-filename
                                   buff)))
                (new-gist (and new-name
                               (igist-find-by-id-and-file
                                id
                                new-name))))
          (progn
            (message "renamed %s" buff)
            (if-let ((wind (igist-buffer-window buff)))
                (progn (select-window wind)
                       (kill-buffer buff)
                       (igist-edit-buffer new-gist))
              (igist-edit-buffer new-gist)))
        (with-current-buffer buff
          (message "Refreshed %s" buff)
          (when-let ((content (igist-download-url (igist-alist-get 'raw_url
                                                                   gist))))
            (replace-region-contents (point-min)
                                     (point-max)
                                     (lambda () content))))))))

;;;###autoload
(defun igist-save-current-gist ()
  "Save current gist."
  (interactive)
  (let ((buff (current-buffer)))
    (if-let ((id (igist-alist-get 'id igist-current-gist))
             (gist-filename
              (igist-alist-get 'filename igist-current-gist)))
        (let ((payload
               `((description .
                              ,(or
                                (buffer-local-value 'igist-current-description
                                                    buff)
                                (igist-alist-get
                                 'description
                                 igist-current-gist)))
                 (files
                  (,(intern gist-filename) .
                   ((filename .
                              ,(or
                                (buffer-local-value
                                 'igist-current-filename buff)
                                gist-filename))
                    (content .
                             ,(with-current-buffer
                                  buff
                                (buffer-substring-no-properties
                                 (point-min)
                                 (point-max)))))))
                 (public (if ,(or
                               (buffer-local-value 'igist-current-public buff)
                               (igist-alist-get 'public igist-current-gist))))))
              (gist (seq-copy igist-current-gist)))
          (ghub-patch (concat "/gists/" id)
                      nil
                      :unpaginate t
                      :payload payload
                      :auth 'gist
                      :callback
                      (lambda (value _headers _status _req)
                        (message "saved %s " value)
                        (run-with-timer 0.5 nil 'igist-refresh-gist
                                        gist))))
      (let* ((file (buffer-local-value 'igist-current-filename
                                       buff))
             (payload
              `((description . ,file)
                (public . ,(buffer-local-value 'igist-current-public
                                               buff))
                (files
                 (,(intern file)
                  . ((content .
                              ,(with-current-buffer
                                   buff
                                 (buffer-substring-no-properties
                                  (point-min)
                                  (point-max))))))))))
        (print payload)
        (ghub-post "/gists"
                   nil
                   :payload payload
                   :host "api.github.com"
                   :username (igist-git-command "config" "github.user")
                   :auth 'gist
                   :errorback (lambda (err &rest _)
                                (message "Gist error: %s" err))
                   :callback
                   (lambda (value _headers _status _req)
                     (let ((gist (igists-normalize-gists
                                  (list value))))
                       (igist-edit-buffer gist)
                       (when (buffer-live-p buff)
                         (kill-buffer buff)))))))))

(defun igist-edit-buffer (gist &rest setup-args)
  "Display GIST in popup window.

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
                      (get-buffer-create (concat "*" gist-id "-" filename "*"))
                    (get-buffer-create
                     (concat "*" "newgist" "-" filename "*"))))
          (keymaps (seq-filter #'keymapp setup-args))
          (stx-table (seq-find #'syntax-table-p setup-args))
          (mode-fn (seq-find #'functionp setup-args)))
      (with-current-buffer buffer
        (erase-buffer)
        (setq buffer-read-only nil)
        (progn
          (insert content)
          (set-buffer-modified-p nil)
          (setq buffer-undo-list nil)
          (save-excursion
            (if (active-minibuffer-window)
                (delay-mode-hooks
                  (igist-get-major-mode filename))
              (igist-get-major-mode filename)))
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
                                           (yes-or-no-p "Public?"))))
      (pop-to-buffer buffer)
      (igist-popup-minibuffer-select-window))))

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

(defun igist-read-gists (gists)
  "Read gist in minibuffer with completions GISTS."
  (let ((key (completing-read "Gist" gists)))
    (assoc key gists)))

;;;###autoload
(defun igist-edit-gists ()
  "Read user gists in minibuffer and open it in edit buffer."
  (interactive)
  (cond ((and (eq completing-read-function 'ivy-completing-read)
              (fboundp 'ivy-read))
         (igist-ivy-read-gists #'igist-edit-gist))
        (t
         (setq igist-list (igist-request-users-gists (igist-git-user)))
         (igist-edit-gist (igist-read-gists igist-list)))))

;;;###autoload
(defun igist-ivy-read-gists (&optional action)
  "Read GISTS gist with ivy and apply ACTION."
  (interactive)
  (when (fboundp 'ivy-read)
    (setq igist-list (igist-request-users-gists (igist-git-user)))
    (let ((key (ivy-read "Gist:" igist-list
                         :caller 'igist-ivy-read-gists
                         :action (or action 'identity))))
      (assoc key igist-list))))

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
  (ivy-configure 'igist-ivy-read-gists
    :display-transformer-fn 'igist-ivy-display-transformer))

(provide 'igist)
;;; igist.el ends here