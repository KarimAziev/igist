;;; igist.el --- List, create, update and delete GitHub gists -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/igist
;; Version: 1.4.1
;; Keywords: tools
;; Package-Requires: ((emacs "27.1") (ghub "3.6.0") (transient "0.4.1"))
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

;; Edit, create and view your GitHub gists.

;; Usage

;;  `igist-dispatch' - Invoke transient menu with the list of available
;;   commands.

;; Tabulated display:

;; M-x `igist-list-gists' - Display your gists as table.
;; M-x `igist-list-other-user-gists' - Display public gists of any user.
;; M-x `igist-explore-public-gists' - List public gists sorted.

;; Completions display:

;; M-x `igist-edit-list' Read Gist to edit from the minibuffer.

;;; Create commands:

;; M-x `igist-create-new-gist'
;;      Create the editable gist buffer with the content of the current buffer.

;; M-x `igist-new-gist-from-buffer'
;;      Setup new gist buffer whole buffer contents.

;; M-x `igist-list-add-file'
;;      Add new file name to gist at point.

;; M-x `igist-fork-gist'
;;      Fork gist at point in `igist-list-mode' or currently opened.

;; M-x `igist-post-files' Post multiple files to Gist. If
;;      there are marked files in the Dired buffer, use them; otherwise, read
;;      the directory in the minibuffer with completions and then read multiple
;;      files.


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

;; - `igist-current-user-name': This variable should be set to a string
;;   that contains your GitHub username.

;; - `igist-auth-marker': This variable can either be a string that
;;   contains the OAuth token or a symbol indicating where to retrieve
;;   the OAuth token.

;; - `igist-message-function': A custom function for displaying messages.
;;   Should accept the same arguments as the `message' function.

;; - `igist-per-page-limit': The number of results displayed per page
;;   should be a value ranging between 30 to 100. The default value is 30.

;; - `igist-ask-for-description': Determines when to prompt for a
;;   description before posting new gists. The default setting prompts
;;   for a description before saving a new gist.

;; - `igist-enable-copy-gist-url-p': Specifies whether and when to add
;;   the URL of a new or updated gist to the kill ring. The default
;;   setting is after the creation of new gists.

;; - `igist-list-format': Specifies the format of the user's Tabulated
;;   Gists buffers.

;; - `igist-explore-format': Specifies the format of the Explore Public
;;   Gists tabulated buffers.


;;; Keymaps

;; `igist-comments-list-mode-map'
;;      A keymap used for displaying comments.

;; `igist-list-mode-map'
;;      Keymap used in tabulated gists views.

;; `igist-comments-edit-mode-map'
;;      Keymap for posting and editing comments.

;; `igist-edit-mode-map'
;;      Keymap used in edit gist buffers.


;;; Code:

(eval-when-compile
  (require 'subr-x))

(require 'transient)
(require 'parse-time)
(require 'ghub)

(declare-function text-property-search-backward "text-property-search")

(defvar-local igist-list-hidden-ids nil)
(defvar-local igist-list-response nil)
(defvar-local igist-table-list-format nil)
(defvar-local igist-render-timer nil)
(defvar-local igist-sync-timer nil)
(defvar-local igist-languages-filters nil)
(defvar-local igist-files-filter nil)
(defvar-local igist-description-filter nil)
(defvar-local igist-filters nil)
(defvar-local igist-rendered-hash nil)
(defvar-local igist-default-collapsed nil)


(defvar igist-tabulated-list--original-order nil)

(defcustom igist-tabulated-list-gui-sort-indicator-asc ?▼
  "Indicator for columns sorted in ascending order, for GUI frames.
See `igist-tabulated-list-tty-sort-indicator-asc' for the indicator used on
`text-mode' frames."
  :group 'igist
  :type 'character)

(defcustom igist-tabulated-list-gui-sort-indicator-desc ?▲
  "Indicator for columns sorted in descending order, for GUI frames.
See `igist-tabulated-list-tty-sort-indicator-desc' for the indicator used on
`text-mode' frames."
  :group 'igist
  :type 'character)

(defcustom igist-tabulated-list-tty-sort-indicator-asc ?v
  "Indicator for columns sorted in ascending order, for `text-mode' frames.
See `igist-tabulated-list-gui-sort-indicator-asc' for the indicator used on GUI
frames."
  :group 'igist
  :type 'character)

(defcustom igist-tabulated-list-tty-sort-indicator-desc ?^
  "Indicator for columns sorted in ascending order, for `text-mode' frames.
See `igist-tabulated-list-gui-sort-indicator-asc' for the indicator used on GUI
frames."
  :group 'igist
  :type 'character)

(defcustom igist-tabulated-list-padding 2
  "Number of characters preceding each IGist List mode entry."
  :group 'igist
  :type 'integer)

(defcustom igist-debug-enabled-p nil
  "Whether to allow debug logging."
  :group 'igist
  :type 'boolean)

(defcustom igist-use-header-line t
  "Whether the Tabulated List buffer should use a header line."
  :type 'boolean
  :group 'igist)

(defvar-local igist-tabulated-list-entries nil
  "Entries displayed in the current IGist Buffer.")
(put 'igist-tabulated-list-entries 'permanent-local t)


(defvar-local igist-tabulated-list-sort-key nil
  "Sort key for the current Igist List mode buffer.
If nil, no additional sorting is performed.
Otherwise, this should be a cons cell (NAME . FLIP).
NAME is a string matching one of the column names in
`igist-list-format' (the corresponding SORT entry in
`igist-list-format' then specifies how to sort).  FLIP, if
non-nil, means to invert the resulting sort.")
(put 'igist-tabulated-list-sort-key 'permanent-local t)

(defsubst igist-tabulated-list-get-id (&optional pos)
  "Return the entry ID of the IGist entry at POS.
POS, if omitted or nil, defaults to point."
  (get-text-property (or pos (point)) 'igist-tabulated-list-id))

(defun igist-log (msg &rest args)
  "Log debug messages if `igist-debug-enabled-p' is non nil.

Argument MSG is a string that represents the message to be logged.
Optional argument ARGS is a list of arguments that can be used to format the
message string."
  (and igist-debug-enabled-p
       (apply #'message (concat (or msg ""))
              args)))

(defun igist-find-entry-bounds (id)
  "Find and return the boundaries of a specific entry in a tabulated list.

Argument ID is a value that is used to find the entry bounds in the igist
tabulated list."
  (pcase-let ((`(,beg . ,end)
               (cond ((equal (igist-tabulated-list-get-id) id)
                      (igist-property-boundaries 'igist-tabulated-list-id))
                     ((text-property-search-forward
                       'igist-tabulated-list-id
                       id
                       t)
                      (forward-char -1)
                      (igist-property-boundaries 'igist-tabulated-list-id))
                     ((text-property-search-backward 'igist-tabulated-list-id
                                                     id
                                                     t)
                      (igist-property-boundaries 'igist-tabulated-list-id)))))
    (when beg
      (cons beg end))))

(defun igist-update-entry (data)
  "Update a specific row in the tabulated list with new DATA.

Argument DATA is a list that contains the DATA to be updated in the row."
  (require 'text-property-search)
  (pcase-let* ((col (current-column))
               (`(,beg . ,end)
                (igist-find-entry-bounds (cdr (assq 'id data)))))
    (when (and beg end)
      (goto-char beg)
      (let ((inhibit-read-only t))
        (delete-region beg (1+ end))
        (igist-render-entry data)
        (goto-char beg)
        (move-to-column col)))))

(defun igist-remove-entry (id)
  "Remove a specific entry identified by its ID from the igist tabulated list.

Argument ID is a value that identifies the entry to be removed from the list."
  (require 'text-property-search)
  (pcase-let* ((col (current-column))
               (`(,beg . ,end)
                (save-excursion
                  (igist-find-entry-bounds id))))
    (when (and beg end)
      (goto-char beg)
      (let ((inhibit-read-only t))
        (delete-region beg (1+ end))
        (goto-char beg)
        (move-to-column col)))))

(defun igist-collapse-row-children ()
  "Collapse the children of a row in a tabulated list."
  (when-let ((id (igist-tabulated-list-get-id)))
    (setq igist-list-hidden-ids (push id igist-list-hidden-ids))
    (igist-update-entry (gethash id igist-rendered-hash))))

(defun igist-expand-row-children ()
  "Expand the row children in a tabulated list.

Argument VALUE is an optional parameter that represents the entries of
subcolumns if provided, otherwise it get the entries from the
`igist-list-response'."
  (when-let ((id (igist-tabulated-list-get-id)))
    (setq igist-list-hidden-ids (delete id igist-list-hidden-ids))
    (igist-update-entry (gethash id igist-rendered-hash))))

(defun igist-entry-expanded-p (id)
  "Check if ID is a member of `igist-list-hidden-ids''.

Argument ID is the identifier that will be checked for membership in the list
`igist-list-hidden-ids'."
  (not (member id igist-list-hidden-ids)))

(defun igist-toggle-children-row (&optional _subentries)
  "Toggle visibility of row's children in igist mode.

Argument SUBENTRIES is an optional argument that specifies the subentries to be
expanded when toggling the children row."
  (when-let ((id (igist-tabulated-list-get-id)))
    (if (igist-entry-expanded-p id)
        (igist-collapse-row-children)
      (igist-expand-row-children))))

(defun igist-toggle-all-children ()
  "Toggle the visibility of all children in the igist tabulated list."
  (interactive)
  (setq igist-default-collapsed (not igist-list-hidden-ids))
  (setq igist-list-hidden-ids (if igist-list-hidden-ids
                                  nil
                                (mapcar (apply-partially #'igist-alist-get 'id)
                                        igist-tabulated-list-entries)))
  (igist-tabulated-list-print t))

(defun igist-toggle-row-children-at-point ()
  "Toggle visibility of current row's children at point."
  (interactive)
  (igist-toggle-children-row))

(defun igist-format-time-diff (time)
  "Calculate and format the time difference from the current TIME.

Argument TIME is the time value that will be compared with the current time to
calculate the time difference."
  (let ((diff-secs (- (float-time (current-time))
                      (float-time time))))
    (pcase-let ((`(,format-str . ,value)
                 (cond ((< diff-secs 60)
                        (cons "%d second" (truncate diff-secs)))
                       ((< diff-secs 3600)
                        (cons "%d minute" (truncate (/ diff-secs 60))))
                       ((< diff-secs 86400)
                        (cons "%d hour" (truncate (/ diff-secs 3600))))
                       ((< diff-secs 2592000)
                        (cons "%d day" (truncate (/ diff-secs 86400))))
                       (t
                        (cons "%d month" (truncate (/ diff-secs 2592000)))))))
      (format (concat format-str (if (= value 1) " ago" "s ago")) value))))

(defun igist-render-time (value)
  "Format a given VALUE as a date and time.

Argument VALUE is the input value that will be used to calculate the rendered
time."
  (if value
      (igist-format-time-diff
       (parse-iso8601-time-string value))
    ""))

(defun igist-render-files (files)
  "Render FILES and join them with newlines and padding.

Argument FILES is an alist of gist files."
  (list
   (format "%s" (length files))
   'action
   #'igist-toggle-children-row 'button-data files))

(defun igist-render-comments (comments)
  "Default renderer for COMMENTS column."
  (cons
   (format "%s" comments)
   (list
    'action
    #'igist-load-comments)))

(defun igist-render-user (owner)
  "Default renderer for OWNER column."
  (let ((user (igist-alist-get
               'login owner)))
    (list (or (igist-alist-get
               'login owner)
              "")
          'button-data user
          'action #'igist-explore-load-other-user-gists)))

(defun igist-render-public (public)
  "Default renderer for PUBLIC column."
  (if public "Public" "Secret"))

(defun igist-render-url (url)
  "Render URL as a browseable button.

Render the URL for browsing."
  (list url 'action #'browse-url 'button-data url))

(defun igist-render-api-url (url)
  "Render API URL and download it.

Render the API URL for the given URL."
  (list url 'action (lambda (it)
                      (kill-new it)
                      (igist-message "Copied %s" it))
        'button-data url))

(defun igist-render-language (language)
  "Default renderer for LANGUAGE column."
  (list
   (or language "None")
   'action #'igist-toggle-language-filter
   'button-data (or language "None")))

(defvar igist-default-formats
  '((id "ID" 9 nil "%s" :pad-right 4)
    (description "Description" 50 igist-sort-pred-string "%s")
    (created_at "Created" 15 igist-sort-pred-date igist-render-time)
    (updated_at "Updated" 15 igist-sort-pred-date igist-render-time)
    (owner "User" 10 t igist-render-user)
    (comments "Comments" 9 igist-sort-pred-integer igist-render-comments
              :center-align t)
    (public "Public" 8 igist-sort-pred-boolean igist-render-public)
    (files "Files" 10 igist-sort-pred-list "%s"
           :children ((filename "File" 90 nil "%s")
                      (language "Language" 8
                                nil igist-render-language :right-align t))
           :align-to-column 1)
    (comments_url "Comment URL" 8 igist-sort-pred-string "%s")
    (html_url "URL" 20 igist-sort-pred-string igist-render-url)
    (forks_url "Forks" 20 igist-sort-pred-string igist-render-api-url)
    (truncated "Trunc." 4 t "%s" :center-align t)
    (git_push_url "Push URL" 20 igist-sort-pred-string igist-render-api-url)
    (git_pull_url "Pull URL" 20 igist-sort-pred-string igist-render-api-url)))

(defvar igist-sort-pred-alist
  '((id . igist-sort-pred-string)
    (url . igist-sort-pred-string)
    (forks_url . igist-sort-pred-string)
    (commits_url . igist-sort-pred-string)
    (node_id . igist-sort-pred-string)
    (git_pull_url . igist-sort-pred-string)
    (git_push_url . igist-sort-pred-string)
    (html_url . igist-sort-pred-string)
    (public . igist-sort-pred-boolean)
    (created_at . igist-sort-pred-date)
    (updated_at . igist-sort-pred-date)
    (description . igist-sort-pred-string)
    (comments . igist-sort-pred-integer)
    (comments_url . igist-sort-pred-string)
    (owner . igist-sort-pred-owner-login)
    (files . igist-sort-pred-list)))

(defvar igist-explore-buffer-name "*igist-explore*"
  "Buffer name for tabulated gists display of multiple owners.")

(defun igist-pick-from-alist (keys alist)
  "Filter ALIST by KEYS.

Argument ALIST is a list of key-value pairs.
Argument KEYS is a list of KEYS to filter the alist by."
  (let ((filtered-alist '()))
    (dolist (key keys)
      (when-let ((cell (assq key alist)))
        (setq filtered-alist (cons cell filtered-alist))))
    (nreverse filtered-alist)))

(defcustom igist-explore-gists-init-collapsed t
  "Whether the gists should be collapsed by default in explore buffers."
  :type 'boolean
  :group 'igist)

(defcustom igist-user-gists-init-collapsed nil
  "Whether the gists should be collapsed by default in user buffers."
  :type 'boolean
  :group 'igist)

(defun igist-make-column-custom-type (field-name &optional width)
  "Create a custom column type for the igist interface.

Argument FIELD-NAME is the name of the field for which a custom column type is
being created.
Argument WIDTH is an optional argument that specifies the WIDTH of the column;
if not provided, the length of the field name is used as the WIDTH."
  (let ((title (capitalize (car
                            (split-string
                             (symbol-name field-name) "[^a-z]" t))))
        (default-sorter (cdr (assq field-name igist-sort-pred-alist)))
        (default-formatter (or
                            (car (nthcdr 4 (assq field-name
                                                 igist-default-formats)))
                            "%s"))
        (props `(set
                 :inline t
                 (list
                  :inline t
                  (radio :value :center-align
                         (const
                          :tag "Right"
                          :right-align)
                         (const
                          :tag "Center"
                          :center-align))
                  (const :format "" t))
                 (list
                  :format "%v"
                  :inline t
                  (const
                   :format ""
                   :pad-right)
                  (integer :tag ":pad-right" 1)))))
    (when (eq field-name 'files)
      (setq props (append props '((list
                                   :format "%v"
                                   :inline t
                                   (const
                                    :format ""
                                    :align-to-column)
                                   (integer :tag ":align-to-column" 1))
                                  (list
                                   :format "%v"
                                   :inline t
                                   (const
                                    :format ""
                                    :children)
                                   (alist
                                    :tag "Subrow"
                                    :key-type (choice
                                               (const :tag "Filename" filename)
                                               (const :tag "Language" language)
                                               (const :tag "Raw_url" raw_url)
                                               (const :tag "Size" size))
                                    :value-type
                                    (list
                                     (string :tag "Column Name")
                                     (integer :tag "Column Width" 10)
                                     (choice
                                      (boolean :tag "Sortable")
                                      (function :tag "Sort function"))
                                     (choice :value "%s"
                                             (string
                                              :tag "Format")
                                             (function :tag "Formatter"))
                                     (set
                                      :inline t
                                      (list
                                       :inline t
                                       (radio :value :center-align
                                              (const
                                               :tag "Right"
                                               :right-align)
                                              (const
                                               :tag "Left"
                                               :center-align))
                                       (const :format "" t))
                                      (list
                                       :format "%v"
                                       :inline t
                                       (const
                                        :format ""
                                        :pad-right)
                                       (integer :tag ":pad-right" 1))))))))))
    `(list
      :tag ,(symbol-name field-name)
      (const ,field-name)
      (string
       :tag "Column Name"
       :completions
       (lambda
         (string pred action)
         (let ((completion-ignore-case t))
           (complete-with-action action (list ,title) string pred)))
       :value ,title)
      (integer :tag "Column Width" ,(or width
                                        (length title)))
      (choice
       :tag "Sortable"
       :value ,default-sorter
       (boolean :tag "Sortable" t)
       (function :tag "Sort function" ,default-sorter))
      ,(if (stringp default-formatter)
           `(radio :value ,default-formatter
                   (string :tag "Format" "%s")
                   (function :tag "Formatter"))
         `(radio :value ,default-formatter
                 (string :tag "Format" "%s")
                 (function :tag "Formatter" ,default-formatter)))
      ,props)))


(defcustom igist-explore-format '((id "ID" 10 nil "%s" :pad-right 4)
                                  (description "Description" 50
                                               igist-sort-pred-string "%s")
                                  (updated_at "Updated" 15 igist-sort-pred-date
                                              igist-render-time)
                                  (owner "User" 10 t igist-render-user
                                         :center-align t
                                         :pad-right 4)
                                  (comments "Comments" 9 igist-sort-pred-integer
                                            igist-render-comments
                                            :center-align t)
                                  (files "Files" 10 igist-sort-pred-list "%s"
                                         :children
                                         ((filename "File" 90 nil "%s")
                                          (language "Language" 8 nil
                                                    igist-render-language
                                                    :right-align t))
                                         :align-to-column 1))
  "The format of the Explore Public Gists tabulated buffers.

Each element in the alist represents a column and has the following structure:

\(FIELD COLUMN-NAME WIDTH SORTABLE FORMAT-FUNCTION/FORMAT-STRING PROPS)

- FIELD: Specifies the field name or key for the column.
- COLUMN-NAME: Specifies the name or label for the column.
- WIDTH: Specifies the width of the column.
- SORTABLE: Indicates whether the column is sortable. Can be t, nil or
a function.
- FORMAT-FUNCTION/FORMAT-STRING: Specifies the function or format string
 used to format the data. A function will be called with the value of
the corresponding field name.
- PROPS is a plist of additional column properties.

Currently supported properties are:
   - :center-align or :right-align:
If non-nil, align the column should be center or right-aligned.
   - :pad-right: Number of additional padding spaces to theright of the column,
   - :children - Specification expandable row values in the same format,
      but without PROPS:
\(FIELD COLUMN-NAME WIDTH SORTABLE FORMAT-FUNCTION/FORMAT-STRING)
   - :align-to-column - index of parent column to align children.

User can interactively add or remove columns, edit them adjusts the
width of the columns with transient inferface:

- `igist-table-menu'

and save the result with command `igist-save-column-settings'."
  :type `(repeat
          (choice
           :tag "Column"
           :value ,(car (last igist-default-formats))
           ,@(mapcar (lambda (it)
                       (igist-make-column-custom-type it))
                     '(id description comments owner
                          user
                          files
                          node_id git_pull_url
                          url forks_url commits_url
                          git_push_url
                          html_url public created_at
                          updated_at
                          comments_url truncated))))
  :group 'igist)

(defcustom igist-list-format (igist-pick-from-alist
                              '(id
                                description
                                public
                                updated_at
                                comments
                                files)
                              (copy-tree igist-default-formats))
  "The format of the Gists to display in users buffers.

Each element in the alist represents a column and has the following structure:

\(FIELD COLUMN-NAME WIDTH SORTABLE FORMAT-FUNCTION/FORMAT-STRING PROPS)

- FIELD: Specifies the field name or key for the column.
- COLUMN-NAME: Specifies the name or label for the column.
- WIDTH: Specifies the width of the column.
- SORTABLE: Indicates whether the column is sortable. Can be t, nil or
a function.
- FORMAT-FUNCTION/FORMAT-STRING: Specifies the function or format string
 used to format the data. A function will be called with the value of
the corresponding field name.
- PROPS is a plist of additional column properties.

Currently supported properties are:
   - :center-align or :right-align:
If non-nil, align the column should be center or right-aligned.
   - :pad-right: Number of additional padding spaces to theright of the column,
   - :children - Specification expandable row values in the same format,
      but without PROPS:
\(FIELD COLUMN-NAME WIDTH SORTABLE FORMAT-FUNCTION/FORMAT-STRING)
   - :align-to-column - index of parent column to align children.

User can interactively add or remove columns, edit them adjusts the
width of the columns with transient inferface:

- `igist-table-menu'

and save the result with command `igist-save-column-settings'."
  :type `(repeat
          (choice
           :tag "Column"
           :value ,(car (last igist-default-formats))
           ,@(mapcar (lambda (it)
                       (igist-make-column-custom-type it))
                     '(id description comments owner
                          user
                          files
                          node_id git_pull_url
                          url forks_url commits_url
                          git_push_url
                          html_url public created_at
                          updated_at
                          comments_url truncated))))
  :group 'igist)

(defcustom igist-enable-copy-gist-url-p 'after-new
  "Whether and when to add new or updated gist's URL to kill ring."
  :group 'igist
  :type '(radio
          (const :tag "After creating new and saving existing gists" t)
          (const :tag "After saving existing gists" after-update)
          (const :tag "After creating new gist" after-new)
          (const :tag "Never" nil)))

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

(defun igist-check-per-page-range (n)
  "Validate per page N."
  (and (numberp n)
       (>= n 1)
       (<= n 100)))

(defcustom igist-per-page-limit 30
  "The number of results per page. It should be the value between 30 to 100."
  :type '(restricted-sexp
          :tag "Per page"
          :value 30
          :match-alternatives (igist-check-per-page-range))
  :group 'igist)

(defcustom igist-message-function 'minibuffer-message
  "Function to show messages.
Should accept the same arguments as `message'."
  :type '(choice
          (const :tag "None" nil)
          (function :tag "Function"))
  :group 'igist)

(defvar igist-before-save-hook '()
  "A list of hooks run before posting gist.")

(defcustom igist-current-user-name ""
  "The GitHub username for the current user.

This is the username used by the `igist' for interactions
with the GitHub service. Setting this variable allows the
package to make requests on behalf of the user for actions such
as listing or creating gists."
  :group 'igist
  :type 'string)

(defcustom igist-auth-marker 'igist
  "GitHub OAuth token or suffix added to the USERNAME^MARKER in auth sources.

This variable can either hold an explicit OAuth token as a string,
in which case igist will use this token for authentication, or a
symbol that is used as a suffix for the `login' field in `auth-sources'.

For example, if you set this to the symbol `igist' (the default setting),
you would add an entry like this to your auth-sources:

\"machine api.github.com login GITHUB_USERNAME^igist password GITHUB_TOKEN\".

Igist will then use the GITHUB_TOKEN from this entry when authenticating
with the GitHub API."
  :group 'igist
  :type '(radio
          (string :tag "OAuth Token")
          (symbol :tag "Suffix" igist)))

(defvar igist-github-token-scopes '(gist)
  "The required GitHub API scopes.

You need the gist OAuth scope and a token.

You have to manually create or update the token at
https://github.com/settings/tokens.  This variable
only serves as documentation.")

(defvar-local igist-current-gist nil
  "Current gist in the edit buffer.")

(put 'igist-current-gist 'permanent-local t)

(defvar-local igist-current-description nil
  "Current gist description in the edit buffer.")

(put 'igist-current-description 'permanent-local t)

(defvar-local igist-current-public nil
  "Whether the current gist in the edit buffer is public or private.")

(put 'igist-current-public 'permanent-local t)

(defvar-local igist-current-filename nil
  "Current gist filename.")

(put 'igist-current-filename 'permanent-local t)

(defvar-local igist-comment-gist-id nil
  "Current gist's id in the comment buffer.")

(put 'igist-comment-gist-id 'permanent-local t)

(defvar-local igist-comment-id nil
  "Current comment id.")

(put 'igist-comment-id 'permanent-local t)

(defvar-local igist-list-loading nil)
(defvar-local igist-list-page 0)
(defvar-local igist-list-cancelled nil)

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
    (define-key map (kbd "+") #'igist-list-add-file)
    (define-key map (kbd "-") #'igist-delete-current-filename)
    (define-key map (kbd "g") #'igist-list-refresh)
    (define-key map (kbd "G") #'igist-tabulated-list-revert)
    (define-key map (kbd "c") #'igist-load-comments)
    (define-key map (kbd "w") #'igist-copy-gist-url)
    (define-key map (kbd "a") #'igist-add-comment)
    (define-key map (kbd "f") #'igist-fork-gist)
    (define-key map (kbd "d") #'igist-list-edit-description)
    (define-key map (kbd "r") #'igist-browse-gist)
    (define-key map (kbd "S") #'igist-star-gist)
    (define-key map (kbd "U") #'igist-unstar-gist)
    (define-key map (kbd "D") #'igist-delete-current-gist)
    (define-key map (kbd "RET") #'igist-list-edit-gist-at-point)
    (define-key map (kbd "C-j") #'igist-list-view-current)
    (define-key map (kbd "v") #'igist-list-view-current)
    (define-key map (kbd "s") #'igist-tabulated-list-sort)
    (define-key map (kbd "}") #'igist-tabulated-list-widen-current-column)
    (define-key map (kbd "{") #'igist-tabulated-list-narrow-current-column)
    (define-key map (kbd "K") #'igist-list-cancel-load)
    (define-key map (kbd "<backtab>") #'igist-toggle-all-children)
    (define-key map (kbd "<tab>") #'igist-toggle-row-children-at-point)
    (define-key map (kbd "C") #'igist-table-menu)
    (define-key map (kbd "M-[") #'igist-swap-current-column-backward)
    (define-key map (kbd "M-]") #'igist-swap-current-column)
    (define-key map (kbd "M-{") #'igist-swap-current-column-backward)
    (define-key map (kbd "M-}") #'igist-swap-current-column)
    (define-key map (kbd "?") #'igist-dispatch)
    (define-key map (kbd "/") #'igist-filters-menu)
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "p") #'previous-line)
    (define-key map (kbd "C-M-f") #'igist-tabulated-forward-column)
    (define-key map (kbd "C-M-b") #'igist-tabulated-backward-column)
    (define-key map (kbd "C-M-n") #'igist-list-forward-row-and-preview)
    (define-key map (kbd "C-M-p") #'igist-list-backward-row-and-preview)
    (set-keymap-parent map (make-composed-keymap button-buffer-map
                                                 special-mode-map))
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

(defvar igist-tabulated-list-sort-button-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<header-line> <mouse-1>")
                #'igist-tabulated-list-col-sort)
    (define-key map (kbd "<header-line> <mouse-2>")
                #'igist-tabulated-list-col-sort)
    (define-key map (kbd "<mouse-1>") #'igist-tabulated-list-col-sort)
    (define-key map (kbd "<mouse-2>") #'igist-tabulated-list-col-sort)
    "RET"                     #'igist-tabulated-list-sort
    map))

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

(defmacro igist-pipe (&rest functions)
  "Return left-to-right composition from FUNCTIONS."
  (declare (debug t)
           (pure t)
           (side-effect-free t))
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
  (declare (debug t)
           (pure t)
           (side-effect-free t))
  `(igist-pipe ,@(reverse functions)))

(defmacro igist-with-exisiting-buffer (buffer-or-name &rest body)
  "Expand BODY in buffer BUFFER-OR-NAME if it exists and visible."
  (declare (indent 1))
  (let ((buffname-var (make-symbol "buffname-var"))
        (buff-var (make-symbol "buff-var")))
    `(let* ((,buffname-var ,buffer-or-name)
            (,buff-var (and ,buffname-var
                            (get-buffer ,buffname-var))))
       (when (buffer-live-p ,buff-var)
         (with-current-buffer ,buff-var
           (progn ,@body))))))

(defun igist-get-current-user-name ()
  "Return the current user's name if it's a non-empty string."
  (when (and igist-current-user-name
             (stringp igist-current-user-name)
             (not (string-empty-p igist-current-user-name)))
    igist-current-user-name))

(defun igist-get-user-buffer-name (user)
  "Return the name of buffer with USER's gists."
  (when user
    (concat "*igist-" user "*")))

(defun igist-get-logged-user-buffer ()
  "Return the name of buffer with USER's gists."
  (let ((buff-name (igist-get-user-buffer-name
                    igist-current-user-name)))
    (and buff-name
         (get-buffer buff-name))))


(defun igist-get-gist-buffer (id filename)
  "Return gist's FILENAME buffer with ID."
  (get-buffer (concat id "-" filename)))

(defun igist-comments-list-mode-p ()
  "Return non nil if `igist-comments-list-mode' is active."
  (and (boundp 'igist-comments-list-mode)
       (symbol-value 'igist-comments-list-mode)))

(defun igist-edit-mode-p ()
  "Return non nil if `igist-edit-mode' is active."
  (and (boundp 'igist-edit-mode)
       (symbol-value 'igist-edit-mode)))

(defun igist-edit-ensure-edit-mode ()
  "Turn on `igist-edit-mode' if it is not active."
  (unless (igist-edit-mode-p)
    (igist-edit-mode)))

(defun igist-message (&rest args)
  "Apply `igist-message-function' with ARGS."
  (when igist-message-function
    (apply igist-message-function args)))


(defun igist--all-pass (item filters)
  "Apply all FILTERS to an ITEM and return t if it passes all.

Argument ITEM is the object that will be evaluated by the function or macro.
Argument FILTERS is a list of functions that will be applied to the item in
order."
  (not (catch 'found
         (dolist (filter filters)
           (unless (funcall filter item)
             (throw 'found t))))))

(defun igist-get-current-list-format-sym ()
  "Determine the current list format symbol based on the buffer type."
  (if (eq major-mode 'igist-explore-mode)
      'igist-explore-format
    'igist-list-format))

(defun igist-editable-p (&optional gist)
  "Check whether user `igist-current-user-name' can edit GIST."
  (and (igist-get-current-user-name)
       (cond (gist
              (if-let ((owner (igist-get-owner
                               gist)))
                  (equal (igist-get-current-user-name)
                         owner)
                t))
             ((derived-mode-p 'igist-list-mode)
              (when-let ((owner (igist-get-owner
                                 (igist-tabulated-gist-at-point))))
                (equal (igist-get-current-user-name) owner)))
             ((igist-edit-mode-p)
              (if-let ((owner (igist-get-owner
                               igist-current-gist)))
                  (equal (igist-get-current-user-name)
                         owner)
                t)))))

(defun igist-not-editable-p (&optional gist)
  "Return t if user `igist-current-user-name' cannot edit GIST."
  (not (igist-editable-p gist)))

(defun igist-forkable (&optional gist)
  "Return t if user `igist-current-user-name' can fork GIST."
  (and
   (igist-get-current-user-name)
   (when-let ((owner (igist-get-owner (or
                                       gist
                                       (igist-tabulated-gist-at-point)
                                       igist-current-gist))))
     (not (equal (igist-get-current-user-name) owner)))))

(defun igist-get-current-gist-url ()
  "Return HTML URL from `igist-current-gist'."
  (igist-alist-get 'html_url igist-current-gist))

(defun igist-get-current-gist-id ()
  "Return id from `igist-current-gist'."
  (igist-alist-get 'id igist-current-gist))

(defun igist-get-comment-id-at-point ()
  "Return the value of `igist-comment-id' text property at point."
  (get-text-property (point) 'igist-comment-id))

;; Request api
(cl-defun igist-request (method resource &optional params &key query payload
                                headers silent unpaginate noerror reader auth
                                username host forge callback errorback value
                                buffer extra)
  "Make a METHOD request for RESOURCE with `ghub-request'.

If BUFFER is non-nil, it should be a buffer to show the spinner in the
mode line.

Arguments PARAMS, QUERY, PAYLOAD, HEADERS, SILENT, UNPAGINATE, NOERROR, READER,
USERNAME, AUTH, HOST, FORGE, CALLBACK, ERRORBACK, VALUE and EXTRA have the same
 meaning, as in `ghub-request'."
  (when buffer
    (igist-set-loading t buffer))
  (ghub-request method
                resource
                params
                :username (or username (igist-get-current-user-name))
                :query query
                :auth (or auth
                          (when (igist-get-current-user-name)
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

If BUFFER is non-nil, it should be a buffer to show the spinner in the
mode line.

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

If BUFFER is non-nil, it should be a buffer to show the spinner in the mode
 line.

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

If BUFFER is non-nil, it should be a buffer to show the spinner in
the mode line.

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

(defun igist-take-last (n lst)
  "Return the last N elements from a list.

Argument N is an integer that specifies the number of elements to take from the
end of the list.
Argument LST is a list from which the last N elements will be taken."
  (let ((len (length lst)))
    (if (> n len)
        lst
      (nthcdr (- len n) lst))))

(defun igist-insert-at (list elem index)
  "Insert ELEM at INDEX in LIST."
  (if (zerop index)
      (cons elem list)
    (let ((prev (nthcdr (1- index) list)))
      (setcdr prev (cons elem (cdr prev)))
      list)))

(defun igist-swap (i j lst)
  "Swap the elements at positions I and J in a given list LST.

Argument I is the index of the first element in the list that you want to swap.
Argument J is the index of the second element in the list that you want to swap
with the first element.
Argument LST is the list in which the swapping of elements will take place."
  (let ((elem (nth i lst)))
    (setf (nth i lst)
          (nth j lst))
    (setf (nth j lst) elem))
  lst)

(defun igist-seq-split (sequence length)
  "Split SEQUENCE into a list of sub-sequences of at most LENGTH elements.
All the sub-sequences will be LENGTH long, except the last one,
which may be shorter."
  (when (< length 1)
    (error "Sub-sequence length must be larger than zero"))
  (let ((result nil)
        (seq-length (length sequence))
        (start 0))
    (while (< start seq-length)
      (push (seq-subseq sequence start
                        (setq start (min seq-length (+ start length))))
            result))
    (nreverse result)))

(defun igist-alist-get (key alist)
  "Find the first element of ALIST whose car equals KEY and return its cdr."
  (cdr (assoc key alist)))

(defun igist-download-url (url)
  "Download URL and return string."
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

(defun igist-find-by-id-and-file (id filename response)
  "Return gists with ID and FILENAME in RESPONSE."
  (let ((gist (seq-find (lambda (cell)
                          (and (equal id (igist-alist-get 'id cell))))
                        response)))
    (igist-normalize-gist-file gist filename)))

(defun igist-set-major-mode (filename)
  "Guess major mode for FILENAME."
  (let ((buffer-file-name (or
                           (if (file-name-absolute-p filename)
                               filename
                             (expand-file-name filename default-directory)))))
    (ignore-errors
      (set-auto-mode)
      (font-lock-ensure))))

(defun igist-alist-find-by-prop (prop value alist)
  "Return first element in ALIST which property PROP equals VALUE."
  (seq-find (lambda (cell)
              (and (equal value
                          (igist-alist-get prop cell))))
            alist))

(defun igist-tabulated-gist-at-point ()
  "Get tabulated gist at point."
  (when-let ((id (igist-tabulated-list-get-id)))
    (and igist-rendered-hash
         (gethash id igist-rendered-hash))))

(defun igist-gist-filename-at-point ()
  "Search for the closest text property filename on the current line."
  (or (get-text-property (point) 'filename)
      (save-excursion
        (skip-chars-forward "\s\t")
        (get-text-property (point) 'filename))))

(defun igist-tabulated-gist-file-at-point ()
  "Get tabulated gist with file at point."
  (when-let ((parent (igist-tabulated-gist-at-point))
             (filename (igist-gist-filename-at-point)))
    (cdr (igist-normalize-gist-file parent filename))))

(defun igist-read-gist-file (prompt gist)
  "Read file in GIST with PROMPT in mini-buffer.
GIST should be raw GitHub item."
  (let ((filename (completing-read
                   prompt
                   (igist-alist-get 'files gist))))
    (cdr (igist-normalize-gist-file gist filename))))

(defun igist-copy-gist-url ()
  "Copy URL of gist at point or currently open."
  (interactive)
  (when-let ((gist-url
              (igist-alist-get 'html_url
                               (or (igist-tabulated-gist-at-point)
                                   igist-current-gist))))
    (kill-new gist-url)
    (igist-message "Copied %s" gist-url)))

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
      (when-let ((parent (igist-tabulated-gist-at-point)))
        (if (= 1 (length (igist-alist-get 'files parent)))
            (cdr (igist-normalize-gist-file parent
                                            (igist-alist-get 'filename
                                                             (cdar
                                                              (igist-alist-get
                                                               'files
                                                               parent)))))
          (igist-read-gist-file "Filename: " parent)))))

(defun igist-list-edit-gist-at-point (&optional _entry)
  "Edit the gist at the current point in a new window."
  (interactive)
  (when-let ((gist (igist-list-gist-to-fetch)))
    (let ((buff (igist-setup-edit-buffer gist)))
      (switch-to-buffer-other-window buff))))

(defun igist-list-view-current (gist)
  "Display GIST in other window, without selecting it."
  (interactive (list (igist-list-gist-to-fetch)))
  (when gist
    (let ((current-window (selected-window)))
      (with-selected-window current-window
        (when-let ((wnd (or (window-right current-window)
                            (window-left current-window)
                            (split-window-right nil current-window))))
          (with-selected-window wnd
            (let ((buff (igist-setup-edit-buffer gist)))
              (pop-to-buffer-same-window buff))))))))

(defun igist-list-forward-row-and-preview (&optional n)
  "Move N lines forward and preview gist."
  (interactive)
  (when (zerop (forward-line n))
    (if-let ((file (igist-tabulated-gist-file-at-point)))
        (igist-list-view-current file)
      (when-let* ((parent (igist-tabulated-gist-at-point))
                  (file
                   (cdr
                    (igist-normalize-gist-file
                     parent
                     (igist-alist-get 'filename
                                      (cdar
                                       (igist-alist-get
                                        'files
                                        parent)))))))
        (let ((buff (igist-setup-edit-buffer file)))
          (unless (get-buffer-window buff)
            (let ((current-window (selected-window)))
              (with-selected-window current-window
                (when-let ((wnd (or (window-right current-window)
                                    (window-left current-window)
                                    (split-window-right nil current-window))))
                  (with-selected-window wnd
                    (pop-to-buffer-same-window buff)))))))))))

(defun igist-list-backward-row-and-preview (&optional n)
  "Move N lines forward and preview gist."
  (interactive)
  (igist-list-forward-row-and-preview (or
                                       (and n (- n))
                                       -1)))

(defun igist-explore-load-other-user-gists (user)
  "Load gists from another USER.

Argument USER is the username of the user whose gists will be loaded."
  (interactive
   (list
    (or (igist-get-owner (igist-tabulated-gist-at-point))
        (read-string "User: "))))
  (igist-list-load-gists user))


(defun igist-list-edit-description (&optional id)
  "Edit the description of a specified gist pin the igist list.

Optional argument ID is a string that represents the identifier of a gist.
It has no default value."
  (interactive
   (list
    (or (igist-tabulated-list-get-id)
        (cdr (assq 'id (cdr (igist-completing-read-gists "Gist Id: ")))))))
  (let* ((gist (or (and igist-rendered-hash
                        (gethash id igist-rendered-hash))
                   (igist-alist-find-by-prop 'id id igist-list-response)
                   (ghub-get (concat "/gists/" id)
                             nil
                             :username (igist-get-current-user-name)
                             :auth igist-auth-marker
                             :silent t)))
         (description (igist-alist-get
                       'description gist)))
    (cond ((igist-editable-p gist)
           (let*
               ((input
                 (if
                     (and
                      description
                      (not
                       (string-empty-p
                        description)))
                     description
                   (let* ((files (igist-alist-get
                                  'files
                                  gist))
                          (file
                           (alist-get
                            'filename
                            (cdar files))))
                     (and file
                          (with-temp-buffer
                            (insert (file-name-base
                                     file))
                            (let ((case-fold-search nil))
                              (while (re-search-backward "[A-Z]+" nil t 1)
                                (unless (bobp)
                                  (insert "\s"))))
                            (string-trim
                             (buffer-substring-no-properties
                              (point-min)
                              (point-max))))))))
                (payload `((description .
                                        ,(read-string "Description: " input)))))
             (igist-patch (concat "/gists/" id)
                          nil
                          :silent t
                          :payload payload
                          :callback (lambda (val &rest _)
                                      (when val
                                        (igist-load-logged-user-gists))))))
          (t (if description
                 (igist-message "igist: %s" description)
               (igist-message "igist: No description"))))))

(defun igist-read-filename-new (gist)
  "Prompt for a new filename for a GIST, ensuring it doesn't already exist.

Argument GIST is an associative list (alist) representing a GitHub gist, which
includes information such as the gist's ID and files."
  (let* ((filenames (mapcar (apply-partially #'igist-alist-get 'filename)
                            (igist-alist-get 'files gist)))
         (id (igist-alist-get 'id gist))
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

(defun igist-sort-pred-string (a b)
  "The default function to sort strings in ascending order.

Arguments A and B are the strings to be sorted."
  (string< (or a "")
           (or b "")))

(defun igist-sort-pred-list (a b)
  "The default function to sort lists in ascending order.

A and B are the lists to be sorted."
  (< (length a)
     (length b)))

(defun igist-sort-pred-integer (a b)
  "The default function to sort integers in ascending order.

A and B are the lists to be sorted."
  (< (or a 0)
     (or b 0)))

(defun igist-sort-pred-date (a b)
  "The default function to sort date strings in ascending order.

A and B are the dates to be sorted."
  (time-less-p (or (and a (parse-iso8601-time-string a))
                   0)
               (or (and b (parse-iso8601-time-string b))
                   0)))

(defun igist-sort-pred-boolean (a b)
  "The default function to sort boolean values in ascending order.

A and B are t or nil."
  (< (if a 1 -1)
     (if b 1 -1)))

(defun igist-sort-pred-owner-login (a b)
  "The default function to sort by login name in ascending order.

Arguments A and B are the alists of owner's fields, including login."
  (igist-sort-pred-string
   (igist-alist-get 'login a)
   (igist-alist-get 'login b)))

(defun igist-tabulated-list--column-number (name)
  "Determine the column number of a given NAME in the `igist-table-list-format'.

Argument NAME is a variable that is expected to hold the NAME of the column in
the `igist-table-list-format'."
  (seq-position igist-table-list-format name (lambda (a b)
                                               (equal (cadr a) b))))

(defun igist-list--get-sorter ()
  "Return a sorting predicate for the current tabulated-list.
Return nil if `igist-tabulated-list-sort-key' specifies an unsortable
column.  Negate the predicate that would be returned if
`igist-tabulated-list-sort-key' has a non-nil cdr."
  (when (and igist-tabulated-list-sort-key
             (car igist-tabulated-list-sort-key))
    (let* ((format-spec igist-table-list-format)
           (n (igist-tabulated-list--column-number
               (car igist-tabulated-list-sort-key)))
           (spec (nth n format-spec))
           (field (car spec))
           (sort-spec (nth 3 spec))
           (sort-fn (if (eq sort-spec t)
                        (cdr (assq field igist-sort-pred-alist))
                      sort-spec)))
      (if (cdr igist-tabulated-list-sort-key)
          (lambda (a b)
            (funcall sort-fn
                     (igist-alist-get field b)
                     (igist-alist-get field a)))
        (lambda (a b)
          (funcall sort-fn
                   (igist-alist-get field a)
                   (igist-alist-get field b)))))))

(defun igist-tabulated-list-col-sort (&optional e)
  "Sort Igist Tabulated List entries by the column of the mouse click E."
  (interactive "e")
  (let* ((pos (event-start e))
         (obj (posn-object pos)))
    (with-current-buffer (window-buffer (posn-window pos))
      (igist-tabulated-list--sort-by-column-name
       (get-text-property (if obj (cdr obj)
                            (posn-point pos))
                          'igist-tabulated-list-column-name
                          (car obj))))))


(defun igist-render-overlay-header ()
  "Render the header overlay for the Igist table list."
  (when header-line-format
    (setq-local header-line-format nil))
  (let* ((pos (point-min))
         (end pos))
    (dolist (ov (overlays-in pos end))
      (when (overlay-get ov 'igist-header)
        (delete-overlay ov)))
    (let ((str (concat (igist-render-header
                        igist-table-list-format
                        t)
                       "\n")))
      (add-text-properties 0 (length str)
                           `(face
                             (:weight bold))
                           str)
      (igist-overlay-make pos
                          end
                          nil
                          nil
                          nil
                          'after-string
                          str
                          'igist-header
                          t))))

(defun igist-overlay-make (start end &optional buffer front-advance rear-advance
                                 &rest props)
  "Create a new overlay with range BEG to END in BUFFER and return it.
If omitted, BUFFER defaults to the current buffer.
START and END may be integers or markers.
The fourth arg FRONT-ADVANCE, if non-nil, makes the marker
for the front of the overlay advance when text is inserted there
\(which means the text *is not* included in the overlay).
The fifth arg REAR-ADVANCE, if non-nil, makes the marker
for the rear of the overlay advance when text is inserted there
\(which means the text *is* included in the overlay).
PROPS is a plist to put on overlay."
  (let ((overlay (make-overlay start end buffer front-advance
                               rear-advance)))
    (dotimes (idx (length props))
      (when (eq (logand idx 1) 0)
        (let* ((prop-name (nth idx props))
               (val (plist-get props prop-name)))
          (overlay-put overlay prop-name val))))
    overlay))


(defun igist-render-header (list-format &optional allow-children)
  "Render the header for a tabulated list.

Argument LIST-FORMAT is a list that defines the format of the table.

Optional argument ALLOW-CHILDREN is a boolean value.
If it is non-nil, the function will allow nested tables.
Its default value is nil."
  (let* ((x (max igist-tabulated-list-padding 0))
         (button-props `(help-echo "Click to sort by column"
                                   mouse-face header-line-highlight
                                   keymap
                                   ,igist-tabulated-list-sort-button-map))
         (last-col (car (last list-format)))
         (cols nil)
         (children)
         (children-indent)
         (widths))
    (push (propertize " " 'display
                      `(space :align-to (+ header-line-indent-width ,x)))
          cols)
    (pcase-dolist (`(,field-name ,col-name ,width ,sortable ,_format-spec .
                                 ,props)
                   list-format)
      (let* ((not-last-col (not (eq field-name (car last-col))))
             (label col-name)
             (lablen (string-width label))
             (pname label)
             (pad-right (or (plist-get props :pad-right) 1))
             (right-align (plist-get props :right-align))
             (center-align (plist-get props :center-align))
             (next-x (+ x pad-right width))
             (available-space
              (and not-last-col
                   width)))
        (when (and allow-children)
          (push next-x widths)
          (when (plist-get props :children)
            (setq children (plist-get props :children))
            (setq children-indent (plist-get props :align-to-column))))
        (when (and (>= lablen 3)
                   not-last-col
                   (> lablen available-space))
          (setq label
                (truncate-string-to-width label available-space nil t t)))
        (when (and label (string-match-p "[\n\r\f]" label))
          (setq label (string-join (split-string label "[\n\r\f]" t) " ")))
        (push
         (cond ((not sortable)
                (propertize label 'igist-tabulated-list-column-name
                            pname
                            'field-name field-name))
               ((equal col-name
                       (car igist-tabulated-list-sort-key))
                (let ((suffix
                       (cond ((and (< lablen 3) not-last-col) "")
                             ((cdr igist-tabulated-list-sort-key)
                              (format
                               " %c"
                               igist-tabulated-list-gui-sort-indicator-desc))
                             (t
                              (format
                               " %c"
                               igist-tabulated-list-gui-sort-indicator-asc)))))
                  (apply #'propertize
                         (concat label
                                 suffix)
                         'face 'bold
                         'igist-tabulated-list-column-name pname
                         button-props)))
               (t (apply #'propertize label
                         'igist-tabulated-list-column-name pname
                         button-props)))
         cols)
        (when-let* ((label-width (string-width (car cols)))
                    (shift
                     (cond (center-align
                            (/ (- width label-width) 2))
                           (right-align (- width label-width)))))
          (when (> shift 0)
            (setq cols
                  (cons (car cols)
                        (cons
                         (propertize
                          (make-string shift ?\s)
                          'display
                          `(space :align-to
                                  (+ header-line-indent-width ,(+ x shift))))
                         (cdr cols))))
            (setq x (+ x shift))))
        (if (>= pad-right 0)
            (push (propertize
                   " "
                   'display `(space :align-to
                                    (+ header-line-indent-width ,next-x))
                   'face 'fixed-pitch)
                  cols))
        (setq x next-x)))
    (if children
        (let* ((igist-tabulated-list-padding
                (car
                 (seq-take (reverse widths)
                           children-indent)))
               (child-cols (igist-render-header children))
               (parcols (apply #'concat (nreverse cols))))
          (concat parcols "\n" child-cols))
      (apply #'concat (nreverse cols)))))


(defun igist-tabulated-list-init-header ()
  "Set up header line for the Igist Tabulated List buffer."
  (if (not igist-use-header-line)
      (igist-render-overlay-header)
    (setq header-line-format (list "" 'header-line-indent
                                   (igist-render-header
                                    igist-table-list-format)))))

(defun igist-tabulated-list-render-col (spec data used-width not-last-col)
  "Render a column in a tabulated list with specified properties and format.

Argument SPEC is a list that specifies the column specification.
Argument DATA is an association list that represents the values to be rendered.
Argument USED-WIDTH is an integer that represents the width used so far.
Argument NOT-LAST-COL is a boolean value that indicates whether the current
column is the last one or not."
  (pcase-let ((`(,field-name ,column-name ,width ,_sortable ,format-val .
                             ,props)
               spec))
    (let ((value (cdr (assq field-name data)))
          (opoint (point))
          (available-space width)
          (pad-right (or (plist-get props :pad-right) 1)))
      (let* ((col-desc
              (cond ((and (plist-get props :children))
                     (if value
                         (list
                          (format "%s" (length value))
                          'action
                          #'igist-toggle-children-row 'button-data value)
                       (format "%s" (length value))))
                    ((functionp format-val)
                     (or (funcall format-val value) ""))
                    (t (format
                        (or format-val "%s")
                        (or value "")))))
             (label
              (let ((d
                     (cond ((stringp col-desc) col-desc)
                           ((eq (car col-desc) 'image) " ")
                           (t (car col-desc)))))
                (if (and d (string-match-p "[\n\r\f]" d))
                    (string-join (split-string d "[\n\r\f]" t) " ")
                  d)))
             (label-width (string-width label))
             (help-echo (concat column-name ": " label)))
        (when (and not-last-col
                   (>= label-width available-space))
          (setq label (truncate-string-to-width
                       label available-space nil nil t "|")
                label-width available-space))
        (setq label (bidi-string-mark-left-to-right label))
        (when-let ((shift
                    (cond ((not (> width label-width))
                           nil)
                          ((plist-get props :center-align)
                           (/ (- width label-width) 2))
                          ((plist-get props :right-align)
                           (- width label-width)))))
          (insert (propertize (make-string shift ?\s)
                              'display
                              `(space :align-to ,(+ used-width
                                                    shift))))
          (setq width (- width shift))
          (setq used-width (+ used-width shift)))
        (cond ((stringp col-desc)
               (insert (if (get-text-property 0 'help-echo label)
                           label
                         (propertize label 'help-echo help-echo))))
              ((eq (car col-desc) 'image)
               (insert (propertize " "
                                   'display col-desc
                                   'help-echo help-echo)))
              (t (apply #'insert-text-button label (cdr col-desc))))
        (let ((next-x (+ used-width pad-right width)))
          (when not-last-col
            (when (> pad-right 0)
              (insert (make-string pad-right ?\s)))
            (insert (propertize
                     (make-string (- width (min width label-width)) ?\s)
                     'display `(space :align-to ,next-x))))
          (add-text-properties opoint (point)
                               (list 'igist-tabulated-list-column-name
                                     column-name
                                     'field-name
                                     field-name
                                     field-name
                                     value))
          next-x)))))


(defun igist-list-render-list-format (list-format data &optional padding props)
  "Render a DATA list format with optional PADDING and properties.

Argument LIST-FORMAT is a list that specifies the format of the list to be
rendered.
Argument DATA is an alist of the first spec.
Optional argument PADDING is an integer that specifies the total width of
previous columns in the row.
If not provided, its default value is 0.
Optional argument PROPS is a list of properties to be added to the text.
If not provided, its default value is nil."
  (let ((beg (point))
        (children-spec)
        (widths)
        (last-col-spec (car (last list-format)))
        (used-width (or padding 0))
        (col-names))
    (when (and padding (> padding 0))
      (insert (make-string padding ?\s)))
    (dolist (spec list-format)
      (let ((col-name (cadr spec)))
        (when-let ((pl (nthcdr 5 spec))
                   (children (plist-get pl :children))
                   (value (cdr (assq (car spec) data))))
          (setq children-spec (list children
                                    value
                                    (car spec)
                                    (plist-get pl :align-to-column))))
        (push col-name col-names)
        (let ((w (igist-tabulated-list-render-col
                  spec
                  data
                  used-width
                  (not (eq (car spec)
                           (car last-col-spec))))))
          (push w widths)
          (setq used-width w))))
    (add-text-properties
     beg (point)
     (if props
         (append `(columns ,(nreverse col-names)) props)
       `(columns ,(nreverse col-names))))
    (append children-spec (list (reverse widths)))))

(defun igist-render-entry (data)
  "Render a single entry of DATA in the Igist list table."
  (let* ((id (cdr (assq 'id data)))
         (hash (gethash id igist-rendered-hash)))
    (unless (assq 'deleted hash)
      (let ((inhibit-read-only t)
            (beg (point)))
        (pcase-let
            ((`(,children ,child-data ,parent-field ,align-to-col ,widths)
              (igist-list-render-list-format igist-table-list-format
                                             data
                                             igist-tabulated-list-padding)))
          (when (and igist-default-collapsed (not hash))
            (push id igist-list-hidden-ids))
          (when (and child-data
                     (igist-entry-expanded-p id))
            (pcase-dolist (`(,key . ,subdata) child-data)
              (insert ?\n)
              (igist-list-render-list-format children
                                             subdata
                                             (when (and align-to-col widths)
                                               (apply #'+
                                                      (seq-take
                                                       widths
                                                       align-to-col)))
                                             `(subrow ,key parent-field
                                                      ,parent-field))))
          (puthash id data igist-rendered-hash))
        (insert ?\n)
        (add-text-properties
         beg (point)
         `(igist-tabulated-list-id ,id))))))

(defmacro igist-with-position (&rest body)
  "Save and restore cursor position while executing provided code BODY."
  (let ((saved-pt (make-symbol "saved-pt"))
        (saved-col (make-symbol "saved-col"))
        (saved-wind-start (make-symbol "saved-wind-start"))
        (saved-buff (make-symbol "saved-buff"))
        (id (make-symbol "id")))
    `(let ((,id (igist-tabulated-list-get-id))
           (,saved-pt (point))
           (,saved-col (current-column))
           (,saved-buff (current-buffer))
           (,saved-wind-start))
       (when (get-buffer-window ,saved-buff)
         (setq ,saved-wind-start (window-start
                                  (get-buffer-window ,saved-buff))))
       (unwind-protect
           (progn ,@body)
         (when (buffer-live-p ,saved-buff)
           (with-current-buffer ,saved-buff
             (goto-char ,saved-pt))
           (dolist (wnd (get-buffer-window-list ,saved-buff nil t))
             (set-window-point wnd ,saved-pt)
             (when ,saved-wind-start
               (set-window-start wnd ,saved-wind-start))))))))

(defmacro igist-remember-pos (remember-pos &rest body)
  "Save and restore cursor position and column in a buffer after executing BODY.

Argument REMEMBER-POS is a boolean value that determines whether the position
should be remembered or not.


Argument BODY is a list of expressions that will be evaluated in the context of
the macro."
  (let ((saved-pt (make-symbol "saved-pt"))
        (saved-col (make-symbol "saved-col"))
        (saved-buff (make-symbol "saved-buff"))
        (entry-id (make-symbol "entry-id"))
        (saved-offset (make-symbol "saved-offset"))
        (saved-wind-start (make-symbol "saved-wind-start")))
    `(let ((,saved-pt)
           (,saved-col)
           (,saved-offset)
           (,entry-id)
           (,saved-buff)
           (,saved-wind-start))
       (when ,remember-pos
         (setq ,entry-id (igist-tabulated-list-get-id))
         (when ,entry-id
           (setq ,saved-buff (current-buffer))
           (when (get-buffer-window ,saved-buff)
             (setq ,saved-wind-start (window-start
                                      (get-buffer-window ,saved-buff))))
           (setq ,saved-col (current-column))
           (while (get-text-property (point)
                                     'subrow)
             (forward-line -1)
             (setq ,saved-offset (1+ (or ,saved-offset 0))))
           (setq ,saved-pt (point))))
       (unwind-protect
           (progn ,@body)
         (if (not ,remember-pos)
             (goto-char (point-min))
           (when (and ,entry-id
                      (buffer-live-p ,saved-buff))
             (with-current-buffer ,saved-buff
               (goto-char ,saved-pt)
               (let ((bounds (igist-find-entry-bounds ,entry-id)))
                 (when bounds
                   (goto-char (car bounds))
                   (when ,saved-offset
                     (forward-line ,saved-offset)
                     (when (> (point)
                              (cdr bounds))
                       (goto-char (cdr bounds))))
                   (when ,saved-col
                     (move-to-column ,saved-col))))
               (setq ,saved-pt (point)))
             (dolist (wnd (get-buffer-window-list ,saved-buff nil t))
               (set-window-point wnd ,saved-pt)
               (when ,saved-wind-start
                 (set-window-start wnd ,saved-wind-start)))))))))

(defun igist-tabulated-list-print (&optional remember-pos)
  "Prints a tabulated list with optional sorting.

Argument UPDATE is an optional boolean flag that determines whether the
tabulated list should be updated or not.
Argument REMEMBER-POS is an optional boolean flag that determines whether the
current position in the tabulated list should be remembered or not."
  (igist-remember-pos remember-pos
                      (let ((inhibit-read-only t)
        (entries igist-tabulated-list-entries)
        (sorter (igist-list--get-sorter)))
    (when sorter
      (setq entries (sort entries sorter)))
    (delete-region (point-min)
                   (point-max))
    (while entries
      (when (or (not igist-filters)
                (igist--all-pass (car entries) igist-filters))
        (igist-render-entry (car entries)))
      (setq entries (cdr entries)))
    (set-buffer-modified-p nil)
    (setq-local mode-name (format "Gists[%d]" (length igist-tabulated-list-entries))))))

(defun igist-tabulated-list-update-entries (entries)
  "Update ENTRIES in the `igist-rendered-hash' if they exist.

Argument ENTRIES is a list that contains the ENTRIES to be updated in the
tabulated list.

REQ is a `ghub--req' struct, used for loading next page."
  (when igist-rendered-hash
    (igist-with-position
     (dolist (entry entries)
       (when (gethash (cdr (assq 'id entry))
                      igist-rendered-hash)
         (igist-update-entry entry)))
     (set-buffer-modified-p nil))))

(defun igist-tabulated-list-sort (&optional n)
  "Sort Tabulated List entries by the column at point.
With a numeric prefix argument N, sort the Nth column.

If the numeric prefix is -1, restore order the list was
originally displayed in."
  (interactive "P")
  (when (and n
             (or (>= n (length igist-table-list-format))
                 (< n -1)))
    (user-error "Invalid column number"))
  (if (equal n -1)
  ;; Restore original order.
      (progn
        (unless igist-tabulated-list--original-order
          (error "Order is already in original order"))
        (setq igist-tabulated-list-entries
              (sort igist-tabulated-list-entries
                    (lambda (e1 e2)
                      (< (gethash e1 igist-tabulated-list--original-order)
                         (gethash e2 igist-tabulated-list--original-order)))))
        (setq igist-tabulated-list-sort-key nil)
        (igist-tabulated-list-init-header)
        (igist-tabulated-list-print t))
    (let ((spec (if n
                    (nth n igist-table-list-format)
                  (igist-find-column-spec
                   (igist-closest-column)))))
      (if (nth 3 spec)
          (igist-tabulated-list--sort-by-column-name (cadr spec))
        (user-error "Cannot sort by %s" (cadr spec))))))

(defun igist-tabulated-list--sort-by-column-name (name)
  "Sort the tabulated list by the specified column NAME.
Argument NAME is the name of the column to sort by."
  (when (and name (derived-mode-p 'igist-list-mode))
    (unless igist-tabulated-list--original-order
    ;; Store the original order so that we can restore it later.
      (setq igist-tabulated-list--original-order (make-hash-table))
      (cl-loop for elem in igist-tabulated-list-entries
               for i from 0
               do (setf (gethash elem igist-tabulated-list--original-order) i)))
               ;; Flip the sort order on a second click.
    (if (equal name (car igist-tabulated-list-sort-key))
        (setcdr igist-tabulated-list-sort-key
                (not (cdr igist-tabulated-list-sort-key)))
      (setq igist-tabulated-list-sort-key (cons name nil)))
    (igist-tabulated-list-init-header)
    (igist-tabulated-list-print t)))

(defun igist-tabulated-column-at-point (&optional pos)
  "Get tabulated column name at position POS or current point."
  (get-text-property (or pos (point)) 'igist-tabulated-list-column-name))

(defun igist-closest-column ()
  "Determine the closest column to the current point in a tabulated list."
  (let ((pos (point)))
    (or (igist-tabulated-column-at-point pos)
        (let ((line-beg (line-beginning-position))
              (line-end (line-end-position)))
          (or
           (if-let ((closest-pos
                     (cond ((= line-beg pos)
                            (next-single-property-change
                             pos
                             'igist-tabulated-list-column-name
                             nil
                             line-end))
                           ((= line-end pos)
                            (previous-single-property-change
                             pos
                             'igist-tabulated-list-column-name
                             nil
                             line-beg)))))
               (igist-tabulated-column-at-point closest-pos))
           (let ((positions
                  (seq-filter
                   #'cdr
                   (mapcar
                    (lambda (prop-pos)
                      (and prop-pos
                           (cons
                            prop-pos
                            (igist-tabulated-column-at-point
                             prop-pos))))
                    (list
                     (previous-single-property-change
                      pos
                      'igist-tabulated-list-column-name
                      nil
                      line-beg)
                     (next-single-property-change
                      pos
                      'igist-tabulated-list-column-name
                      nil
                      line-end))))))
             (cdar (seq-sort-by (lambda (p)
                                  (let ((delta (- pos
                                                  (car p))))
                                    (if (< delta 0)
                                        (- delta)
                                      delta)))
                                #'< positions))))))))

(defun igist-tabulated-list-goto-column (column-name)
  "Go to specified column in tabulated list.

Argument COLUMN-NAME is the name of the column to which the function will
navigate in a tabulated list."
  (unless (equal column-name
                 (igist-tabulated-column-at-point))
    (pcase-let ((`(,beg . ,end)
                 (igist-property-boundaries 'igist-tabulated-list-id
                                            (point))))
      (when beg
        (goto-char beg))
      (while (and beg end
                  (not (equal column-name
                              (igist-tabulated-column-at-point)))
                  (let ((pos (point)))
                    (and (>= pos beg)
                         (> end pos))))
        (when-let ((next (next-single-property-change
                          (point) 'igist-tabulated-list-column-name
                          nil
                          end)))
          (when (<= next end)
            (goto-char next)))))))

(defun igist-goto-column (column-name)
  "Jump to specified column COLUMN-NAME in tabulated list.

Argument COLUMN-NAME is the name of the column to which the function will
navigate in a tabulated list."
  (interactive
   (list (completing-read "Column: "
                          (mapcar #'cadr
                                  igist-table-list-format))))
  (igist-tabulated-list-goto-column column-name))

(defvar-local igist-table-current-column nil
  "Name of the column to edit in `igist-table-menu'.")

(defun igist-table-init-current-column ()
  "Initialize and set value for `igist-table-current-column'."
  (setq igist-table-current-column
        (igist-closest-column)))

(defun igist-tabulated-forward--column (&optional arg)
  "Go to the start of the next column after point on the current line.
If ARG is provided, move that many columns."
  (unless arg (setq arg 1))
  (pcase-let* ((`(,beg . ,end)
                (igist-property-boundaries 'igist-tabulated-list-id
                                           (point)))
               (fn (if (> arg 0)
                       #'next-single-property-change
                     #'previous-single-property-change))
               (limit (if (> arg 0)
                          end
                        beg)))
    (dotimes (_ (if (> arg 0)
                    arg
                  (- arg)))
      (let ((next
             (funcall fn
                      (point) 'igist-tabulated-list-column-name
                      nil limit)))
        (when next
          (goto-char next))))))

(defun igist-tabulated-forward-column (&optional arg)
  "Go to the start of the next column after point on the current line.
If ARG is provided, move that many columns."
  (interactive "p")
  (igist-tabulated-forward--column (or arg 1)))

(defun igist-tabulated-backward-column (&optional arg)
  "Go to the start of the next column after point on the current line.
If ARG is provided, move that many columns."
  (interactive "P")
  (igist-tabulated-forward--column (or arg -1)))

(defun igist-get-all-cols ()
  "Extract all column names from a tabulated list format."
  (let* ((child-cols (mapcar #'cadr
                             (plist-get
                              (nthcdr 5
                                      (seq-find
                                       (lambda (it)
                                         (plist-get (nthcdr 5 it)
                                                    :children))
                                       igist-table-list-format))
                              :children))))
    (append (mapcar #'cadr igist-table-list-format)
            child-cols)))


(defun igist-list-render (&optional _req)
  "Render list of GISTS."
  (if igist-tabulated-list-entries
      (progn (setq igist-tabulated-list-entries igist-list-response)
             (igist-tabulated-list-print t))
    (setq igist-tabulated-list-entries igist-list-response)
    (igist-tabulated-list-print)))

(defun igist--run-in-buffer (buffer timer-sym fn &rest args)
  "Run a function FN in a BUFFER and cancel timer TIMER-SYM.

Argument TIMER-SYM is a symbol that represents a timer.
Argument BUFFER is the buffer in which the function/macro will be executed.
Argument FN is the function or macro that will be executed.
Argument ARGS is a list of additional arguments that will be passed to the FN."
  (when (and buffer (buffer-live-p buffer))
    (with-current-buffer buffer
      (let ((wnd (get-buffer-window buffer)))
        (if wnd
            (with-selected-window wnd
              (apply fn args))
          (apply fn args)))
      (igist-cancel-timer timer-sym))))

(defun igist-cancel-timer (timer-sym)
  "Cancel a timer if it exists and set the value of TIMER-SYM to nil.

Argument TIMER-SYM is a symbol that represents the timer to be canceled."
  (when-let ((timer-value (symbol-value timer-sym)))
    (when (timerp timer-value)
      
      (cancel-timer timer-value)
      (set timer-sym nil))))

(defun igist-debounce (timer-sym delay fn &rest args)
  "Debounce execution FN with ARGS for DELAY.
TIMER-SYM is a symbol to use as a timer."
  (igist-cancel-timer timer-sym)
  (set timer-sym (apply #'run-with-idle-timer delay nil
                        #'igist--run-in-buffer
                        (current-buffer)
                        timer-sym
                        fn
                        args)))


(defun igist-ensure-buffer-visible (buffer &optional select)
  "Ensure the specified BUFFER is visible, optionally selecting it.

Argument BUFFER is a buffer object or a string that is the name of a buffer.

Optional argument SELECT is a boolean value.
If `non-nil', the function will SELECT the window displaying the BUFFER."
  (let ((buff-wind (get-buffer-window buffer)))
    (cond ((and select
                buff-wind
                (not (eq (selected-window) buff-wind)))
           (select-window buff-wind))
          ((and (not buff-wind)
                (minibuffer-window-active-p (selected-window)))
           (with-minibuffer-selected-window
             (pop-to-buffer-same-window buffer)))
          ((not buff-wind)
           (pop-to-buffer buffer)))))

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
  (let ((filtered-cell (remove (assq 'files gist) gist))
        (files (mapcar #'cdr (cdr (assq 'files gist))))
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

(defun igist--sync-gists-edit-buffers (response)
  "Synchronize gists buffers with RESPONSE."
  (dolist (buff (buffer-list))
    (when (buffer-local-value 'igist-current-gist buff)
      (with-current-buffer buff
        (when-let ((gist
                    (and
                     (cdr (assq 'created_at igist-current-gist))
                     (igist-find-by-id-and-file
                      (cdr (assq 'id igist-current-gist))
                      (cdr (assq 'filename igist-current-gist))
                      response))))
          (igist-refresh-buffer-vars gist))))))

(defun igist-sync-gists-lists (response)
  "Synchronize gists buffers with RESPONSE."
  (igist-debounce 'igist-sync-timer
                  1
                  #'igist--sync-gists-edit-buffers
                  response))

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
    (when-let ((buff (igist-get-gist-buffer id filename)))
      (when (buffer-live-p buff)
        (with-current-buffer buff
          (set-buffer-modified-p nil))
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
        (with-current-buffer buff
          (set-buffer-modified-p nil))
        (kill-buffer buff)))))

(defun igist-request-delete (id)
  "Delete GIST with ID."
  (igist-delete (concat "/gists/" id)
                nil
                :callback (lambda (&rest _)
                            (igist-with-exisiting-buffer
                                (igist-get-user-buffer-name
                                 (igist-get-current-user-name))
                              (when-let ((data
                                          (and igist-rendered-hash
                                               (gethash id
                                                        igist-rendered-hash))))
                                (unless (assq 'deleted data)
                                  (setq data (push '(deleted . t) data))
                                  (puthash id data igist-rendered-hash)
                                  (save-excursion
                                    (igist-remove-entry id))))))))

(defun igist-get-github-users ()
  "Return list of users in auth sources with host `api.github.com'."
  (let
      ((all-users (delq nil (mapcar (igist-rpartial plist-get :user)
                                    (auth-source-search
                                     :host "api.github.com"
                                     :require
                                     '(:user :secret)
                                     :max
                                     most-positive-fixnum))))
       (suffix
        (when (symbolp igist-auth-marker)
          (regexp-quote (concat "^" (symbol-name igist-auth-marker))))))
    (or (and suffix
             (seq-filter (apply-partially #'string-match-p suffix) all-users))
        all-users)))

(defun igist-popup-minibuffer-select-window ()
  "Select mini buffer window if it is active."
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
  "Pluck error `igist-message' and status from VALUE and display it."
  (let ((str
         (if-let ((status (seq-find #'numberp value)))
             (let ((msg (igist-alist-get 'message (car (last value)))))
               (format "IGist request error: %s (%s)" status msg))
           (format "IGist request error: %s" value))))
    (message
     (or (if (facep 'error)
             (propertize str 'face 'error)
           str)
         "igist error"))))

(defun igist-star-gist ()
  "Star currently viewing gist or gist at point."
  (interactive)
  (if-let ((id (or (igist-alist-get 'id igist-current-gist)
                   (igist-tabulated-list-get-id))))
      (igist-request "PUT"
                     (concat "/gists/" id "/star")
                     nil
                     :buffer (current-buffer)
                     :callback
                     (lambda (&rest _)
                       (igist-message "Gist starred")))
    (user-error "No gist")))

(defun igist-unstar-gist ()
  "Unstar currently viewing gist or gist at point."
  (interactive)
  (if-let ((id (or (igist-alist-get 'id igist-current-gist)
                   (igist-tabulated-list-get-id))))
      (igist-request "DELETE"
                     (concat "/gists/" id "/star")
                     nil
                     :buffer (current-buffer)
                     :callback
                     (lambda (&rest _)
                       (igist-message "Gist unstarred")))
    (user-error "No gist")))

(defun igist-fork-gist ()
  "Fork the current gist."
  (interactive)
  (if-let ((id (or (igist-alist-get 'id igist-current-gist)
                   (igist-tabulated-list-get-id))))
      (igist-post (concat "/gists/" id
                          "/forks")
                  nil
                  :buffer (current-buffer)
                  :callback
                  (lambda (&rest _)
                    (when (igist-get-current-user-name)
                      (igist-load-logged-user-gists)
                      (igist-message "Gist forked"))))
    (user-error "No gist for forking")))


(defun igist-save-existing-gist (buffer &optional callback)
  "Save gist in BUFFER. If CALLBACK is non-nil, call it without arguments."
  (let* ((gist (buffer-local-value 'igist-current-gist buffer))
         (orig-filename (igist-alist-get 'filename gist))
         (new-filename (buffer-local-value
                        'igist-current-filename
                        buffer)))
    (igist-patch (concat "/gists/" (igist-alist-get 'id gist))
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
                   (if-let ((id (igist-alist-get 'id val)))
                       (igist-with-exisiting-buffer buffer
                         (let ((new-gist
                                (igist-normalize-gist-file val
                                                           new-filename)))
                           (unless (equal orig-filename new-filename)
                             (rename-buffer
                              (concat id "-" new-filename))
                             (setq buffer-file-name (concat
                                                     (temporary-file-directory)
                                                     (buffer-name)))
                             (set-auto-mode)
                             (font-lock-ensure)
                             (igist-edit-mode))
                           (igist-setup-local-vars new-gist new-filename)
                           (set-buffer-modified-p nil)
                           (when (memq igist-enable-copy-gist-url-p
                                       '(t after-update))
                             (when-let ((url (igist-get-current-gist-url)))
                               (kill-new url)
                               (igist-message "Copied %s" url)))
                           (when callback
                             (funcall callback))))
                     (igist-message "Couldn't save gist."))
                   (igist-with-exisiting-buffer (igist-get-logged-user-buffer)
                     (igist-list-load-gists igist-current-user-name t))))))

(defun igist-files-to-gist-alist (files)
  "Convert a list of FILES into an association list for creating GitHub gists.

Argument FILES is a list of file paths that the function will use to create a
list of alists, where each alist represents a file and its content."
  (let ((gist-files))
    (dolist (file files)
      (let ((obj `(,(intern (file-name-nondirectory file)) .
                   ((content .
                             ,(with-temp-buffer
                                (insert-file-contents file)
                                (buffer-string)))))))
        (push obj gist-files)))
    (nreverse gist-files)))

(defun igist-post-files-request (files &optional description public)
  "Post FILES to a gist with optional DESCRIPTION and PUBLIC visibility.

Argument FILES is a list of files that will be posted to the gist.
Argument DESCRIPTION is an optional parameter that provides a DESCRIPTION for
the gist.
Argument PUBLIC is an optional boolean parameter that determines whether the
gist is PUBLIC or not."
  (let ((gist-files (igist-files-to-gist-alist files))
        (buffer (igist-get-user-buffer-name (igist-get-current-user-name))))
    (igist-post "/gists" nil
                :payload
                `((description . ,(or description ""))
                  (public . ,public)
                  (files . ,gist-files))
                :buffer buffer
                :callback
                (lambda (value &rest _)
                  (when value
                    (when (memq igist-enable-copy-gist-url-p
                                '(t after-new))
                      (when-let ((url (igist-alist-get 'html_url value)))
                        (kill-new url)
                        (igist-message "Copied %s" url)))
                    (igist-with-exisiting-buffer buffer
                      (if igist-list-loading
                          (igist-load-logged-user-gists)
                        (setq igist-list-response (push value
                                                        igist-list-response))
                        (igist-list-render))))))))

;;;###autoload
(defun igist-post-files (files &optional public)
  "Post FILES to Gist with an optional PUBLIC visibility and description.

The argument FILES is a list of files that the user wants to post on Gist. If
there are marked files in the Dired buffer, use them; otherwise, read the
directory in the minibuffer with completions and then read multiple files.

The argument PUBLIC is an optional boolean value that determines whether the
posted Gist should be PUBLIC or not.

The Gist will be created without editing."
  (interactive
   (list
    (or
     (when (fboundp 'dired-get-marked-files)
       (when-let ((dired-files (dired-get-marked-files)))
         (when (or (derived-mode-p 'dired-mode)
                   (yes-or-no-p (format "Post %d files?" (length dired-files))))
           dired-files)))
     (let ((dir (read-directory-name "Directory")))
       (mapcar (igist-rpartial expand-file-name dir)
               (completing-read-multiple
                "Files: "
                (seq-remove
                 #'file-directory-p
                 (directory-files
                  dir nil
                  directory-files-no-dot-files-regexp))))))
    (yes-or-no-p "Public?")))
  (while (not (igist-get-current-user-name))
    (setq igist-current-user-name (read-string "User: ")))
  (let ((description (if igist-ask-for-description
                         (read-string "Description: ")
                       "")))
    (igist-post-files-request files description public)))

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
                  (when value
                    (igist-with-exisiting-buffer buffer
                      (set-buffer-modified-p nil)
                      (let ((new-gist (igist-normalize-gist-file value
                                                                 file)))
                        (rename-buffer
                         (concat
                          (igist-alist-get 'id value) "-" file))
                        (igist-setup-local-vars
                         new-gist file))
                      (when (memq igist-enable-copy-gist-url-p
                                  '(t after-new))
                        (when-let ((url (igist-get-current-gist-url)))
                          (kill-new url)
                          (igist-message "Copied %s" url)))
                      (when callback
                        (funcall callback)))
                    (igist-load-logged-user-gists))))))

(defun igist-setup-local-vars (gist filename)
  "Set up local variables with given GIST and FILENAME.

Argument FILENAME is a string that represents the name of a file."
  (let ((gist-id (cdr (assq 'id gist))))
    (setq-local header-line-format (list "" 'header-line-indent
                                         (format
                                          "Gist %s %s"
                                          (or filename
                                              (igist-alist-get 'filename gist))
                                          (igist-make-file-counter gist))))
    (setq-local igist-current-gist gist)
    (setq-local igist-current-filename (if gist-id
                                           (or igist-current-filename
                                               filename)
                                         filename))
    (setq-local igist-current-description (or igist-current-description
                                              (cdr (assq
                                                    'description
                                                    gist))
                                              ""))
    (setq-local igist-current-public (if gist-id
                                         (or igist-current-public
                                             (cdr (assq
                                                   'public
                                                   gist)))
                                       (yes-or-no-p "Public?")))))

(defun igist-get-owner (gist)
  "Get the owner of a given GIST.

Argument GIST is a variable representing a gist object."
  (igist-alist-get 'login (igist-alist-get 'owner gist)))

(defun igist-setup-edit-buffer (gist &optional setup-fn)
  "Set up a buffer for editing a specified GIST in Emacs.

Argument GIST is a data structure that contains information about a specific
gist, such as its ID and filename.
Argument SETUP-FN is an optional function that is called to perform additional
setup on the buffer after it has been created and filled with the gist's
content."
  (let* ((filename (or (igist-alist-get 'filename gist)
                       (read-string "Filename: ")))
         (gist-id (igist-alist-get 'id gist))
         (buffer (and filename
                      gist-id
                      (igist-get-gist-buffer gist-id filename)))
         (name))
    (setq name (igist-make-gist-key
                `((id . ,(or gist-id "newgist"))
                  (filename . ,filename))))
    (if (and buffer
             (buffer-live-p buffer)
             (buffer-modified-p buffer))
        (with-current-buffer buffer
          (setq buffer-read-only nil)
          (setq buffer-file-name (concat (temporary-file-directory)
                                         name))
          (igist-set-major-mode buffer-file-name)
          (igist-setup-local-vars gist filename)
          (igist-edit-ensure-edit-mode)
          (current-buffer))
      (setq buffer (get-buffer-create name))
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
            (decode-coding-region (point-min)
                                  (point-max) 'dos)
            (setq buffer-file-name (concat (temporary-file-directory)
                                           name))
            (igist-set-major-mode buffer-file-name)
            (when setup-fn
              (funcall setup-fn)))
          (igist-setup-local-vars gist filename)
          (igist-edit-ensure-edit-mode)
          (setq buffer-undo-list nil)
          (set-buffer-modified-p nil))
        buffer))))

(defun igist-setup-new-gist-buffer (filename content)
  "Create a new buffer for a gist with a specified FILENAME and CONTENT.

Argument FILENAME is the name of the file that will be created in the new gist
buffer.
Argument CONTENT is the text or code that will be inserted into the newly
created file."
  (let ((buffer (get-buffer-create
                 (igist-make-gist-key
                  `((id . ,"newgist")
                    (filename . ,filename))))))
    (with-current-buffer buffer
      (erase-buffer)
      (setq buffer-read-only nil)
      (progn
        (set-buffer-modified-p nil)
        (setq buffer-undo-list nil)
        (setq buffer-file-name (concat (temporary-file-directory)
                                       (buffer-name)))
        (igist-set-major-mode filename)
        (save-excursion
          (when content
            (insert content))
          (set-buffer-modified-p nil))
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
  "Set up a buffer for editing a comment on a specific gist.

Argument GIST-ID is the unique identifier of the Gist for which the comment
buffer is being set up.
Argument COMMENT-ID is the optional unique identifier of the comment in the
Gist.
Argument COMMENT-BODY is the optional text content of the comment in the Gist."
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

(defun igist--remove-filter (fn)
  "Remove FN from list of filters `igist-filters'."
  (setq igist-filters
        (if (memq fn igist-filters)
            (delq fn igist-filters)
          igist-filters)))

(defun igist--add-filter (fn)
  "Add FN from list of filters `igist-filters'."
  (setq igist-filters
        (if (memq fn igist-filters)
            igist-filters
          (push fn igist-filters))))

(defun igist-get-languages (gists)
  "Get languages used in GISTS and their corresponding gist IDs.

Argument GISTS is a list of gists."
  (seq-reduce
   (lambda (acc gist)
     (let ((id (cdr (assq 'id gist))))
       (pcase-dolist (`(,_file . ,props)
                      (cdr (assq 'files gist)))
         (let* ((lang (or (cdr (assq 'language props))
                          "None"))
                (cell (assoc-string lang acc)))
           (if (not cell)
               (setq acc (push (list lang id) acc))
             (unless (member id (cdr cell))
               (setcdr cell (append (cdr cell)
                                    (list id)))))))
       acc))
   gists
   '()))

(defun igist-read-language (&optional prompt)
  "Sort and display programming languages based on their usage in gists.

Optional argument PROMPT is a string that is used to PROMPT the user for input.
If not provided, the default value is \"Language: \"."
  (let* ((langs (igist-get-languages igist-list-response))
         (active-langs (mapcar (igist-rpartial assoc-string langs)
                               igist-languages-filters))
         (sorter (apply-partially #'seq-sort-by
                                  (igist-compose length cdr)
                                  #'>))
         (alist
          (if active-langs (append
                            (funcall sorter
                                     active-langs)
                            (funcall sorter
                                     (seq-remove
                                      (igist-compose
                                       (igist-rpartial
                                        member
                                        igist-languages-filters)
                                       car)
                                      langs)))
            (funcall sorter langs)))
         (annotf (lambda (str)
                   (if (assoc-string str igist-languages-filters)
                       (format " %d (Active)"
                               (length (cdr (assoc-string str
                                                          alist))))
                     (format " %d " (length (cdr (assoc-string str alist))))))))
    (completing-read (or prompt "Language: ")
                     (lambda (str pred action)
                       (if (eq action 'metadata)
                           `(metadata
                             (annotation-function . ,annotf))
                         (complete-with-action action alist str pred))))))

(defun igist--has-language-pred (gist)
  "Check whether GIST has language specified in `igist-languages-filters'."
  (seq-find
   (pcase-lambda (`(,_file . ,props))
     (member (or (cdr (assq 'language props)) "None")
             igist-languages-filters))
   (cdr (assq 'files gist))))


(defun igist-toggle-language-filter (lang)
  "Toggle the language filter by adding or removing a language LANG."
  (interactive (list
                (igist-read-language "Add or remove filter by language: ")))
  (setq igist-languages-filters
        (if (or (not lang)
                (string-empty-p lang)
                (member lang igist-languages-filters))
            nil
          (list lang)))
  (if igist-languages-filters
      (igist--add-filter #'igist--has-language-pred)
    (igist--remove-filter
     #'igist--has-language-pred))
  (igist-tabulated-list-print t)
  (when transient-current-command
    (transient-setup transient-current-command)))

(defun igist--file-match-p-pred (gist)
  "Check whether GIST has file that match `igist-files-filter'."
  (or (not igist-files-filter)
      (seq-find
       (pcase-lambda (`(,file . ,_props))
         (string-match-p igist-files-filter (symbol-name file)))
       (cdr (assq 'files gist)))))

(defun igist--description-match-p-pred (gist)
  "Check whether GIST has file that match `igist-files-filter'."
  (or (not igist-description-filter)
      (when-let ((descr (cdr (assq 'description gist))))
        (string-match-p igist-description-filter descr))))



(defun igist-set-filter (value-sym value pred)
  "Set, add or remove a filter in igist based on the provided VALUE and predicate.

Argument VALUE-SYM is a symbol where the VALUE will be set.
Argument VALUE is the actual VALUE to be set to the symbol VALUE-SYM.
Argument PRED is a predicate function used to add or remove filters."
  (cond ((or (not value)
             (string-empty-p value))
         (set value-sym nil)
         (igist--remove-filter pred))
        (t (set value-sym (regexp-quote value))
           (igist--add-filter pred))))


(defun igist-search-by-regex (prompt value-sym pred)
  "Search IGipst files by regex and update the list in `real-time'.

Argument PROMPT is a string that is used as a PROMPT in the minibuffer.

Argument VALUE-SYM is a symbol that is used to store the value of the minibuffer
content.

Argument PRED is a function that is used as a predicate to filter the list of
gists."
  (let ((buff (current-buffer)))
    (unwind-protect
        (minibuffer-with-setup-hook
            (lambda ()
              (let ((map (make-sparse-keymap)))
                (define-key map [remap scroll-up-command] #'scroll-other-window)
                (define-key map [remap scroll-down-command]
                            #'scroll-other-window-down)
                (use-local-map (make-composed-keymap map (current-local-map))))
              (add-hook 'after-change-functions
                        (lambda (&rest _)
                          (when-let* ((wnd
                                       (active-minibuffer-window))
                                      (value
                                       (with-selected-window
                                           wnd
                                         (minibuffer-contents-no-properties))))
                            (when (buffer-live-p buff)
                              (with-current-buffer buff
                                (igist-set-filter
                                 value-sym
                                 value
                                 pred)
                                (igist-schedule-render)))))
                        nil t))
          (read-string prompt))
      (when transient-current-command
        (transient-setup transient-current-command)))))

(defun igist-combine-filters (gist)
  "Combine filters to match GIST descriptions or files.

Argument GIST is an alist representing a GIST."
  (or (igist--description-match-p-pred gist)
      (igist--file-match-p-pred gist)))


(defun igist-search-by-files-and-description-regex ()
  "Search IGipst files by regex and update the list in `real-time'."
  (interactive)
  (let ((buff (current-buffer)))
    (unwind-protect
        (minibuffer-with-setup-hook
            (lambda ()
              (let ((map (make-sparse-keymap)))
                (define-key map [remap scroll-up-command] #'scroll-other-window)
                (define-key map [remap scroll-down-command]
                            #'scroll-other-window-down)
                (use-local-map (make-composed-keymap map (current-local-map))))
              (add-hook 'after-change-functions
                        (lambda (&rest _)
                          (when-let* ((wnd
                                       (active-minibuffer-window))
                                      (value
                                       (with-selected-window
                                           wnd
                                         (minibuffer-contents-no-properties))))
                            (when (buffer-live-p buff)
                              (with-current-buffer buff
                                (igist-set-filter
                                 'igist-files-filter
                                 value
                                 #'igist--file-match-p-pred)
                                (igist-set-filter
                                 'igist-description-filter
                                 value
                                 #'igist--description-match-p-pred)
                                (igist--remove-filter
                                 #'igist--file-match-p-pred)
                                (igist--remove-filter
                                 #'igist--description-match-p-pred)
                                (setq igist-filters (igist--add-filter
                                                     'igist-combine-filters))
                                (igist-debounce
                                 'igist-render-timer
                                 0.5
                                 #'igist-tabulated-list-print
                                 t)))))
                        nil t))
          (read-string "Search: "))
      (when transient-current-command
        (transient-setup transient-current-command)))))

(defun igist-search-by-descriptions ()
  "Incremental search of gist files hiding the non-matches as we go."
  (interactive nil igist-list-mode)
  (igist-search-by-regex "Description: "
                         'igist-description-filter
                         #'igist--description-match-p-pred))


(defun igist-search-files ()
  "Incremental search of gist files hiding the non-matches as we go."
  (interactive nil igist-list-mode)
  (setq igist-list-hidden-ids nil)
  (igist-search-by-regex "File: "
                         'igist-files-filter
                         #'igist--file-match-p-pred))



(defun igist-reset-all-filters ()
  "Incremental search of gist files hiding the non-matches as we go."
  (interactive nil igist-list-mode)
  (setq igist-filters nil)
  (igist-tabulated-list-print t)
  (when transient-current-command
    (transient-setup transient-current-command)))

(transient-define-prefix igist-filters-menu ()
  "A menu for filtering tabulated Gist's views."
  ["Filters"
   ("l" igist-toggle-language-filter
    :description (lambda ()
                   (concat "Languages "
                           (if (and igist-filters
                                    igist-languages-filters)
                               (propertize
                                (string-join
                                 igist-languages-filters " ")
                                'face
                                'transient-value)
                             ""))))
   ("f" igist-search-files
    :description (lambda ()
                   (concat "Files "
                           (if (and igist-filters
                                    igist-files-filter)
                               (propertize
                                (substring-no-properties
                                 igist-files-filter)
                                'face
                                'transient-value)
                             ""))))
   ("d" igist-search-by-descriptions
    :description (lambda ()
                   (concat "Description "
                           (if (and igist-filters
                                    igist-description-filter)
                               (propertize
                                (substring-no-properties
                                 igist-description-filter)
                                'face
                                'transient-value)
                             ""))))
   ("/" igist-search-by-files-and-description-regex
    :description (lambda ()
                   (concat "Description and files "
                           (if (and igist-filters
                                    igist-description-filter
                                    igist-files-filter
                                    (string= igist-files-filter
                                             igist-description-filter))
                               igist-files-filter
                             ""))))
   ("r" igist-reset-all-filters
    :description (lambda ()
                   (concat "Reset "
                           (propertize (format "(%d)" (length
                                                       igist-filters))
                                       'face
                                       (if igist-filters
                                           'transient-value
                                         'transient-inactive-value))
                           " filters"
                           " "))
    :inapt-if-nil igist-filters)]
  (interactive)
  (transient-setup #'igist-filters-menu))

(defun igist-print-languages-chart ()
  "Show a chart showing the occurrence of languages in a tabulated buffer."
  (interactive)
  (require 'chart)
  (let ((alist (mapcar
                (lambda (it)
                  (setcdr it (length (cdr it)))
                  it)
                (igist-get-languages igist-list-response))))
    (when (fboundp 'chart-bar-quickie)
      (chart-bar-quickie 'horizontal "Gists"
                         (mapcar #'car alist) "Language"
                         (mapcar #'cdr alist) "# of occurrences"
                         20
                         (lambda (a b)
                           (> (cdr a)
                              (cdr b)))))))

(defun igist-edit-buffer (gist &optional setup-fn)
  "Display GIST in popup window.
If SETUP-FN is a non nil, it will be called without args."
  (pop-to-buffer (apply #'igist-setup-edit-buffer (list gist setup-fn)))
  (igist-popup-minibuffer-select-window))

(defun igist-edit-gist (gist-cell)
  "Edit a gist in an edit buffer.

Argument GIST-CELL is the cell containing the gist to be edited."
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
  "A function for annotating GIST-KEY in mini buffer completions.
MAX is length of most longest key."
  (let* ((cell (cdr (assoc gist-key igist-normalized-gists)))
         (extra (format "(%s/%s)" (1+ (igist-alist-get 'idx cell))
                        (igist-alist-get 'total cell)))
         (description (truncate-string-to-width (igist-alist-get
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

(defun igist-imenu-prev-index-position ()
  "Move point to previous line in current buffer."
  (unless (bobp)
    (forward-line -1)))

(defun igist-imenu-extract-index-name ()
  "Return the name of entity at point for `imenu'."
  (string-trim (buffer-substring (line-beginning-position)
                                 (line-end-position))))

(defun igist--revert-tabulated-buffers (sym newval &rest _)
  "Update the format of tabulated buffers based on the new value provided.

Argument SYM is a symbol that determines the type of buffer to revert,
specifically whether it's an `igist-explore-format' buffer or a buffer in
`igist-list-mode'.
Argument NEWVAL is the new value to be set for `igist-table-list-format'."
  (let ((mode-sym (if (eq sym 'igist-explore-format)
                      'igist-explore-mode
                    'igist-list-mode)))
    (dolist (buff (buffer-list))
      (when (eq (buffer-local-value 'major-mode buff)
                mode-sym)
        (with-current-buffer buff
          (let ((loading igist-list-loading))
            (when loading
              (igist-list-cancel-load))
            (setq igist-table-list-format
                  newval)
            (let ((igist-table-list-format newval))
              (setq igist-table-current-column nil)
              (igist-tabulated-list-init-header)
              (igist-tabulated-list-print t)
              (when loading
                (igist-list-refresh)))))))))

(defun igist--tabulated-list-revert ()
  "Revert the tabulated list to its original format in Igist."
  (igist-tabulated-list-print t)
  (igist-tabulated-list-init-header))

(add-variable-watcher 'igist-list-format #'igist--revert-tabulated-buffers)
(add-variable-watcher 'igist-explore-format #'igist--revert-tabulated-buffers)

(defun igist-tabulated-list-revert (&rest _ignored)
  "The `revert-buffer-function' for `igist-list-mode'."
  (interactive)
  (unless (derived-mode-p 'igist-list-mode)
    (error "The current buffer is not in Igist-list-mode"))
  (igist--tabulated-list-revert))

(define-derived-mode igist-list-mode special-mode "Gists"
  "Major mode for displaying Gists in a table view.

In this major mode, each gist is displayed as an table entry.

An expanded table entry will display nested row entries - gist's files.

The exact format of an entry, as well as the addition, editing, reordering,
or removal of columns, can be configured by either editing the custom
variable `igist-list-format', or interactively via `igist-table-menu'.

Users can configure whether the gists should be collapsed by default in user
buffers initial view by editing the custom variable
`igist-user-gists-init-collapsed'.

The mode also provides incremental filtering via the `igist-filters-menu'
command.

In general, most mode commands can be accessed via
the `igist-dispatch' menu command.

Other custom variables related to this mode include:

- `igist-tabulated-list-padding'
- `igist-use-header-line'
- `igist-tabulated-list-gui-sort-indicator-asc'
- `igist-tabulated-list-gui-sort-indicator-desc'
- `igist-tabulated-list-tty-sort-indicator-asc'
- `igist-tabulated-list-tty-sort-indicator-desc'

\\<igist-list-mode-map>
\\{igist-list-mode-map}."
  (setq-local truncate-lines t)
  (setq-local buffer-undo-list t)
  (setq-local glyphless-char-display
              (let ((table
                     (make-char-table
                      'glyphless-char-display nil)))
                (set-char-table-parent table glyphless-char-display)
                (aset table
                      igist-tabulated-list-gui-sort-indicator-desc
                      (cons nil
                            (char-to-string
                             igist-tabulated-list-tty-sort-indicator-desc)))
                (aset table
                      igist-tabulated-list-gui-sort-indicator-asc
                      (cons nil
                            (char-to-string
                             igist-tabulated-list-tty-sort-indicator-asc)))
                table))
  (setq-local text-scale-remap-header-line t)
  (setq-local igist-rendered-hash (make-hash-table :test #'equal))
  (setq-local igist-tabulated-list--original-order nil)
  (setq bidi-paragraph-direction 'left-to-right)
  (when (fboundp 'header-line-indent-mode)
    (header-line-indent-mode))
  (setq igist-table-list-format
        igist-list-format)
  (setq igist-default-collapsed igist-user-gists-init-collapsed)
  (setq-local revert-buffer-function #'igist-tabulated-list-revert)
  (setq-local imenu-prev-index-position-function
              #'igist-imenu-prev-index-position)
  (setq-local imenu-extract-index-name-function
              #'igist-imenu-extract-index-name)
  (when (bound-and-true-p visual-line-mode)
    (visual-line-mode -1))
  (when (and (bound-and-true-p visual-fill-column-mode)
             (fboundp 'visual-fill-column-mode))
    (visual-fill-column-mode -1))
  (font-lock-add-keywords nil '(("#[^[:space:]]*" . 'font-lock-keyword-face)))
  (use-local-map igist-list-mode-map))

(put 'igist-list-mode 'mode-class 'special)

(define-derived-mode igist-explore-mode igist-list-mode "Gists-Explore"
  "Major mode for exploring public Gists in a special mode.

This mode allow to add, edit, reorder, or remove columns
interactively with the transient command - `igist-table-menu'.

\\<igist-list-mode-map>
\\{igist-list-mode-map}."
  (setq igist-default-collapsed igist-explore-gists-init-collapsed)
  (setq igist-table-list-format
        igist-explore-format))

(defun igist-render-comment-to-md (gist-id comment-alist)
  "Render and comment COMMENT-ALIST for gist with GIST-ID in markdown format."
  (let ((comment (alist-get 'body comment-alist))
        (comment-id (alist-get 'id comment-alist))
        (updated
         (format-time-string "%D %R"
                             (parse-iso8601-time-string (igist-alist-get 'updated_at
                                                               comment-alist))))
        (author (alist-get 'login (alist-get 'user comment-alist))))
    (propertize
     (format "## **%s** commented on %s\n\n%s" author updated comment)
     'igist-comment-id comment-id
     'igist-comment-gist-id gist-id
     'igist-gist-author author)))

(defun igist-render-comment-to-org (gist-id comment-alist)
  "Convert a GitHub Gist comment into an `Org-mode' entry.

Argument GIST-ID is a string that represents the unique identifier of the gist.

Argument COMMENT-ALIST is an association list that contains the details of the
comment."
  (let ((comment (alist-get 'body comment-alist))
        (comment-id (alist-get 'id comment-alist))
        (updated
         (format-time-string "%D %R"
                             (parse-iso8601-time-string (igist-alist-get
                                                         'updated_at
                                                         comment-alist))))
        (author (alist-get 'login (alist-get 'user comment-alist))))
    (propertize
     (format "** %s:\n%s\n\s\s%s" author
             (string-join (list ":PROPERTIES:" (format ":UPDATED: %s" updated)
                                ":END:")
                          "\n")
             comment)
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

(defun igist-render-comments-list (comments gist-id)
  "Render COMMENTS for gist with GIST-ID.
GIST-ID is used to create comments buffer."
  (if (not comments)
      (igist-message "No comments in gist %s." gist-id)
    (when-let* ((md-comments (mapconcat (apply-partially
                                         (pcase igist-mode-for-comments
                                           ('org-mode
                                            #'igist-render-comment-to-org)
                                           (_ #'igist-render-comment-to-md))
                                         gist-id)
                                        comments
                                        "\n\n"))
                (buffer (get-buffer-create (concat "*" gist-id "-comments*"))))
      (with-current-buffer buffer
        (setq buffer-read-only nil)
        (erase-buffer)
        (progn
          (save-excursion
            (insert md-comments))
          (funcall igist-mode-for-comments))
        (setq buffer-undo-list nil)
        (igist-comments-list-mode)
        (set-buffer-modified-p nil)
        (setq buffer-read-only t)
        (setq igist-comment-gist-id gist-id)
        (pop-to-buffer (current-buffer))))))

(defun igist-load-comments (&rest _)
  "Load comments for gist at point or edit buffer."
  (interactive)
  (when-let ((gist-id
              (or
               igist-comment-gist-id
               (igist-alist-get 'id igist-current-gist)
               (when (derived-mode-p 'igist-list-mode)
                 (igist-tabulated-list-get-id))))
             (buff (current-buffer)))
    (igist-with-exisiting-buffer
        (get-buffer-create
         (concat "*" gist-id "-comments*"))
      (igist-spinner-show))
    (igist-get (concat "/gists/" gist-id "/comments") nil
               :buffer buff
               :callback
               (lambda (val &rest _)
                 (igist-render-comments-list val gist-id)
                 (let ((buff-name (concat "*" gist-id "-comments*")))
                   (igist-with-exisiting-buffer
                       buff-name
                     (igist-spinner-stop)))))))

(defun igist-property-boundaries (prop &optional pos)
  "Return property boundaries for PROP at POS."
  (if pos
      (goto-char pos)
    (setq pos (point)))
  (let ((position
         (cond ((and (bolp)
                     (not (eobp)))
                (1+ (line-beginning-position)))
               ((and (eolp)
                     (not (bobp)))
                (1- (line-end-position)))
               (t (point)))))
    (when-let ((value (get-text-property position prop)))
      (when-let ((beg (or (previous-single-property-change position prop)
                          (point-min)))
                 (end (or (next-single-property-change position prop)
                          (point-max))))
        (cons beg (if (equal value (get-text-property end prop))
                      end
                    (1- end)))))))

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

(defun igist-overlay-prompt-region (begin end fn &rest args)
  "Darken the background color of a specified region and apply a function to it.

Argument BEGIN is the starting point of the region in the buffer.

Argument END is the ending point of the region in the buffer.

Argument FN is the function to be applied to the region.

Optional argument ARGS is a list of additional arguments that will be passed to
the function FN."
  (let* ((buffer (current-buffer))
         (ov (make-overlay begin end buffer)))
    (unwind-protect
        (progn (overlay-put ov 'face `(:inverse-video t))
               (apply fn args))
      (delete-overlay ov))))

(defun igist-add-comment ()
  "Add a new comment for the current gist."
  (interactive)
  (when-let ((gist-id (igist-get-gist-id)))
    (pop-to-buffer-same-window (igist-setup-comment-buffer
                                gist-id)
                               t)))

(defun igist-get-gist-id ()
  "Return id for gist from all sources."
  (or
   (when (derived-mode-p 'igist-list-mode)
     (igist-tabulated-list-get-id))
   igist-comment-gist-id
   (get-text-property (point) 'igist-comment-gist-id)
   (igist-alist-get 'id igist-current-gist)))

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
                                  gist-id)))))

(defun igist-delete-comment-at-point (&rest _)
  "Add or edit a comment for gist at point or edit buffer."
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
                                           'yes-or-no-p "Delete comment?")
            (yes-or-no-p "Delete comment?"))
        (igist-delete (format "/gists/%s/comments/%s" gist-id comment-id)
                      nil
                      :callback (lambda (&rest _)
                                  (let ((buff-name (concat "*" gist-id
                                                           "-comments*")))
                                    (igist-with-exisiting-buffer
                                        buff-name
                                      (setq igist-comment-gist-id gist-id)
                                      (igist-load-comments)))
                                  (igist-load-logged-user-gists))))
    (user-error "Not in gist comment")))

(defun igist-post-comment ()
  "Save the currently edited or created comment."
  (interactive)
  (when-let* ((buffer (current-buffer))
              (gist-id (buffer-local-value 'igist-comment-gist-id buffer))
              (content (with-current-buffer buffer
                         (buffer-substring-no-properties
                          (point-min)
                          (point-max))))
              (callback-fn (lambda (&rest _)
                             (when (buffer-live-p buffer)
                               (with-current-buffer buffer
                                 (set-buffer-modified-p nil))
                               (kill-buffer buffer))
                             (igist-load-logged-user-gists
                              (lambda ()
                                (let ((buff-name (concat "*" gist-id
                                                         "-comments*")))
                                  (igist-with-exisiting-buffer buff-name
                                    (setq igist-comment-gist-id gist-id)
                                    (igist-load-comments))))))))
    (if-let ((comment-id (buffer-local-value 'igist-comment-id buffer)))
        (igist-patch  (format "/gists/%s/comments/%s" gist-id comment-id)
                      nil
                      :payload `((body . ,content))
                      :buffer buffer
                      :callback callback-fn)
      (igist-post (format "/gists/%s/comments" gist-id)  nil
                  :callback callback-fn
                  :payload `((body . ,content))))))

(defun igist-list-add-file ()
  "Add a new file name to current gist at the point."
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

(defun igist-kill-all-gists-buffers ()
  "Kill all editable Gists buffers."
  (interactive)
  (dolist (buff (buffer-list))
    (when (buffer-local-value 'igist-current-gist buff)
      (with-current-buffer buff
        (set-buffer-modified-p nil))
      (kill-buffer buff))))

(defun igist-list-get-per-page-query (buffer)
  "Return estimed gists count for BUFFER."
  (let ((estimed-gists-count
         (if-let ((prev-response (and (bufferp buffer)
                                      (buffer-live-p buffer)
                                      (buffer-local-value
                                       'igist-list-response
                                       buffer))))
             (length prev-response)
           (or igist-per-page-limit 30))))
    (min estimed-gists-count 100)))

(defun igist-list-cancel-load ()
  "Cancel loading of gists list."
  (interactive)
  (setq igist-list-cancelled (lambda ()
                               (igist-cancel-timer 'igist-render-timer)
                               (igist-list-render)
                               (igist-spinner-stop))))

(defun igist-list-refresh ()
  "Refresh gists in the current `igist-list-mode' buffer."
  (interactive)
  (if (string= igist-explore-buffer-name (buffer-name (current-buffer)))
      (igist-explore-public-gists)
    (when-let ((owner (igist-get-owner
                       (car igist-list-response))))
      (igist-list-load-gists owner))))

(defun igist-schedule-render (&optional delay)
  "Schedule the rendering of the gists with an optional DELAY.

Optional argument DELAY is a number, with a default value of 0.5."
  (igist-debounce 'igist-render-timer (or delay 0.5)
                  #'igist-list-render))


(defun igist-list-loaded-callback (buffer value req callback callback-args)
  "Update the BUFFER with the loaded VALUE and trigger the CALLBACK.

REQ is a `ghub--req' struct, used for loading next page.

Argument CALLBACK-ARGS is a variable that holds the arguments to be passed to
the CALLBACK function."
  (let ((cancel-fn))
    (cond ((not (buffer-live-p buffer))
           nil)
          ((setq cancel-fn (buffer-local-value 'igist-list-cancelled buffer))
           (with-current-buffer buffer
             (setq igist-list-response value
                   igist-list-cancelled nil
                   igist-list-loading nil)
             (when (functionp cancel-fn)
               (funcall cancel-fn))))
          ((not (bufferp (ghub-continue req)))
           (with-current-buffer buffer
             (setq igist-list-response value
                   igist-list-loading nil
                   igist-list-cancelled nil)
             (igist-cancel-timer 'igist-render-timer)
             (igist-list-render)
             (when callback callback-args
                   (apply callback callback-args))
             (igist-spinner-stop)
             (when value
               (igist-sync-gists-lists
                value))))
          ((>= (length value)
               (1- (length
                    (buffer-local-value 'igist-tabulated-list-entries buffer))))
           (with-current-buffer buffer
             (setq igist-list-response value
                   igist-list-loading t
                   igist-list-page (1+ (or igist-list-page 0)))
             (when (get-buffer-window buffer)
               (if (or (not igist-rendered-hash)
                       (hash-table-empty-p igist-rendered-hash))
                   (igist-list-render)
                 (igist-schedule-render)))
             (when value
               (igist-sync-gists-lists
                value))))
          (t
           (with-current-buffer buffer
             (setq igist-list-response value
                   igist-list-loading t
                   igist-list-page (1+ (or igist-list-page 0)))
             (let ((per-page (and (get-buffer-window buffer)
                                  (ghub-req-extra req))))
               (when per-page
                 (igist-debounce 'igist-render-timer 0.5
                                 #'igist-tabulated-list-update-entries
                                 (igist-take-last per-page value))))
             (when value
               (igist-sync-gists-lists
                value)))))))


(defun igist-list-load-gists (user &optional background callback callback-args)
  "List the GitHub USER's gists asynchronously.

Next, execute the function CALLBACK with the arguments CALLBACK-ARGS.

If optional argument BACKGROUND is non-nil, the buffer will not be shown.

Loading of subsequent pages may be halted by the command
`igist-list-cancel-load'."
  (igist-list-request
   (concat "/users/" user "/gists") user background callback callback-args))

(defun igist-load-logged-user-gists (&optional cb &rest args)
  "Load gists asynchronously with callback CB and ARGS."
  (while (not (igist-get-current-user-name))
    (setq igist-current-user-name (read-string "User: ")))
  (igist-list-load-gists igist-current-user-name
                         t
                         cb args))



(defun igist-json-parse-string (str &optional object-type array-type null-object
                                    false-object)
  "Parse STR with natively compiled function or with json library.

The argument OBJECT-TYPE specifies which Lisp type is used
to represent objects; it can be `hash-table', `alist' or `plist'.  It
defaults to `alist'.

The argument ARRAY-TYPE specifies which Lisp type is used
to represent arrays; `array'`vector' and `list'.

The argument NULL-OBJECT specifies which object to use
to represent a JSON null value.  It defaults to `:null'.

The argument FALSE-OBJECT specifies which object to use to
represent a JSON false value.  It defaults to `:false'."
  (if (and (fboundp 'json-parse-string)
           (fboundp 'json-available-p)
           (json-available-p))
      (json-parse-string str
                         :object-type (or object-type 'alist)
                         :array-type
                         (pcase array-type
                           ('list 'list)
                           ('vector 'array)
                           (_ 'list))
                         :null-object null-object
                         :false-object false-object)
    (require 'json)
    (let ((json-object-type (or object-type 'alist))
          (json-array-type
           (pcase array-type
             ('list 'list)
             ('array 'vector)
             (_ 'vector)))
          (json-null (or null-object :null))
          (json-false (or false-object :false)))
      (json-read-from-string str))))

(defun igist--read-json-payload (_status)
  "Parse and handle JSON payload from Github API response."
  (let ((raw (ghub--decode-payload)))
    (and raw
         (condition-case nil
             (igist-json-parse-string raw)
           ((json-parse-error json-readtable-error)
            `((message
               . ,(if (looking-at "<!DOCTYPE html>")
                      (if (re-search-forward
                           "<p>\\(?:<strong>\\)?\\([^<]+\\)" nil t)
                          (match-string 1)
                        "error description missing")
                    (string-trim (buffer-substring (point)
                                                   (point-max)))))
              (documentation_url . "https://github.com/magit/ghub/wiki/Github-Errors")))))))

(defun igist-list-request (url user &optional background callback callback-args)
  "Fetch and possibly display a list of gists from a specified URL and USER.

Argument URL is a string that represents the url for the request.

Argument USER is a string that represents the GitHub username for the request.

If optional argument BACKGROUND is non-nil, the buffer will not be shown.

Optional argument CALLBACK is a function that will be called when the request is
completed.

Optional argument CALLBACK-ARGS is a list of arguments that will be passed to
the CALLBACK function.

Loading next pages can be stopped by the command `igist-list-cancel-load'."
  (pcase-let ((`(,buffer . ,mode)
               (if user
                   (cons (get-buffer-create (igist-get-user-buffer-name user))
                         (if (and igist-current-user-name
                                  (string= user igist-current-user-name))
                             'igist-list-mode
                           'igist-explore-mode))
                 (cons (get-buffer-create igist-explore-buffer-name)
                       'igist-explore-mode))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'igist-list-mode)
        (funcall mode)
        (igist-tabulated-list-init-header))
      (if igist-list-loading
          (progn
            (setq igist-list-cancelled
                  (lambda ()
                    (igist-list-request url user
                                        background
                                        callback
                                        callback-args))))
        (setq igist-list-loading t)
        (setq igist-list-page 1)
        (igist-spinner-show)
        (let ((per-page (igist-list-get-per-page-query
                         buffer)))
          (ghub-request "GET" url
                        nil
                        :auth (if (igist-get-current-user-name)
                                  igist-auth-marker
                                'none)
                        :username (igist-get-current-user-name)
                        :host "api.github.com"
                        :reader #'igist--read-json-payload
                        :query `((per_page . ,per-page))
                        :forge 'github
                        :errorback
                        (lambda (&rest args)
                          (when buffer
                            (igist-with-exisiting-buffer
                                buffer
                              (igist-spinner-stop)))
                          (igist-show-request-error (car args)))
                        :callback
                        (lambda (value _headers _status req)
                          (condition-case nil
                              (progn
                                (setf (ghub-req-extra req) per-page)
                                (igist-list-loaded-callback buffer value req
                                                            callback
                                                            callback-args))
                            (error (setq igist-list-cancelled nil)
                                   (setq igist-list-loading nil)
                                   (igist-spinner-stop))))))
        (unless background
          (igist-ensure-buffer-visible buffer))))))

(defun igist-delete-other-gist-or-file (gist)
  "Delete a GIST or file based on the user input.

Argument GIST is the gist that the user wants to delete."
  (interactive
   (list
    (or
     igist-current-gist
     (igist-completing-read-gists "Delete gist: "))))
  (let* ((id (igist-alist-get
              'id
              gist))
         (filename (igist-alist-get 'filename gist))
         (saved (seq-find
                 (lambda (it)
                   (equal
                    filename
                    (igist-alist-get 'filename it)))
                 (igist-alist-get 'files
                                  gist)))
         (actions `((?y
                     ,(if saved
                          (format "only current file: %s" filename)
                        (format "only current buffer: %s" (or filename ""))))
                    (?Y
                     ,(if-let ((files (igist-alist-get 'files gist)))
                          (format "gist with all (%s) files"
                                  (length
                                   files))
                        "whole gist"))
                    (?n "no")
                    (?q "cancel")))
         (answer (read-multiple-choice "Delete"
                                       actions)))
    (pcase (car answer)
      (?y
       (if saved
           (igist-request-delete-filename gist)
         (set-buffer-modified-p nil)
         (kill-buffer (current-buffer))))
      (?Y
       (igist-delete-gists-buffers-by-id id)
       (igist-request-delete id)))))

(defun igist-delete-current-filename ()
  "Delete the current filename from a gist."
  (interactive)
  (when-let ((confirmed-gist
              (if-let* ((file (if (derived-mode-p 'igist-list-mode)
                                  (igist-tabulated-gist-file-at-point)
                                igist-current-gist))
                        (filename (igist-alist-get
                                   'filename
                                   file)))
                  (when
                      (cond ((and igist-current-gist
                                  (not (seq-find
                                        (lambda (it)
                                          (equal
                                           filename
                                           (igist-alist-get 'filename it)))
                                        (igist-alist-get 'files file))))
                             (kill-current-buffer)
                             nil)
                            ((derived-mode-p 'igist-list-mode)
                             (igist-overlay-prompt-region
                              (line-beginning-position)
                              (line-end-position)
                              'yes-or-no-p
                              (format "Delete file %s from gist?" filename)))
                            (t (yes-or-no-p (format "Delete file %s from gist?"
                                                    filename))))
                    file)
                (when-let ((parent (igist-tabulated-gist-at-point)))
                  (igist-read-gist-file "Delete file from gist: "
                                        parent)))))
    (igist-request-delete-filename confirmed-gist)))

(defun igist-delete-current-gist ()
  "Delete the current gist with all files."
  (interactive)
  (cond ((igist-alist-get 'id igist-current-gist)
         (igist-delete-other-gist-or-file
          igist-current-gist))
        ((and igist-current-gist)
         (set-buffer-modified-p nil)
         (kill-buffer (current-buffer)))
        ((derived-mode-p 'igist-list-mode)
         (when-let ((id (igist-tabulated-list-get-id))
                    (bounds (igist-property-boundaries
                             'igist-tabulated-list-id
                             (point))))
           (when (igist-overlay-prompt-region (car bounds)
                                              (cdr bounds)
                                              'yes-or-no-p
                                              "Delete gist?")
             (igist-delete-gists-buffers-by-id id)
             (igist-request-delete id))))))

(defun igist-toggle-public (&rest _)
  "Toggle value of variable `igist-current-public'."
  (interactive)
  (setq igist-current-public (not igist-current-public)))

(defun igist-add-file-to-gist ()
  "Add a new file to the existing gist."
  (interactive)
  (cond ((and igist-current-filename
              (not (igist-alist-get 'id igist-current-gist)))
         (let ((gist (igist-completing-read-gists
                      "Add file to gist: ")))
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
                          (igist-completing-read-gists "Add file to gist: ")))
                (data (igist-pick-from-alist
                       '(owner id files description) gist)))
           (pop-to-buffer
            (igist-setup-edit-buffer data))))))

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

(defun igist-read-filename (&rest _args)
  "Update the filename for the current gist without saving."
  (interactive)
  (let ((file (read-string
               (format "Rename (%s) to "
                       (or igist-current-filename
                           (igist-alist-get 'filename
                                            igist-current-gist)))
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
  "Save the gist BUFFER with optional CALLBACK.

Argument CALLBACK is an optional function or macro that will be called after the
gist BUFFER is saved.

Also run hooks from `igist-before-save-hook'."
  (with-current-buffer buffer
    (setq-local header-line-format (list ""
                                         (if (facep 'warning)
                                             (propertize
                                              "Saving..."
                                              'face  'warning)
                                           "Saving...")))
    (run-hooks 'igist-before-save-hook)
    (run-hooks 'before-save-hook))
  (if
      (not (igist-alist-get 'id (buffer-local-value
                                 'igist-current-gist buffer)))
      (igist-save-new-gist buffer callback)
    (when (igist-gist-modified-p buffer)
      (igist-save-existing-gist buffer callback))))

(defun igist-save-current-gist ()
  "Post the current gist and stay in the buffer."
  (interactive)
  (igist-save-gist-buffer (current-buffer)
                          (lambda ()
                            (igist-message "Gist saved"))))

(defun igist-save-current-gist-and-exit ()
  "Save current gist and kill it's buffer."
  (interactive)
  (igist-save-gist-buffer (current-buffer)
                          (lambda ()
                            (kill-buffer (buffer-name))
                            (igist-message "Gist saved"))))

(defun igist-ivy-read-gists (prompt url)
  "Read a gist in the minibuffer, with Ivy completion.

PROMPT is a string to prompt with; normally it ends in a colon and a space.

Argument URL is the url of a GitHub gist."
  (interactive)
  (when (and (fboundp 'ivy-update-candidates)
             (fboundp 'ivy--reset-state)
             (fboundp 'ivy--exhibit)
             (boundp 'ivy-text)
             (boundp 'ivy-exit)
             (boundp 'ivy-last)
             (boundp 'ivy--all-candidates)
             (boundp 'cl-struct-ivy-state-tags)
             (boundp 'ivy--index)
             (fboundp 'ivy-read)
             (fboundp 'ivy-recompute-index-swiper-async)
             (fboundp 'ivy-configure))
    (let ((caller this-command))
      (ivy-configure caller
        :index-fn #'ivy-recompute-index-swiper-async)
      (let* ((gists-alist)
             (gists-keys)
             (buff (current-buffer))
             (output-buffer
              (ghub-get url nil
                        :query `((per_page . 30))
                        :auth (if (igist-get-current-user-name)
                                  igist-auth-marker
                                'none)
                        :callback
                        (lambda (value _headers _status req)
                          (when (and (active-minibuffer-window)
                                     (buffer-live-p buff))
                            (with-current-buffer buff
                              (setq gists-alist
                                    (igist-normalize-gists value))
                              (setq gists-keys
                                    (mapcar #'car gists-alist)))
                            (ivy-update-candidates
                             gists-keys)
                            (let ((input ivy-text)
                                  (pos
                                   (when-let ((wind
                                               (active-minibuffer-window)))
                                     (with-selected-window
                                         wind
                                       (point)))))
                              (when (active-minibuffer-window)
                                (with-selected-window (active-minibuffer-window)
                                  (delete-minibuffer-contents)))
                              (progn
                                (or
                                 (progn
                                   (and
                                    (memq
                                     (type-of ivy-last)
                                     cl-struct-ivy-state-tags)
                                    t))
                                 (signal 'wrong-type-argument
                                         (list 'ivy-state ivy-last)))
                                (let* ((v ivy-last))
                                  (aset v 2 ivy--all-candidates)))
                              (when (fboundp 'ivy-state-preselect)
                                (progn
                                  (or
                                   (progn
                                     (and
                                      (memq
                                       (type-of ivy-last)
                                       cl-struct-ivy-state-tags)
                                      t))
                                   (signal 'wrong-type-argument
                                           (list 'ivy-state ivy-last)))
                                  (let* ((v ivy-last))
                                    (aset v 7 ivy--index))))
                              (ivy--reset-state
                               ivy-last)
                              (when-let ((wind
                                          (active-minibuffer-window)))
                                (with-selected-window
                                    wind
                                  (insert input)
                                  (goto-char
                                   (when pos
                                     (if (> pos
                                            (point-max))
                                         (point-max)
                                       pos)))
                                  (ivy--exhibit)))
                              (ghub-continue req)))))))
        (unwind-protect
            (assoc (ivy-read prompt gists-keys
                             :action (lambda (gist)
                                       (if ivy-exit
                                           gist
                                         (with-current-buffer buff
                                           (igist-edit-buffer
                                            (cdr (assoc gist gists-alist))))))
                             :caller caller)
                   gists-alist)
          (when (buffer-live-p output-buffer)
            (let ((message-log-max nil))
              (with-temp-message (or (current-message) "")
                (kill-buffer output-buffer)))))))))

;;;###autoload
(defun igist-ivy-read-user-gists (user)
  "Read and display gists for a specific USER.

Argument USER is the username of the user whose gists will be displayed."
  (interactive (read-string "User: "))
  (igist-ivy-read-gists "User gist: " (concat "/users/" user "/gists")))

(defun igist-ivy-read-user-logged-gists (&optional prompt)
  "Read a gist in the minibuffer with PROMPT, using Ivy completions."
  (while (not (igist-get-current-user-name))
    (setq igist-current-user-name (igist-change-user)))
  (igist-ivy-read-gists (or prompt
                            "Gists: ")
                        (concat "/users/" igist-current-user-name
                                "/gists")))

;;;###autoload
(defun igist-ivy-read-public-gists ()
  "Explore public gists in the minibuffer, using Ivy completions."
  (interactive)
  (igist-ivy-read-gists "Public gist: " "/gists/public"))

(defun igist-completing-read-gists (&optional prompt action initial-input)
  "Read gist in minibuffer with PROMPT and INITIAL-INPUT.
If ACTION is non nil, call it with gist."
  (interactive)
  (unless prompt (setq prompt "Gist: "))
  (cond ((and (not igist-list-response)
              (eq completing-read-function 'ivy-completing-read))
         (if action
             (funcall action (igist-ivy-read-user-logged-gists prompt))
           (igist-ivy-read-user-logged-gists prompt)))
        (t
         (setq igist-normalized-gists (igist-normalize-gists
                                       igist-list-response))
         (let* ((interactived (called-interactively-p
                               'any))
                (enhanced-action (lambda (g)
                                   (funcall
                                    (or action (if (or
                                                    (active-minibuffer-window)
                                                    interactived)
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
                                       :preselect
                                       (funcall
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
                    (funcall enhanced-action key))))))))

;;;###autoload
(defun igist-edit-list ()
  "Read the user's gists in the minibuffer and open them in the edit buffer."
  (interactive)
  (while (not (igist-get-current-user-name))
    (setq igist-current-user-name (igist-change-user)))
  (cond ((eq completing-read-function 'ivy-completing-read)
         (igist-completing-read-gists "Edit gist: "
                                      #'igist-edit-gist))
        (t (igist-load-logged-user-gists #'igist-completing-read-gists
                                         "Edit gist\s"
                                         #'igist-edit-gist))))

;;;###autoload
(defun igist-explore-public-gists (&optional background)
  "List up to 3000 public gists, sorted by the most recent.

If BACKGROUND is non-nil, the user's buffer should not be displayed.

Loading of subsequent pages may be halted by the command
`igist-list-cancel-load'."
  (interactive)
  (igist-list-request "/gists/public" nil
                      background))

;;;###autoload
(defun igist-list-starred ()
  "List the authenticated user's starred gists.

Loading of subsequent pages may be stopped by the command
`igist-list-cancel-load'."
  (interactive)
  (while (not (igist-get-current-user-name))
    (setq igist-current-user-name (igist-change-user)))
  (igist-list-request "/gists/starred"
                      igist-current-user-name))

;;;###autoload
(defun igist-list-other-user-gists (user)
  "List public gists of a specified GitHub USER.

Argument USER is a string representing the username of the other user whose
gists are to be listed."
  (interactive (list (read-string "User: ")))
  (igist-list-load-gists user nil))

;;;###autoload
(defun igist-list-gists ()
  "List the gists of `igist-current-user-name'.

Loading of subsequent pages may be stopped by the command
`igist-list-cancel-load', executed in tabulated buffer.

See also `igist-list-mode'."
  (interactive)
  (while (not (igist-get-current-user-name))
    (setq igist-current-user-name (igist-change-user)))
  (igist-list-load-gists
   igist-current-user-name nil))

;;;###autoload
(defun igist-new-gist-from-buffer ()
  "Create a new gist from the current buffer."
  (interactive)
  (when-let ((content (or (igist-get-region-content)
                          (buffer-substring-no-properties (point-min)
                                                          (point-max)))))
    (let ((filename (read-string "Filename: "
                                 (if buffer-file-name
                                     (file-name-nondirectory buffer-file-name)
                                   (buffer-name)))))
      (pop-to-buffer
       (igist-setup-new-gist-buffer filename content)
       nil
       t))))

;;;###autoload
(defun igist-create-new-gist ()
  "Set up and switch to the editable gist buffer.

If Transient Mark mode is enabled and the mark is active,
insert it as initial content."
  (interactive)
  (let ((region-content (igist-get-region-content)))
    (let ((filename (read-string "Filename: "
                                 (when region-content
                                   (igist-suggest-filename)))))
      (pop-to-buffer (igist-setup-new-gist-buffer filename
                                                  (or region-content ""))))))

;;;###autoload
(defun igist-change-user (&optional prompt initial-input history)
  "Change the user for authentication prompting with string PROMPT.

Optional argument PROMPT is the initial value to be displayed in the prompt.

If non-nil, optional argument INITIAL-INPUT is a string to insert before
reading.

The third arg HISTORY, if non-nil, specifies a history list and optionally the
initial position in the list."
  (interactive)
  (if (stringp igist-auth-marker)
      (read-string (or prompt "User: ") initial-input history)
    (let* ((alist (mapcar (lambda (it)
                            (let ((parts (split-string it "[\\^]" t)))
                              (cons (pop parts)
                                    (pop parts))))
                          (igist-get-github-users)))
           (annotf (lambda (str)
                     (format "^%s" (cdr (assoc str alist)))))
           (login-name (completing-read (or prompt
                                            "Github user name: ")
                                        (lambda (str pred action)
                                          (if (eq action 'metadata)
                                              `(metadata
                                                (annotation-function
                                                 . ,annotf))
                                            (complete-with-action
                                             action alist
                                             str
                                             pred)))
                                        nil
                                        nil
                                        initial-input
                                        history))
           (marker (igist-alist-get login-name alist)))
      (when-let ((marker (and marker (intern marker))))
        (unless (eq marker igist-auth-marker)
          (setq igist-auth-marker marker)))
      login-name)))

(defun igist-set-current-user ()
  "Read user name and assign it in the variable `igist-current-user-name'."
  (interactive)
  (setq-default igist-current-user-name
                (igist-change-user "GitHub user name: "
                                   igist-current-user-name))
  (when transient-current-command
    (transient-setup transient-current-command)))

;; Transient
(transient-define-argument igist-set-current-filename-variable ()
  "Define an argument to rename the current filename in igist."
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

(transient-define-argument igist-set-current-description-variable ()
  "Read description and assign it in the variable `igist-current-description'."
  :description "Description"
  :inapt-if-not (lambda ()
                  (or igist-current-filename
                      igist-current-gist))
  :class 'transient-lisp-variable
  :always-read t
  :reader #'igist-read-description
  :argument "--description"
  :variable 'igist-current-description)

(transient-define-argument igist-transient-toggle-public ()
  "Toggle gist visibility and assign it in the variable `igist-current-public'."
  :description "Public"
  :if-not #'igist-get-current-gist-id
  :always-read t
  :class 'transient-lisp-variable
  :init-value (lambda (ob)
                (setf
                 (slot-value ob 'value)
                 igist-current-public))
  :shortarg "-P"
  :variable 'igist-current-public
  :reader #'igist-toggle-public
  :argument "affirmative")

(defun igist--transient-switch-column ()
  "Switch to the next column in a transient menu."
  (interactive)
  (let* ((choices (igist-get-all-cols))
         (current-idx (or (seq-position choices igist-table-current-column) -1))
         (next-idx (% (1+ current-idx)
                      (length choices)))
         (next-choice
          (if (> (length choices)
                 10)
              (let ((col (completing-read
                          "Column: "
                          choices
                          nil
                          t)))
                (unless (member col choices)
                  (user-error "Bad language: %s" col))
                col)
            (nth next-idx choices))))
    (setq igist-table-current-column next-choice)
    (igist-tabulated-list-goto-column
     igist-table-current-column)
    (transient-setup transient-current-command)
    next-choice))

(defun igist-table-current-column-spec ()
  "Find the current column specification in the table."
  (igist-find-column-spec igist-table-current-column))

(defun igist-find-column-spec (column-name)
  "Find the column specification for a given COLUMN-NAME.

Argument COLUMN-NAME is the name of the column that the function/macro
`igist-find-column-spec' is searching for."
  (and column-name
       (or (seq-find (igist-compose
                      (apply-partially #'string=
                                       column-name)
                      cadr)
                     igist-table-list-format)
           (when-let ((subcols
                       (plist-get
                        (nthcdr 5
                                (seq-find
                                 (igist-compose
                                  (apply-partially #'seq-find
                                                   (igist-compose
                                                    (apply-partially
                                                     #'string=
                                                     column-name)
                                                    cadr))
                                  (igist-rpartial plist-get :children)
                                  (apply-partially #'nthcdr 5))
                                 igist-table-list-format))
                        :children)))
             (seq-find (igist-compose
                        (apply-partially #'string=
                                         column-name)
                        cadr)
                       subcols)))))

(defun igist-get-prev-column-if-last (column-name)
  "Get previous column is COLUMN-NAME is last column.

Argument COLUMN-NAME is the name of the column for which the previous column is
to be retrieved."
  (let* ((rows (igist-seq-split (igist-get-all-cols)
                                (length igist-table-list-format)))
         (found (seq-find
                 (igist-compose (apply-partially #'string= column-name)
                                cadr last)
                 rows)))
    (cadr (reverse found))))

(defun igist-table-inc-column-width (step)
  "Set the column width of the current table.

Argument STEP is the amount by which the column width should be increased."
  (when-let ((spec
              (if-let ((prev-col-name (igist-get-prev-column-if-last
                                       igist-table-current-column)))
                  (igist-find-column-spec prev-col-name)
                (igist-table-current-column-spec))))
    (setf (caddr spec)
          (max 1 (+ (caddr spec) step)))))

(defun igist-tabulated-list-widen-current-column (&optional n)
  "Widen or narrow the current column in a tabulated list.

Argument N is an optional argument that specifies the number of columns to
widen."
  (interactive "p")
  (unless n (setq n 1))
  (let ((col (igist-closest-column))
        (keep-old-col
         (memq last-command
               '(igist-tabulated-list-widen-current-column
                 igist-tabulated-list-narrow-current-column))))
    (cond ((and
            keep-old-col
            igist-table-current-column
            (not (equal col igist-table-current-column)))
           (let* ((cols (igist-get-all-cols))
                  (curr-idx (seq-position cols col))
                  (saved-idx (seq-position cols igist-table-current-column)))
             (if (and curr-idx saved-idx)
                 (igist-tabulated-forward--column (- saved-idx curr-idx))
               (igist-tabulated-list-goto-column igist-table-current-column))))
          (t (setq igist-table-current-column
                   (igist-tabulated-column-at-point))))
    (when igist-table-current-column
      (igist-table-inc-column-width n)
      (igist-remember-pos t (igist--tabulated-list-revert))
      (when transient-current-command
        (transient-setup transient-current-command)))))

(defun igist-tabulated-list-narrow-current-column (&optional n)
  "Narrow the current tabulated list column by N chars.
Interactively, N is the prefix numeric argument, and defaults to
1."
  (interactive "p")
  (igist-tabulated-list-widen-current-column (- n)))

(defun igist-table-widen-current-column ()
  "Widen current column in table."
  (interactive)
  (igist-table-inc-column-width 1)
  (igist--tabulated-list-revert))

(defun igist-table-narrow-current-column ()
  "Narrow current column in table."
  (interactive)
  (igist-table-inc-column-width -1)
  (igist--tabulated-list-revert))

(defun igist-transient-setup-current-command ()
  "Setup the transient specified by `transient-current-command' if it is non nil."
  (when transient-current-command
    (transient-setup transient-current-command)))

(defun igist-table-update-current-column-name ()
  "Read and set new name for current column."
  (interactive)
  (when-let ((spec (igist-table-current-column-spec)))
    (let ((newval (read-string "Column name: " (cadr spec))))
      (setf (cadr spec)
            newval)
      (igist--tabulated-list-revert)
      (setq igist-table-current-column newval)
      (igist-transient-setup-current-command))))

(defun igist-table-update-current-column-width ()
  "Read and update the current column width value in the minibuffer."
  (interactive)
  (when-let ((spec (igist-table-current-column-spec)))
    (setf (caddr spec)
          (max 1 (+ (caddr spec)
                    (read-number "Width: " (or (caddr spec) 1)))))
    (igist--tabulated-list-revert)))

(defun igist-table-update-current-column-sortable ()
  "Toggle whether the current column is sortable."
  (interactive)
  (when-let ((spec (igist-table-current-column-spec)))
    (pcase-let ((`(,_key ,name ,old-width ,sortable ,format-val .
                         ,extra-props)
                 spec))
      (setf (cdr spec)
            (append (list name old-width (not sortable) format-val)
                    extra-props))
      (cond (sortable
             (when (equal (car igist-tabulated-list-sort-key)
                          name)
               (igist-tabulated-list-sort -1)))
            ((not sortable)
             (igist-tabulated-list--sort-by-column-name name))))
    (igist-transient-setup-current-command)))

(defun igist-table-update-current-column-pad-right ()
  "Update the right padding of the current column in the igist table."
  (interactive)
  (when-let ((spec (igist-table-current-column-spec)))
    (setf (nthcdr 5 spec)
          (plist-put (nthcdr 5 spec) :pad-right
                     (read-number "Pad right: "
                                  (plist-get (nthcdr 5 spec) :pad-right))))
    (igist--tabulated-list-revert)
    (igist-transient-setup-current-command)))

(defun igist-table-update-current-column-align ()
  "Update the alignment of the current column in the igist table."
  (interactive)
  (when-let ((spec (igist-table-current-column-spec)))
    (setf (nthcdr 5 spec)
          (cond ((plist-get (nthcdr 5 spec)
                            :center-align)
                 (plist-put (plist-put (nthcdr 5 spec)
                                       :right-align t)
                            :center-align nil))
                ((plist-get (nthcdr 5 spec)
                            :right-align)
                 (plist-put (plist-put (nthcdr 5 spec)
                                       :center-align nil)
                            :right-align nil))
                (t (plist-put (nthcdr 5 spec)
                              :center-align t))))
    (igist--tabulated-list-revert)
    (igist-transient-setup-current-command)))

(defun igist-save-column-settings ()
  "Save column settings for igist explore or igist list format."
  (interactive)
  (let* ((sym (igist-get-current-list-format-sym))
         (new-value igist-table-list-format))
    (customize-save-variable sym new-value)
    new-value))

(defun igist-reset-columns-settings ()
  "Reset the column settings of the current igist buffer."
  (interactive)
  (let ((sym (igist-get-current-list-format-sym)))
    (set-default sym
                 (or (eval (car (get sym 'saved-value)))
                     (eval (car (get sym 'standard-value)))))))

(defun igist-list-add-column (column-name)
  "Add a new column to the igist table list format.

Argument COLUMN-NAME is a string that represents the name of the column to be
added."
  (interactive
   (list
    (let ((col-names (seq-difference (igist-pluck-columns-names-from-list-format
                                      igist-default-formats)
                                     (igist-pluck-columns-names-from-list-format
                                      igist-table-list-format))))
      (completing-read
       "Add column: " col-names
       nil t))))
  (let* ((spec
          (let ((igist-table-list-format igist-default-formats))
            (purecopy (igist-find-column-spec column-name))))
         (closes-col (igist-find-column-spec (igist-closest-column)))
         (pos (or (seq-position igist-table-list-format closes-col))))
    (setq igist-table-list-format
          (if pos
              (igist-insert-at igist-table-list-format spec pos)
            (igist-insert-at igist-table-list-format spec
                             (max 0
                                  (1-
                                   (length
                                    igist-table-list-format))))))
    (igist--tabulated-list-revert)))

(defun igist-list-remove-column (column-name)
  "Hide a specified COLUMN-NAME in the current list or explore buffer."
  (interactive
   (list
    (let* ((col-at-point (igist-closest-column))
           (cols (igist-pluck-columns-names-from-list-format
                  igist-table-list-format)))
      (completing-read
       "Remove column: " cols
       nil t
       (and (member
             col-at-point cols)
            col-at-point)))))
  (setq igist-table-list-format
        (igist-remove-column-from-list-format
         column-name
         igist-table-list-format))
  (igist--tabulated-list-revert))

(defun igist-remove-column-from-list-format (column-name cols)
  "Remove a specified column from a list format COLS.

Argument COLUMN-NAME is the name of the column that the user wants to remove
from the list format.
Argument COLS is a list of columns from which the specified column will be
removed."
  (let ((col)
        (found)
        (processed))
    (while (and cols (not found))
      (setq col (pop cols))
      (if (equal (cadr col) column-name)
          (progn (setq found col)
                 (let ((width (caddr col))
                       (prev-col (car processed)))
                   (when prev-col
                     (setf (caddr prev-col)
                           (max 1 (+ width (caddr prev-col)))))))
        (when-let* ((pl (nthcdr 5 col))
                    (children (plist-get
                               pl
                               :children))
                    (item (seq-find
                           (lambda (s)
                             (string= column-name (cadr s)))
                           children)))
          (setq found t)
          (setq pl (plist-put pl :children (remove item children))))
        (push col processed)))
    (setq processed (nreverse processed))
    (setq cols (if (and cols processed)
                   (nconc processed cols)
                 (or processed cols)))
    cols))

(defun igist-pluck-columns-names-from-list-format (list-format)
  "Extract column names from a given list format.

Argument LIST-FORMAT is a list of lists, where each sub-list represents a column
and contains information such as key, name, old-width, sortable, format-val, and
extra-props."
  (let ((cols))
    (pcase-dolist (`(,_key ,name ,_old-width ,_sortable ,_format-val .
                           ,extra-props)
                   list-format)
      (setq cols (append cols
                         (if (plist-get extra-props :children)
                             (append (list name)
                                     (mapcar #'cadr
                                             (plist-get extra-props :children)))
                           (list name)))))
    cols))

(defun igist-swap-current-column-backward ()
  "Swap the current column at point backward with another column."
  (interactive)
  (igist-swap-current-column -1))

(defun igist--adjust-next-pos (pos next-pos cols)
  "Adjust the next position based on the length of the columns COLS.

Argument POS is the current position in the list of columns.
Argument NEXT-POS is the position to which we want to move in the list of
columns.
Argument COLS is the list of columns in which we are moving."
  (if (and pos next-pos
           (or (< next-pos 0)
               (> next-pos (1- (length cols)))))
      (if (< next-pos 0)
          (1- (length cols))
        0)
    next-pos))

(defun igist-swap-current-column (&optional n)
  "Swap the current column with another column in a tabulated list buffer.

Argument N is the number of columns to move forward or backward from the current
column position."
  (interactive "P")
  (unless n
    (setq n 1))
  (let* ((col-name (igist-closest-column))
         (cols (mapcar #'cadr igist-table-list-format))
         (pos (seq-position cols col-name))
         (next-pos
          (when pos
            (igist--adjust-next-pos pos (+ n pos)
                                    cols)))
         (next-value
          (if (and col-name (not pos))
              (mapcar
               (lambda (p)
                 (let* ((pl (nthcdr 5 p))
                        (children (plist-get pl :children)))
                   (if (not children)
                       p
                     (let* ((cols (mapcar #'cadr children))
                            (pos (seq-position cols col-name))
                            (next-pos
                             (when pos
                               (igist--adjust-next-pos pos
                                                       (+ n pos)
                                                       cols))))
                       (if (not next-pos)
                           p
                         (setq pl (plist-put pl :children
                                             (igist-swap pos next-pos
                                                         children)))
                         p)))))
               igist-table-list-format)
            (and pos next-pos
                 (igist-swap pos next-pos igist-table-list-format)))))
    (when next-value
      (setq igist-table-list-format
            next-value)
      (igist--tabulated-list-revert)
      (igist-goto-column col-name)
      (setq igist-table-current-column col-name)
      (when transient-current-command
        (transient-setup transient-current-command)))))

(defun igist-transient-render-cols (list-format)
  "Render columns for a transient description LIST-FORMAT.

Argument LIST-FORMAT is a list that defines the format of the table."
  (let* ((cols nil)
         (children))
    (pcase-dolist (`(,_field-name ,col-name ,_width ,_sortable ,_format-spec .
                                  ,props)
                   list-format)
      (setq cols (push
                  (propertize (substring-no-properties col-name)
                              'face (if
                                        (and
                                         igist-table-current-column
                                         (string=
                                          col-name
                                          igist-table-current-column))
                                        'transient-value
                                      'transient-inactive-value))
                  cols))
      (when (plist-get props :children)
        (setq children (plist-get props :children))))
    (if children
        (let* ((child-cols (igist-transient-render-cols children))
               (parcols (string-join (nreverse cols)
                                     (propertize "|"
                                                 'face
                                                 'transient-inactive-value))))
          (concat parcols "\n" child-cols))
      (string-join (nreverse cols)
                   (propertize "|"
                               'face
                               'transient-inactive-value)))))

(defun igist-transient-column-descriptions ()
  "Generate and format the column descriptions for the transient menu."
  (let ((label "Column: "))
    (concat
     "Column: "
     (propertize "[" 'face
                 'transient-inactive-value)
     (replace-regexp-in-string
      "\n"
      (concat "\n" (make-string (+ 3 (length label)) ?\ ))
      (igist-transient-render-cols igist-table-list-format))
     (propertize "]" 'face
                 'transient-inactive-value))))

;;;###autoload (autoload 'igist-table-menu "igist" nil t)
(transient-define-prefix igist-table-menu ()
  "Invoke a transient prefix command to modify table columns in Igist list mode."
  ["Edit column"
   ("c" igist--transient-switch-column :description
    igist-transient-column-descriptions)]
  [:setup-children
   (lambda (&rest _argsn)
     (mapcar
      (apply-partially #'transient-parse-suffix
                       transient--prefix)
      (pcase-let ((`(,_field-name ,col-name ,width ,sortable ,format-spec .
                                  ,props)
                   (igist-table-current-column-spec)))
        (list (list "n" #'igist-table-update-current-column-name
                    :description
                    (lambda ()
                      (concat "Column Name: "
                              (propertize (or col-name "") 'face
                                          'transient-value))))
              (list "w" #'igist-table-update-current-column-width
                    :description
                    (lambda ()
                      (concat "Width: "
                              (propertize
                               (format "%s"
                                       width)
                               'face
                               'transient-value))))
              (list "<right>" #'igist-table-widen-current-column
                    :description (lambda
                                   ()
                                   (format "Increase %s width (%s)"
                                           (or igist-table-current-column "")
                                           (caddr
                                            (igist-table-current-column-spec))))
                    :transient t)
              (list "<left>" #'igist-table-narrow-current-column
                    :description (lambda
                                   ()
                                   (format "Decrease %s width (%s)"
                                           (or igist-table-current-column "")
                                           (caddr
                                            (igist-table-current-column-spec))))
                    :transient t)
              ""
              (list "s" #'igist-table-update-current-column-sortable
                    :description
                    (lambda ()
                      (format "Sortable: %s" sortable)))
              (list "f" 'ignore :description
                    (lambda ()
                      (format "Format: %s " format-spec)))
              (list "a" #'igist-table-update-current-column-align
                    :description
                    (lambda ()
                      (concat
                       "Align: "
                       (propertize "[" 'face
                                   'transient-inactive-value)
                       (mapconcat
                        (lambda (key)
                          (let ((value (plist-get props
                                                  key)))
                            (propertize (format "%s" key)
                                        'face
                                        (if
                                            value
                                            'transient-value
                                          'transient-inactive-value))))
                        '(:center-align :right-align)
                        (propertize "|" 'face
                                    'transient-inactive-value))
                       (propertize "]" 'face
                                   'transient-inactive-value))))
              (list "p" #'igist-table-update-current-column-pad-right
                    :description
                    (lambda ()
                      (format ":pad-right %s"
                              (plist-get
                               props
                               :pad-right))))))))]
  [("M-<left>" "Move column backward" igist-swap-current-column-backward
    :transient nil)
   ("M-<right>" "Move column forward" igist-swap-current-column
    :transient nil)
   ("r" "Remove column" igist-list-remove-column)
   ("+" "Add column" igist-list-add-column)]
  ["Settings"
   ("R" "Reset" igist-reset-columns-settings)
   ("S" "Save"
    igist-save-column-settings
    :transient t)]
  (interactive)
  (igist-table-init-current-column)
  (transient-setup #'igist-table-menu))

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
  :lighter " Igst-ed"
  :keymap igist-edit-mode-map
  :global nil
  (when igist-edit-mode
    (setq buffer-read-only nil)
    (set-buffer-modified-p nil)))

(put 'igist-edit-mode 'permanent-local t)

;;;###autoload (autoload 'igist-dispatch "igist" nil t)
(transient-define-prefix igist-dispatch ()
  "Invoke a transient menu to manage GitHub gists through various actions.

The commands available in the menu vary based on the current major mode and
editing mode."
  :transient-non-suffix #'transient--do-stay
  [[:if-derived
    igist-list-mode
    "Gist at point"
    ("RET" "Edit" igist-list-edit-gist-at-point :inapt-if-not
     igist-tabulated-list-get-id)
    ("v" "View" igist-list-view-current :inapt-if-not
     igist-tabulated-list-get-id)
    ("f" "Fork" igist-fork-gist :inapt-if-not igist-forkable)
    ("w" "Copy Url" igist-copy-gist-url :inapt-if-not
     igist-tabulated-list-get-id)
    ("r" "Browse" igist-browse-gist :inapt-if-not igist-tabulated-list-get-id)
    ("S" "Star" igist-star-gist :inapt-if-not igist-tabulated-list-get-id)
    ("U" "Unstar" igist-unstar-gist :inapt-if-not igist-tabulated-list-get-id)
    ("D" "Delete" igist-delete-current-gist :inapt-if-not igist-editable-p)
    ("d" "Description" igist-list-edit-description :inapt-if-not
     igist-tabulated-list-get-id)
    ("{" igist-tabulated-list-widen-current-column
     :description (lambda
                    ()
                    (format "Increase %s width (%s)"
                            (or igist-table-current-column
                                (igist-tabulated-column-at-point) "")
                            (cadr
                             (igist-table-current-column-spec))))
     :transient nil)
    ("}" igist-tabulated-list-narrow-current-column
     :description (lambda
                    ()
                    (format "Decrease %s width (%s)"
                            (or igist-table-current-column
                                (igist-tabulated-column-at-point)
                                "")
                            (cadr
                             (igist-table-current-column-spec))))
     :transient nil)
    ("<tab>" "Toggle visibility of row children"
     igist-toggle-row-children-at-point
     :inapt-if-not igist-tabulated-list-get-id
     :transient t)
    ("<backtab>" "Toggle visibility of subrows"
     igist-toggle-all-children
     :transient t)]
   [:if
    igist-edit-mode-p
    "Actions"
    ("RET" "Save" igist-save-current-gist :inapt-if-not igist-editable-p)
    ("f" "Fork" igist-fork-gist :inapt-if-not igist-forkable)
    ("w" "Copy URL" igist-copy-gist-url
     :inapt-if-not igist-get-current-gist-url)
    ("r" "Browse" igist-browse-gist :inapt-if-not igist-get-current-gist-url)
    ("S" "Star" igist-star-gist :inapt-if-not igist-get-current-gist-id)
    ("U" "Unstar" igist-unstar-gist :inapt-if-not igist-get-current-gist-id)
    ("D" "Delete" igist-delete-current-gist :inapt-if-not igist-editable-p)
    ("P" igist-transient-toggle-public)
    ("R" igist-set-current-filename-variable)
    ("d" igist-set-current-description-variable)]
   ["List"
    ("l" "My gists" igist-list-gists :inapt-if-not igist-get-current-user-name)
    ("m" "Starred" igist-list-starred :inapt-if-not igist-get-current-user-name)
    ("E" "Explore" igist-explore-public-gists :inapt-if-mode igist-explore-mode)
    ("o" "Other user" igist-list-other-user-gists)
    ("g" "Refresh" igist-list-refresh :inapt-if-not-derived igist-list-mode)
    ("K" "Cancel load" igist-list-cancel-load :inapt-if-not-derived
     igist-list-mode)
    ("X" "Kill buffers" igist-kill-all-gists-buffers)]
   [:if-not-derived igist-list-mode
                    "Create"
                    ("n" "New" igist-create-new-gist :inapt-if-not
                     igist-get-current-user-name)
                    ("b" "New from buffer" igist-new-gist-from-buffer
                     :inapt-if-not
                     igist-get-current-user-name)
                    ("p" igist-post-files
                     :description
                     (lambda ()
                       (if-let ((marked-files
                                 (and (fboundp
                                       'dired-get-marked-files)
                                      (dired-get-marked-files))))
                           (format "Post %d marked files" (length marked-files))
                         "Post files"))
                     :inapt-if-not igist-get-current-user-name)]]
  [:if-non-nil
   igist-current-gist
   ["Files"
    ("-" "Delete" igist-delete-current-filename :inapt-if-not
     igist-get-current-gist-url)
    ("+" "Add" igist-add-file-to-gist :inapt-if-not igist-get-current-gist-url)]
   ["Comments"
    ("a" "Add" igist-add-comment  :inapt-if-not igist-get-current-gist-url)
    ("c" "Show" igist-load-comments  :inapt-if-not
     igist-get-current-gist-url)
    ("e" "Edit" igist-add-or-edit-comment :inapt-if-not
     igist-get-comment-id-at-point)]]
  [:if-derived
   igist-list-mode
   ["Create"
    ("n" "New" igist-create-new-gist :inapt-if-not igist-get-current-user-name)
    ("b" "New from buffer" igist-new-gist-from-buffer :inapt-if-not
     igist-get-current-user-name)]
   ["Files"
    ("+" "Add" igist-list-add-file :inapt-if-not igist-editable-p)
    ("-" "Delete" igist-delete-current-filename :inapt-if-not igist-editable-p)]
   ["Comments"
    ("a" "Add" igist-add-comment :inapt-if-not igist-tabulated-list-get-id)
    ("c" "Show" igist-load-comments :inapt-if-not igist-tabulated-list-get-id)]
   ["Settings"
    ("C" "Configure table view" igist-table-menu)
    ("/" igist-filters-menu
     :description (lambda ()
                    (concat "Filters "
                            (propertize (format "(%d)" (length igist-filters))
                                        'face (if igist-filters
                                                  'transient-value
                                                'transient-inactive-value))
                            " ")))
    ("s" "Show languages statistics" igist-print-languages-chart)]]
  [:if igist-comments-list-mode-p
       ["Comments"
        ("a" "Add" igist-add-comment :inapt-if-not igist-get-current-user-name)
        ("g" "Reload" igist-load-comments :inapt-if-not
         igist-get-current-user-name)
        ("e" "Edit" igist-add-or-edit-comment :inapt-if-not
         igist-get-comment-id-at-point)
        ("D" "Delete" igist-delete-comment-at-point :inapt-if-not
         igist-get-comment-id-at-point)]]
  ["User"
   ("u" igist-set-current-user
    :description (lambda ()
                   (concat "Login Name "
                           (propertize
                            (substring-no-properties
                             (or
                              igist-current-user-name
                              ""))
                            'face 'transient-value)))
    :transient nil)
   ("q" "Quit" transient-quit-all)]
  (interactive)
  (when (derived-mode-p 'igist-list-mode)
    (igist-table-init-current-column))
  (transient-setup #'igist-dispatch))

(provide 'igist)
;;; igist.el ends here