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

(require 'transient)
(require 'timezone)
(require 'ghub)

(eval-when-compile
  (require 'subr-x))

(defvar-local igist-list-hidden-ids nil)
(defvar-local igist-list-response nil)
(defvar-local igist-table-list-format nil)
(defvar-local igist-render-timer nil)

(defun igist-scan-make-indicator (count action &optional data)
  "Create button with COUNT of children.
DATA should be an argument for ACTION."
  (funcall
   (if (fboundp 'buttonize) 'buttonize 'button-buttonize)
   (format "%s" count)
   action
   data
   "Click to expand"))

(defun igist-find-children-spec (list-spec)
  "Find the position, spec, and parent spec of children in a LIST-SPEC.

Argument LIST-SPEC is a list of specifications that the function uses to find
and return the position, specification, and parent specification of the
subcolumns."
  (let ((subcolumns-spec)
        (subcolumns-parent-spec)
        (pos))
    (dotimes (n (length list-spec))
      (let ((spec (nth n list-spec)))
        (when (memq :children spec)
          (setq pos n)
          (setq subcolumns-spec spec)
          (setq subcolumns-parent-spec (plist-get
                                        (nthcdr 3
                                                (aref tabulated-list-format n))
                                        :children)))))
    (list pos subcolumns-spec subcolumns-parent-spec)))

(defun igist-collapse-row-children ()
  "Collapse the children of a row in a tabulated list."
  (pcase-let ((`(,beg . ,end)
               (igist-property-boundaries 'tabulated-list-id))
              (id (tabulated-list-get-id)))
    (when (and beg end)
      (goto-char beg)
      (let ((inhibit-read-only t))
        (delete-region (line-end-position) end)))
    (when (igist-entry-expanded-p id)
      (setq igist-list-hidden-ids (push id igist-list-hidden-ids)))))

(defun igist-expand-row-children (&optional value)
  "Expand the row children in a tabulated list.

Argument VALUE is an optional parameter that represents the entries of
subcolumns if provided, otherwise it get the entries from the
`igist-list-response'."
  (pcase-let
      ((`(,beg . ,end)
        (igist-property-boundaries 'tabulated-list-id))
       (id (tabulated-list-get-id))
       (`(,subcolumns-spec-n ,subcolumns-spec ,subcolumns-parent-spec)
        (igist-find-children-spec igist-table-list-format)))
    (when (and beg end subcolumns-spec-n)
      (let* ((pl (nthcdr 5 subcolumns-spec))
             (list-format (plist-get pl :children))
             (list-padding (max (igist-list-calc-column-length
                                 (or (plist-get pl :align-to-column)
                                     0))
                                tabulated-list-padding))
             (subcolumns-entries
              (or value
                  (and
                   subcolumns-spec
                   (cdr (assq (car subcolumns-spec)
                              (igist-alist-find-by-prop
                               'id id
                               igist-list-response))))))
             (inhibit-read-only t))
        (goto-char beg)
        (goto-char (line-end-position))
        (igist-render-subcolumns subcolumns-entries
                                 list-format
                                 list-padding
                                 subcolumns-parent-spec)
        (add-text-properties
         beg (point)
         `(tabulated-list-id ,id))
        (unless (igist-entry-expanded-p id)
          (setq igist-list-hidden-ids (delete id igist-list-hidden-ids)))))))

(defun igist-entry-expanded-p (id)
  "Check if ID is a member of `igist-list-hidden-ids''.

Argument ID is the identifier that will be checked for membership in the list
`igist-list-hidden-ids'."
  (not (member id igist-list-hidden-ids)))

(defun igist-toggle-children-row (&optional subentries)
  "Toggle visibility of row's children in igist mode.

Argument SUBENTRIES is an optional argument that specifies the subentries to be
expanded when toggling the children row."
  (when-let ((id (tabulated-list-get-id)))
    (if (igist-entry-expanded-p id)
        (igist-collapse-row-children)
      (igist-expand-row-children subentries))))

(defun igist-toggle-all-children ()
  "Toggle the visibility of all children in the igist tabulated list."
  (interactive)
  (setq igist-list-hidden-ids (if igist-list-hidden-ids
                                  nil
                                (mapcar (apply-partially #'igist-alist-get 'id)
                                        tabulated-list-entries)))
  (igist-tabulated-list-print t))

(defun igist-toggle-row-children-at-point ()
  "Toggle visibility of current row's children at point."
  (interactive)
  (save-excursion
    (igist-toggle-children-row)))

(defun igist-format-time-diff (time)
"Calculate and format the time difference from the current TIME.

Argument TIME is the time value that will be compared with the current time to
calculate the time difference."
  (let ((diff-seconds (- (float-time (current-time)) (float-time time))))
    (pcase-let ((`(,format-str . ,value)
                 (cond ((< diff-seconds 60)
                        (cons "%d second" (truncate diff-seconds)))
                       ((< diff-seconds 3600)
                        (cons "%d minute" (truncate (/ diff-seconds 60))))
                       ((< diff-seconds 86400)
                        (cons "%d hour" (truncate (/ diff-seconds 3600))))
                       ((< diff-seconds 2592000)
                        (cons "%d day" (truncate (/ diff-seconds 86400))))
                       (t
                        (cons "%d month" (truncate (/ diff-seconds 2592000)))))))
      (format (concat format-str (if (= value 1) " ago" "s ago")) value))))


(defun igist-render-time (value)
  "Format a given VALUE as a date and time.

Argument VALUE is the input value that will be used to calculate the rendered
time."
  (if value
      (igist-format-time-diff
       (igist--get-time value))
    ""))

(defun igist-render-files (files)
  "Render FILES and join them with newlines and padding.

Argument FILES is an alist of gist files."
  (igist-scan-make-indicator
   (length files)
   #'igist-toggle-children-row files))

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

(defvar igist-default-formats
  `((id "ID" 9 nil "%s" :pad-right 4)
    (description "Description" 50 t "%s")
    (created_at "Created" 15 t igist-render-time)
    (updated_at "Updated" 15 t igist-render-time)
    (owner "User" 10 t igist-render-user)
    (comments "Comments" 9 t igist-render-comments)
    (public "Public" 8 t igist-render-public)
    (files "Files" 10 t igist-render-files
           :children ((filename "File" 90 nil "%s")
                      (language "Language" 8 nil "%s"))
           :align-to-column 1)))

(defun igist-pick-from-alist (keys alist)
  "Filter ALIST by KEYS.

Argument ALIST is a list of key-value pairs.
Argument KEYS is a list of KEYS to filter the alist by."
  (let ((filtered-alist '()))
    (dolist (key keys)
      (when-let ((cell (assq key alist)))
        (setq filtered-alist (cons cell filtered-alist))))
    (nreverse filtered-alist)))

(defvar igist-list-custom-type
  '(alist
    :key-type
    (choice
     (const :tag "Id" id)
     (const :tag "Url" url)
     (const :tag "Forks Url" forks_url)
     (const :tag "Commits Url" commits_url)
     (const :tag "Node Id" node_id)
     (const :tag "Git Pull Url" git_pull_url)
     (const :tag "Git Push Url" git_push_url)
     (const :tag "Html Url" html_url)
     (const :tag "Public" public)
     (const :tag "Created At" created_at)
     (const :tag "Updated At" updated_at)
     (const :tag "Description" description)
     (const :tag "Comments" comments)
     (const :tag "Comments Url" comments_url)
     (const :tag "User" user)
     (const :tag "Owner" owner)
     (const :tag "Truncated" truncated)
     (const :tag "Files" files))
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
       :format "%v"
       :inline t
       (const
        :format ""
        :right-align)
       (boolean :tag ":right-align" t))
      (list
       :format "%v"
       :inline t
       (const
        :format ""
        :pad-right)
       (integer :tag ":pad-right" 1))
      (list
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
        :key-type symbol
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
           :format "%v"
           :inline t
           (const
            :format ""
            :right-align)
           (boolean :tag ":right-align" t))
          (list
           :format "%v"
           :inline t
           (const
            :format ""
            :pad-right)
           (integer :tag ":pad-right" 1))
          (list
           :format "%v"
           :inline t
           (const
            :format ""
            :align-to-column)
           (integer :tag ":align-to-column" 1))))))))))



(defvar igist-explore-buffer-name "*igist-explore*"
  "Buffer name for tabulated gists display of multiple owners.")

(defcustom igist-explore-format (append (igist-pick-from-alist
                                         '(id
                                           description
                                           owner
                                           comments)
                                         (copy-tree igist-default-formats))
                                        '((files "Files" 0 t igist-render-files
                                                 :children
                                                 ((filename "File" 79
                                                            nil "%s")
                                                  (language "Language"
                                                            0 nil "%s"))
                                                 :align-to-column 1)))
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
   - :right-align: If non-nil, the column should be right-aligned.
   - :pad-right: Number of additional padding spaces to theright of the column,
   - :children - Specification expandable row values in the same format,
      but without PROPS:
\(FIELD COLUMN-NAME WIDTH SORTABLE FORMAT-FUNCTION/FORMAT-STRING)
   - :align-to-column - index of parent column to align children.

User can interactively adjusts the width of the columns with commands:

- `igist-tabulated-list-widen-current-column'
- `igist-tabulated-list-narrow-current-column'
- `igist-table-menu'

and save the result with command `igist-save-column-settings'."
  :type igist-list-custom-type
  :group 'igist)

(defcustom igist-list-format (igist-pick-from-alist
                              '(id
                                description
                                public
                                updated_at
                                comments
                                files)
                              (copy-tree igist-default-formats))
  "The format of the user Tabulated Gists buffers.

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
   - :right-align: If non-nil, the column should be right-aligned.
   - :pad-right: Number of additional padding spaces to theright of the column,
   - :children - Specification expandable row values in the same format,
      but without PROPS:
\(FIELD COLUMN-NAME WIDTH SORTABLE FORMAT-FUNCTION/FORMAT-STRING)
   - :align-to-column - index of parent column to align children.

User can interactively adjusts the width of the columns with commands:

- `igist-tabulated-list-widen-current-column'
- `igist-tabulated-list-narrow-current-column'
- `igist-table-menu'

and save the result with command `igist-save-column-settings'."
  :type igist-list-custom-type
  :group 'igist)

(defcustom igist-enable-copy-gist-url-p 'after-new
  "Whether and when to add new or updated gist's URL to kill ring."
  :group 'igist
  :type '(radio
          (const :tag "After creating new and saving existing gists" t)
          (const :tag "After saving existing gists" after-update)
          (const :tag "After creating new gist" after-new)
          (const :tag "Never" nil)))

(defun igist-list-calc-column-length (&optional n)
  "Calculate the total length of N columns in a list.

Argument N is an optional argument that specifies the number of elements to take
from the `tabulated-list-format'' list."
  (let ((l
         (if n
             (seq-take (append tabulated-list-format nil)
                       n)
           (append tabulated-list-format nil))))
    (seq-reduce (lambda (acc it)
                  (let ((props (nthcdr 3 it)))
                    (if-let ((width (nth 1 it)))
                        (+ acc
                           (+ width
                              (or
                               (plist-get props :pad-right)
                               1)))
                      acc)))
                l
                (or tabulated-list-padding 0))))

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
       (>= n 30)
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
    (set-keymap-parent map tabulated-list-mode-map)
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
    (when (fboundp 'tabulated-list-widen-current-column)
      (define-key map [remap tabulated-list-widen-current-column]
                  #'igist-tabulated-list-widen-current-column))
    (when (fboundp 'tabulated-list-narrow-current-column)
      (define-key map [remap tabulated-list-narrow-current-column]
                  #'igist-tabulated-list-narrow-current-column))
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
  (declare (indent 1)
           (debug t))
  `(when (and
          (get-buffer ,buffer-or-name)
          (buffer-live-p (get-buffer ,buffer-or-name)))
     (with-current-buffer (get-buffer ,buffer-or-name)
       (progn ,@body))))

(defun igist-get-current-user-name ()
  "Return the current user's name if it's a non-empty string."
  (when (and igist-current-user-name
             (stringp igist-current-user-name)
             (not (string-empty-p igist-current-user-name)))
    igist-current-user-name))

(defun igist-get-user-buffer-name (user)
  "Return the name of buffer with USER's gists."
  (when user (concat "*igist-" user "*")))

(defun igist-get-gist-buffer (id filename)
  "Return gist's FILENAME buffer with ID."
  (get-buffer (concat id "-" filename)))

(defun igist-ensure-gist-list-mode ()
  "Turn on `igist-list-mode' if it is not active."
  (unless (eq major-mode 'igist-list-mode)
    (igist-list-mode)))

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

(defun igist-get-current-list-format-sym ()
  "Determine the current list format symbol based on the buffer type."
  (if (igist-explore-buffer-p (current-buffer))
      'igist-explore-format
    'igist-list-format))

(defun igist-editable-p (&optional gist)
  "Check whether user `igist-current-user-name' can edit GIST."
  (and (igist-get-current-user-name)
       (cond ((eq major-mode 'igist-list-mode)
              (when-let ((owner (igist-get-owner
                                 (or gist (igist-tabulated-gist-at-point)))))
                (equal (igist-get-current-user-name) owner)))
             ((igist-edit-mode-p)
              (if-let ((owner (igist-get-owner
                               (or gist igist-current-gist))))
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

(defun igist-current-buffer-explore-p ()
  "Return t if current buffer is explore buffer."
  (igist-explore-buffer-p (current-buffer)))

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

(defun igist-visible-windows ()
  "Return a list of the visible, non-popup (dedicated) windows."
  (seq-filter (igist-or
               (igist-rpartial window-parameter 'visible)
               (igist-compose not window-dedicated-p))
              (window-list)))

(defun igist-get-all-edit-buffers ()
  "Return all buffers with the loaded content of gists."
  (seq-filter
   (igist-and buffer-live-p
              (apply-partially #'buffer-local-value 'igist-current-gist))
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
  (let ((buffer-file-name (or
                           (if (file-name-absolute-p filename)
                               filename
                             (expand-file-name filename default-directory)))))
    (ignore-errors
      (set-auto-mode)
      (font-lock-ensure))))

(defun igist--get-time (value)
  "Return timestamp from VALUE."
  (let* ((date (timezone-parse-date value))
         (time (timezone-parse-time (aref date 3))))
    (encode-time (string-to-number (aref time 2))
                 (string-to-number (aref time 1))
                 (string-to-number (aref time 0))
                 (string-to-number (aref date 2))
                 (string-to-number (aref date 1))
                 (string-to-number (aref date 0))
                 (aref date 4))))

(defun igist-alist-find-by-prop (prop value alist)
  "Return first element in ALIST which property PROP equals VALUE."
  (seq-find (lambda (cell)
              (and (equal value
                          (igist-alist-get prop cell))))
            alist))

(defun igist-tabulated-gist-at-point ()
  "Get tabulated gist at point."
  (when-let ((id (tabulated-list-get-id)))
    (igist-alist-find-by-prop 'id id igist-list-response)))

(defun igist-explore-buffer-p (buffer)
  "Check whether BUFFER is supposed to list gits of multiple users."
  (string= (buffer-name buffer) igist-explore-buffer-name))

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

(defun igist-list-view-current ()
  "Display gist in other window, without selecting it."
  (interactive)
  (when-let ((current-window (selected-window))
             (gist (igist-list-gist-to-fetch)))
    (with-selected-window current-window
      (let ((buff (igist-setup-edit-buffer gist)))
        (switch-to-buffer-other-window buff)))))

(defun igist-explore-load-other-user-gists (user)
  "Load gists from another USER.

Argument USER is the username of the user whose gists will be loaded."
  (interactive
   (list
    (or (igist-get-owner (igist-tabulated-gist-at-point))
        (read-string "User: "))))
  (igist-list-load-gists user))

(defun igist-list-edit-description (&rest _)
  "Edit the description of the gist at the current point."
  (interactive)
  (if-let ((gist (igist-tabulated-gist-at-point)))
      (let ((description (igist-alist-get
                          'description gist)))
        (if (igist-editable-p gist)
            (igist-patch (concat "/gists/" (igist-alist-get 'id gist))
                         nil
                         :payload `((description . ,(read-string "Description: "
                                                                 description)))
                         :callback (lambda (&rest _)
                                     (igist-load-logged-user-gists)))
          (message (or description
                       "No description"))))
    (user-error "No gist at point")))

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
  (time-less-p (or (and a (igist--get-time a))
                   0)
               (or (and b (igist--get-time b))
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

(defun igist-list--get-sorter ()
  "Return a sorting predicate for the current tabulated-list.
Return nil if `tabulated-list-sort-key' specifies an unsortable
column.  Negate the predicate that would be returned if
`tabulated-list-sort-key' has a non-nil cdr."
  (when (and tabulated-list-sort-key
             (car tabulated-list-sort-key))
    (let* ((format-spec igist-table-list-format)
           (n (tabulated-list--column-number (car tabulated-list-sort-key)))
           (spec (nth n format-spec))
           (field (car spec))
           (sort-spec (nth 3 spec))
           (sort-fn (if (eq sort-spec t)
                        (cdr (assq field igist-sort-pred-alist))
                      sort-spec)))
      (if (cdr tabulated-list-sort-key)
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
                          'tabulated-list-column-name
                          (car obj))))))



(defun igist-tabulated-list-init-header ()
  "Set up header line for the Igist Tabulated List buffer."
  (let* ((x (max tabulated-list-padding 0))
         (button-props `(help-echo "Click to sort by column"
                                   mouse-face header-line-highlight
                                   keymap ,igist-tabulated-list-sort-button-map))
         (len (length tabulated-list-format))
         (cols nil))
    (push (propertize " " 'display
                      `(space :align-to (+ header-line-indent-width ,x)))
          cols)
    (dotimes (n len)
      (let* ((col (aref tabulated-list-format n))
             (not-last-col (< n (1- len)))
             (label (nth 0 col))
             (lablen (length label))
             (pname label)
             (width (nth 1 col))
             (props (nthcdr 3 col))
             (pad-right (or (plist-get props :pad-right) 1))
             (right-align (plist-get props :right-align))
             (next-x (+ x pad-right width))
             (available-space
              (and not-last-col
                   width)))
        (when (and (>= lablen 3)
                   not-last-col
                   (> lablen available-space))
          (setq label (truncate-string-to-width label available-space
                                                nil nil t)))
        (push
         (cond ((not (nth 2 col))
                (propertize label 'tabulated-list-column-name pname))
               ((equal (car col)
                       (car tabulated-list-sort-key))
                (apply #'propertize
                       (concat label
                               (cond ((and (< lablen 3) not-last-col) "")
                                     ((cdr tabulated-list-sort-key)
                                      (format " %c"
                                              tabulated-list-gui-sort-indicator-desc))
                                     (t
                                      (format " %c"
                                              tabulated-list-gui-sort-indicator-asc))))
                       'face 'bold
                       'tabulated-list-column-name pname
                       button-props))
               (t (apply #'propertize label
                         'tabulated-list-column-name pname
                         button-props)))
         cols)
        (when right-align
          (let ((shift (- width (string-width (car cols)))))
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
              (setq x (+ x shift)))))
        (if (>= pad-right 0)
            (push (propertize
                   " "
                   'display `(space :align-to
                                    (+ header-line-indent-width ,next-x))
                   'face 'fixed-pitch)
                  cols))
        (setq x next-x)))
    (setq cols (apply #'concat (nreverse cols)))
    (if tabulated-list-use-header-line
        (setq header-line-format (list "" 'header-line-indent cols))
      (setq-local tabulated-list--header-string cols))))

(defun igist-tabulated-list-render-col (spec data used-width not-last-col)
  "Render a column DATA in a tabulated list with specified specification SPEC.
SPEC is list of (FIELD-NAME COLUMN-NAME WIDTH SORTABLE FORMAT-VAL PROPS).
First element in the SPEC should be a key for a value data
DATA should be an alist of first spec.

Argument USED-WIDTH is the total width of previous columns in the row.

Argument NOT-LAST-COL is a boolean value that indicates whether the current
column is the last column in the table."
  (pcase-let* ((`(,field-name ,column-name ,width ,_sortable ,format-val .
                              ,props)
                spec)
               (pad-right (or (plist-get props :pad-right) 1))
               (right-align (plist-get props :right-align))
               (value (cdr (assq field-name data)))
               (col-desc
                (cond ((functionp format-val)
                       (or (funcall format-val value) ""))
                      (t (format
                          (or format-val "%s")
                          (or value "")))))
               (label
                (cond ((stringp col-desc) col-desc)
                      ((eq (car col-desc) 'image) " ")
                      (t (car col-desc))))
               (label-width (string-width label))
               (help-echo (concat column-name ": " label))
               (opoint (point))
               (available-space width))
    (when (and not-last-col
               (> label-width available-space))
      (setq label (truncate-string-to-width
                   label available-space nil nil t t)
            label-width available-space))
    (setq label (bidi-string-mark-left-to-right label))
    (when (and right-align (> width label-width))
      (let ((shift (- width label-width)))
        (insert (propertize (make-string shift ?\s)
                            'display `(space :align-to ,(+ used-width shift))))
        (setq width (- width shift))
        (setq used-width (+ used-width shift))))
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
                           (list 'tabulated-list-column-name
                                 column-name
                                 'field-name
                                 field-name
                                 field-name
                                 value))
      next-x)))

(defun igist-render-entry (elt list-format list-padding parent-spec &optional
                               extra-props)
  "Render an entry ELT in a tabulated list with formatting LIST-FORMAT.

LIST-PADDING is a number of characters preceding each Tabulated List mode entry.

LIST-FORMAT is a list that specifies the properties of the entry columns in the
format (field-name column-name width sortable renderer [extra-props]).

PARENT-SPEC is a LIST-FORMAT in the Tabulated list format.

Argument EXTRA-PROPS is an optional argument that allows the user to provide
additional properties to be added to the text."
  (let ((x  (max list-padding 0))
        (ncols (length list-format))
        (inhibit-read-only t)
        (beg (point))
        (col-names))
    (if (> list-padding 0)
        (insert (make-string x ?\s)))
    (dotimes (n ncols)
      (let ((width (nth 1 (aref parent-spec n)))
            (spec (nth n list-format))
            (col-name))
        (setq col-name (cadr spec))
        (push col-name col-names)
        (setq x (igist-tabulated-list-render-col
                 (append (list (car spec) col-name
                               width)
                         (seq-drop spec 3))
                 elt
                 x
                 (< (1+ n)
                    (length list-format))))))
    (add-text-properties
     beg (point)
     (if extra-props
         (append `(columns ,(nreverse col-names))
                 extra-props)
       `(columns ,(nreverse col-names))))))

(defun igist-render-subcolumns (entries list-format list-padding parent-spec)
  "Render subcolumns for a list with specified format and padding.

Argument PARENT-SPEC is a list that specifies the properties of the parent
columns in the format `(column-name width [property value]...)`.
Argument LIST-PADDING is the number of spaces to be inserted before each line in
the output.
Argument LIST-FORMAT is a list that specifies the properties of the subcolumns
in the format `(column-name property value ...)`.
Argument ENTRIES is a list of ENTRIES to be rendered in the subcolumns."
  (let ((count))
    (while entries
      (let* ((elt  (car entries)))
        (setq count (if count (1+ count) 0))
        (let ((inhibit-read-only t))
          (insert "\n")
          (igist-render-entry elt
                              list-format
                              list-padding
                              parent-spec
                              `(subrow ,count))))
      (setq entries (cdr entries)))))

(defun igist-tabulated-list-print-entry (gist)
  "Prints an entry in a tabulated list with given GIST.

Argument GIST is the input gist that contains information about a specific
entry."
  (let ((id (cdr (assq 'id gist)))
        (beg   (point))
        (list-spec igist-table-list-format)
        (inhibit-read-only t)
        (subcolumns-spec)
        (subcolumns-parent-spec))
    (igist-render-entry gist list-spec (max tabulated-list-padding 0)
                        tabulated-list-format)
    (dotimes (n (length list-spec))
      (let ((spec (nth n list-spec)))
        (when (memq :children spec)
          (setq subcolumns-spec spec)
          (setq subcolumns-parent-spec (plist-get
                                        (nthcdr 3
                                                (aref tabulated-list-format n))
                                        :children)))))
    (when-let ((subcolumns-entries (and
                                    subcolumns-spec
                                    (igist-entry-expanded-p id)
                                    (cdr (assq (car subcolumns-spec) gist))))
               (pl (nthcdr 5 subcolumns-spec)))
      (let ((list-format (plist-get pl :children))
            (list-padding (max (igist-list-calc-column-length
                                (or (plist-get pl :align-to-column)
                                    0))
                               tabulated-list-padding)))
        (igist-render-subcolumns subcolumns-entries
                                 list-format
                                 list-padding
                                 subcolumns-parent-spec)))
    (insert ?\n)
    (add-text-properties
     beg (point)
     `(tabulated-list-id ,id))))

(defun igist-restore-position (position &optional column saved-offset)
  "Restore POSITION and COLUMN in Emacs Lisp.

Argument SAVED-OFFSET is the number of lines to move forward from the given
POSITION.
Argument COLUMN is the optional COLUMN number to move the cursor to.
Argument POSITION is the POSITION in the buffer to move the cursor to."
  (goto-char position)
  (when saved-offset
    (forward-line saved-offset))
  (when column
    (move-to-column column)))

(defun igist-tabulated-list-print (&optional remember-pos update)
  "Prints a tabulated list with optional sorting.

Argument UPDATE is an optional boolean flag that determines whether the
tabulated list should be updated or not.
Argument REMEMBER-POS is an optional boolean flag that determines whether the
current position in the tabulated list should be remembered or not."
  (let ((inhibit-read-only t)
        (entries (if (functionp tabulated-list-entries)
                     (funcall tabulated-list-entries)
                   tabulated-list-entries))
        (sorter (igist-list--get-sorter))
        saved-pt
        saved-col
        saved-offset
        entry-id)
    (when remember-pos
      (setq entry-id (tabulated-list-get-id))
      (setq saved-col (current-column))
      (when entry-id
        (while (get-text-property (point)
                                  'subrow)
          (forward-line -1)
          (setq saved-offset (1+ (or saved-offset 0)))))
      (setq saved-pt (point)))
    (when sorter
      (setq entries (sort entries sorter)))
    (unless (functionp tabulated-list-entries)
      (setq tabulated-list-entries entries))
    (when (and update (not sorter))
      (setq update nil))
    (if update
        (goto-char (point-min))
      (erase-buffer)
      (unless tabulated-list-use-header-line
        (tabulated-list-print-fake-header)))
    (while entries
      (let* ((gist (car entries))
             (id (cdr (assq 'id gist))))
        (and entry-id
             (equal entry-id id)
             (setq entry-id nil
                   saved-pt (point)))
        (if (or (not update)
                (eobp))
            (funcall tabulated-list-printer gist)
          (while
              (let ((local-id (tabulated-list-get-id)))
                (cond ((equal id local-id)
                       (forward-line 1)
                       nil)
                      ((or (not local-id)
                           (funcall sorter gist
                                    (list local-id
                                          (tabulated-list-get-entry))))
                       (funcall tabulated-list-printer gist)
                       nil)
                      (t t)))
            (let ((old (point)))
              (forward-line 1)
              (delete-region old (point))))))
      (setq entries (cdr entries)))
    (when update
      (delete-region (point)
                     (point-max)))
    (set-buffer-modified-p nil)
    (if saved-pt
        (igist-restore-position saved-pt
                                saved-col
                                saved-offset)
      (goto-char (point-min)))))

(defun igist-tabulated-list-sort (&optional n)
  "Sort Tabulated List entries by the column at point.
With a numeric prefix argument N, sort the Nth column.

If the numeric prefix is -1, restore order the list was
originally displayed in."
  (interactive "P")
  (when (and n
             (or (>= n (length tabulated-list-format))
                 (< n -1)))
    (user-error "Invalid column number"))
  (if (equal n -1)
      ;; Restore original order.
      (progn
        (unless tabulated-list--original-order
          (error "Order is already in original order"))
        (setq tabulated-list-entries
              (sort tabulated-list-entries
                    (lambda (e1 e2)
                      (< (gethash e1 tabulated-list--original-order)
                         (gethash e2 tabulated-list--original-order)))))
        (setq tabulated-list-sort-key nil)
        (igist-tabulated-list-init-header)
        (igist-tabulated-list-print t))
    ;; Sort based on a column name.
    (let ((name (if n
        (car (aref tabulated-list-format n))
      (get-text-property (point)
             'tabulated-list-column-name))))
      (if (nth 2 (assoc name (append tabulated-list-format nil)))
          (igist-tabulated-list--sort-by-column-name name)
        (user-error "Cannot sort by %s" name)))))


(defun igist-tabulated-list--sort-by-column-name (name)
  "Sort the tabulated list by the specified column NAME.
Argument NAME is the name of the column to sort by."
  (when (and name (derived-mode-p 'tabulated-list-mode))
    (unless tabulated-list--original-order
    ;; Store the original order so that we can restore it later.
      (setq tabulated-list--original-order (make-hash-table))
      (cl-loop for elem in tabulated-list-entries
               for i from 0
               do (setf (gethash elem tabulated-list--original-order) i)))
               ;; Flip the sort order on a second click.
    (if (equal name (car tabulated-list-sort-key))
        (setcdr tabulated-list-sort-key
                (not (cdr tabulated-list-sort-key)))
      (setq tabulated-list-sort-key (cons name nil)))
    (igist-tabulated-list-init-header)
    (igist-tabulated-list-print t)))

(defun igist-tabulated-column-at-point ()
  "Get tabulated column name at point."
  (get-text-property (point) 'tabulated-list-column-name))

(defun igist-tabulated-list-goto-column (column-name)
  "Go to specified column in tabulated list.

Argument COLUMN-NAME is the name of the column to which the function will
navigate in a tabulated list."
  (unless (equal column-name
                 (igist-tabulated-column-at-point))
    (pcase-let ((`(,beg . ,end)
                 (igist-property-boundaries 'tabulated-list-id
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
                          (point) 'tabulated-list-column-name
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
                          (mapcar #'car
                                  tabulated-list-format))))
  (igist-tabulated-list-goto-column column-name))

(defvar-local igist-table-current-column nil
  "Name of the column to edit in `igist-table-menu'.")

(defun igist-table-init-current-column ()
  "Initialize and set value for `igist-table-current-column'."
  (setq igist-table-current-column
        (or
         (if (member igist-table-current-column (igist-get-row-columns-at-point))
             igist-table-current-column
           (get-text-property (point) 'tabulated-list-column-name))
         (car (igist-get-row-columns-at-point)))))

(defun igist-tabulated-forward-column (&optional arg)
  "Go to the start of the next column after point on the current line.
If ARG is provided, move that many columns."
  (unless arg (setq arg 1))
  (pcase-let* ((`(,beg . ,end)
                (igist-property-boundaries 'tabulated-list-id
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
                      (point) 'tabulated-list-column-name
                      nil limit)))
        (when next
          (goto-char next))))))

(defun igist-get-all-cols ()
  "Extract all column names from a tabulated list format."
  (let* ((child-cols (mapcar #'car
                             (plist-get
                              (nthcdr 3
                                      (seq-find
                                       (lambda (it)
                                         (plist-get (nthcdr
                                                     3 it)
                                                    :children))
                                       tabulated-list-format))
                              :children))))
    (append (mapcar #'car tabulated-list-format)
            child-cols)))

(defun igist-list-render (gists)
  "Render list of GISTS."
  (setq-local mode-name (format "Gists[%d]" (length gists)))
  (if tabulated-list-entries
      (progn (setq tabulated-list-entries gists)
             (igist-tabulated-list-print t))
    (setq tabulated-list-entries gists)
    (igist-tabulated-list-print)))

(defun igist--run-in-buffer (buffer timer-sym fn &rest args)
  "Run a function FN in a BUFFER and cancel timer TIMER-SYM.

Argument TIMER-SYM is a symbol that represents a timer.
Argument BUFFER is the buffer in which the function/macro will be executed.
Argument FN is the function or macro that will be executed.
Argument ARGS is a list of additional arguments that will be passed to the FN."
  (when (and buffer (buffer-live-p buffer))
    (with-current-buffer buffer
      (if (and (get-buffer-window buffer)
               (not (eq (selected-window)
                        (get-buffer-window buffer))))
          (with-selected-window (get-buffer-window buffer)
            (apply fn args))
        (apply fn args))
      (when-let ((timer-value (symbol-value timer-sym)))
        (when (timerp timer-value)
          (cancel-timer timer-value))))))

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
  (when-let ((timer-value (symbol-value timer-sym)))
    (when (timerp timer-value)
      (cancel-timer timer-value)))
  (set timer-sym (apply #'run-with-timer delay nil
                        #'igist--run-in-buffer
                        (current-buffer)
                        timer-sym
                        fn
                        args)))

(defun igist-debounce-revert ()
  "Update tabulated entries in the current buffer and reschedule update timer."
  (igist-tabulated-list-print t)
  (igist-tabulated-list-init-header))

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
                            (igist-load-logged-user-gists))))

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
  (if-let ((status (seq-find #'numberp value)))
      (let ((msg (igist-alist-get 'igist-message (car (last value)))))
        (igist-message "Igist: request failed with %s status%s"
                       status
                       (if msg (concat ":\s" msg) "")))
    (igist-message "Igist: request error: %s" value)))

(defun igist-star-gist ()
  "Star currently viewing gist or gist at point."
  (interactive)
  (if-let ((id (or (igist-alist-get 'id igist-current-gist)
                   (tabulated-list-get-id))))
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
                   (tabulated-list-get-id))))
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
                   (tabulated-list-get-id))))
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
                             (concat id "-" new-filename))
                            (setq buffer-file-name (concat
                                                    (temporary-file-directory)
                                                    (buffer-name)))
                            (set-auto-mode)
                            (font-lock-ensure)
                            (igist-edit-mode))
                          (igist-setup-local-vars new-gist new-filename)
                          (set-buffer-modified-p nil)
                          (igist-load-logged-user-gists)
                          (when (memq igist-enable-copy-gist-url-p
                                      '(t after-update))
                            (when-let ((url (igist-get-current-gist-url)))
                              (kill-new url)
                              (igist-message "Copied %s" url)))
                          (when callback
                            (funcall callback))))
                     (igist-message "Couldn't save gist."))))))

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
                      (when-let ((url (igist-get-current-gist-url)))
                        (kill-new url)
                        (igist-message "Copied %s" url)))
                    (igist-with-exisiting-buffer buffer
                      (if igist-list-loading
                          (igist-load-logged-user-gists)
                        (setq igist-list-response (push value
                                                        igist-list-response))
                        (igist-debounce 'igist-render-timer 0.5
                                        #'igist-list-render
                                        igist-list-response))))))))

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
Argument NEWVAL is the new value to be set for `igist-table-list-format' and
used to get the new `tabulated-list-format'."
  (dolist (buff (buffer-list))
    (when (if (eq sym 'igist-explore-format)
              (igist-explore-buffer-p buff)
            (with-current-buffer buff (derived-mode-p 'igist-list-mode)))
      (with-current-buffer buff
        (let ((loading igist-list-loading))
          (when loading
            (igist-list-cancel-load))
          (setq igist-table-list-format
                newval)
          (let ((igist-table-list-format newval))
            (setq igist-table-current-column nil)
            (setq tabulated-list-printer #'igist-tabulated-list-print-entry)
            (setq tabulated-list-format
                  (igist-get-tabulated-list-format
                   newval))
            (igist-tabulated-list-init-header)
            (igist-tabulated-list-print t)
            (when loading
              (igist-list-refresh))))))))

(defun igist--tabulated-list-revert ()
  "Revert the tabulated list to its original format in Igist."
  (setq igist-table-list-format
        (igist-get-current-list-format-sym))
  (setq tabulated-list-format
        (igist-get-tabulated-list-format
         igist-table-list-format)
        tabulated-list-padding 2)
  (igist-tabulated-list-init-header)
  (run-hooks 'tabulated-list-revert-hook)
  (igist-tabulated-list-print t))

(add-variable-watcher 'igist-list-format #'igist--revert-tabulated-buffers)
(add-variable-watcher 'igist-explore-format #'igist--revert-tabulated-buffers)

(defun igist-tabulated-list-revert (&rest _ignored)
  "The `revert-buffer-function' for `igist-list-mode'.
It runs `tabulated-list-revert-hook', then calls `igist-tabulated-list-print'."
  (interactive)
  (unless (derived-mode-p 'igist-list-mode)
    (error "The current buffer is not in Igist-list-mode"))
  (igist--tabulated-list-revert))

(defun igist-get-tabulated-list-format (list-format)
  "Convert LIST-FORMAT to tabulated.

Argument LIST-FORMAT is a list of elements representing the format of a
tabulated list, where each element consists of a key, name, width, sortable
flag, format value, and extra properties."
  (apply #'vector
         (mapcar
          (pcase-lambda (`(,_key ,name ,width ,sortable ,_fmt . ,extra-props))
            (if extra-props
                (let ((children (plist-get extra-props :children)))
                  (if children
                      (let ((pl (seq-copy extra-props)))
                        (setq pl (plist-put pl :children
                                            (igist-get-tabulated-list-format
                                             children)))
                        (append (list name width sortable) pl))
                    (append (list name width sortable) extra-props)))
              (list name width sortable)))
          list-format)))

(defun igist-update-fields-format-from-tabulated-format (list-format
                                                         tabulated-format)
  "Update fields format from tabulated format TABULATED-FORMAT.
Argument LIST-FORMAT is a field specification.
Argument TABULATED-FORMAT is a variable that represents the format of a
tabulated list, which is used to display data in a table-like format in Emacs."
  (seq-map-indexed
   (pcase-lambda (`(,key ,name ,_old-width ,sortable ,format-val .
                         ,extra-props)
                  i)
     (let ((spec (aref tabulated-format i))
           (width))
       (setq width (nth 1 spec))
       (if extra-props
           (let ((children (plist-get extra-props :children)))
             (if children
                 (let ((pl (seq-copy extra-props)))
                   (setq pl (plist-put
                             pl :children
                             (igist-update-fields-format-from-tabulated-format
                              children
                              (plist-get (nthcdr 3 spec) :children))))
                   (append (list key name width sortable format-val) pl))
               (append (list key name width sortable format-val) extra-props)))
         (list key name width sortable format-val))))
   list-format))

(define-derived-mode igist-list-mode tabulated-list-mode "Gists"
  "Major mode for browsing gists.
\\<igist-list-mode-map>
\\{igist-list-mode-map}"
  (setq igist-table-list-format
        (if (igist-explore-buffer-p (current-buffer))
            igist-explore-format
          igist-list-format))
  (setq tabulated-list-format
        (igist-get-tabulated-list-format
         igist-table-list-format)
        tabulated-list-padding 2)
  (igist-tabulated-list-init-header)
  (setq tabulated-list-printer #'igist-tabulated-list-print-entry)
  (setq-local imenu-prev-index-position-function
              #'igist-imenu-prev-index-position)
  (setq-local imenu-extract-index-name-function
              #'igist-imenu-extract-index-name)
  (font-lock-add-keywords nil '(("#[^[:space:]]*" . 'font-lock-keyword-face)))
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
                             (igist--get-time (igist-alist-get 'updated_at
                                                               comment-alist))))
        (author (alist-get 'login (alist-get 'user comment-alist))))
    (propertize
     (format "## **%s** commented on %s\n\n%s" author updated comment)
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
                 (igist-render-comments-list val gist-id)
                 (let ((buff-name (concat "*" gist-id "-comments*")))
                   (igist-with-exisiting-buffer
                       buff-name
                     (igist-spinner-stop)))))))

(defun igist-property-boundaries (prop &optional pos)
  "Return property boundaries for PROP at POS."
  (unless pos (setq pos (point)))
  (goto-char pos)
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

(defun igist-overlay-prompt-region (beg end face fn &rest args)
  "Highlight region from BEG to END with FACE while invoking FN with ARGS."
  (let ((overlay (make-overlay beg end)))
    (unwind-protect
        (progn (overlay-put overlay 'face face)
               (apply fn args))
      (delete-overlay overlay))))

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
   (when (eq major-mode 'igist-list-mode)
     (tabulated-list-get-id))
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
      (when (if-let ((bounds (igist-get-comment-bounds t)))
                (igist-overlay-prompt-region (car bounds)
                                             (cdr bounds)
                                             'error
                                             'yes-or-no-p "Delete comment?")
              (yes-or-no-p "Delete comment?"))
        (igist-delete (format "/gists/%s/comments/%s" gist-id comment-id)
                      nil
                      :callback (lambda (&rest _)
                                  (igist-with-exisiting-buffer
                                      (concat "*" gist-id
                                              "-comments*")
                                    (setq igist-comment-gist-id gist-id)
                                    (igist-load-comments))
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
  (dolist (buff (igist-get-all-edit-buffers))
    (when (buffer-live-p buff)
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
    (funcall
     (igist-compose number-to-string min)
     estimed-gists-count
     100)))

(defun igist-list-cancel-load ()
  "Cancel loading of gists list."
  (interactive)
  (setq igist-list-cancelled t)
  (igist-spinner-stop))

(defun igist-list-refresh ()
  "Refresh gists in the current `igist-list-mode' buffer."
  (interactive)
  (if (igist-explore-buffer-p (current-buffer))
      (igist-explore-public-gists)
    (when-let ((owner (igist-get-owner
                       (car igist-list-response))))
      (igist-list-load-gists owner))))

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
                   igist-list-loading nil
                   igist-list-cancelled nil
                   igist-list-loading nil)
             (when (functionp cancel-fn)
               (funcall cancel-fn))
             (igist-cancel-timer 'igist-render-timer)
             (igist-list-render igist-list-response)))
          ((not (ghub-continue req))
           (with-current-buffer buffer
             (setq igist-list-response value
                   igist-list-loading nil
                   igist-list-cancelled nil)
             (igist-cancel-timer 'igist-render-timer)
             (igist-list-render value)
             (when callback callback-args
                   (apply callback callback-args))
             (igist-spinner-stop))
           (igist-sync-gists-lists
            igist-list-response))
          ((>= (length value)
               (1- (length
                    (buffer-local-value 'tabulated-list-entries buffer))))
           (with-current-buffer buffer
             (setq igist-list-response value
                   igist-list-loading t)
             (when (get-buffer-window buffer)
               (igist-debounce 'igist-render-timer 0.5
                               #'igist-list-render igist-list-response))))
          (t (with-current-buffer
                 (setq igist-list-loading t
                       igist-list-page
                       (1+ (or igist-list-page 0))))))))

(defun igist-list-load-gists (user &optional background callback callback-args)
  "List USER's gists sorted by most recently updated to least recently updated.

Then execute CALLBACK with CALLBACK-ARGS.
To stop or pause loading use command `igist-list-cancel-load'.

If BACKGROUND is nil, don't show user's buffer."
  (igist-list-request
   (concat "/users/" user "/gists") user background callback callback-args))

(defun igist-load-logged-user-gists (&optional cb &rest args)
  "Load gists asynchronously with callback CB and ARGS."
  (while (not (igist-get-current-user-name))
    (setq igist-current-user-name (read-string "User: ")))
  (igist-list-load-gists igist-current-user-name
                         t
                         cb args))

(defun igist-list-request (url user &optional background callback callback-args)
  "Request URL to list USER's gists with pagination.

Then execute CALLBACK with CALLBACK-ARGS.

To stop or pause loading use command `igist-list-cancel-load'.

If BACKGROUND is nil, don't show user's buffer."
  (let ((buffer (get-buffer-create
                 (if user
                     (igist-get-user-buffer-name user)
                   "*igist-explore*"))))
    (with-current-buffer buffer
      (igist-ensure-gist-list-mode)
      (if igist-list-loading
          (progn
            (setq igist-list-cancelled
                  (lambda ()
                    (igist-list-request url user
                                        background
                                        callback
                                        callback-args))))
        (setq igist-list-loading t)
        (igist-spinner-show)
        (ghub-request "GET" url
                      nil
                      :auth (if (igist-get-current-user-name)
                                igist-auth-marker
                              'none)
                      :username (igist-get-current-user-name)
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
                        (condition-case nil
                            (igist-list-loaded-callback buffer value req
                                                        callback
                                                        callback-args)
                          (error (setq igist-list-cancelled nil)
                                 (setq igist-list-loading nil)))))
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
              (if-let* ((file (if (eq major-mode 'igist-list-mode)
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
                            ((eq major-mode 'igist-list-mode)
                             (igist-overlay-prompt-region
                              (line-beginning-position)
                              (line-end-position)
                              'font-lock-warning-face
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
        ((eq major-mode 'igist-list-mode)
         (when-let ((id (tabulated-list-get-id))
                    (bounds (igist-property-boundaries
                             'tabulated-list-id
                             (point))))
           (when (igist-overlay-prompt-region (car bounds)
                                              (cdr bounds) 'error
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
  "Read user gists in the minibuffer and open it in the edit buffer."
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
  "List public gists sorted by most recently updated to least recently updated.

Render and load up to 3000 gists with pagination.

To stop or pause loading use command `igist-list-cancel-load'.

If BACKGROUND is non-nil, don't show buffer."
  (interactive)
  (igist-list-request "/gists/public" nil
                      background))

;;;###autoload
(defun igist-list-starred ()
  "List the authenticated user's starred gists.

Then execute CALLBACK with CALLBACK-ARGS.
To stop or pause loading use command `igist-list-cancel-load'.

If BACKGROUND is nil, don't show user's buffer."
  (interactive)
  (while (not (igist-get-current-user-name))
    (setq igist-current-user-name (igist-change-user)))
  (igist-list-request "/gists/starred"
                      igist-current-user-name))

;;;###autoload
(defun igist-list-other-user-gists (user)
  "List the public gists of USER."
  (interactive (list (read-string "User: ")))
  (igist-list-load-gists user nil))

;;;###autoload
(defun igist-list-gists ()
  "List the authenticated user's gists and activate `igist-list-mode'.

To stop or pause loading use command `igist-list-cancel-load'.

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

(defun igist-get-row-columns-at-point ()
  "Return list of columns for the current tabulated list."
  (when (tabulated-list-get-id)
    (get-text-property (point) 'columns)))

(defun igist--transient-switch-column ()
  "Switch to the next column in a transient menu."
  (interactive)
  (let* ((choices (igist-get-row-columns-at-point))
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
    (transient-setup #'igist-table-menu)
    next-choice))

(defun igist-table-current-column-spec ()
  "Find the current column specification in the table."
  (igist-find-column-spec igist-table-current-column))

(defun igist-find-column-spec (column-name)
  "Find the column specification for a given COLUMN-NAME.

Argument COLUMN-NAME is the name of the column that the function/macro
`igist-find-column-spec' is searching for."
  (or (seq-find (igist-compose
                 (apply-partially #'string=
                                  column-name)
                 car)
                tabulated-list-format)
      (when-let ((subcols
                  (plist-get
                   (nthcdr 3
                           (seq-find
                            (igist-compose
                             (apply-partially #'seq-find
                                              (igist-compose
                                               (apply-partially
                                                #'string=
                                                column-name)
                                               car))
                             (igist-rpartial plist-get :children)
                             (apply-partially #'nthcdr 3))
                            tabulated-list-format))
                   :children)))
        (seq-find (igist-compose
                   (apply-partially #'string=
                                    column-name)
                   car)
                  subcols))))

(defun igist-get-prev-column-if-last (column-name)
  "Get previous column is COLUMN-NAME is last column.

Argument COLUMN-NAME is the name of the column for which the previous column is
to be retrieved."
  (let* ((rows (igist-seq-split (igist-get-all-cols)
                                (length tabulated-list-format)))
         (found (seq-find
                 (igist-compose (apply-partially #'string= column-name)
                                car last)
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
    (setf (cadr spec)
          (max 1 (+ (cadr spec) step)))))

(defun igist-tabulated-list-widen-current-column (&optional n)
  "Widen or narrow the current column in a tabulated list.

Argument N is an optional argument that specifies the number of columns to
widen."
  (interactive "p")
  (unless n (setq n 1))
  (let ((col (igist-tabulated-column-at-point))
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
                 (igist-tabulated-forward-column (- saved-idx curr-idx))
               (igist-tabulated-list-goto-column igist-table-current-column))))
          (t (setq igist-table-current-column
                   (igist-tabulated-column-at-point))))
    (when igist-table-current-column
      (igist-table-inc-column-width n)
      (igist-debounce-revert)
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
  (igist-debounce-revert))

(defun igist-table-narrow-current-column ()
  "Narrow current column in table."
  (interactive)
  (igist-table-inc-column-width -1)
  (igist-debounce-revert))

(defun igist-table-update-current-column-width ()
  "Read and update the current column width value in the minibuffer."
  (interactive)
  (when-let ((spec (igist-table-current-column-spec)))
    (setf (cadr spec)
          (max 1 (+ (cadr spec)
                    (read-number "Width: " (or (cadr spec) 1)))))
    (igist-debounce-revert)))

(defun igist-save-column-settings ()
  "Save column settings for igist explore or igist list format."
  (interactive)
  (let* ((sym (igist-get-current-list-format-sym))
         (spec (symbol-value sym))
         (new-value (igist-update-fields-format-from-tabulated-format
                     spec
                     tabulated-list-format)))
    (customize-save-variable sym new-value)
    new-value))

(defun igist-reset-columns-settings ()
  "Reset the column settings of the current igist buffer."
  (interactive)
  (let ((sym (igist-get-current-list-format-sym)))
    (customize-set-variable
     sym
     (eval (car (get sym 'standard-value))))))

(defun igist-list-remove-column (column-name)
  "Hide a specified COLUMN-NAME in the current list or explore buffer."
  (interactive
   (list
    (let* ((sym
            (igist-get-current-list-format-sym))
           (col-at-point (igist-tabulated-column-at-point))
           (cols (igist-pluck-columns-names-from-list-format
                  (symbol-value sym))))
      (completing-read
       "Remove column: " cols
       nil t
       (and (member
             col-at-point cols)
            col-at-point)))))
  (let ((sym (igist-get-current-list-format-sym)))
    (customize-set-variable sym
                            (igist-remove-column-from-list-format
                             column-name
                             (igist-update-fields-format-from-tabulated-format
                              (symbol-value
                               sym)
                              tabulated-list-format)))))

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
  (let* ((col-name (or (igist-tabulated-column-at-point)
                       (save-excursion
                         (skip-chars-forward "\s\t")
                         (igist-tabulated-column-at-point))
                       (and (eolp)
                            (save-excursion
                              (forward-char -1)
                              (igist-tabulated-column-at-point)))))
         (sym (igist-get-current-list-format-sym))
         (sym-value (symbol-value sym))
         (cols (mapcar #'cadr sym-value))
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
               sym-value)
            (and pos next-pos
                 (igist-swap pos next-pos sym-value)))))
    (when next-value
      (customize-set-variable sym
                              next-value)
      (igist-goto-column col-name)
      (setq igist-table-current-column col-name)
      (when transient-current-command
        (transient-setup transient-current-command)))))

;;;###autoload (autoload 'igist-table-menu "igist" nil t)
(transient-define-prefix igist-table-menu ()
  "A menu for editing, saving, and adjusting column settings in igist table."
  ["Edit column"
   ("c" igist--transient-switch-column :description
    (lambda ()
      (concat
       "Column "
       (propertize "[" 'face
                   'transient-inactive-value)
       (mapconcat
        (lambda (choice)
          (propertize choice
                      'face
                      (if
                          (and
                           igist-table-current-column
                           (string=
                            choice
                            igist-table-current-column))
                          'transient-value
                        'transient-inactive-value)))
        (igist-get-row-columns-at-point)
        (propertize "|" 'face
                    'transient-inactive-value))
       (propertize "]" 'face
                   'transient-inactive-value))))]
  [("w" igist-table-update-current-column-width
    :description (lambda ()
                   (format "Width: (%s)"
                           (cadr
                            (igist-table-current-column-spec)))))
   ("<right>" igist-table-widen-current-column
    :description (lambda
                   ()
                   (format "Increase %s width (%s)"
                           (or igist-table-current-column "")
                           (cadr
                            (igist-table-current-column-spec))))
    :transient t)
   ("<left>" igist-table-narrow-current-column
    :description (lambda
                   ()
                   (format "Decrease %s width (%s)"
                           (or igist-table-current-column "")
                           (cadr
                            (igist-table-current-column-spec))))
    :transient t)
   ("M-<left>" "Move column backward" igist-swap-current-column-backward
    :transient nil)
   ("M-<right>" "Move column forward" igist-swap-current-column
    :transient nil)
   ("r" "Remove column" igist-list-remove-column)]
  ["Settings"
   ("R" "Reset" igist-reset-columns-settings)
   ("S" "Save"
    igist-save-column-settings
    :transient t)]
  (interactive)
  (igist-table-init-current-column)
  (transient-setup #'igist-table-menu))

(defun igist-set-current-user ()
  "Read user name and assign it in the variable `igist-current-user-name'."
  (interactive)
  (customize-set-variable
   'igist-current-user-name
   (igist-change-user "GitHub user name:  "
                      igist-current-user-name))
  (when transient-current-command
    (transient-setup transient-current-command)))

;;;###autoload (autoload 'igist-dispatch "igist" nil t)
(transient-define-prefix igist-dispatch ()
  "Transient menu for gists."
  :transient-non-suffix #'transient--do-stay
  [[:if-mode
    igist-list-mode
    "Gist at point"
    ("RET" "Edit" igist-list-edit-gist-at-point :inapt-if-not
     tabulated-list-get-id)
    ("v" "View" igist-list-view-current :inapt-if-not tabulated-list-get-id)
    ("f" "Fork" igist-fork-gist :inapt-if-not igist-forkable)
    ("w" "Copy Url" igist-copy-gist-url :inapt-if-not tabulated-list-get-id)
    ("r" "Browse" igist-browse-gist :inapt-if-not tabulated-list-get-id)
    ("S" "Star" igist-star-gist :inapt-if-not tabulated-list-get-id)
    ("U" "Unstar" igist-unstar-gist :inapt-if-not tabulated-list-get-id)
    ("D" "Delete" igist-delete-current-gist :inapt-if-not igist-editable-p)
    ("d" "Description" igist-list-edit-description :inapt-if-not
     tabulated-list-get-id)
    ("C" "Configure table view" igist-table-menu)
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
     :inapt-if-not tabulated-list-get-id
     :transient t)
    ("<backtab>" "Toggle visibility of subrows"
     igist-toggle-all-children
     :transient t)
    ("s" "Show languages statistics" igist-print-languages-chart)]
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
    ("E" "Explore" igist-explore-public-gists :inapt-if
     igist-current-buffer-explore-p)
    ("o" "Other user" igist-list-other-user-gists)
    ("g" "Refresh" igist-list-refresh :inapt-if-not-derived igist-list-mode)
    ("K" "Cancel load" igist-list-cancel-load :inapt-if-not-derived
     igist-list-mode)
    ("X" "Kill buffers" igist-kill-all-gists-buffers)]
   [:if-not-mode
    igist-list-mode
    "Create"
    ("n" "New" igist-create-new-gist :inapt-if-not igist-get-current-user-name)
    ("b" "New from buffer" igist-new-gist-from-buffer :inapt-if-not
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
  [:if-mode
   igist-list-mode
   ["Create"
    ("n" "New" igist-create-new-gist :inapt-if-not igist-get-current-user-name)
    ("b" "New from buffer" igist-new-gist-from-buffer :inapt-if-not
     igist-get-current-user-name)]
   ["Files"
    ("+" "Add" igist-list-add-file :inapt-if-not igist-editable-p)
    ("-" "Delete" igist-delete-current-filename :inapt-if-not igist-editable-p)]
   ["Comments"
    ("a" "Add" igist-add-comment :inapt-if-not tabulated-list-get-id)
    ("c" "Show" igist-load-comments :inapt-if-not tabulated-list-get-id)]]
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