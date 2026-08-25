;;; igist-test.el --- Test Suite for GPTAI -*- lexical-binding: t; -*-

;; Copyright (C) Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>

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

;; A suite of tests for the igist functions to ensure they work as
;; expected.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'igist)

(defun igist-test--get-gist-stub ()
  "Return a stub of GitHub Gist data for testing purposes."
  '((url . "https://api.github.com/gists/7c71af123452ff7b2360cece9108a6f4")
    (forks_url . "https://api.github.com/gists/7c71af123452ff7b2360cece9108a6f4/forks")
    (commits_url . "https://api.github.com/gists/7c71af123452ff7b2360cece9108a6f4/commits")
    (id . "7c71af123452ff7b2360cece9108a6f4")
    (node_id . "G_kwDOAXx-BNoAIDdjNzFhZjEyMzQ1MmZmN2IyMzYwY2VjZTkxMDhhNmY0")
    (git_pull_url . "https://gist.github.com/7c71af123452ff7b2360cece9108a6f4.git")
    (git_push_url . "https://gist.github.com/7c71af123452ff7b2360cece9108a6f4.git")
    (html_url . "https://gist.github.com/KarimAziev/7c71af123452ff7b2360cece9108a6f4")
    (files
     (LICENSE
      (filename . "LICENSE")
      (type . "text/plain")
      (language . "Text")
      (raw_url . "https://gist.githubusercontent.com/KarimAziev/7c71af123452ff7b2360cece9108a6f4/raw/f288702d2fa16d3cdf0035b15a9fcbc552cd88e7/LICENSE")
      (size . 35149))
     (README.md
      (filename . "README.md")
      (type . "text/markdown")
      (language . "Markdown")
      (raw_url . "https://gist.githubusercontent.com/KarimAziev/7c71af123452ff7b2360cece9108a6f4/raw/0fc80463e75190ad915a08384cb496ce6ba7c015/README.md")
      (size . 12552))
     (README.org
      (filename . "README.org")
      (type . "application/vnd.lotus-organizer")
      (language . "Org")
      (raw_url . "https://gist.githubusercontent.com/KarimAziev/7c71af123452ff7b2360cece9108a6f4/raw/4aa4f32f5bd2402950f7b5355bcccd0353c5ca44/README.org")
      (size . 12901)))
    (public)
    (created_at . "2023-08-16T08:09:59Z")
    (updated_at . "2023-08-16T08:10:00Z")
    (description . "readmes")
    (comments . 0)
    (user)
    (comments_url . "https://api.github.com/gists/7c71af123452ff7b2360cece9108a6f4/comments")
    (owner
     (login . "KarimAziev")
     (id . 24935940)
     (node_id . "MDQ6VXNlcjI0OTM1OTQw")
     (avatar_url . "https://avatars.githubusercontent.com/u/24935940?v=4")
     (gravatar_id . "")
     (url . "https://api.github.com/users/KarimAziev")
     (html_url . "https://github.com/KarimAziev")
     (followers_url . "https://api.github.com/users/KarimAziev/followers")
     (following_url . "https://api.github.com/users/KarimAziev/following{/other_user}")
     (gists_url . "https://api.github.com/users/KarimAziev/gists{/gist_id}")
     (starred_url . "https://api.github.com/users/KarimAziev/starred{/owner}{/repo}")
     (subscriptions_url . "https://api.github.com/users/KarimAziev/subscriptions")
     (organizations_url . "https://api.github.com/users/KarimAziev/orgs")
     (repos_url . "https://api.github.com/users/KarimAziev/repos")
     (events_url . "https://api.github.com/users/KarimAziev/events{/privacy}")
     (received_events_url . "https://api.github.com/users/KarimAziev/received_events")
     (type . "User")
     (site_admin))
    (truncated)))

(ert-deftest igist-test-provider-github-descriptor ()
  "Test the initial GitHub provider descriptor."
  (should (equal (igist--provider-get :label 'github) "GitHub"))
  (should (equal (igist--provider-get :host 'github) "api.github.com"))
  (should (eq (igist--provider-operation :list 'github)
              'igist--github-list-request))
  (should (eq (igist--provider-operation :create 'github)
              'igist--github-create-request))
  (should (eq (igist--provider-operation :delete-item 'github)
              'igist--github-delete-item-request))
  (should (eq (igist--provider-operation :read-content 'github)
              'igist--github-read-content))
  (should (eq (igist--provider-operation :update 'github)
              'igist--github-update-request))
  (should (igist--provider-capable-p 'read 'github))
  (should (igist--provider-capable-p 'star 'github))
  (should (igist--provider-capable-p 'other-users 'github))
  (should-not (igist--provider-capable-p 'unknown 'github)))

(ert-deftest igist-test-provider-gitlab-descriptor ()
  "Test the initial read-only GitLab provider descriptor."
  (let ((igist-gitlab-host "gitlab.example/api/v4"))
    (should (equal (igist--provider-get :label 'gitlab) "GitLab"))
    (should (equal (igist--provider-host 'gitlab)
                   "gitlab.example/api/v4"))
    (should (eq (igist--provider-operation :list 'gitlab)
                'igist--gitlab-list-request))
    (should (eq (igist--provider-operation :create 'gitlab)
                'igist--gitlab-create-request))
    (should (eq (igist--provider-operation :delete-file 'gitlab)
                'igist--gitlab-delete-file-request))
    (should (eq (igist--provider-operation :delete-item 'gitlab)
                'igist--gitlab-delete-item-request))
    (should (eq (igist--provider-operation :get-item 'gitlab)
                'igist--gitlab-get-item-request))
    (should (eq (igist--provider-operation :read-content 'gitlab)
                'igist--gitlab-read-content))
    (should (eq (igist--provider-operation :update 'gitlab)
                'igist--gitlab-update-request))
    (should (eq (igist--provider-operation :update-description 'gitlab)
                'igist--gitlab-update-description-request))
    (should (igist--provider-capable-p 'read 'gitlab))
    (should (igist--provider-capable-p 'create 'gitlab))
    (should (igist--provider-capable-p 'delete 'gitlab))
    (should (igist--provider-capable-p 'edit 'gitlab))
    (should (igist--provider-capable-p 'multi-file 'gitlab))
    (should (igist--provider-capable-p 'public-list 'gitlab))
    (should-not (igist--provider-capable-p 'star 'gitlab))))

(ert-deftest igist-test-provider-item-names-follow-context ()
  "Test selected and buffer item names are provider-aware."
  (let ((igist-forge 'gitlab))
    (should (equal (igist--selected-item-name) "snippet"))
    (should (equal (igist--selected-item-name t) "snippets"))
    (with-temp-buffer
      (setq-local igist--context '(:forge github :host "api.github.com"))
      (should (equal (igist--current-item-name) "gist"))
      (should (equal (igist--current-item-name t) "gists")))))

(ert-deftest igist-test-current-forge-precedence ()
  "Test object, buffer and user-option forge precedence."
  (let ((igist-forge 'github))
    (with-temp-buffer
      (should (eq (igist--current-forge) 'github))
      (setq-local igist--context
                  '(:forge gitlab :host "gitlab.example/api/v4"))
      (should (eq (igist--current-forge) 'gitlab))
      (should (eq (igist--current-forge '((forge . github)))
                  'github)))))

(ert-deftest igist-test-current-context-precedence ()
  "Test provider context precedence and host derivation."
  (let ((igist-forge 'github))
    (with-temp-buffer
      (setq-local igist--context
                  '(:forge gitlab :host "gitlab.example/api/v4"))
      (should (equal (igist--current-context)
                     '(:forge gitlab :host "gitlab.example/api/v4")))
      (should (equal (igist--current-context
                      '((forge . github)))
                     '(:forge github :host "api.github.com")))
      (should (equal (igist--current-context
                      '((forge . gitlab)
                        (host . "other.example/api/v4")))
                     '(:forge gitlab :host "other.example/api/v4"))))))

(ert-deftest igist-test-buffer-context-is-permanent-local ()
  "Test that changing a major mode does not discard provider context."
  (with-temp-buffer
    (igist--set-buffer-context
     '(:forge gitlab :host "gitlab.example/api/v4"))
    (fundamental-mode)
    (should (equal igist--context
                   '(:forge gitlab :host "gitlab.example/api/v4")))))

(ert-deftest igist-test-provider-lookup-errors-for-unknown-forge ()
  "Test that selecting an unregistered forge signals an error."
  (should-error (igist--provider 'unknown)
                :type 'error))

(ert-deftest igist-test-provider-operation-errors-when-unsupported ()
  "Test that an unsupported provider operation signals a useful error."
  (should-error (igist--provider-operation :unknown 'github)
                :type 'error))

(ert-deftest igist-test-require-provider-capability ()
  "Test command-level provider capability enforcement."
  (should (igist--require-provider-capability 'star 'github))
  (should-error (igist--require-provider-capability 'star 'gitlab)
                :type 'user-error))

(ert-deftest igist-test-provider-call-dispatches-operation ()
  "Test operation dispatch through a plist provider descriptor."
  (let ((igist--providers
         '((test
            :host "forge.example/api"
            :operations (:echo igist-test--echo)))))
    (cl-letf (((symbol-function 'igist-test--echo)
               (lambda (&rest arguments) arguments)))
      (should (equal (igist--provider-call :echo 'test 1 "two")
                     '(1 "two"))))))

(ert-deftest igist-test-provider-create-item-contract ()
  "Test provider-neutral creation validates and dispatches file data."
  (let ((igist--providers
         '((test
            :host "forge.example/api"
            :operations (:create igist-test--create)
            :capabilities (create))))
        arguments)
    (cl-letf (((symbol-function 'igist-test--create)
               (lambda (&rest args)
                 (setq arguments args)
                 'created)))
      (let ((files '(((filename . "one.el")
                      (content . "one")))))
        (should (eq (igist--provider-create-item
                     'test files "Example" t nil #'ignore)
                    'created))
        (should (equal arguments
                       (list files "Example" t nil #'ignore))))
      (should-error
       (igist--provider-create-item 'test nil "Empty" nil)
       :type 'user-error))))

(ert-deftest igist-test-provider-create-payloads ()
  "Test provider payload conversion from canonical create files."
  (let ((files '(((filename . "one.el") (content . "one"))
                 ((filename . "dir/two.org") (content . "two")))))
    (should
     (equal
      (igist--github-create-payload files "Example" t)
      '((description . "Example")
        (public . t)
        (files
         (one\.el (content . "one"))
         (dir/two\.org (content . "two"))))))
    (should
     (equal
      (igist--gitlab-create-payload files "Example" nil)
      '((title . "Example")
        (description . "Example")
        (visibility . "private")
        (files
         . [((file_path . "one.el") (content . "one"))
            ((file_path . "dir/two.org") (content . "two"))]))))
    (should
     (equal (igist-alist-get-symb
             'title (igist--gitlab-create-payload files "" t))
            "one.el"))))

(ert-deftest igist-test-provider-update-payloads ()
  "Test update payloads encode provider-specific file semantics."
  (let* ((object
          '((visibility . "internal")
            (files
             ((filename . "one.el"))
             ((filename . "two.org")))))
         (update
          (igist--gitlab-update-payload
           object "one.el" "one.el" "Description" "updated" nil))
         (move
          (igist--gitlab-update-payload
           object "one.el" "renamed.el" "Description" "moved" t))
         (create
          (igist--gitlab-update-payload
           object "new.el" "new.el" "Description" "new" nil)))
    (should
     (equal
      (igist--github-update-payload
       "one.el" "renamed.el" "Description" "updated")
      '((description . "Description")
        (files
         (one\.el
          (filename . "renamed.el")
          (content . "updated"))))))
    (should (equal (igist-alist-get-symb 'visibility update) "internal"))
    (should
     (equal
      (aref (igist-alist-get-symb 'files update) 0)
      '((action . "update")
        (file_path . "one.el")
        (content . "updated"))))
    (should (equal (igist-alist-get-symb 'visibility move) "public"))
    (should
     (equal
      (aref (igist-alist-get-symb 'files move) 0)
      '((action . "move")
        (previous_path . "one.el")
        (file_path . "renamed.el")
        (content . "moved"))))
    (should
     (equal
      (aref (igist-alist-get-symb 'files create) 0)
      '((action . "create")
        (file_path . "new.el")
        (content . "new"))))
    (should
     (equal (igist--github-delete-file-payload "one.el")
            '((files (one\.el (content . ""))))))
    (should
     (equal (igist--gitlab-delete-file-payload "one.el")
            '((files . [((action . "delete")
                         (file_path . "one.el"))]))))))

(ert-deftest igist-test-id-buffer-lookup-respects-provider-context ()
  "Test equal item IDs do not cross provider buffer boundaries."
  (let ((github-buffer (generate-new-buffer " *igist-github-id-test*"))
        (gitlab-buffer (generate-new-buffer " *igist-gitlab-id-test*"))
        (github-context '(:forge github :host "api.github.com"))
        (gitlab-context '(:forge gitlab :host "gitlab.example/api/v4")))
    (unwind-protect
        (progn
          (with-current-buffer github-buffer
            (setq-local igist-current-gist '((id . "42")))
            (setq-local igist--context github-context))
          (with-current-buffer gitlab-buffer
            (setq-local igist-current-gist '((id . "42")))
            (setq-local igist--context gitlab-context))
          (should
           (equal (igist-get-gist-buffers-by-id "42" github-context)
                  (list github-buffer)))
          (should
           (equal (igist-get-gist-buffers-by-id "42" gitlab-context)
                  (list gitlab-buffer))))
      (dolist (buffer (list github-buffer gitlab-buffer))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest igist-test-editable-p-uses-item-provider-user ()
  "Test editability compares owners using provider-specific users."
  (let ((igist-current-user-name "github-user")
        (igist-gitlab-current-user-name "gitlab-user"))
    (should
     (igist-editable-p
      '((forge . gitlab)
        (host . "gitlab.example/api/v4")
        (owner (login . "gitlab-user")))))
    (should-not
     (igist-editable-p
      '((forge . gitlab)
        (host . "gitlab.example/api/v4")
        (owner (login . "github-user")))))))

(ert-deftest igist-test-save-new-item-uses-buffer-provider ()
  "Test saving a new buffer delegates creation using its provider context."
  (let ((buffer (generate-new-buffer " *igist-provider-create-test*"))
        create-arguments loaded-forge callback-called)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (insert "content")
            (setq-local igist--context
                        '(:forge gitlab :host "gitlab.example/api/v4"))
            (setq-local igist-current-gist nil)
            (setq-local igist-current-filename "one.el")
            (setq-local igist-current-description "Example")
            (setq-local igist-current-public nil))
          (cl-letf
              (((symbol-function 'igist--provider-create-item)
                (lambda (forge files description public request-buffer callback)
                  (setq create-arguments
                        (list forge files description public request-buffer))
                  (funcall
                   callback
                   '((forge . gitlab)
                     (host . "gitlab.example/api/v4")
                     (id . "42")
                     (description . "Example")
                     (visibility . "private")
                     (owner (login . "gitlab-user"))
                     (files
                      (one\.el
                       (filename . "one.el")
                       (raw_url . "https://gitlab.example/raw")))))))
               ((symbol-function 'igist--load-provider-user-items)
                (lambda (forge) (setq loaded-forge forge))))
            (igist-save-new-gist
             buffer (lambda () (setq callback-called t))))
          (should (eq (car create-arguments) 'gitlab))
          (should
           (equal (cadr create-arguments)
                  '(((filename . "one.el") (content . "content")))))
          (should (equal (nth 2 create-arguments) "Example"))
          (should-not (nth 3 create-arguments))
          (should (eq (nth 4 create-arguments) buffer))
          (should callback-called)
          (should (eq loaded-forge 'gitlab))
          (should
           (equal (buffer-local-value 'igist--context buffer)
                  '(:forge gitlab :host "gitlab.example/api/v4")))
          (should
           (equal
            (igist-alist-get-symb
             'id (buffer-local-value 'igist-current-gist buffer))
            "42")))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer)))))

(ert-deftest igist-test-save-existing-item-uses-buffer-provider ()
  "Test saving an existing buffer delegates update using its provider."
  (let ((buffer (generate-new-buffer " *igist-provider-update-test*"))
        update-arguments callback-called)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (insert "updated content")
            (setq-local igist--context
                        '(:forge gitlab :host "gitlab.example/api/v4"))
            (setq-local
             igist-current-gist
             '((forge . gitlab)
               (host . "gitlab.example/api/v4")
               (id . "42")
               (filename . "one.el")
               (description . "Old")
               (visibility . "private")
               (owner (login . "gitlab-user"))
               (files ((filename . "one.el")))))
            (setq-local igist-current-filename "renamed.el")
            (setq-local igist-current-description "Updated")
            (setq-local igist-current-public nil))
          (cl-letf
              (((symbol-function 'igist--provider-update-item)
                (lambda (forge object original-filename filename description
                               content public request-buffer callback)
                  (setq update-arguments
                        (list forge object original-filename filename
                              description content public request-buffer))
                  (funcall
                   callback
                   '((forge . gitlab)
                     (host . "gitlab.example/api/v4")
                     (id . "42")
                     (description . "Updated")
                     (visibility . "private")
                     (owner (login . "gitlab-user"))
                     (files
                      (renamed\.el
                       (filename . "renamed.el")
                       (raw_url . "https://gitlab.example/raw")))))))
               ((symbol-function 'igist--provider-user-buffer)
                (lambda (&optional _forge) nil)))
            (igist-save-existing-gist
             buffer (lambda () (setq callback-called t))))
          (should (eq (nth 0 update-arguments) 'gitlab))
          (should (equal (nth 2 update-arguments) "one.el"))
          (should (equal (nth 3 update-arguments) "renamed.el"))
          (should (equal (nth 4 update-arguments) "Updated"))
          (should (equal (nth 5 update-arguments) "updated content"))
          (should-not (nth 6 update-arguments))
          (should (eq (nth 7 update-arguments) buffer))
          (should callback-called)
          (should
           (equal
            (igist-alist-get-symb
             'filename (buffer-local-value 'igist-current-gist buffer))
            "renamed.el"))
          (should
           (equal (buffer-local-value 'igist--context buffer)
                  '(:forge gitlab :host "gitlab.example/api/v4"))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (set-buffer-modified-p nil))
        (kill-buffer buffer)))))

(ert-deftest igist-test-github-list-resource ()
  "Test semantic GitHub list scopes and the compatibility resource scope."
  (should (equal (igist--github-list-resource 'public nil)
                 "/gists/public"))
  (should (equal (igist--github-list-resource 'starred "octocat")
                 "/gists/starred"))
  (should (equal (igist--github-list-resource 'user "octocat")
                 "/users/octocat/gists"))
  (should (equal (igist--github-list-resource
                  '(:resource "/legacy/resource") nil)
                 "/legacy/resource"))
  (should-error (igist--github-list-resource 'user nil)
                :type 'error))

(ert-deftest igist-test-gitlab-list-resource ()
  "Test supported GitLab list scopes and explicit unsupported cases."
  (let ((igist-gitlab-current-user-name "octocat"))
    (should (equal (igist--gitlab-list-resource 'user "octocat")
                   "/snippets"))
    (should (equal (igist--gitlab-list-resource 'public nil)
                   "/snippets/public"))
    (should-error (igist--gitlab-list-resource 'user "another-user")
                  :type 'user-error)
    (should-error (igist--gitlab-list-resource 'starred "octocat")
                  :type 'user-error)))

(ert-deftest igist-test-gitlab-convert-snippet ()
  "Test conversion from GitLab fields to igist's existing alist shape."
  (let* ((snippet
          '((id . 42)
            (title . "Example")
            (description)
            (visibility . "public")
            (created_at . "2026-01-01T00:00:00Z")
            (updated_at . "2026-01-02T00:00:00Z")
            (web_url . "https://gitlab.example/-/snippets/42")
            (raw_url . "https://gitlab.example/-/snippets/42/raw")
            (http_url_to_repo
             . "https://gitlab.example/snippets/42.git")
            (author
             (id . 7)
             (username . "octocat")
             (avatar_url . "https://gitlab.example/avatar.png")
             (web_url . "https://gitlab.example/octocat"))
            (files
             ((path . "one.el")
              (raw_url . "https://gitlab.example/one.el/raw"))
             ((path . "two.org")
              (raw_url . "https://gitlab.example/two.org/raw")))))
         (converted
          (igist--gitlab-convert-snippet
           snippet "gitlab.example/api/v4")))
    (should (equal (igist-alist-get-symb 'id converted) "42"))
    (should (equal (igist-alist-get-symb 'description converted)
                   "Example"))
    (should (igist-alist-get-symb 'public converted))
    (should (equal (igist-get-owner converted) "octocat"))
    (should (equal (mapcar
                    (lambda (file)
                      (igist-alist-get-symb 'filename (cdr file)))
                    (igist-alist-get-symb 'files converted))
                   '("one.el" "two.org")))
    (let ((normalized (igist-normalize-gist converted)))
      (should (equal (mapcar #'car normalized)
                     '("gitlab@gitlab.example%2Fapi%2Fv4:42-one.el"
                       "gitlab@gitlab.example%2Fapi%2Fv4:42-two.org"))))))

(ert-deftest igist-test-gitlab-convert-single-file-list-entry ()
  "Test a compact GitLab list entry becomes one normalizable file."
  (let* ((snippet '((id . 9)
                    (title . "Compact")
                    (file_name . "compact.el")
                    (raw_url . "https://gitlab.example/raw")
                    (author (username . "octocat"))))
         (converted
          (igist--gitlab-convert-snippet
           snippet "gitlab.example/api/v4"))
         (file (cdar (igist-alist-get-symb 'files converted))))
    (should (equal (igist-alist-get-symb 'filename file) "compact.el"))
    (should (equal (igist-alist-get-symb 'raw_url file)
                   "https://gitlab.example/raw"))))

(ert-deftest igist-test-gitlab-content-resource ()
  "Test GitLab raw-file resources encode paths and preserve the ref."
  (should
   (equal
    (igist--gitlab-content-resource
     '((id . "42")
       (filename . "lib/example file.el")
       (ref . "feature/ref")))
    "/snippets/42/files/feature%2Fref/lib%2Fexample%20file.el/raw"))
  (should
   (equal
    (igist--gitlab-content-resource
     '((id . "42") (filename . "example.el")))
    "/snippets/42/files/main/example.el/raw"))
  (should-error (igist--gitlab-content-resource '((id . "42")))
                :type 'user-error))

(ert-deftest igist-test-provider-read-content-dispatch ()
  "Test content reading dispatches using object provider provenance."
  (let ((igist--providers
         '((test
            :host "forge.example/api"
            :operations (:read-content igist-test--read-content)
            :capabilities (read)))))
    (cl-letf (((symbol-function 'igist-test--read-content)
               (lambda (object &optional callback _errorback)
                 (let ((content
                        (concat "content:"
                                (igist-alist-get-symb 'filename object))))
                   (if callback
                       (funcall callback content)
                     content)))))
      (let ((object '((forge . test)
                      (host . "forge.example/api")
                      (filename . "example.el"))))
        (should (equal (igist--provider-read-content object)
                       "content:example.el"))
        (let (async-content)
          (igist--provider-read-content
           object (lambda (content) (setq async-content content)))
          (should (equal async-content "content:example.el")))))))

(ert-deftest igist-test-list-items-dispatches-with-buffer-context ()
  "Test generic list setup delegates transport and retains its context."
  (let* ((user "igist-provider-test-user")
         (igist--providers
          '((test
             :host "forge.example/api"
             :user-name-function igist-test--current-user
             :operations (:list igist-test--list-request))))
         (arguments nil)
         (buffer nil))
    (cl-letf (((symbol-function 'igist-test--current-user)
               (lambda () user))
              ((symbol-function 'igist-test--list-request)
               (lambda (&rest args) (setq arguments args))))
      (unwind-protect
          (progn
            (setq buffer (igist--list-items 'user user t nil nil 'test))
            (should (buffer-live-p buffer))
            (should (equal (buffer-local-value 'igist--context buffer)
                           '(:forge test :host "forge.example/api")))
            (should (eq (car arguments) 'user))
            (should (equal (cadr arguments) user))
            (should (eq (nth 2 arguments) buffer)))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(ert-deftest igist-test-provider-user-name ()
  "Test provider-specific user-name lookup."
  (let ((igist-current-user-name "octocat"))
    (should (equal (igist--provider-user-name 'github)
                   "octocat")))
  (let ((igist-current-user-name ""))
    (should-not (igist--provider-user-name 'github))))

(ert-deftest igist-test-or ()
  "Test the `igist-or'' function with different lambda functions and inputs."
  (let* ((fn1 (lambda (it)
                (if (eq it 'a) 'first nil)))
         (fn2 (lambda (it)
                (if (eq it 'a) 'second nil)))
         (or-fn (igist-or `,fn1 `,fn2)))
    (should (eq (funcall or-fn 'a) 'first))
    (should (eq (funcall or-fn 'b) nil))
    (should (eq
             (funcall (igist-or (lambda (it)
                                  (if (eq it 'a) 'first nil))
                                (lambda (it)
                                  (if (eq it 'a) 'second nil)))
                      'a)
             'first))
    (should (eq
             (funcall (igist-or (lambda (it)
                                  (if (eq it 'a) 'first nil))
                                (lambda (it)
                                  (if (eq it 'a) 'second nil)))
                      'b)
             nil))))

(ert-deftest igist-test-rpartial ()
  "Test the `igist-rpartial'."
  (let ((partial-get-key-fn (igist-rpartial plist-get :key)))
    (should (eq (funcall (igist-rpartial plist-get :key)
                         '(:key 1
                                :value "abcde"))
                1))
    (should (eq (funcall partial-get-key-fn '(:key 8
                                                   :value "abcde"))
                8))
    (should (string= (funcall (igist-rpartial plist-get :value)
                              '(:key 1
                                     :value "abcde"))
                     "abcde"))))

(ert-deftest igist-test-alist-get ()
  "Test the `igist-alist-get'' function for retrieving values from an alist."
  (let ((alist '((key1 . val1)
                 (key2 . val2))))
    (should (equal (igist-alist-get 'key1 alist) 'val1))
    (should (equal (igist-alist-get 'key2 alist) 'val2))
    (should (equal (igist-alist-get 'key3 alist) nil))))

(ert-deftest igist-test-make-gist-key ()
  "Test the function that generates a unique key for a gist."
  (let ((gist '((id . "7c71af123452ff7b2360cece9108a6f4")
                (filename . "LICENSE"))))
    (should (equal (igist-make-gist-key gist)
                   "7c71af123452ff7b2360cece9108a6f4-LICENSE"))
    (should (equal (igist-make-gist-key
                    `((id . ,"newgist")
                      (filename . "my-filename")))
                   "newgist-my-filename"))))

(ert-deftest igist-test-make-object-key-preserves-github-identity ()
  "Test that default GitHub internal identities remain byte-for-byte stable."
  (let ((object '((id . "42") (filename . "example.el"))))
    (should (equal (igist--make-object-key
                    object '(:forge github :host "api.github.com"))
                   "42-example.el"))
    (let ((igist-forge 'github))
      (with-temp-buffer
        (should (equal (igist--make-object-key object)
                       (igist-make-gist-key object)))))))

(ert-deftest igist-test-make-object-key-separates-provider-contexts ()
  "Test that forge and host prevent collisions between remote objects."
  (let* ((object '((id . "42") (filename . "example.el")))
         (gitlab-key
          (igist--make-object-key
           object '(:forge gitlab :host "gitlab.com/api/v4")))
         (self-hosted-key
          (igist--make-object-key
           object '(:forge gitlab :host "gitlab.example/api/v4"))))
    (should (equal gitlab-key
                   "gitlab@gitlab.com%2Fapi%2Fv4:42-example.el"))
    (should-not (equal gitlab-key self-hosted-key))
    (should-not (equal gitlab-key
                       (igist-make-gist-key object)))))

(ert-deftest igist-test-contextual-buffer-name-preserves-github-name ()
  "Test provider-qualified list names and exact GitHub compatibility."
  (should (equal (igist--contextual-buffer-name
                  "*igist-octocat*"
                  '(:forge github :host "api.github.com"))
                 "*igist-octocat*"))
  (should (equal (igist--contextual-buffer-name
                  "*igist-octocat*"
                  '(:forge gitlab :host "gitlab.com/api/v4"))
                 "*gitlab@gitlab.com%2Fapi%2Fv4:igist-octocat*")))

(ert-deftest igist-test-normalize-gist-uses-buffer-context-for-identity ()
  "Test normalization keys use context without changing normalized values."
  (let ((gist '((id . "42")
                (files (example.el (filename . "example.el"))))))
    (with-temp-buffer
      (igist--set-buffer-context
       '(:forge gitlab :host "gitlab.example/api/v4"))
      (let ((normalized (igist-normalize-gist gist)))
        (should (equal (caar normalized)
                       (concat
                        "gitlab@gitlab.example%2Fapi%2Fv4:"
                        "42-example.el")))
        (should (equal (cdar normalized)
                       '((id . "42")
                         (filename . "example.el")
                         (idx . 0)
                         (total . 1)
                         (files ((filename . "example.el"))))))))))

(ert-deftest igist-test-normalize-gist ()
  "Test the function that normalizes GitHub Gist data."
  (let* ((gist '((id . "7c71af123452ff7b2360cece9108a6f4")
                 (files
                  (LICENSE
                   (filename . "LICENSE"))
                  (README.md
                   (filename . "README.md"))
                  (README.org
                   (filename . "README.org")))))
         (normalized-gist (igist-normalize-gist gist)))
    (should (equal normalized-gist '(("7c71af123452ff7b2360cece9108a6f4-LICENSE"
                                      (id . "7c71af123452ff7b2360cece9108a6f4")
                                      (filename . "LICENSE")
                                      (idx . 0)
                                      (total . 3)
                                      (files
                                       ((filename . "LICENSE"))
                                       ((filename . "README.md"))
                                       ((filename . "README.org"))))
                                     ("7c71af123452ff7b2360cece9108a6f4-README.md"
                                      (id . "7c71af123452ff7b2360cece9108a6f4")
                                      (filename . "README.md")
                                      (idx . 1)
                                      (total . 3)
                                      (files
                                       ((filename . "LICENSE"))
                                       ((filename . "README.md"))
                                       ((filename . "README.org"))))
                                     ("7c71af123452ff7b2360cece9108a6f4-README.org"
                                      (id . "7c71af123452ff7b2360cece9108a6f4")
                                      (filename . "README.org")
                                      (idx . 2)
                                      (total . 3)
                                      (files
                                       ((filename . "LICENSE"))
                                       ((filename . "README.md"))
                                       ((filename . "README.org")))))))
    (should (equal (cdr (assoc (igist-make-gist-key '((id . "7c71af123452ff7b2360cece9108a6f4")
                                                      (filename . "README.md")))
                               normalized-gist))
                   '((id . "7c71af123452ff7b2360cece9108a6f4")
                     (filename . "README.md")
                     (idx . 1)
                     (total . 3)
                     (files ((filename . "LICENSE"))
                            ((filename . "README.md"))
                            ((filename . "README.org"))))))
    (should (equal (cdr (assoc (igist-make-gist-key '((id . "7c71af123452ff7b2360cece9108a6f4")
                                                      (filename . "LICENSE")))
                               normalized-gist))
                   '((id . "7c71af123452ff7b2360cece9108a6f4")
                     (filename . "LICENSE")
                     (idx . 0)
                     (total . 3)
                     (files
                      ((filename . "LICENSE"))
                      ((filename . "README.md"))
                      ((filename . "README.org"))))))))



(ert-deftest igist-test-normalize-full-gist ()
  "Check if normalized gist data matches expected structure and length."
  (let* ((gist (igist-test--get-gist-stub))
         (normalized-gist (igist-normalize-gist gist)))
    (should (equal (length normalized-gist) 3))
    (should (equal normalized-gist
                   '(("7c71af123452ff7b2360cece9108a6f4-LICENSE"
                      (url . "https://api.github.com/gists/7c71af123452ff7b2360cece9108a6f4")
                      (forks_url . "https://api.github.com/gists/7c71af123452ff7b2360cece9108a6f4/forks")
                      (commits_url . "https://api.github.com/gists/7c71af123452ff7b2360cece9108a6f4/commits")
                      (id . "7c71af123452ff7b2360cece9108a6f4")
                      (node_id . "G_kwDOAXx-BNoAIDdjNzFhZjEyMzQ1MmZmN2IyMzYwY2VjZTkxMDhhNmY0")
                      (git_pull_url . "https://gist.github.com/7c71af123452ff7b2360cece9108a6f4.git")
                      (git_push_url . "https://gist.github.com/7c71af123452ff7b2360cece9108a6f4.git")
                      (html_url . "https://gist.github.com/KarimAziev/7c71af123452ff7b2360cece9108a6f4")
                      (public)
                      (created_at . "2023-08-16T08:09:59Z")
                      (updated_at . "2023-08-16T08:10:00Z")
                      (description . "readmes")
                      (comments . 0)
                      (user)
                      (comments_url . "https://api.github.com/gists/7c71af123452ff7b2360cece9108a6f4/comments")
                      (owner
                       (login . "KarimAziev")
                       (id . 24935940)
                       (node_id . "MDQ6VXNlcjI0OTM1OTQw")
                       (avatar_url . "https://avatars.githubusercontent.com/u/24935940?v=4")
                       (gravatar_id . "")
                       (url . "https://api.github.com/users/KarimAziev")
                       (html_url . "https://github.com/KarimAziev")
                       (followers_url . "https://api.github.com/users/KarimAziev/followers")
                       (following_url . "https://api.github.com/users/KarimAziev/following{/other_user}")
                       (gists_url . "https://api.github.com/users/KarimAziev/gists{/gist_id}")
                       (starred_url . "https://api.github.com/users/KarimAziev/starred{/owner}{/repo}")
                       (subscriptions_url . "https://api.github.com/users/KarimAziev/subscriptions")
                       (organizations_url . "https://api.github.com/users/KarimAziev/orgs")
                       (repos_url . "https://api.github.com/users/KarimAziev/repos")
                       (events_url . "https://api.github.com/users/KarimAziev/events{/privacy}")
                       (received_events_url . "https://api.github.com/users/KarimAziev/received_events")
                       (type . "User")
                       (site_admin))
                      (truncated)
                      (filename . "LICENSE")
                      (type . "text/plain")
                      (language . "Text")
                      (raw_url . "https://gist.githubusercontent.com/KarimAziev/7c71af123452ff7b2360cece9108a6f4/raw/f288702d2fa16d3cdf0035b15a9fcbc552cd88e7/LICENSE")
                      (size . 35149)
                      (idx . 0)
                      (total . 3)
                      (files
                       ((filename . "LICENSE")
                        (type . "text/plain")
                        (language . "Text")
                        (raw_url . "https://gist.githubusercontent.com/KarimAziev/7c71af123452ff7b2360cece9108a6f4/raw/f288702d2fa16d3cdf0035b15a9fcbc552cd88e7/LICENSE")
                        (size . 35149))
                       ((filename . "README.md")
                        (type . "text/markdown")
                        (language . "Markdown")
                        (raw_url . "https://gist.githubusercontent.com/KarimAziev/7c71af123452ff7b2360cece9108a6f4/raw/0fc80463e75190ad915a08384cb496ce6ba7c015/README.md")
                        (size . 12552))
                       ((filename . "README.org")
                        (type . "application/vnd.lotus-organizer")
                        (language . "Org")
                        (raw_url . "https://gist.githubusercontent.com/KarimAziev/7c71af123452ff7b2360cece9108a6f4/raw/4aa4f32f5bd2402950f7b5355bcccd0353c5ca44/README.org")
                        (size . 12901))))
                     ("7c71af123452ff7b2360cece9108a6f4-README.md"
                      (url . "https://api.github.com/gists/7c71af123452ff7b2360cece9108a6f4")
                      (forks_url . "https://api.github.com/gists/7c71af123452ff7b2360cece9108a6f4/forks")
                      (commits_url . "https://api.github.com/gists/7c71af123452ff7b2360cece9108a6f4/commits")
                      (id . "7c71af123452ff7b2360cece9108a6f4")
                      (node_id . "G_kwDOAXx-BNoAIDdjNzFhZjEyMzQ1MmZmN2IyMzYwY2VjZTkxMDhhNmY0")
                      (git_pull_url . "https://gist.github.com/7c71af123452ff7b2360cece9108a6f4.git")
                      (git_push_url . "https://gist.github.com/7c71af123452ff7b2360cece9108a6f4.git")
                      (html_url . "https://gist.github.com/KarimAziev/7c71af123452ff7b2360cece9108a6f4")
                      (public)
                      (created_at . "2023-08-16T08:09:59Z")
                      (updated_at . "2023-08-16T08:10:00Z")
                      (description . "readmes")
                      (comments . 0)
                      (user)
                      (comments_url . "https://api.github.com/gists/7c71af123452ff7b2360cece9108a6f4/comments")
                      (owner
                       (login . "KarimAziev")
                       (id . 24935940)
                       (node_id . "MDQ6VXNlcjI0OTM1OTQw")
                       (avatar_url . "https://avatars.githubusercontent.com/u/24935940?v=4")
                       (gravatar_id . "")
                       (url . "https://api.github.com/users/KarimAziev")
                       (html_url . "https://github.com/KarimAziev")
                       (followers_url . "https://api.github.com/users/KarimAziev/followers")
                       (following_url . "https://api.github.com/users/KarimAziev/following{/other_user}")
                       (gists_url . "https://api.github.com/users/KarimAziev/gists{/gist_id}")
                       (starred_url . "https://api.github.com/users/KarimAziev/starred{/owner}{/repo}")
                       (subscriptions_url . "https://api.github.com/users/KarimAziev/subscriptions")
                       (organizations_url . "https://api.github.com/users/KarimAziev/orgs")
                       (repos_url . "https://api.github.com/users/KarimAziev/repos")
                       (events_url . "https://api.github.com/users/KarimAziev/events{/privacy}")
                       (received_events_url . "https://api.github.com/users/KarimAziev/received_events")
                       (type . "User")
                       (site_admin))
                      (truncated)
                      (filename . "README.md")
                      (type . "text/markdown")
                      (language . "Markdown")
                      (raw_url . "https://gist.githubusercontent.com/KarimAziev/7c71af123452ff7b2360cece9108a6f4/raw/0fc80463e75190ad915a08384cb496ce6ba7c015/README.md")
                      (size . 12552)
                      (idx . 1)
                      (total . 3)
                      (files
                       ((filename . "LICENSE")
                        (type . "text/plain")
                        (language . "Text")
                        (raw_url . "https://gist.githubusercontent.com/KarimAziev/7c71af123452ff7b2360cece9108a6f4/raw/f288702d2fa16d3cdf0035b15a9fcbc552cd88e7/LICENSE")
                        (size . 35149))
                       ((filename . "README.md")
                        (type . "text/markdown")
                        (language . "Markdown")
                        (raw_url . "https://gist.githubusercontent.com/KarimAziev/7c71af123452ff7b2360cece9108a6f4/raw/0fc80463e75190ad915a08384cb496ce6ba7c015/README.md")
                        (size . 12552))
                       ((filename . "README.org")
                        (type . "application/vnd.lotus-organizer")
                        (language . "Org")
                        (raw_url . "https://gist.githubusercontent.com/KarimAziev/7c71af123452ff7b2360cece9108a6f4/raw/4aa4f32f5bd2402950f7b5355bcccd0353c5ca44/README.org")
                        (size . 12901))))
                     ("7c71af123452ff7b2360cece9108a6f4-README.org"
                      (url . "https://api.github.com/gists/7c71af123452ff7b2360cece9108a6f4")
                      (forks_url . "https://api.github.com/gists/7c71af123452ff7b2360cece9108a6f4/forks")
                      (commits_url . "https://api.github.com/gists/7c71af123452ff7b2360cece9108a6f4/commits")
                      (id . "7c71af123452ff7b2360cece9108a6f4")
                      (node_id . "G_kwDOAXx-BNoAIDdjNzFhZjEyMzQ1MmZmN2IyMzYwY2VjZTkxMDhhNmY0")
                      (git_pull_url . "https://gist.github.com/7c71af123452ff7b2360cece9108a6f4.git")
                      (git_push_url . "https://gist.github.com/7c71af123452ff7b2360cece9108a6f4.git")
                      (html_url . "https://gist.github.com/KarimAziev/7c71af123452ff7b2360cece9108a6f4")
                      (public)
                      (created_at . "2023-08-16T08:09:59Z")
                      (updated_at . "2023-08-16T08:10:00Z")
                      (description . "readmes")
                      (comments . 0)
                      (user)
                      (comments_url . "https://api.github.com/gists/7c71af123452ff7b2360cece9108a6f4/comments")
                      (owner
                       (login . "KarimAziev")
                       (id . 24935940)
                       (node_id . "MDQ6VXNlcjI0OTM1OTQw")
                       (avatar_url . "https://avatars.githubusercontent.com/u/24935940?v=4")
                       (gravatar_id . "")
                       (url . "https://api.github.com/users/KarimAziev")
                       (html_url . "https://github.com/KarimAziev")
                       (followers_url . "https://api.github.com/users/KarimAziev/followers")
                       (following_url . "https://api.github.com/users/KarimAziev/following{/other_user}")
                       (gists_url . "https://api.github.com/users/KarimAziev/gists{/gist_id}")
                       (starred_url . "https://api.github.com/users/KarimAziev/starred{/owner}{/repo}")
                       (subscriptions_url . "https://api.github.com/users/KarimAziev/subscriptions")
                       (organizations_url . "https://api.github.com/users/KarimAziev/orgs")
                       (repos_url . "https://api.github.com/users/KarimAziev/repos")
                       (events_url . "https://api.github.com/users/KarimAziev/events{/privacy}")
                       (received_events_url . "https://api.github.com/users/KarimAziev/received_events")
                       (type . "User")
                       (site_admin))
                      (truncated)
                      (filename . "README.org")
                      (type . "application/vnd.lotus-organizer")
                      (language . "Org")
                      (raw_url . "https://gist.githubusercontent.com/KarimAziev/7c71af123452ff7b2360cece9108a6f4/raw/4aa4f32f5bd2402950f7b5355bcccd0353c5ca44/README.org")
                      (size . 12901)
                      (idx . 2)
                      (total . 3)
                      (files
                       ((filename . "LICENSE")
                        (type . "text/plain")
                        (language . "Text")
                        (raw_url . "https://gist.githubusercontent.com/KarimAziev/7c71af123452ff7b2360cece9108a6f4/raw/f288702d2fa16d3cdf0035b15a9fcbc552cd88e7/LICENSE")
                        (size . 35149))
                       ((filename . "README.md")
                        (type . "text/markdown")
                        (language . "Markdown")
                        (raw_url . "https://gist.githubusercontent.com/KarimAziev/7c71af123452ff7b2360cece9108a6f4/raw/0fc80463e75190ad915a08384cb496ce6ba7c015/README.md")
                        (size . 12552))
                       ((filename . "README.org")
                        (type . "application/vnd.lotus-organizer")
                        (language . "Org")
                        (raw_url . "https://gist.githubusercontent.com/KarimAziev/7c71af123452ff7b2360cece9108a6f4/raw/4aa4f32f5bd2402950f7b5355bcccd0353c5ca44/README.org")
                        (size . 12901)))))))))



(ert-deftest igist-test-igist-files-to-gist-alist ()
  "Test conversion of file paths to gist alist with content verification."
  (let ((temp-file-path (make-temp-file "test-file"))
        (test-content "Testing igist-files-to-gist-alist"))
    (with-temp-file temp-file-path
      (insert test-content))
    (let ((gist-alist (igist-files-to-gist-alist (list temp-file-path))))
      (should (equal (car (car gist-alist))
                     (intern (file-name-nondirectory temp-file-path))))
      (should (string= (cdadar gist-alist) test-content)))))

(ert-deftest igist-test-igist-property-boundaries ()
  "Check boundaries of `igist-tabulated-list-id' property in buffer text."
  (with-temp-buffer
    (insert "entry 1\nentry 2")
    (put-text-property 1 7 'igist-tabulated-list-id 1)
    (put-text-property 9 15 'igist-tabulated-list-id 2)
    (should (equal (igist-property-boundaries 'igist-tabulated-list-id 1)
                   (cons 1 6)))
    (should (equal (igist-property-boundaries 'igist-tabulated-list-id 9)
                   (cons 9 14)))
    (should-not (igist-property-boundaries 'igist-tabulated-list-id 7))))

(ert-deftest igist-test-igist-find-entry-bounds ()
  "Test finding bounds of entries with specific IDs in a buffer."
  (with-temp-buffer
    (insert "entry 1\nentry 2")
    (put-text-property 1 7 'igist-tabulated-list-id 1)
    (put-text-property 9 15 'igist-tabulated-list-id 2)
    (should (equal (igist-find-entry-bounds 1)
                   (cons 1 6)))
    (should (equal (igist-find-entry-bounds 2)
                   (cons 9 14)))
    (should (equal (igist-find-entry-bounds 2)
                   (cons 9 14)))
    (should-not (igist-find-entry-bounds 3))))

(ert-deftest igist-test-igist--format-plural ()
  "Test plural formatting of words based on quantity."
  (should (string= (igist--format-plural 1 "cat") "1 cat"))
  (should (string= (igist--format-plural 0 "cat") "0 cats"))
  (should (string= (igist--format-plural 2 "cat") "2 cats"))
  (should (string= (igist--format-plural -1 "cat") "-1 cats")))


(ert-deftest igist-test-igist-format-time-diff ()
  "Test formatting of time differences as human-readable strings."
  (cl-letf (((symbol-function 'current-time)
             (lambda ()
               (encode-time 0 0 0 1 1 2000))))
    (should
     (string= (igist-format-time-diff (encode-time 0 0 0 1 1 2000)) "Now"))
    (should (string= (igist-format-time-diff (encode-time 59 59 23 31 12 1999))
                     "1 minute ago"))
    (should (string= (igist-format-time-diff (encode-time 59 58 23 31 12 1999))
                     "2 minutes ago"))
    (should (string= (igist-format-time-diff (encode-time 0 59 22 31 12 1999))
                     "1 hour ago"))
    (should (string= (igist-format-time-diff (encode-time 0 0 0 31 12 1999))
                     "1 day ago"))
    (should (string= (igist-format-time-diff (encode-time 0 0 0 1 12 1999))
                     "1 month ago"))
    (should (string= (igist-format-time-diff (encode-time 0 0 0 1 1 1999))
                     "1 year ago"))))

(ert-deftest igist-test-igist--generate-description ()
  "Test generating human-readable descriptions from file names."
  ;; Test without extension
  (should (equal (igist--generate-description "fileName") "File Name"))
  ;; Test with extension
  (should (equal (igist--generate-description "anotherFileName.ext")
                 "Another File Name #ext"))
  ;; Test mixed case and numbers
  (should (equal (igist--generate-description "test123File") "Test123 File"))
  ;; Test special characters
  (should (equal (igist--generate-description "special-char_file-Name.data")
                 "Special Char File Name #data"))
  ;; Test all lower case with extension
  (should (equal (igist--generate-description "lowercase.ext") "Lowercase #ext"))
  ;; Test all upper case with extension
  (should (equal (igist--generate-description "UPPERCASE.EXT") "Uppercase #EXT")))

(ert-deftest test-igist--generate-description-from-files ()
  "Test generating descriptions from file names and extensions."
  ;; Test with multiple filenames, no primary-file specified
  (should (equal (igist--generate-description-from-files '("testFile.txt"
                                                           "anotherFile.md"
                                                           "image.png"))
                 "Test File #txt #md #png"))
  ;; Test with primary-file specified
  (should (equal
           (igist--generate-description-from-files
            '("fileOne.docx"
              "fileTwo.xlsx"
              "importantPresentation.pptx")
            "importantPresentation.pptx")
           "Important Presentation #pptx #docx #xlsx"))
  ;; Test with repeated extensions
  (should (equal (igist--generate-description-from-files '("firstScript.py"
                                                           "secondScript.py"
                                                           "note.md"))
                 "First Script #py #md"))
  ;; Test with a single file and without extension
  (should (equal (igist--generate-description-from-files '("README"))
                 "Readme"))
  ;; Test all lower case files with digits and underscores
  (should (equal (igist--generate-description-from-files '("code_snippet_123.js"
                                                           "documentation.txt"
                                                           "image2.jpg"
                                                           "image1.jpg")
                                                         "test_cases.py")
                 "Test Cases #py #js #txt #jpg"))
  ;; Tests with special characters and mixed case
  (should (equal (igist--generate-description-from-files
                  '("My-Project-Summary_V1.doc"
                    "Financial-Report_V1.xlsx")
                  "Project-Overview_V1.ppt")
                 "Project Overview V1 #ppt #doc #xlsx")))



(provide 'igist-test)
;;; igist-test.el ends here
