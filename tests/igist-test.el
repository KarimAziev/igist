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
(require 'igist-gitlab)

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



;;; GitLab snippets support

(defun igist-test--get-snippet-stub ()
  "Return a stub of GitLab Snippet data for testing purposes."
  '((id . 42)
    (title . "My test snippet")
    (description . "A longer description")
    (visibility . "internal")
    (author
     (id . 22)
     (name . "User 0")
     (username . "user0"))
    (created_at . "2018-09-18T01:12:26.383Z")
    (updated_at . "2018-09-18T01:12:26.383Z")
    (web_url . "https://gitlab.com/-/snippets/42")
    (raw_url . "https://gitlab.com/-/snippets/42/raw")
    (ssh_url_to_repo . "ssh://git@gitlab.com:snippets/42.git")
    (http_url_to_repo . "https://gitlab.com/snippets/42.git")
    (files
     ((path . "hello.py")
      (raw_url . "https://gitlab.com/-/snippets/42/raw/main/hello.py"))
     ((path . "docs/read me.org")
      (raw_url . "https://gitlab.com/-/snippets/42/raw/main/docs/read%20me.org")))))

(ert-deftest igist-test-gitlab-normalize-snippet ()
  "Test `igist-gitlab--normalize-snippet' maps a snippet to a gist alist."
  (let* ((igist-gitlab-host "gitlab.com/api/v4")
         (normalized (igist-gitlab--normalize-snippet
                      (igist-test--get-snippet-stub))))
    ;; id is stringified
    (should (equal (igist-alist-get-symb 'id normalized) "42"))
    ;; title becomes the description
    (should (equal (igist-alist-get-symb 'description normalized)
                   "My test snippet"))
    ;; internal visibility is not public
    (should (equal (igist-alist-get-symb 'public normalized) nil))
    ;; author becomes owner with a login
    (should (equal (igist-alist-get-symb 'login
                                         (igist-alist-get-symb 'owner
                                                               normalized))
                   "user0"))
    ;; web url and forge marker
    (should (equal (igist-alist-get-symb 'html_url normalized)
                   "https://gitlab.com/-/snippets/42"))
    (should (eq (igist-alist-get-symb 'forge normalized) 'gitlab))
    ;; gitlab specific props are preserved
    (should (equal (igist-alist-get-symb 'gitlab_visibility normalized)
                   "internal"))
    (should (equal (igist-alist-get-symb 'gitlab_description normalized)
                   "A longer description"))
    ;; clone urls are mapped to git url cells
    (should (equal (igist-alist-get-symb 'git_push_url normalized)
                   "ssh://git@gitlab.com:snippets/42.git"))
    (should (equal (igist-alist-get-symb 'git_pull_url normalized)
                   "https://gitlab.com/snippets/42.git"))
    ;; files are an alist keyed by the interned filename
    (let* ((files (igist-alist-get-symb 'files normalized))
           (file-names (mapcar #'car files)))
      (should (equal (length files) 2))
      (should (memq 'hello.py file-names))
      (should (memq 'docs/read\ me.org file-names))
      (let ((py (cdr (assq 'hello.py files)))
            (org (cdr (assq 'docs/read\ me.org files))))
        (should (equal (igist-alist-get-symb 'filename py) "hello.py"))
        ;; raw_url points at the API raw endpoint with the branch from the
        ;; web raw url
        (should (equal (igist-alist-get-symb 'raw_url py)
                       "https://gitlab.com/api/v4/snippets/42/files/main/hello.py/raw"))
        ;; paths with slashes and spaces are URL-encoded
        (should (equal (igist-alist-get-symb 'raw_url org)
                       "https://gitlab.com/api/v4/snippets/42/files/main/docs%2Fread%20me.org/raw"))
        (should (equal (igist-alist-get-symb 'language py) "Python"))
        (should (equal (igist-alist-get-symb 'language org) "Org"))))
    ;; normalized snippets work with the generic row normalization
    (let ((rows (igist-normalize-gist normalized)))
      (should (equal (length rows) 2))
      (should (eq (igist-alist-get-symb 'forge (cdar rows)) 'gitlab))
      (should (equal (igist-alist-get-symb 'id (cdar rows)) "42")))))

(ert-deftest igist-test-gitlab-normalize-snippet-public ()
  "Test visibility mapping of public snippets."
  (let* ((igist-gitlab-host "gitlab.com/api/v4")
         (snippet (igist-test--get-snippet-stub))
         (normalized (igist-gitlab--normalize-snippet
                      (append (list (cons 'visibility "public")) snippet))))
    (should (eq (igist-alist-get-symb 'public normalized) t))))

(ert-deftest igist-test-gitlab-normalize-notes ()
  "Test `igist-gitlab--normalize-notes' maps authors and drops system notes."
  (let ((notes (igist-gitlab--normalize-notes
                '(((id . 1)
                   (body . "Looks good")
                   (system)
                   (author (username . "user0")))
                  ((id . 2)
                   (body . "changed the title")
                   (system . t)
                   (author (username . "user0")))))))
    (should (equal (length notes) 1))
    (should (equal (igist-alist-get-symb
                    'login (igist-alist-get-symb 'user (car notes)))
                   "user0"))
    (should (equal (igist-alist-get-symb 'body (car notes)) "Looks good"))))

(ert-deftest igist-test-gitlab-visibility ()
  "Test `igist-gitlab--visibility' preserves internal visibility."
  ;; not toggled: original visibility is preserved
  (should (equal (igist-gitlab--visibility '((public) (gitlab_visibility . "internal"))
                                           nil)
                 "internal"))
  (should (equal (igist-gitlab--visibility '((public . t) (gitlab_visibility . "public"))
                                           t)
                 "public"))
  ;; toggled: map to public or private
  (should (equal (igist-gitlab--visibility '((public) (gitlab_visibility . "internal"))
                                           t)
                 "public"))
  (should (equal (igist-gitlab--visibility '((public . t) (gitlab_visibility . "public"))
                                           nil)
                 "private"))
  ;; no original visibility (e.g. new snippet)
  (should (equal (igist-gitlab--visibility nil nil) "private"))
  (should (equal (igist-gitlab--visibility nil t) "public")))

(ert-deftest igist-test-gitlab-make-file-payload ()
  "Test `igist-gitlab--make-file-payload' file actions."
  ;; new file
  (should (equal (igist-gitlab--make-file-payload nil "a.py" "print(1)")
                 '(((action . "create")
                    (file_path . "a.py")
                    (content . "print(1)")))))
  ;; same name update
  (should (equal (igist-gitlab--make-file-payload "a.py" "a.py" "print(2)")
                 '(((action . "update")
                    (file_path . "a.py")
                    (content . "print(2)")))))
  ;; rename produces a move and an update
  (should (equal (igist-gitlab--make-file-payload "a.py" "b.py" "print(3)")
                 '(((action . "move")
                    (previous_path . "a.py")
                    (file_path . "b.py"))
                   ((action . "update")
                    (file_path . "b.py")
                    (content . "print(3)"))))))

(ert-deftest igist-test-gitlab-api-raw-url ()
  "Test `igist-gitlab--api-raw-url' branch parsing and path encoding."
  (let ((igist-gitlab-host "gitlab.example.com/api/v4"))
    (should (equal (igist-gitlab--api-raw-url
                    "1"
                    '((path . "snippet.rb")
                      (raw_url . "https://gitlab.example.com/-/snippets/1/raw/master/snippet.rb")))
                   "https://gitlab.example.com/api/v4/snippets/1/files/master/snippet.rb/raw"))
    ;; unknown branch falls back to main
    (should (equal (igist-gitlab--api-raw-url "2" '((path . "snippet.rb")))
                   "https://gitlab.example.com/api/v4/snippets/2/files/main/snippet.rb/raw"))))

(ert-deftest igist-test-gitlab-clone-url ()
  "Test `igist-gitlab--clone-url' protocol handling."
  (let ((gist '((git_pull_url . "https://gitlab.com/snippets/42.git")
                (git_push_url . "ssh://git@gitlab.com:snippets/42.git")
                (html_url . "https://gitlab.com/-/snippets/42"))))
    (should (equal (igist-gitlab--clone-url gist "https://")
                   "https://gitlab.com/snippets/42.git"))
    (should (equal (igist-gitlab--clone-url gist "git@")
                   "ssh://git@gitlab.com:snippets/42.git")))
  ;; fall back to deriving from the web url
  (should (equal (igist-gitlab--clone-url '((html_url . "https://gitlab.com/-/snippets/42"))
                                          "git@")
                 "git@gitlab.com:-/snippets/42.git")))

(ert-deftest igist-test-gitlab-gist-forge ()
  "Test `igist-gist-forge' resolution order."
  ;; explicit gist prop wins
  (should (eq (igist-gist-forge '((forge . gitlab))) 'gitlab))
  (should (eq (igist-gist-forge '((id . "1"))) 'github))
  ;; buffer-local variable is used without a gist
  (with-temp-buffer
    (setq igist-buffer-forge 'gitlab)
    (should (eq (igist-gist-forge) 'gitlab))
    (setq igist-buffer-forge nil)
    (should (eq (igist-gist-forge) 'github))))

(ert-deftest igist-test-gitlab-request-defaults ()
  "Test that `igist-request' picks GitLab defaults with `:forge 'gitlab'."
  (let ((captured nil)
        (igist-gitlab-host "gitlab.example.com/api/v4")
        (igist-gitlab-user-name "gluser")
        (igist-gitlab-auth-marker 'igist))
    (cl-letf (((symbol-function 'igist--request)
               (lambda (&rest args)
                 (setq captured args)
                 nil)))
      (igist-request "GET" "/snippets" nil :forge 'gitlab)
      (should (eq (plist-get (nthcdr 3 captured) :forge) 'gitlab))
      (should (equal (plist-get (nthcdr 3 captured) :host)
                     "gitlab.example.com/api/v4"))
      (should (equal (plist-get (nthcdr 3 captured) :username) "gluser"))
      (should (equal (plist-get (nthcdr 3 captured) :auth) 'igist))))
  ;; github remains the default
  (let ((captured nil)
        (igist-current-user-name "ghuser")
        (igist-auth-marker 'igist))
    (cl-letf (((symbol-function 'igist--request)
               (lambda (&rest args)
                 (setq captured args)
                 nil)))
      (igist-request "GET" "/gists" nil)
      (should (eq (plist-get (nthcdr 3 captured) :forge) 'github))
      (should (equal (plist-get (nthcdr 3 captured) :host) "api.github.com"))
      (should (equal (plist-get (nthcdr 3 captured) :username) "ghuser")))))

(ert-deftest igist-test-gitlab-save-dispatch ()
  "Test that `igist-save-gist-buffer' dispatches to GitLab save functions."
  (cl-letf (((symbol-function 'igist-editable-p) (lambda (&rest _) t)))
    (with-temp-buffer
      (let ((called nil))
        (setq-local igist-buffer-forge 'gitlab)
        (setq-local igist-current-filename "a.txt")
        (cl-letf (((symbol-function 'igist-gitlab-save-new-snippet)
                   (lambda (&rest _)
                     (setq called 'new)))
                  ((symbol-function 'igist-gitlab-save-existing-snippet)
                   (lambda (&rest _)
                     (setq called 'existing)))
                  ((symbol-function 'igist-save-new-gist)
                   (lambda (&rest _)
                     (setq called 'gh-new)))
                  ((symbol-function 'igist-save-existing-gist)
                   (lambda (&rest _)
                     (setq called 'gh-existing))))
          ;; new snippet
          (setq-local igist-current-gist nil)
          (igist-save-gist-buffer (current-buffer))
          (should (eq called 'new))
          ;; existing snippet
          (setq-local igist-current-gist '((id . "42")
                                           (filename . "a.txt")
                                           (forge . gitlab)))
          (igist-save-gist-buffer (current-buffer))
          (should (eq called 'existing)))))))

(ert-deftest igist-test-gitlab-editable-p ()
  "Test `igist-editable-p' uses the GitLab username for GitLab snippets."
  (let ((igist-gitlab-user-name "gluser")
        (igist-current-user-name "ghuser"))
    (should (eq (igist-editable-p '((forge . gitlab)
                                    (owner (login . "gluser"))))
                t))
    (should (eq (igist-editable-p '((forge . gitlab)
                                    (owner (login . "ghuser"))))
                nil))
    (should (eq (igist-editable-p '((owner (login . "ghuser")))) t))))

(ert-deftest igist-test-gitlab-forkable ()
  "Test that GitLab snippets are not forkable."
  (let ((igist-current-user-name "ghuser"))
    (should (eq (igist-forkable '((forge . gitlab)
                                  (owner (login . "someone-else"))))
                nil))
    (should (eq (igist-forkable '((owner (login . "someone-else")))) t))))

(ert-deftest igist-test-gitlab-file-language ()
  "Test `igist-gitlab--file-language' guesses modes from filenames."
  (should (equal (igist-gitlab--file-language "hello.py") "Python"))
  (should (equal (igist-gitlab--file-language "init.el") "Emacs Lisp"))
  (should (equal (igist-gitlab--file-language "notes.org") "Org"))
  (should (eq (igist-gitlab--file-language nil) nil)))

(ert-deftest igist-test-gitlab-token-string ()
  "Test `igist-gitlab--token' with a string marker."
  (let ((igist-gitlab-auth-marker "glpat-secret"))
    (should (equal (igist-gitlab--token) "glpat-secret"))))

(ert-deftest igist-test-gitlab-download-headers ()
  "Test `igist-gitlab--download-headers' only applies to GitLab."
  (let ((igist-gitlab-auth-marker "glpat-secret"))
    (should (equal (igist-gitlab--download-headers '((forge . gitlab)))
                   '(("PRIVATE-TOKEN" . "glpat-secret"))))
    (should (eq (igist-gitlab--download-headers '((id . "abc"))) nil))))

(ert-deftest igist-test-gitlab-list-request-integration ()
  "Test the GitLab list pipeline with a stubbed transport."
  (let* ((igist-gitlab-host "gitlab.com/api/v4")
         (igist-gitlab-user-name "gluser")
         (igist-gitlab-auth-marker 'igist)
         (captured nil)
         (snippets (list (igist-test--get-snippet-stub)))
         (buff-name (igist-gitlab-get-user-buffer-name "gluser")))
    (when-let* ((buf (get-buffer buff-name)))
      (kill-buffer buf))
    (cl-letf (((symbol-function 'igist--request)
               (lambda (method resource params &rest args)
                 (setq captured (list method resource args))
                 (when-let* ((callback (plist-get args :callback)))
                   (funcall callback snippets nil nil
                            (ghub--make-req :forge 'gitlab)))
                 nil)))
      (igist-gitlab-list-request "/snippets" nil t)
      (should (equal (nth 0 captured) "GET"))
      (should (equal (nth 1 captured) "/snippets"))
      (should (eq (plist-get (nth 2 captured) :forge) 'gitlab))
      (should (equal (plist-get (nth 2 captured) :host) "gitlab.com/api/v4"))
      (should (equal (plist-get (nth 2 captured) :username) "gluser"))
      (should (equal (plist-get (nth 2 captured) :auth) 'igist))
      (with-current-buffer buff-name
        (should (derived-mode-p 'igist-list-mode))
        (should (eq igist-buffer-forge 'gitlab))
        (should (equal (length igist-list-response) 1))
        (should (equal (igist-alist-get-symb 'id (car igist-list-response))
                       "42"))
        ;; two files produce two rows
        (should (equal (length (igist-normalize-gists igist-list-response)) 2))
        (should igist-rendered-hash)
        (let ((entry (gethash "42" igist-rendered-hash)))
          (should entry)
          (should (eq (igist-alist-get-symb 'forge entry) 'gitlab))
          (should (equal (igist-alist-get-symb 'description entry)
                         "My test snippet")))))
    (when-let* ((buf (get-buffer buff-name)))
      (kill-buffer buf))))



(provide 'igist-test)
;;; igist-test.el ends here