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
  "Test the function that normalizes GitHub Gist data."
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
  (let ((temp-file-path (make-temp-file "test-file"))
        (test-content "Testing igist-files-to-gist-alist"))
    (with-temp-file temp-file-path
      (insert test-content))
    (let ((gist-alist (igist-files-to-gist-alist (list temp-file-path))))
      (should (equal (car (car gist-alist))
                     (intern (file-name-nondirectory temp-file-path))))
      (should (string= (cdadar gist-alist) test-content)))))

(ert-deftest igist-test-igist-property-boundaries ()
  (with-temp-buffer
    (insert "entry 1\nentry 2")
    (put-text-property 1 7 'igist-tabulated-list-id 1)
    (put-text-property 9 15 'igist-tabulated-list-id 2)
    (should (equal (igist-property-boundaries 'igist-tabulated-list-id 1)
                   (cons 1 7)))
    (should (equal (igist-property-boundaries 'igist-tabulated-list-id 9)
                   (cons 9 15)))
    (should-not (igist-property-boundaries 'igist-tabulated-list-id 7))))

(ert-deftest igist-test-igist-find-entry-bounds ()
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


(provide 'igist-test)
;;; igist-test.el ends here