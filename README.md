# igist

List, create, update and delete GitHub gists in Emacs.

![](./igist-demo.gif)

## Overview

The Emacs everywhere goal continues. These are the main features of
`igist` to help you never leave Emacs to manage your gists.

  - Edit a gist
  - List gists
  - Create a gist
  - Delete a gist
  - Fork a gist
  - Edit, view, list, and create comments
  - UI
      - transient api
      - tabulated/minibuffer display
  - Use auth-sources

## Requirements

  - Emacs \>= 28.1
  - ghub
  - transient
  - timezone
  - spinner (optional)
  - [Github API
    token](https://magit.vc/manual/forge/Token-Creation.html#Token-Creation)

## Installation

### Manually

Download the repository and it to your load path in your init file:

``` elisp

(add-to-list 'load-path "/path/to/igist)

(require 'igist)
```

### With use-package and straight

``` elisp

(use-package igist
  :straight (igist
             :repo "KarimAziev/igist"
             :type git
             :host github)
  :bind (("M-o" . igist-dispatch)))

```


<details>
  <summary>Example configuration with default keybindings</summary>

```elisp
(use-package igist
  :straight (igist
             :repo "KarimAziev/igist"
             :type git
             :host github)
  :bind (("M-o" . igist-dispatch)
         (:map igist-edit-mode-map
               ([remap save-buffer] . igist-save-current-gist)
               ("M-o" . igist-dispatch)
               ("C-c C-c" . igist-save-current-gist-and-exit)
               ("C-c C-k" . kill-current-buffer)
               ("C-c '" . igist-save-current-gist-and-exit))
         (:map igist-list-mode-map
               ("C-j" . igist-list-view-current)
               ("RET" . igist-list-view-current)
               ("+" . igist-list-add-file)
               ("-" . igist-delete-current-gist)
               ("D" . igist-delete-current-gist)
               ("a" . igist-add-comment)
               ("c" . igist-load-comments)
               ("e" . igist-list-edit-description)
               ("f" . igist-fork-gist)
               ("g" . igist-list-gists)
               ("v" . igist-list-view-current))
         (:map igist-comments-edit-mode-map
               ("M-o" . igist-dispatch)
               ("C-c C-c" . igist-post-comment)
               ("C-c C-k" . kill-current-buffer))
         (:map igist-comments-list-mode-map
               ("+" . igist-add-comment)
               ("-" . igist-delete-comment-at-point)
               ("D" . igist-delete-comment-at-point)
               ("e" . igist-add-or-edit-comment)
               ("g" . igist-load-comments))))
```
</details>

## Auth

You need to ensure that you have [a GitHub API
token](https://github.com/settings/tokens) with scope `gist`.

For example, your GitHub username is `km`, and you have a token
"012345abcdef…". Add such an entry in `auth-sources` (`M-x
describe-variable` `RET` and type `auth-sources`, usually it is
`~/.authinfo` or `~/.authinfo.gpg`).

``` example
machine api.github.com login km^igist password 012345abcdef
```

You can read more in
[ghub](https://magit.vc/manual/forge/Token-Creation.html#Token-Creation)
manual as igist relies on the provided API.

## Usage

### General

The simplest way is to invoke a transient popup with the list of
available commands for the current buffer:

  - `M-x igist-dispatch` - in `igists` buffers it is bound to `M-o`.

### List gists

There are two ways in which gists can be presented, as a table or as
minibuffer completions.

  - `M-x igist-list-gists` - to display your gists
  - `M-x igist-list-other-user-gists` - to display public gists of any
    user.

In `igist` tabulated list mode, such commands are available:

| Key | Command       |
| --- | ------------- |
| C-j | view gist     |
| v   | view gist     |
| RET | edit gist     |
| \-  | delete file   |
| \+  | add file      |
| D   | delete gist   |
| c   | load comments |
| g   | refresh gists |
| f   | fork gist     |

To customize these keys, see the variable `igist-list-mode-map`.

  - `M-x igist-edit-list` - to list gists in the minibuffer.

### Edit gist

You can view, edit and save gists in buffers with
`igist-comments-edit-mode`. This minor mode is turned on after command
`igist-edit-gist`.

| Key     | Command              |
| ------- | -------------------- |
| M-o     | transient popup      |
| C-c C-c | save and exit        |
| C-c '   | save and exit        |
| C-x C-s | save without exiting |

To customize these keys see the variable `igist-comments-edit-mode-map`.

### List comments

This minor mode is turned on after command `igist-load-comments`.

In comments list mode, such commands are available:

| Key | Command                     |
| --- | --------------------------- |
| \+  | add comment                 |
| \-  | delete the comment at point |
| D   | delete the comment at point |
| e   | add or edit                 |
| g   | refresh comments            |

To customize these keybindings edit the variable
`igist-comments-list-mode-map`.

### Editing comment

This minor mode is turned on after commands `igist-edit-comment` and
`igist-add-comment`. Keymap for posting and editing comments:

| Key     | Command      |
| ------- | ------------ |
| C-c C-c | post comment |

To customize these keybindings edit the variable
`igist-comments-edit-mode-map`.
