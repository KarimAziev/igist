# About

The Emacs everywhere goal continues. These are the main features of
`igist` to help you never leave Emacs to manage your gists.

![](./igist-demo.gif)

  - Edit a gist
  - List gists
  - Create a gist
  - Delete a gist
  - Fork a gist
  - Edit, view, list, and create comments
  - UI
      - transient api
      - tabulated/minibuffer display
  - auth-sources support

# igist

>   - [About](#about)
>       - [Requirements](#requirements)
>       - [Installation](#installation)
>           - [Manually](#manually)
>           - [With use-package and
>             straight](#with-use-package-and-straight)
>       - [Auth](#auth)
>           - [Secure](#secure)
>           - [Insecure](#insecure)
>       - [Usage](#usage)
>           - [General](#general)
>           - [List gists](#list-gists)
>           - [Edit gist](#edit-gist)
>           - [List comments](#list-comments)
>           - [Editing comment](#editing-comment)

## Requirements

  - Emacs \>= 28.1
  - ghub
  - transient
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
               ("-" . igist-delete-current-filename)
               ("D" . igist-delete-current-gist)
               ("a" . igist-add-comment)
               ("c" . igist-load-comments)
               ("e" . igist-list-edit-description)
               ("f" . igist-fork-gist)
               ("g" . igist-list-gists)
               ("v" . igist-list-view-current)
               ("b" . igist-browse-gist)
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
token](https://github.com/settings/tokens) with scope `gist`. Then there
are two options to feed it.

### Secure

To use `auth-sources`, you need to add a such entry with a Github
username and token, e.g.:

``` example
machine api.github.com login MY_GITHUB_USERNAME^igist password MY_GITHUB_TOKEN
```

Replace MY<sub>GITHUBUSERNAME</sub> with your actual GitHub username and
MY<sub>GITHUBTOKEN</sub> with the token.

To use other suffixes instead of `^igist` (`M-x customize-variable`
`RET` and type `igist-auth-marker`).

You can read more in
[ghub](https://magit.vc/manual/forge/Token-Creation.html#Token-Creation)
manual, as igist relies on the provided API.

### Insecure

If the value of the variable `igist-auth-marker` is a string, it will be
used as a token.

## Usage

### General

The simplest way is to invoke a transient popup with the list of
available commands for the current buffer:

  - `M-x igist-dispatch` - in `igists` buffers it is bound to `M-o`.

### List gists

There are two ways in which gists can be presented - as a table or as
minibuffer completions.

1.  table
    
      - `M-x igist-list-gists` - to display gists of logged GitHub user.
      - `M-x igist-list-other-user-gists` - to display public gists of
        non-logged user.
      - `M-x igist-explore-public-gists` - list public gists sorted by
        most recently updated to least recently updated.
    
    This commands render and load gists with pagination. To stop or
    pause loading use command `igist-list-cancel-load` (default
    keybinding is `K`).
    
    | Key | Command        |
    | --- | -------------- |
    | C-j | view gist      |
    | v   | view gist      |
    | RET | edit gist      |
    | \-  | delete file    |
    | \+  | add file       |
    | D   | delete gist    |
    | c   | load comments  |
    | a   | add comment    |
    | g   | refresh gists  |
    | f   | fork gist      |
    | b   | browse gist    |
    | K   | cancel loading |
    

    To customize these keys, see the variable `igist-list-mode-map`.

2.  minibuffer completions
    
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
