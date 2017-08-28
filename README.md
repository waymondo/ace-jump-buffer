[![MELPA](http://melpa.org/packages/ace-jump-buffer-badge.svg)](http://melpa.org/#/ace-jump-buffer)
[![MELPA Stable](http://stable.melpa.org/packages/ace-jump-buffer-badge.svg)](http://stable.melpa.org/#/ace-jump-buffer)

## Ace Jump Buffer

`ace-jump-buffer` is an extension for [`avy`](https://github.com/abo-abo/avy) and the native `bs` buffer menu that lets you hop to Emacs buffers in 2-3 key strokes.

### Installation

Install from [MELPA](melpa.milkbox.net) with `package-install ace-jump-buffer`, or drop `ace-jump-buffer.el`, `avy.el`, and [`dash.el`](https://github.com/magnars/dash.el) into your load path.

### Commands

###### `(ace-jump-buffer)`
Open the buffer menu and go to the selected buffer in the current window.

###### `(ace-jump-buffer-with-configuration)`
Select a `bs-configuration`, then show its buffer menu and go to the selected buffer.  

###### `(ace-jump-buffer-other-window)`
Open the buffer menu and go to the selected buffer in other window.  

###### `(ace-jump-buffer-in-one-window)`
Open the buffer menu and go to the selected buffer in full window.  

###### `(ace-jump-same-mode-buffers)`
Show a menu of buffers of the same mode as the current buffer.

###### `(ace-jump-persp-buffers)`
Show a menu of buffers in the current perspective (`persp-mode` required.).

###### `(ace-jump-projectile-buffers)`
Show a menu of buffers in the current project (`projectile` required.).

### Variables

###### `ajb-max-window-height`
The max window height for the buffer menu. The default is 20.

###### `ajb-sort-function`
The function for sorting buffers in the menu. The default is `nil`.

###### `ajb-bs-configuration`
The `bs` configuration to use when displaying the menu with `ace-jump-buffer`. The default is `"all"`. If you use [`perspective`](https://github.com/nex3/perspective-el), you may set this to `"persp"` to scope the buffer list to your current workspace/project. If you use [`projectile`](https://github.com/bbatsov/projectile), you may set this to `"projectile"` to scope the buffer list to your current project.

### Macro

`(make-ace-jump-buffer-function)` takes two variables: a string name and the body of a buffer rejection filter function.

For example, if you wanted an `ace-jump-buffer` function for pulling up your `*shell-mode*` buffers:

``` elisp
(make-ace-jump-buffer-function "shell"
  (with-current-buffer buffer
    (not (eq major-mode 'shell-mode))))
```

This would create a `bs-configurations` option named "shell" and a callable defun named `(ace-jump-shell-buffers)`.

### Changelog

*0.4* - Changed the internal jumping library from `ace-jump-mode` to
`avy`. `ajb-home-row-keys` was depreciated as the default `avy-keys`
are set to the lower-cased home-row keys.

*0.3* - Introduced a macro for easily making custom `ace-jump-buffer`
filters and interactive functions
