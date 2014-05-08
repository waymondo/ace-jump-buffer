## Ace Jump Buffer

`ace-jump-buffer` is an extension for [`ace-jump-mode`](https://github.com/winterTTr/ace-jump-mode) and the native `bs` buffer menu that lets you hop to Emacs buffers in two key strokes.

### Installation

Install from [MELPA](melpa.milkbox.net) with `package-install ace-jump-buffer`, or drop `ace-jump-buffer.el`, `ace-jump-mode.el`, and [`dash.el`](https://github.com/magnars/dash.el) into your load path. 

### Usage

Command | Description
--------|------------
`(ace-jump-buffer)`               | Open the buffer menu and go to the selected buffer in the current window.
`(ace-jump-buffer-other-window)`  | Open the buffer menu and go to the selected buffer in other window.      
`(ace-jump-buffer-in-one-window)` | Open the buffer menu and go to the selected buffer in full window.       

Variable | Description
---------|------------
`ajb-max-window-height` | The max window height for the buffer menu. The default is 27 (fits the lowercase alphabet).
`ajb-sort-function`     | The function for sorting buffers in the menu. The default is `(bs-sort-buffers-by-recentf)`.
`ajb-bs-configuration`  | The `bs` configuration to use when displaying the menu. The default is `"all"`. If you use [`perspective`](https://github.com/nex3/perspective-el), you can set this to `"persp"` to scope the buffer list to your current workspace/project.
`ajb-reuse-windows`     | If you call `(ace-jump-buffer-other-window)` with this set to non-nil,
it will display the selected buffer in an existing other window if available.
