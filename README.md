## Ace Jump Buffer

`ace-jump-buffer` is an extension for [`ace-jump-mode`](https://github.com/winterTTr/ace-jump-mode) and the native `bs` buffer menu that lets you hop to any of your current Emacs buffers in two key strokes: one to display the buffer menu, and one to select the line on which the buffer is on.

It also supports [`perspective`](https://github.com/nex3/perspective-el) to scope the buffer list to your current workspace/project.

### Installation

My recommendation for Emacs 24+ is to simply add [MELPA](melpa.milkbox.net) to your package archives list and run `package-install ace-jump-buffer`. Or to install manually, make sure `ace-jump-buffer.el`, `ace-jump-mode.el`, (and optionally, `perspective.el`) are in your load path. 

Then simply `(require 'ace-jump-buffer)` bind the command `ace-jump-buffer` to a key binding of your choice and flip away.


