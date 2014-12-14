.emacs.d
========

## Setup

arch linux
`pacman -S python-virtualenv auctex lynx markdown`

#### packages ####

* ace-jump-mode
* silversearcher-ag
* alert
* auto-complete  ---
* css mode
* custom themes
* multiple cursors ------------


#### possible packages #####

* deft mode

### keybidings ###

* `C-c SPC`   - ace-jump-mode
* `C-c C-SPC` - ace-jump-pop-mark
* `M-x ag`    - silversearcher-ag
* `M-k`       - kill this buffer
* `C-z`       - repeat last command
* `M-N`       - winner-redo
* `M-P`       - winner-undo

### General

* `C-g    ` Quit out of whatever mess you've gotten yourself into
* `M-x    ` Run a command by name
* `C-x m  ` Open magit. It's a magical git interface for emacs

### Movement

* `C-x <` Scroll text in current window to the left
* `C-x >` Scroll to the right
*         windmove
### narrowing

* `C-x n n` Narrow down between point and mark
* `C-x n w` Widen to make the entire buffer accessible again
* `C-x n p` Narrow down to the current page
* `C-x n d` Narrow down to the current defun
