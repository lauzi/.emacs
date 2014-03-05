# .emacs #

## Setup ##
[Windows HOME register](http://www.emacswiki.org/emacs/MsWindowsRegistry)

~/.emacs.d/local.el
```elisp
(setq default-directory "D:\\git")

(custom-set-variables '(initial-buffer-choice "D:\\git"))
```

[DLLs needed for picture viewing](https://code.google.com/p/emacs4win/source/browse/#git%2Fdlls)

## How To ##
### Undo-tree ###
* `C-x u` 進入 undo-tree-visualizer-mode
* `p` `n` 上下移動
* `b` `f` 左右切換
* `t` 顯示時間戳
* `q` 退出

### Multiple-cursors ###
* `C-S-c C-S-c` Add cursor to each line selected
* `C->` / `C-<` Mark next/last occurence
* `C-S-C C-<` Mark all like this
* `C-.` / `C-,` Mark next/last word like this
* `C-C C-,` Mark all words like this

### xkcd ###
* `r` Load a random xkcd
* `t` Show alt-text in the minibuffer
* `e` Link to explain-xkcd
