# .emacs #

## Setup ##
Windows HOME register
http://www.emacswiki.org/emacs/MsWindowsRegistry

~/.emacs.d/local.el

```elisp
(setq default-directory "D:\\git")

(custom-set-variables '(initial-buffer-choice "D:\\git"))
```

## How To ##
### Undo-tree ###
* `C-x u` 進入 undo-tree-visualizer-mode
* `p` `n` 上下移動
* `b` `f` 左右切換
* `t` 顯示時間戳
* `q` 退出
