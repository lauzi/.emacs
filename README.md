.emacs
======

Setup
-----
Windows HOME register
http://www.emacswiki.org/emacs/MsWindowsRegistry

~/.emacs.d/local.el

```elisp
(setq default-directory "D:\\git")

(custom-set-variables '(initial-buffer-choice "D:\\git"))
```

Undo-tree
---------
C-x u 進入 undo-tree-visualizer-mode , p n 上下移動，在分支之前 b f 左右切換，t 顯示時間戳，選定需要的狀態後， q 退出
