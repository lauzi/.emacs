# .emacs #

## Setup ##
[Windows HOME register](http://www.emacswiki.org/emacs/MsWindowsRegistry)

~/.emacs.d/local.el
```elisp
(setq default-directory "D:\\git")

(custom-set-variables '(initial-buffer-choice "D:\\git"))

(add-to-list 'sml/replacer-regexp-list '("^G:/" ":Git:"))
```

[DLLs needed for picture viewing](https://code.google.com/p/emacs4win/source/browse/#git%2Fdlls)

## How To ##
### Blablabla ###
* `C-=` er/expand-region
* `M-` lacarte

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

### IDO ###
* `Enter` Current selection
* `C-s`, `C-r` Navigating
* `M-n`, `M-p` Change to previous/next directories in history
* `M-s` Search for file matching input
* `C-SPC` Iterative filtering
* `C-f` find file without ido-mode; `C-b` buffer
* `C-d` Enter dired
* `C-j` Create file

### Magit ###
[Cheatsheet](http://daemianmack.com/magit-cheatsheet.html)


### structured-haskell-mode ###
[GitHub Repo](https://github.com/chrisdone/structured-haskell-mode)

#### Setup ####
1. Compile the executive

   ```bash
   $ git clone https://github.com/chrisdone/structured-haskell-mode.git
   $ cd structured-haskell-mode
   $ cabal install
   ```
2. Add `C:\Users\LauZi\AppData\Roaming\Cabal\bin\` to `PATH`

### Org-Mode ###
* [Cheatsheet](http://emacsclub.github.io/html/org_tutorial.html)
* `C-c .` Insert timestamp
* `C-c C-t` Mark TODO

### Ace-jump-mode ###
* `C-c SPC`/`C-c C-SPC` Jump to word
* `C-u C-c SPC` Jump to char
* `C-u C-u C-c SPC` Jump to line

### Projectile ###
* `C-c p f` Find file in project
* `C-c p g` Find text matches in project

### God-mode ###
* `C-z` Enter global god mode
* `xs` -> `C-x C-s`
* `x s` -> `C-x s`
* `gf` -> `M-f`
* `Gx` -> `C-M-x`

### Yasnippet ###
#### Snippet syntax ####
* `$1` First field
* `${1:n}` First field defaulted to n
* `$1 $1` First field is automatically copied
* `${1:${2:a} $2}` Nested templates
* `C-c C-t` Test current template
