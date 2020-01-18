# bugeats/dotfiles

Now managed by [yadm](https://github.com/TheLocehiliosan/yadm), which is pretty nice.

My environment is based around [alacritty](https://github.com/jwilm/alacritty), [tmux](https://github.com/tmux/tmux), and [neovim](https://neovim.io/). [xmonad](https://xmonad.org/) on Linux. [Divvy](https://apps.apple.com/us/app/divvy-window-manager/id413857545?mt=12) on macOS.


## Key binding strategy

There are three nested layers in which operations are performed.

**Super** - Window Manager / xmonad
**Control** - Application / tmux
**Space** - Application Context / neovim

The goal is to have keys perform the same conceptual operation within the given layer.  Where *X* is the modifier to target a layer.


### DONE

- **X-n** - (n)ew item (xmonad window, tmux session, neovim tab)
- **X-<number>** - jump to <number> (xmonad workspace, tmux tab, neovim tab)
- **X-0** - show all available (xmonad windows, tmux sessions, neovim buffers)
- **X-g** - to(g)gle minimalist layout (xmonad panel gap, tmux status bar, neovim focus mode)


### TODO

*X-j* - move focus down (xmonad window, tmux tab, neovim tab)
*X-k* - " up
*X-h* - " left
*X-l* - " right

*X-Tab* - rotate between (xmonad window, tmux tab, neovim tab)
*X-Shift-Tab* - " reversed

*X-.* - rotate layout (xmonad layout, tmux ???, neovim ???)
*X-,* - " reversed

*X-Up* - move workspace focus (xmonad display, tmux window)
*X-Down* - " reversed

*X-Left* - resize workspace (xmonad display, tmux ???, neovim window)
*X-Right* - " reversed

*X-w* - close focused item (window/tab)
*X-n* - new item (window/tab)
*X-/* - deep search
*X-p* - fuzzy search
*X-:*, *X-;* - Command Prompt

*X-q* - soft quit (confirm)
*X-Shift-q* - hard quit

*X-s* - split horizontally (xmonad window, tmux window, neovim window)
*X-v* - " vertically

*X-g* - toggle minimalist layout (xmonad gap, tmux chrome, neovim GoYo)

*X-c* - kill / stop / SIGINT
