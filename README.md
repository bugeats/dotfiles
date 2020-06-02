# bugeats/dotfiles

Now managed by [yadm](https://github.com/TheLocehiliosan/yadm), which is pretty nice.

My environment is based around [alacritty](https://github.com/jwilm/alacritty), [tmux](https://github.com/tmux/tmux), and [neovim](https://neovim.io/). [xmonad](https://xmonad.org/) on Linux. [Divvy](https://apps.apple.com/us/app/divvy-window-manager/id413857545?mt=12) on macOS.


## Key binding strategy

There are three nested layers in which operations are performed.

- OS Window Manager (xmonad, amethyst)
  - Application Context (tmux, chromium)
    - Application Operations (kakoune, neovim)

**Super** - OS Window Manager (xmonad, amethyst)
**Control** - Application / tmux
**Space** - Application Context / neovim

The goal is to have keys perform the same conceptual operation within the given layer.  Where *X* is the modifier to target a layer.

----

Key combos that feel good on the fingers (Kinesis Advantage custom layout) in order:

- **[Super] + [Shift]** - Target OS window manager
  - macos: amethyst
  - linux: xmonad 

- **[Alt] + [Shift]** - Target application context
  - chrome: tabs
  - tmux: tabs, panes

- **[Ctrl] + [Shift]** - Target application actions
  - kakoune: user mode bindings
  - neovim: leader bindings

By always using the **[Shift]** modifier, we avoid conflicts with most existing app and OS keybindings.


Keys that should be left alone (used by misc apps):

- Ctrl
- Alt
- Alt


### DONE

- **X-n** - (n)ew item (xmonad window, tmux session, neovim tab)
- **X-<number>** - jump to <number> (xmonad workspace, tmux tab, neovim tab)
- **X-0** - show all available (xmonad windows, tmux sessions, neovim buffers)
- **X-g** - to(g)gle minimalist layout (xmonad panel gap, tmux status bar, neovim focus mode)

### TODO

- **X-<number>** - jump to <number>
  - xmonad: jump to workspace <number>
  - tmux: jump to (or create) tab <number>
  - kakoune: jump to (or create) buffer <number> (TODO)
  - neovim: jump to tab <number>

- *<x>-j* - move down
  - xmonad: focus next window
  - tmux: focus next pane
  - kakoune: open next buffer

- *<x>-k* - move up
  - xmonad: focus prev window
  - tmux: focus prev pane
  - kakoune: open prev buffer

- *<x>-h* - move left
  - xmonad: expand window size
  - tmux: expand pane width
  - kakoune: ???

- *<x>-h* - move right
  - xmonad: shrink window size
  - tmux: shrink pane width
  - kakoune: ???

- *<x>-s* - split horizontally
  - xmonad: split into sub-layout horizontally
  - tmux: split pane horizontally
  - kakoune: ???

- *<x>-v* - split vertically
  - xmonad: split into sub-layout vertically
  - tmux: split pane vertically
  - kakoune: ???

- *<x>-g* - toggle minimalist layout
  - xmonad: show/hide desktop panel
  - tmux: show/hide status bar
  - neovim: toggle GoYo
  - kakoune: ???

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

*X-c* - kill / stop / SIGINT

--------------------------------------------------------------------------------

[MacOS:Command]
[MacOS:Option]
[MacOS:Control]

[Linux:Super]
[Linux:Alt]
[Linux:Ctrl]

Keys used by applications.

Kakoune
  - Alt
  - Ctrl

MacOS
  - Alt
  - Ctrl
  - Ctrl
