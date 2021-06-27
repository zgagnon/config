{pkgs, pkgsUnstable}:
    
with pkgs; [
  _1password-gui
  autorandr
  zsh-powerlevel10k
  (lowPrio vim)
  albert
  ghc
  pkgsUnstable.deadd-notification-center
  discord
  dmenu
  espanso
  exa
  fasd
  feh
  ffmpeg
  flameshot
  fontpreview
  font-awesome
  mesa-demos
  htop
  pkgsUnstable.jetbrains.idea-ultimate
  pkgsUnstable.jetbrains.jdk
  libnotify
  nerdfonts
  nitrogen
  nomacs
  obsidian
  openvpn
  pavucontrol
  picom
  qtile
  slack
  spotify
  xfce.thunar
  trayer
  vimPlugins.Tabular
  vimPlugins.airline
  vimPlugins.molokai
  vimPlugins.nerdtree
  vimPlugins.rainbow_parentheses
  vimPlugins.repeat
  vimPlugins.vim-colors-solarized
  vimPlugins.vim-javascript
  vimPlugins.vim-markdown
  vimPlugins.vim-nix
  vimPlugins.vim-ruby
  vimPlugins.vim-surround
  vivaldi
  pkgsUnstable.wally-cli
  xclip
  zoom-us
]