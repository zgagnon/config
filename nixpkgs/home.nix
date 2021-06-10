{ config, lib, pkgs, ... }:
with import <nixpkgs> {config = { allowUnfree = true; }; };

let pkgsUnstable = import <nixpkgs-unstable> {};

in

{
# Let Home Manager install and manage itself.
	programs.home-manager = {
		enable = true;
		path = "$HOME/rc";
	};

	imports = [
		./role/nixos/index.nix
		./programs/alacritty/default.nix
		./programs/git/default.nix
		./programs/vim/default.nix
		./programs/zsh/default.nix
		./programs/rofi/default.nix
        ./programs/polybar/default.nix
        ./programs/xmonad/default.nix
	];

	home.sessionVariables = {
		EDITOR = "vim";
    PATH = "$PATH:/home/zell/.bin/";
    LOCALES_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
	};

	home.packages = [
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
        ffmpeg
        fontpreview
        htop
        jetbrains.idea-ultimate
        nerdfonts
        nitrogen
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
	];
# Home Manager needs a bit of information about you and the
# paths it should manage.
	home.username = "zell";
	home.homeDirectory = "/home/zell";

	programs.autorandr = {
		enable = true;
	};
	programs.direnv = {
		enable = true;
		enableNixDirenvIntegration = true;
		enableZshIntegration = true;
	};

	services.syncthing.enable = true;

  xsession = {
    enable = true;

    profileExtra = builtins.concatStringsSep "\n" [
      "autorandr --change"
      "nitrogen --random --set-centered --head=0 wallpaper"
      "nitrogen --random --set-centered --head=1 wallpaper"
    ];
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
    };
  };

# This value determines the Home Manager release that your
# configuration is compatible with. This helps avoid breakage
# when a new Home Manager release introduces backwards
# incompatible changes.
#
# You can update Home Manager without changing this value. See
# the Home Manager release notes for a list of state version
# changes in each release.
	home.stateVersion = "21.03";
}
