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
	];

	home.sessionVariables = {
		EDITOR = "vim";
    PATH = "$PATH:/home/zell/.bin/";
    LOCALES_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
	};

	home.packages = [
        autorandr
        zsh-powerlevel10k
        (lowPrio vim)
        albert
        ghc
        dmenu
        espanso
        exa
        fasd
        fontpreview
        jetbrains.idea-ultimate
        nerdfonts
        obsidian
        pavucontrol
        qtile
        slack
        spotify
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
