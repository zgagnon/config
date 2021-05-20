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
		./programs/alacritty.nix
	];

	home.sessionVariables = {
		EDITOR = "vim";
	};

	home.packages = [
		autorandr
		zsh-powerlevel10k
			(lowPrio vim)
      ghc
			dmenu
      espanso
			exa
			fasd
			obsidian
			pavucontrol
      picom
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
      xorg.xmodmap
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

	programs.zsh = {
		enable = true;
		enableAutosuggestions = true;
		enableVteIntegration = true;
		dotDir = "rc";
		initExtra = builtins.concatStringsSep "\n" [
			"source ${pkgs.zsh-powerlevel10k}/share/zsh-powerlevel10k/powerlevel10k.zsh-theme"
			"[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh"
			''eval "$(fasd --init auto)"''						
      "alias pbcopy='xclip -selection clipboard'"
      "alias pbpaste='xclip -selection clipboard -o'"
		];

		history = {
			ignoreSpace = true;
			ignoreDups = true;
		};

		oh-my-zsh = {
			enable = true;
			plugins = [ "git" "fasd" ];
			theme = "powerlevel10k";
		};
	};

	programs.vim = {
		enable = true;
		plugins = with pkgs.vimPlugins; [
			molokai
				vim-colors-solarized
				repeat
				vim-surround
				Tabular
				airline
				rainbow_parentheses
				nerdtree
				vim-ruby
				vim-javascript
				vim-markdown
				vim-nix
		];

		extraConfig = builtins.concatStringsSep "\n" [
			"filetype plugin indent on"
				"syntax on"
		];
		settings = {
			number = true;
			tabstop = 2;
			background = "dark";
		};
	};

	programs.git = {
		enable = true;
		aliases = {
			gst = "git";
			st = "status";
			di = "diff";
			co = "checkout";
			ci = "commit";
			br = "branch";
			sta = "stash";
			llog = "log --date=local";
			flog = "log --pretty=fuller --decorate";
			lg = "log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit --date=relative";
			lol = "log --graph --decorate --oneline";
			lola = "log --graph --decorate --oneline --all";
			blog = "log origin/master... --left-right";
			ds = "diff";
			update = "commit --amend --no-edit";
			fixup = "commit";
			squash = "commit";
			unstage = "reset";
			rum = "rebase master@{u}";
		};
		userEmail = "zoe@zgagnon.com";
		userName = "Zoe Gagnon";
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
