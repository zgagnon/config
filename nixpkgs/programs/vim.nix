{ config, lib, pkgs, ... }:
with import <nixpkgs> {config = { allowUnfree = true; }; };

{
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
}
