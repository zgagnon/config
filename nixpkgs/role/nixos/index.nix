{ config, lib, pkgs, ... }:

{
  programs.alacritty = {
		enable = true;
		settings = {
			font.size = 16;
			shell.program = "/home/zell/.nix-profile/bin/zsh";
		};
  };
}
