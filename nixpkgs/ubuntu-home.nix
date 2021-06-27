{ config, lib, pkgs, ... }:
with import <nixpkgs> {config = { allowUnfree = true; }; };

let
  pkgsUnstable = import <nixpkgs-unstable> {};

  polybar = import ./programs/polybar/default.nix {
      pkgs = pkgs;

      mainMonitor = "\${env:MONITOR:DP-3}";
      sideMonitor = "\${env:MONITOR:HDMI-1}";
    };

    xmonad = import ./programs/xmonad/default.nix {
      pkgs = pkgs;
      terminal = "nixGL alacritty";
    };

    universalPackages = import ./programs/packages.nix;

    packages = polybar.packages
    ++ xmonad.packages
    ++ universalPackages;
in

{
	imports = [
  ./session.nix

  ./programs/home-manager/default.nix
  ./programs/git/default.nix
  ./programs/vim/default.nix
  ./programs/zsh/default.nix
  ./programs/rofi/default.nix
  ./programs/syncthing/default.nix
  ./programs/dunst/default.nix
	];

  programs.alacritty = import ./programs/alacritty/default.nix {shell = "/home/zell/.nix-profile/bin/zsh";};

  # Configure polybar based on this machine's monitors
  services.polybar = polybar.config;

  home.sessionVariables = {
    EDITOR = "emacs";
    PATH = "$PATH:/home/zell/.bin/";
    LOCALES_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
  };

	home.packages = packages;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
	home.username = "zell";
	home.homeDirectory = "/home/zell";

	programs.autorandr = {
		enable = true;
	};

	programs.direnv = {
		enable = true;
		nix-direnv.enable = true;
		enableZshIntegration = true;
	};

  programs.emacs = {
    enable = true;
  };

  services.pasystray.enable = true;
  xsession = {
      enable = true;

      profileExtra = builtins.concatStringsSep "\n" [
          "autorandr --change"
          "feh --bg-fill --randomize wallpaper/"
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
