{ config, lib, pkgs, ... }:
with import <nixpkgs> {config = { allowUnfree = true; }; };

{
    programs.alacritty = {
        enable = true;
        settings = {
        shell = {
          program = "/home/zell/.nix-profile/bin/zsh";
        };
         font = {
                        normal = {
                            family = "MesloLGS NF";
                            style = "Regular";
                            size = 18;
                        };
                    };
            colors = {
                cursor = {
                    text = "#FF0000";
                    cursor = "#00FFFF";
                };
            };
            background_opacity = 0.7;

        };
    };
}
