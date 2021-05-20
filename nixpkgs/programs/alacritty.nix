{ config, lib, pkgs, ... }:
with import <nixpkgs> {config = { allowUnfree = true; }; };

{
    programs.alacritty = {
        enable = true;
        settings = {
              colors = {
                cursor = {
                    text = "#FF0000";
                    cursor = "#00FF00";
                };

              };
            background_opacity = 0.7;
        };
    };
}
