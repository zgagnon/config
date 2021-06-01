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
          family = "MesloLGM Nerd Font";
          style = "Regular";
          size = 16;
          };
          };
          colors = {
            primary = {
              background = "#000000";
              foreground = "#dddddd";
            };
            normal = {
              black = "#000000";
              red = "#cc0403";
              green = "#19cb00";
              yellow = "#cecb00";
              blue = "#0d73cc";
              magenta = "#cb1ed1";
              cyan = "#0dcdcd";
              white = "#dddddd";
            };
            bright = {
              black = "#767676";
              red = "#f2201f";
              green = "#23fd00";
              yellow = "#fffd00";
              blue = "#1a8fff";
              magenta = "#fd28ff";
              cyan = "#14ffff";
              white = "#ffffff";
            };
            cursor = {
              text = "#FF0000";
              cursor = "#00FFFF";
            };
          };
          background_opacity = 0.7;

        };
      };
    }
