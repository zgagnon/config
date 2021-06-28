{config, pkgs, ...}:

{
    programs.rofi = {
        enable = true;
        terminal = "\${pkgs.alacritty}/bin/alacritty";

        extraConfig = {
            modi = "drun,combi";
        };

    };
}