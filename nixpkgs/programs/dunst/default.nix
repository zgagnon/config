{ config, lib, pkgs,  ...}:

{
    services.dunst = {
        enable = true;
        iconTheme = {
          name = "Adwaita";
          package = pkgs.gnome3.adwaita-icon-theme;
          size = "16x16";
        };
        settings = {
          global = {
            monitor = 1;
            geometry = "500x50-50+65";
            transparency = 10;
            frame_color="#518AA8";
            frame_width= 4;
            padding = 16;
            separator_height = 4;
            separator_color = "auto";
            browser = "${pkgs.vivaldi}/bin/vivaldi";
            horizontal_padding = 16;
            font = "JetBrainsMono Nerd Font 20";
            line_height = 4;
            format = ''
            %a
            <b>%s</b>
            %b
            %p'';
          };
          urgency_low = {
            background = "#BDA584";
            foreground = "#726765";
          };

          urgency_normal = {
            background = "#BDA584";
            foreground = "#726765";
          };
        };
      };
}