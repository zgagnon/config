{config, pkgs, ...}:

let
    myPolybar = pkgs.polybar.override {
     alsaSupport = true;
     githubSupport = true;
     pulseSupport = true;
    };

    bars = builtins.readFile ./bars.ini;

    launchScript = builtins.readFile ./polybar.sh;

    colors = {
        background = "#555FB1";
        background-alt = "#504945";
        foreground = "#D4721C";
        foreground-alt = "#FFFF00";


        bgs = [
         "#705f57"
         "#84b6c0"
         "#e5d9ca"
         "#37899a"
         "#c1b9c4"
        ];



        red = "#fb4934";
        green = "#b8bb26";
        yellow = "#fabd2f";
        blue = "#83a598";
        purple = "#d3869b";
        aqua = "#8ec07c";
        orange = "#fe8019";
    };

    colorBar = text: bg: "%{F${builtins.elemAt colors.bgs text}}%{B${builtins.elemAt colors.bgs bg}}";
    sep = text: bg: colorBar text bg + "%{T2} %{T-}";
in
{

  home.packages = [
    pkgs.font-awesome
    ];

  services.polybar = {
    enable = true;
    package = myPolybar;
    script = ''
      polybar example &
      polybar left &
      '';
    settings = {
        "module/xmonad" = {
            type = "custom/script";
            exec = "${pkgs.xmonad-log}/bin/xmonad-log";
            tail = true;

            format = "<label>";
            # See the XMonad config for font colors in the script output
            label = colorBar 4 0 + "%{T1}  "+
                sep 0 1
                + "%output%"
                + sep 1 2;
        };

        "module/cpu" = {
            type = "internal/cpu";
            interval = 2;
            format-prefix-foreground = colors.foreground-alt;
            label = sep 2 3 + " %percentage%%";
            format-padding = 2;
            format-margin = 0;
            format-spacing = 0;
            format-offset = -2;
        };

        "module/date" = {
            type = "internal/date";
            interval = 1;

            date = " %b %d";
            date-alt = " %Y-%m-%d";

            time = "%H:%M:%S";
            time-alt = "%H:%M:%S";

            format = sep 3 4 + " <label>";
            format-background = builtins.elemAt colors.bgs 4;
            format-offset = -36;
            label =  "%date% %time%";
        };

        "module/pulseaudio" = {
            type = "internal/pulseaudio";
        };

        "global/wm" = {
            margin-top = 0;
            margin-bottom = 0;
        };

        "bar/example" = {
            monitor = "\${env:MONITOR:DP-3}";
            monitor-fallback = "\${env:MONITOR:DP-2}";
            width = "100%";
            height = 24;
            offset-x = 0;
            offset-y = 0;

            background = builtins.elemAt colors.bgs 2;
            foreground = colors.foreground;

            overline-size = 2;
            underline-size = 2;

            border-bottom = 0;
            border-bottom-color = "#333";

            spacing = 1;
            padding-left = 0;
            module-margin-left = 1;
            module-margin-right = 2;

            font-0 = "MesloLGM Nerd Font:pixelsize=16;0";
            font-1 = "MesloLGM Nerd Font Mono:pixelsize=18;0";

            modules-left = "xmonad";
            modules-center = "";
            modules-right = "pulseaudio cpu date";

            tray-position = "center";
            tray-padding = 2;
        };
        "bar/left" = {
            monitor = "\${env:MONITOR:HDMI-1}";
            monitor-fallback = "\${env:MONITOR:DP-2}";
            width = "100%";
            height = 24;
            offset-x = 0;
            offset-y = 0;

            background = builtins.elemAt colors.bgs 2;
            foreground = colors.foreground;

            overline-size = 2;
            underline-size = 2;

            border-bottom = 0;
            border-bottom-color = "#333";

            spacing = 1;
            padding-left = 0;
            module-margin-left = 1;
            module-margin-right = 2;

            font-0 = "MesloLGM Nerd Font:pixelsize=16;0";
            font-1 = "MesloLGM Nerd Font Mono:pixelsize=18;0";

            modules-left = "xmonad";
            modules-center = "";
            modules-right = "";

        };
    };
  };

  xsession.profileExtra = launchScript;
}
