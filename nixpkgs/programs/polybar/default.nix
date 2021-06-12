{config, pkgs, ...}:

let
    myPolybar = pkgs.polybar.override {
     alsaSupport = true;
     githubSupport = true;
     pulseSupport = true;
    };

    colors = builtins.readFile ./colors.ini;
    bars = builtins.readFile ./bars.ini;

    launchScript = builtins.readFile ./polybar.sh;

    xmonad = ''
    [module/xmonad]
    type = custom/script
    exec = ${pkgs.xmonad-log}/bin/xmonad-log

    tail = true
    '';
in
{

  home.packages = [
    pkgs.font-awesome
    ];

  services.polybar = {
    enable = true;
    package = myPolybar;
    script = "polybar example &";
    config = ./bars.ini;
    extraConfig = xmonad;
  };

  xsession.profileExtra = launchScript;
}
