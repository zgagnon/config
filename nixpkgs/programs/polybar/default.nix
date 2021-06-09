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
in
{
  services.polybar = {
    enable = true;
    package = myPolybar;
    script = "polybar mainbar &";
    extraConfig = bars + colors;
  };

  xsession.profileExtra = launchScript;
}
