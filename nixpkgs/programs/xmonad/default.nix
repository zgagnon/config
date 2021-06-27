{ pkgs, terminal }:

let
  xmonadConfig = builtins.readFile ./xmonad.hs;
  interpolatedConfig = builtins.replaceStrings
    ["<TERMINAL>"]
    [terminal]
    xmonadConfig;
in
{

    packages = with pkgs; [
        xmonad-log
    ];

 xmonad = {
     enable = true;
     enableContribAndExtras = true;
     config =  pkgs.writeText "xmonad-final.hs" interpolatedConfig;
     extraPackages = haskellPackages: [
        pkgs.haskellPackages.dbus
    ];
     libFiles = {
        "Config.hs" = lib/Config.hs;
     };
   };
}
