{config, pkgs, ...}:

# Configuration for shell sessions
{
    home.sessionVariables = {
        EDITOR = "emacs";
        PATH = "$PATH:/home/zell/.bin/";
        LOCALES_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
    };
}