{ config, lib, pkgs, ... }:
with import <nixpkgs> {config = { allowUnfree = true; }; };

{
	programs.git = {
		enable = true;
		aliases = {
			gst = "git";
			st = "status";
			di = "diff";
			co = "checkout";
			ci = "commit";
			br = "branch";
			sta = "stash";
			llog = "log --date=local";
			flog = "log --pretty=fuller --decorate";
			lg = "log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit --date=relative";
			lol = "log --graph --decorate --oneline";
			lola = "log --graph --decorate --oneline --all";
			blog = "log origin/master... --left-right";
			ds = "diff";
			update = "commit --amend --no-edit";
			fixup = "commit";
			squash = "commit";
			unstage = "reset";
			rum = "rebase master@{u}";
		};
		userEmail = "zoe@zgagnon.com";
		userName = "Zoe Gagnon";
	};

}
