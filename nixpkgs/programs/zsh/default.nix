{ config, lib, pkgs, ... }:
with import <nixpkgs> {config = { allowUnfree = true; }; };

{
	programs.zsh = {
		enable = true;
		enableAutosuggestions = true;
		enableVteIntegration = true;
		dotDir = "rc";
		initExtra = builtins.concatStringsSep "\n" [
			"source ${pkgs.zsh-powerlevel10k}/share/zsh-powerlevel10k/powerlevel10k.zsh-theme"
			"[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh"
			''eval "$(fasd --init auto)"''						
      "alias pbcopy='xclip -selection clipboard'"
      "alias pbpaste='xclip -selection clipboard -o'"
		];

		history = {
			ignoreSpace = true;
			ignoreDups = true;
		};

		oh-my-zsh = {
			enable = true;
			plugins = [ "git" "fasd" ];
			theme = "powerlevel10k";
		};
	};
}
