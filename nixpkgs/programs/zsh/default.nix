{ config, lib, pkgs, ... }:
with import <nixpkgs> {config = { allowUnfree = true; }; };

{
	programs.zsh = {
		enable = true;
		enableAutosuggestions = true;
		enableVteIntegration = true;
		initExtra = builtins.concatStringsSep "\n" [
            "source ${pkgs.zsh-powerlevel10k}/share/zsh-powerlevel10k/powerlevel10k.zsh-theme"
            "[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh"
            ''eval "$(fasd --init auto)"''
            "alias pbcopy='xclip -selection clipboard'"
            "alias pbpaste='xclip -selection clipboard -o'"
            "export NIX_PATH=$HOME/.nix-defexpr/channels\${NIX_PATH:+:}$NIX_PATH"
		];

		history = {
			ignoreSpace = true;
			ignoreDups = true;
		};

    plugins = [
      {
        name = "zsh-nix-shell";
        file = "nix-shell.plugin.zsh";
        src = pkgs.fetchFromGitHub {
          owner = "chisui";
          repo = "zsh-nix-shell";
          rev = "v0.4.0";
          sha256 = "037wz9fqmx0ngcwl9az55fgkipb745rymznxnssr3rx9irb6apzg";
        };
      }
    ];

		oh-my-zsh = {
			enable = true;
      plugins = [ 
        "git" 
        "fasd" 
      ];
			theme = "powerlevel10k";
		};
	};
}
