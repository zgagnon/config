with import <nixpkgs> {};

stdenv.mkDerivation {
    name = "auction_house";
    buildInputs = [
	zsh
    jetbrains.jdk
    ];
}
