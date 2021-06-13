with import <nixpkgs> {};

stdenv.mkDerivation {
    name = "auction_house";
    buildInputs = [
	zsh
  python
  jetbrains.jdk
    ];
}
