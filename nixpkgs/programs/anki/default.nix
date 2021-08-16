with import <nixpkgs>{};

stdenv.mkDerivation rec {
        version = "2.1.35";
        name = "anki-${version}-binary";


        src = fetchurl {
            name = "anki-${version}-linux-amd64.tar.bz2";
            url = "https://github.com/ankitects/anki/releases/download/${version}/anki-${version}-linux-amd64.tar.bz2";
            sha256 = "c0429a5e3a91ae1edf57f4af779072f4337056e363f854dc3db300d3c4eb6b02";
        };

        nativBuildInputs = [
            autoPatchelfHook
        ];

        buildInputs = [
            glibc
            xdg-utils
        ];

#        unpackPhase = true;

        installPhase = ''
            patchelf --set-interpreter ${glibc.out}/lib/ld-linux-x86-64.so.2 bin/anki
            mkdir -p $out
            mkdir -p $out/share/anki
            cp -av * $out/share/anki


            mkdir $out/bin
            ln -sf $out/share/anki/bin/anki $out/bin/

            mkdir -p $out/share/pixmaps
            mkdir -p $out/share/applications
            mkdir -p $out/share/man/man1

            cd $out/share/anki && (\
            	mv anki.xpm anki.png $out/share/pixmaps/;\
            	mv anki.desktop $out/share/applications/;\
            	mv anki.1 $out/share/man/man1/)

            	echo
            	echo "Install complete. Type 'anki' to run."


        '';
        meta = with lib; {
            homepage = "https://studio-link.com";
            description = "Voip transfer";
            platforms = platforms.linux;
          };
}