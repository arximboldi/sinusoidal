with import <nixpkgs> {};

stdenv.mkDerivation rec {
  name = "sinusoid.al";
  buildInputs = [
    sassc
    zlib
    gnumake
    (haskellPackages.ghcWithPackages (p: with p; [
      hakyll
    ]))
  ];
}
