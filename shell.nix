with import <nixpkgs> {};

stdenv.mkDerivation rec {
  name = "sinusoid.al";
  buildInputs = [
    sass
    zlib
    gnumake
    (haskellPackages.ghcWithPackages (p: with p; [
      hakyll
    ]))
  ];
}
