with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "deptt";

  buildInputs = with pkgs; [
    hlint
    stack
  ];
}
