{ pkgs ? import <nixpkgs> { }, ocamlPackages ? pkgs.ocamlPackages }:

pkgs.mkShell {
  buildInputs = [
    ocamlPackages.ocaml
    pkgs.dune_3
    ocamlPackages.ocaml-lsp
    ocamlPackages.odoc
  ];
}
