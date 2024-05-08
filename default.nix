let
  onix = import (builtins.fetchGit {
    url = "https://github.com/rizo/onix.git";
    rev = "6c132df73185efbb46b36a1df54e3cd37bdde5f5";
  }) { verbosity = "info"; };

in onix.env {
  path = ./.;

  vars = {
    "with-test" = true;
    "with-doc" = true;
    "with-dev-setup" = true;
  };

  deps = {
    "ocaml-system" = "*";
    "ocamlformat" = "*";
    "ocaml-lsp-server" = "*";
  };
}
