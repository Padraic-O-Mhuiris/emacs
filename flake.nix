{
  description = "Description for the project";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    emacs.url = "github:nix-community/emacs-overlay";

    from-elisp = {
      url = "github:o-santi/from-elisp";
      flake = false;
    };
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems =
        [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin" ];

      perSystem = { config, self', inputs', system, ... }:
        let
          pkgs = import inputs.nixpkgs {
            inherit system;
            overlays = [ inputs.emacs.overlay ];
          };

          org-tangle = block-predicate: text:
            let
              blocks = (pkgs.callPackage inputs.from-elisp {
                inherit pkgs;
              }).parseOrgModeBabel text;
              block-to-str = (block:
                if block-predicate { inherit (block) language flags; } then
                  block.body
                else
                  "");
            in builtins.concatStringsSep "\n" (map block-to-str blocks);

          org-tangle-elisp-blocks = org-tangle ({ language, flags }:
            let
              is-elisp = (language == "emacs-lisp") || (language == "elisp");
              is-tangle = if flags ? ":tangle" then
                flags.":tangle" == "yes" || flags.":tangle" == "y"
              else
                false;
            in is-elisp && is-tangle);

          config = pkgs.writeText "config.el"
            (org-tangle-elisp-blocks (builtins.readFile ./README.org));

          emacs = pkgs.emacsWithPackagesFromUsePackage {
            inherit config;
            alwaysEnsure = true;
            defaultInitFile = true;
            package = pkgs.emacs-unstable.override { withGTK3 = true; };
            extraEmacsPackages = epkgs:
              with epkgs;
              [
                (treesit-grammars.with-grammars
                  (g: with g; [ tree-sitter-rust tree-sitter-python ]))
              ];
          };

        in {

          packages = {
            inherit config;
            showConfig = pkgs.writeShellScriptBin "showConfig" ''
              cat ${config}
            '';

            inherit emacs;

            default = pkgs.writeShellScriptBin "emacs" ''
              ${emacs}/bin/emacs -q -l ${config} $@
            '';
          };
        };

      flake = let

        pkgs = (inputs.nixpkgs.legacyPackages.x86_64-linux);
      in {
        inherit (inputs.nixpkgs) lib;
        inherit pkgs;
      };
    };
}
