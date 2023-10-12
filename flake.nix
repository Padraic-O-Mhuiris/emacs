{
  description = "Description for the project";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    emacs.url = "github:nix-community/emacs-overlay";


    wrapper-manager = {
      url = "github:Padraic-O-Mhuiris/wrapper-manager";
      inputs.nixpkgs.follows = "nixpkgs";
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

          # tangle = pkgs.writeShellScriptBin "tangle" ''
          #   emacs -Q --batch --eval "
          #        (progn
          #          (require 'ob-tangle)
          #          (dolist (file command-line-args-left)
          #            (with-current-buffer (find-file-noselect file)
          #              (org-babel-tangle))))
          #      " "$@"
          # '';

          emacs = pkgs.emacsWithPackagesFromUsePackage {
            config = ""; # We don't want to create a default.el file
            defaultInitFile = false;
            package = pkgs.emacs-unstable.override { withGTK3 = true; };
            extraEmacsPackages = epkgs:
              with epkgs; [
                visual-fill-column
                embark
                embark-consult
                esup
                highlight-numbers
                marginalia
                org-bullets
                smartparens
                undo-tree
                vertico
                ace-window
                corfu
                orderless
                all-the-icons
                doom-modeline
                doom-themes
                evil
                evil-collection
                general
                magit
                nix-mode
                no-littering
                org
                rainbow-delimiters
                which-key
              ];
          };

          wrapped-emacs = (inputs.wrapper-manager.lib.build {
            inherit pkgs;
            modules = [
              ({ pkgs, ... }: {
                wrappers.emacs = {
                  basePackage = emacs;
                  pathAdd = with pkgs; [
                    (ripgrep.override { withPCRE2 = true; })
                    shellcheck
                    shfmt
                    fd
                    (aspellWithDicts
                      (dicts: with dicts; [ en en-computers en-science ]))
                    emacsPackages.editorconfig
                    terraform
                    graphviz
                    maim
                    html-tidy
                    nodePackages_latest.stylelint
                    nodePackages_latest.js-beautify
                    nixfmt
                  ];
                };
              })
            ];
          });

          # literate-emacs = pkgs.writeShellScriptBin "emacs" ''
          #   TMP_DIR=$(mktemp -d -t ".emacs.dXXXX")
          #   cd $TMP_DIR
          #   ln -s ${./README.org} "$TMP_DIR/README.org"
          #   ${tangle}/bin/tangle ./README.org
          #   ls $TMP_DIR
          #   cd -

          #   ${emacs}/bin/emacs --init-directory $TMP_DIR $@
          # '';

          # my-emacs = pkgs.writeShellScriptBin "emacs" ''
          #   ${wrapped-emacs}/bin/emacs --debug-init $@ 
          # '';

        in { packages.default = wrapped-emacs; };

      flake = let pkgs = (inputs.nixpkgs.legacyPackages.x86_64-linux);
              in {
                inherit (inputs.nixpkgs) lib;
                inherit pkgs;
              };
    };
}
