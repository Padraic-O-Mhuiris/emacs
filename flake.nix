{
  description = "Description for the project";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    emacs.url = "github:nix-community/emacs-overlay";
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

          tangle = pkgs.writeShellScriptBin "tangle" ''
            emacs -Q --batch --eval "
                 (progn
                   (require 'ob-tangle)
                   (dolist (file command-line-args-left)
                     (with-current-buffer (find-file-noselect file)
                       (org-babel-tangle))))
               " "$@"
          '';

          emacs = pkgs.emacsWithPackagesFromUsePackage {
            config = ""; # We don't want to create a default.el file
            defaultInitFile = false;
            package = pkgs.emacs-unstable.override { withGTK3 = true; };
            extraEmacsPackages = epkgs:
              with epkgs; [
                use-package
                evil
		evil-collection
                general
                which-key
                command-log-mode
                ivy
                ivy-rich
                counsel
                swiper
                doom-modeline
                doom-themes
		all-the-icons
                rainbow-delimiters
                helpful
		hydra
                # (treesit-grammars.with-grammars
                #   (g: with g; [ tree-sitter-rust tree-sitter-python ]))
              ];
          };

          literate-emacs = pkgs.writeShellScriptBin "emacs" ''
            TMP_DIR=$(mktemp -d -t ".emacs.dXXXX")
            cd $TMP_DIR
            ln -s ${./README.org} "$TMP_DIR/README.org"
            ${tangle}/bin/tangle ./README.org
            ls $TMP_DIR
            cd -

            ${emacs}/bin/emacs --init-directory $TMP_DIR $@
          '';

          normal-emacs = pkgs.writeShellScriptBin "emacs" ''
            TMP_DIR=$(mktemp -d -t ".emacs.dXXXX")
            ln -s ${./init.el} "$TMP_DIR/init.el"
            ls $TMP_DIR

            ${emacs}/bin/emacs --init-directory $TMP_DIR $@
          '';

        in {

          packages = {
            inherit emacs;
            default = normal-emacs;
          };
        };

      flake = let pkgs = (inputs.nixpkgs.legacyPackages.x86_64-linux);
      in {
        inherit (inputs.nixpkgs) lib;
        inherit pkgs;
      };
    };
}
