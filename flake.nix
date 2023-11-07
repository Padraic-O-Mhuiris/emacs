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

          emacs = pkgs.emacsWithPackagesFromUsePackage {
            config = ""; # We don't want to create a default.el file
            defaultInitFile = false;
            package = pkgs.emacs-unstable.override { withGTK3 = true; };
            extraEmacsPackages = epkgs:
              with epkgs; [
                visual-fill-column
                embark
                org-roam
                org-cliplink
                embark-consult
                consult
                consult-projectile
                consult-org-roam
                consult-notes
                consult-dir
                esup
                highlight-numbers
                marginalia
                org-bullets
                smartparens
                undo-tree
                helpful
                vertico
                ace-window
                corfu
                orderless
                all-the-icons
                projectile
                doom-modeline
                doom-themes
                modus-themes
                evil
                evil-collection
                general
                magit
                nix-mode
                no-littering
                org
                org-contrib
                rainbow-delimiters
                which-key
                ts
                persp-mode
                bufler
                envrc
                lsp-mode
                rustic
                tree-sitter-langs
                lsp-ui
                flycheck
                consult-lsp
                ox-hugo
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
                    rust-analyzer
                    (aspellWithDicts
                      (dicts: with dicts; [ en en-computers en-science ]))
                  ];
                };
              })
            ];
          });

        in { packages.default = wrapped-emacs; };

      flake = let pkgs = (inputs.nixpkgs.legacyPackages.x86_64-linux);
              in {
                inherit (inputs.nixpkgs) lib;
                inherit pkgs;
              };
    };
}
