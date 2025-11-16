{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";
  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" ];

      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);

      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit system;
        overlays = [ self.overlay ];
      });

    in
    {
      overlay = (final: prev:
        {
          systranything = prev.haskellPackages.callCabal2nix
            "systranything" ./.
            { };
        });

      packages = forAllSystems
        (system:
          {
            systranything = nixpkgsFor.${system}.systranything;
          });

      defaultPackage = forAllSystems
        (system:
          self.packages.${system}.systranything
        );

      checks = self.packages;

      devShells = forAllSystems
        (system:
          let pkgs = nixpkgsFor.${system};
          in
          {
            # Default shell for development
            default = pkgs.haskellPackages.shellFor {
              packages = p: [ self.packages.${system}.systranything ];
              withHoogle = true;
              buildInputs = [
                pkgs.haskellPackages.cabal-install
                pkgs.haskellPackages.ghcid
                pkgs.haskellPackages.haskell-ci
                pkgs.haskellPackages.haskell-language-server
                pkgs.zenity
              ];
            };
          } //
          # Additional shells for the compiler versions we support
          pkgs.lib.attrsets.genAttrs [ "ghc948" "ghc964" "ghc982" ] (
            ghc: pkgs.haskell.packages.${ghc}.shellFor {
              # No package in scope. One need to call `cabal update` and start
              # for here.
              packages = p: [ ];
              buildInputs = [
                pkgs.haskellPackages.cabal-install

                # dependencies of haskell-gi and the ayatana bindings
                pkgs.gobject-introspection
                pkgs.gtk3
                pkgs.lerc
                pkgs.libayatana-appindicator
                pkgs.libdatrie
                pkgs.libepoxy
                pkgs.libselinux
                pkgs.libsepol
                pkgs.libsysprof-capture
                pkgs.libthai
                pkgs.libxkbcommon
                pkgs.pcre2
                pkgs.pkg-config
                pkgs.util-linux
                pkgs.xorg.libXdmcp
                pkgs.xorg.libXtst
              ];
            }
          )
        );
    };
}
