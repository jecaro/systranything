{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-24.05";
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
        let
          gi-ayatana-appindicator3 = prev.haskellPackages.callHackageDirect
            {
              pkg = "gi-ayatana-appindicator3";
              ver = "0.1.0";
              sha256 = "sha256-Jcv6hUtyCvfkgbvQqo5H+J8h9rrPAkQdOeheWcqMphg=";
            }
            {
              ayatana-appindicator3 = prev.libayatana-appindicator;
            };
        in
        {
          # genBuildInfo translates the line in pkg.info:
          #   "pkgconfigDepends": "ayatana-appindicator3-0.1"
          # to the pkg-config dependency: "ayatana-appindicator3"
          # however the nixpkgs package is actually libayatana-appindicator
          systranything = prev.haskellPackages.callCabal2nix
            "systranything" ./.
            {
              inherit gi-ayatana-appindicator3;
            };
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

      devShell = forAllSystems (system:
        let pkgs = nixpkgsFor.${system};
        in
        pkgs.haskellPackages.shellFor {
          packages = p: [ self.packages.${system}.systranything ];
          withHoogle = true;
          buildInputs = [
            pkgs.gnome.zenity
            pkgs.haskellPackages.cabal-install
            pkgs.haskellPackages.ghcid
            pkgs.haskellPackages.haskell-ci
            pkgs.haskellPackages.haskell-language-server
          ];
        });
    };
}
