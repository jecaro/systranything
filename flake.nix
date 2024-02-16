{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-23.11";
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
            {
              gi-ayatana-appindicator3 = prev.callPackage
                ./nix/gi-ayatana-appindicator3.nix
                { };
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
            pkgs.haskellPackages.haskell-language-server
            pkgs.haskellPackages.ghcid
            pkgs.haskellPackages.cabal-install
          ];
        });
    };
}
