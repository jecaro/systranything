{ haskellPackages
, libayatana-appindicator
, gobject-introspection
, gtk3
, stdenv
, fetchgit
}:
let
  haskell-gi-src = fetchgit ({
    url = "https://github.com/jecaro/haskell-gi.git";
    rev = "936b5ed0863a1663ea7775bc5f3ab4f9ae315ae9";
    sha256 = "sha256-SpcQDuIlyITOzPou1OnKbvdvK5TnYpsM/YR44Ljsfyg=";
  });

  genBuildInfo = haskellPackages.callCabal2nix "genBuildInfo"
    ("${haskell-gi-src}/bindings")
    { };

  src = stdenv.mkDerivation {
    name = "libayatana-appindicator-src";
    src = "${haskell-gi-src}/bindings/AyatanaAppIndicator3";

    buildInputs = [
      libayatana-appindicator
      gobject-introspection
      gtk3
    ];

    buildPhase = ''
      ${genBuildInfo}/bin/genBuildInfo ./
    '';

    installPhase = ''
      mkdir -p $out
      cp -r * $out
    '';
  };
in
haskellPackages.callCabal2nix
  "gi-ayatana-appindicator3"
  "${src}"
  # genBuildInfo translates the line in pkg.info:
  #   "pkgconfigDepends": "ayatana-appindicator3-0.1"
  # to the pkg-config dependency: "ayatana-appindicator3"
  # however the nixpkgs package is actually libayatana-appindicator
{ ayatana-appindicator3 = libayatana-appindicator; }
