{ pkgs }:

let
src-network-simple-tls = builtins.fetchGit {
  url = "https://github.com/k0001/network-simple-tls";
  rev = "bdce8a8e7b7d29a8c4ae98d1b65d15f5d4ed16ed";
};
src-network-simple-ws = builtins.fetchGit {
  url = "https://github.com/k0001/network-simple-ws";
  rev = "07dbf69a119682ce5594821ae86998f3e3499e88";
};

in
pkgs.lib.composeExtensions
  (import "${src-network-simple-ws}/hs-overlay.nix" { inherit pkgs; })
  (pkgs.lib.composeExtensions
    (import "${src-network-simple-tls}/hs-overlay.nix" { inherit pkgs; })
    (self: super: {
      network-simple-wss = super.callPackage ./pkg.nix {};
    }))
