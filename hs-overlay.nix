{ pkgs }: 

let
src-network-simple-ws = builtins.fetchGit {
  url = "https://github.com/k0001/network-simple-ws";
  rev = "ede14c952cbc648deda1aa7fd48aaf6f6d3b9c35";
};

in
pkgs.lib.composeExtensions
  (import "${src-network-simple-ws}/hs-overlay.nix" { inherit pkgs; })
  (self: super: {
    network-simple-wss = super.callPackage ./pkg.nix {};
    _shell = super.shellFor {
      withHoogle = false;
      buildInputs = [ pkgs.cabal-install ];
      packages = p: [ p.network-simple-wss ];
    };
  })
