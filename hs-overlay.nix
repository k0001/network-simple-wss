{ pkgs }: 

let
src-network-simple-ws = builtins.fetchGit {
  url = "https://github.com/k0001/network-simple-ws";
  rev = "f979498301857a831628e113494a367919a04ef0";
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
