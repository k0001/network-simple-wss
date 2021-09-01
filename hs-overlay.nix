{ pkgs }: 

self: super: {
  network-simple-wss = super.callPackage ./pkg.nix {};
  _shell = super.shellFor {
    withHoogle = false;
    buildInputs = [ pkgs.cabal-install ];
    packages = p: [ p.network-simple-wss ];
  };
}
