{ pkgs }:

let
hs = pkgs.haskell.lib;

src-network-simple-tls = builtins.fetchGit {
  url = "https://github.com/k0001/network-simple-tls";
  rev = "68a152e8e834ed3e0b5f5cac9d60927df385e682";
};

src-websockets = builtins.fetchTarball {
  url = "https://github.com/k0001/websockets/archive/bb96a46734d1c49499d20754518a25744d5de14f.tar.gz";
  sha256 = "09x6zqaada7dvxhq02wsqw48qna0ibfr7aa31wy0fcnqkqm4wqri";
};

pkg-websockets =
  { mkDerivation, attoparsec, base, base64-bytestring, binary
  , bytestring, bytestring-builder, case-insensitive, containers
  , criterion, entropy, HUnit, network, QuickCheck, random, SHA
  , stdenv, streaming-commons, test-framework, test-framework-hunit
  , test-framework-quickcheck2, text
  }:
  mkDerivation {
    pname = "websockets";
    version = "0.12.4.2";
    src = src-websockets;
    isLibrary = true;
    isExecutable = false; # true;
    libraryHaskellDepends = [
      attoparsec base base64-bytestring binary bytestring
      bytestring-builder case-insensitive containers entropy network
      random SHA streaming-commons text
    ];
    testHaskellDepends = [
      attoparsec base base64-bytestring binary bytestring
      bytestring-builder case-insensitive containers entropy HUnit
      network QuickCheck random SHA streaming-commons test-framework
      test-framework-hunit test-framework-quickcheck2 text
    ];
    benchmarkHaskellDepends = [
      attoparsec base base64-bytestring binary bytestring
      bytestring-builder case-insensitive containers criterion entropy
      network random SHA text
    ];
    doCheck = false;
    homepage = "http://jaspervdj.be/websockets";
    description = "A sensible and clean way to write WebSocket-capable servers in Haskell";
    license = stdenv.lib.licenses.bsd3;
  };

inherit (pkgs.haskell.lib) doJailbreak;

in
pkgs.lib.composeExtensions
  (import "${src-network-simple-tls}/hs-overlay.nix" { inherit pkgs; })
  (self: super: {
     network-simple-wss = super.callPackage ./pkg.nix {};
     websockets = super.callPackage pkg-websockets {};
     safe-exceptions = doJailbreak super.safe-exceptions;
  })
