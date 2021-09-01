{ mkDerivation, async, base, bytestring, network-simple-tls
, network-simple-ws, safe-exceptions, stdenv, websockets
}:
mkDerivation {
  pname = "network-simple-wss";
  version = "0.1.1";
  src = ./.;
  libraryHaskellDepends = [
    async base bytestring network-simple-tls network-simple-ws
    safe-exceptions websockets
  ];
  homepage = "https://github.com/k0001/network-simple-wss";
  description = "Simple interface to TLS secured WebSockets";
  license = stdenv.lib.licenses.bsd3;
}
