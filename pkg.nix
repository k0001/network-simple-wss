{ mkDerivation, base, bytestring, exceptions, network-simple-tls
, stdenv, websockets
}:
mkDerivation {
  pname = "network-simple-wss";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring exceptions network-simple-tls websockets
  ];
  homepage = "https://github.com/k0001/network-simple-wss";
  description = "Simple interface to TLS secured WebSockets";
  license = stdenv.lib.licenses.bsd3;
}
