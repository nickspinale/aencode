{ mkDerivation, attoparsec, base, bytestring, containers
, profunctors, stdenv
}:
mkDerivation {
  pname = "bencode";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    attoparsec base bytestring containers profunctors
  ];
  homepage = "https://github.com/nickspinale/bencode.git";
  description = "Efficient bencode parsers and serialization";
  license = stdenv.lib.licenses.mit;
}
