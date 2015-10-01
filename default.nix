{ mkDerivation, attoparsec, base, bytestring, containers
, profunctors, stdenv
}:
mkDerivation {
  pname = "aencode";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    attoparsec base bytestring containers profunctors
  ];
  homepage = "https://github.com/nickspinale/aencode.git";
  description = "Efficient bencode parsers and serialization";
  license = stdenv.lib.licenses.mit;
}
