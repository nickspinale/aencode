# aencode
Efficient bencode parsers and serialization

**Features**  
*   fast: built on [attoparsec](https://github.com/bos/attoparsec), and uses bytestring builders
*   flexible approach to serialization (the bencode type is polymorphic)
*   includes lenses, but does not require [lens](https://github.com/ekmett/lens).
