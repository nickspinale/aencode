# bencode
Efficient bencode parsing and serialization

**Features**  
*   Fast: built on [attoparsec](https://github.com/bos/attoparsec), and uses bytestring builders
*   Flexible approach to serialization (the `BValue` type is polymorphic over intermediate builder method)
*   Includes [lenses](https://github.com/ekmett/lens) without incurring any dependencies
