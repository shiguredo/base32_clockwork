# Clockwork Base32

Clockwork Base32 is a simple variant of Base32 inspired by Crockford's Base32.

See also [a blog post](https://medium.com/shiguredo/base32-の変種を作った話-d8da2e3102ec) (in Japanese).

## Table of Contents

- [Specification Version](#specification-version)
- [Last updated](#last-updated)
- [Features](#features)
- [Difference Between Clockwork Base32 and Other Specifications](#difference)
- [Symbols](#symbols)
- [Algorithm](#algorithm)
  - [Encoding](#algorithm-encoding)
  - [Decoding](#algorithm-decoding)
  - [Notes](#algorithm-notes)
- [Implementations](#implementations)
  - [Reference Implementations](#reference-implementations)
  - [Third-Party Implementations](#third-party-implementations)
- [Examples](#examples)
- [License](#license)
- [Author](#author)
- [Acknowledgements](#acknowledgements)
- [Uses](#uses)
- [Links](#links)
- [Specification Revision History](#specification-revision-history)
- [Document Revision History](#document-revision-history)

<a id="specification-version"></a>

## Specification Version

2020.2 (Updated: 2020-07-27)

<a id="last-updated"></a>

## Last updated

2021-06-06

<a id="features"></a>

## Features

- Human readable
- Octet-aligned binary support
- No padding character at end of encoded text
- Easy to implement (recommends using bitstring libraries)

<a id="difference"></a>

## Difference Between Clockwork Base32 and Other Specifications

| | RFC 4648 | Crockford's Base32 | Clockwork Base32 |
----|----|----|----
| Human readable | Not needed | Needed | Needed |
| Input data | Octet sequence | Integer | Octet sequence (byte array) |
| Encoded representation | ASCII character sequence | Symbol sequence | ASCII character sequence |
| Symbols | 32 alphanum + 1 sign characters | 32 alphanum + 5 sign characters (optional) | 32 alphanum characters |
| Padding of encoded data | Used | None | None |
| Ignored characters in decoding | Non-alphabet characters (optional) | Hyphen | None |
| Checksum | None | 1 character (Optional) | None |

<a id="symbols"></a>

## Symbols

- Clockwork's Base32 symbol set is equal to Crockford's Base32's excluding 5 symbols (`*~$=U`) for checksum.
- Symbol is 1 ASCII character.
- Case-insensitive.

| Value | Decode | Encode |
----|----|----
| 0 | `0` `O` `o` | `0` |
| 1 | `1` `I` `i` `L` `l` | `1` |
| 2 | `2` | `2` |
| 3 | `3` | `3` |
| 4 | `4` | `4` |
| 5 | `5` | `5` |
| 6 | `6` | `6` |
| 7 | `7` | `7` |
| 8 | `8` | `8` |
| 9 | `9` | `9` |
| 10 | `A` `a` | `A` `a` |
| 11 | `B` `b` | `B` `b` |
| 12 | `C` `c` | `C` `c` |
| 13 | `D` `d` | `D` `d` |
| 14 | `E` `e` | `E` `e` |
| 15 | `F` `f` | `F` `f` |
| 16 | `G` `g` | `G` `g` |
| 17 | `H`  `h`| `H` `h` |
| 18 | `J` `j` | `J` `j` |
| 19 | `K` `k` | `K` `k` |
| 20 | `M` `m` | `M` `m` |
| 21 | `N` `n` | `N` `n` |
| 22 | `P` `p` | `P` `p` |
| 23 | `Q` `q` | `Q` `q` |
| 24 | `R` `r` | `R` `r` |
| 25 | `S` `s` | `S` `s` |
| 26 | `T` `t` | `T` `t` |
| 27 | `V` `v` | `V` `v` |
| 28 | `W` `w` | `W` `w` |
| 29 | `X` `x` | `X` `x` |
| 30 | `Y` `y` | `Y` `y` |
| 31 | `Z` `z` | `Z` `z` |

Excluded Characters: `U`

<a id="algorithm"></a>

## Algorithm

<a id="algorithm-encoding"></a>

### Encoding

1. Proceeding from left to right, map each 5 bits representation of input data as block length to a symbol character (upper-case recommended).
   If length of the most right block is under 5 bits, fill with zero bits.
2. Combine the symbol characters into a sequence.
3. Return the sequence.

Bit length of decoded data must be greater than or equal to bit length of the plain data.

<a id="algorithm-decoding"></a>

### Decoding

1. Proceeding from left to right, map each character of input data to 5 bits representation.
2. Combine the 5 bits blocks into an octet sequence.
   If the sum of the block length is indivisible by 8, truncate most right bits which length is equal to a remainder of division by 8.
4. Return the octet sequence.

Some error cases:

- Including invalid characters: e.g. `uU*=`

Some corner cases:

- If an input data is 1 character (e.g. `0`), decoder may return an empty octet sequence or report as error.
- Padding length may be greater than or equal to 5-7 bits. For example, if input data is 3 characters, it represent 15 bits which is 1 character and padding 7 bits. Both of input data `CR0` and `CR` can be decoded as `f`.

<a id="algorithm-notes"></a>

### Notes

- Encoded data does not contain error detection information. Use this algorithm together with any other error detection algorithm to detect errors.

<a id="examples"></a>

## Examples

| Input | Encoded |
----|----
| (empty) | (empty) |
| `f` | `CR` or `CR0` |
| `foobar` | `CSQPYRK1E8` |
| `Hello, world!` | `91JPRV3F5GG7EVVJDHJ22` |
| `The quick brown fox jumps over the lazy dog.` | `AHM6A83HENMP6TS0C9S6YXVE41K6YY10D9TPTW3K41QQCSBJ41T6GS90DHGQMY90CHQPEBG` |

<a id="implementations"></a>

## Implementations

<a id="reference-implementations"></a>

### Reference Implementations

These reference implementations basically are for help with understanding and implementing.
You should not expect improving performance, good API and continuous maintenance.

- C: [szktty/c-clockwork-base32](https://github.com/szktty/c-clockwork-base32)
- Erlang: [shiguredo/erlang-base32](https://github.com/shiguredo/erlang-base32)
- Go: [szktty/go-clockwork-base32](https://github.com/szktty/go-clockwork-base32)
- Swift: [szktty/swift-clockwork-base32](https://github.com/szktty/swift-clockwork-base32)

<a id="third-party-implementations"></a>

### Third-Party Implementations

- C++: [wx257osn2/clockwork_base32_cxx](https://github.com/wx257osn2/clockwork_base32_cxx)
- C++: [objectx/cpp-clockwork-base32](https://github.com/objectx/cpp-clockwork-base32)
- JavaScript: [mganeko/js_clockwork_base32](https://github.com/mganeko/js_clockwork_base32)
- AssemblyScript: [mganeko/as_clockwork_base32](https://github.com/mganeko/as_clockwork_base32)
- Rust: [woxtu/rust-clockwork-base32](https://github.com/woxtu/rust-clockwork-base32)
- Rust: [hnakamur/rs-clockwork-base32](https://github.com/hnakamur/rs-clockwork-base32)
- Go: [shogo82148/go-clockwork-base32](https://github.com/shogo82148/go-clockwork-base32)
- TypeScript: [niyari/base32-ts](https://github.com/niyari/base32-ts)

<a id="license"></a>

## License

This document is distributed under CC BY-ND 4.0.

<a id="author"></a>

## Author

SUZUKI Tetsuya

<a id="acknowledgements"></a>

## Acknowledgements

Shiguredo Inc.

<a id="uses"></a>

## Uses

- [WebRTC SFU Sora](https://sora.shiguredo.jp) by Shiguredo Inc.
  - used for encoding and decoding UUID to be readable and shorten (32 characters -> 26 characters).

<a id="links"></a>

## Links

- [Crockford's Base32](https://www.crockford.com/base32.html)
- [Formal Verification of Encoding and Decoding](https://convenience-logician.de/encode-decode.html)
  - [A sample script (Isabelle/HOL)](https://gist.github.com/pirapira/d012e19d6ed4b2b452bf27221363a62c)
  - [PDF](https://gist.github.com/pirapira/d012e19d6ed4b2b452bf27221363a62c#file-document-pdf)
  - Thanks @pirapira!

<a id="specification-revision-history"></a>

## Specification Revision History

**2020.2** (2020-07-27)

- [CHANGE] Added decoding specification for some corner cases. Thanks @pirapira!
- [CHANGE] Changed decoding 1 character from invalid to valid.

**2020.1** (2020-07-20)

- First release.

<a id="document-revision-history"></a>

## Document Revision History

**2021-06-06**

- [CHANGE] Added a third-party implementation.
  - niyari/base32-ts

**2021-02-11**

- [CHANGE] Added third-party implementations.
  - shogo82148/go-clockwork-base32
  - mganeko/as_clockwork_base32
  - hnakamur/rs-clockwork-base32
  
**2020-08-11**

- [CHANGE] Added ["Uses"](#uses) section.

**2020-08-01**

- [CHANGE] Added a reference implementation.
  - szktty/swift-clockwork-base32
- [CHANGE] Added some links.
  
**2020-07-30**

- [CHANGE] Added a reference implementation.
  - szktty/c-clockwork-base32
  
**2020-07-27**

- Released 2020.2.
- [CHANGE] Added a table of contents.

**2020-07-26**

- [CHANGE] Added a third-party implementation.
  - woxtu/rust-clockwork-base32

**2020-07-25**

- [CHANGE] Added third-party implementations.
  - wx257osn2/clockwork_base32_cxx
  - objectx/cpp-clockwork-base32
  - mganeko/js_clockwork_base32
  
**2020-07-22**

- [CHANGE] Added RFC 4648 to the comparison table of specification.
- [FIX] Input`Hello, world` is incorrect. `Hello, world!` is correct. Thanks @mganeko!

**2020-07-20**

- First release.
