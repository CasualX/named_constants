Named Constants
===============

Procedural macro makes enums behave like named constants in languages like C/C++ or C#.

Put this attribute on an enum and it will be rewritten as a newtype struct.
The enum variants are turned into associated constants.

Examples
--------

```rust
use named_constants::named_constants;

#[named_constants]
// Derives are applied to the newtype wrapper
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
// Required repr to specify the underlying type
#[repr(i32)]
pub enum CardSuit {
	CLUBS,      // (= 0) Starts from zero
	DIAMONDS,   // (= 1) Autoincrements the previous value
	HEARTS = 4, // (= 4) Direct assignment
	SPADES,     // (= 5) Autoincrements the previous value
}

let clubs = CardSuit::CLUBS;
let weird = CardSuit(14); // Legal!
```

Implementation notes
--------------------

In the example above, `CardSuit` is transformed into:

```rust
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
#[repr(transparent)]
pub struct CardSuit(pub i32);
impl CardSuit {
	pub const CLUBS: CardSuit = CardSuit(0);
	pub const DIAMONDS: CardSuit = CardSuit(1);
	pub const HEARTS: CardSuit = CardSuit(4);
	pub const SPADES: CardSuit = CardSuit(4 + 1);
}
```

License
-------

Licensed under [MIT License](https://opensource.org/licenses/MIT), see [license.txt](license.txt).

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, shall be licensed as above, without any additional terms or conditions.
