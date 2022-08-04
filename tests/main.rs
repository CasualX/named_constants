use named_constants::named_constants;

// Trailing commas

#[named_constants]
#[repr(i32)]
enum TrailingComma1 { A }
#[named_constants]
#[repr(i32)]
enum TrailingComma2 { A, }
#[named_constants]
#[repr(i32)]
enum TrailingComma3 { A, B }
#[named_constants]
#[repr(i32)]
enum TrailingComma4 { A, B, }
#[named_constants]
#[repr(i32)]
enum TrailingComma5 { A, B = 3 }
#[named_constants]
#[repr(i32)]
enum TrailingComma6 { A, B = 4, }

// Autoincrement

macro_rules! static_assert {
	($lhs:expr, $rhs:expr) => {
		const _: [(); $lhs.0] = [(); $rhs];
	};
}

#[named_constants]
#[repr(usize)]
enum Autoincrement1 { A, B, C = 20, D, E }

static_assert!(Autoincrement1::A, 0);
static_assert!(Autoincrement1::B, 1);
static_assert!(Autoincrement1::C, 20);
static_assert!(Autoincrement1::D, 21);
static_assert!(Autoincrement1::E, 22);

#[named_constants]
#[derive(Copy, Clone, Debug, Default, Eq, PartialEq, Reflection)]
#[repr(usize)]
pub enum Autoincrement2 { A = 1, B = 2, C = 3, D = 4, E, F, }

static_assert!(Autoincrement2::A, 1);
static_assert!(Autoincrement2::B, 2);
static_assert!(Autoincrement2::C, 3);
static_assert!(Autoincrement2::D, 4);
static_assert!(Autoincrement2::E, 5);
static_assert!(Autoincrement2::F, 6);

#[test]
fn autoincrement2() {
	assert_eq!(Autoincrement2::_names(), &["A", "B", "C", "D", "E", "F"]);
	assert_eq!(Autoincrement2::_values(), &[Autoincrement2::A, Autoincrement2::B, Autoincrement2::C, Autoincrement2::D, Autoincrement2::E, Autoincrement2::F]);
}

// Attributes on constants

#[named_constants]
#[derive(Copy, Clone, Eq, PartialEq, Display, Reflection, Debug, FromStr,)]
#[repr(u32)]
pub enum ConstantAttributes {
	/// Doc comment
	RED = 0xff0000,
	GREEN = 0x00ff00,
	#[allow()]
	BLUE = 0x0000ff
}

#[test]
fn constant_attributes() {
	assert_eq!(ConstantAttributes::_names(), &["RED", "GREEN", "BLUE"]);
	assert_eq!(ConstantAttributes::_values(), &[ConstantAttributes::RED, ConstantAttributes::GREEN, ConstantAttributes::BLUE]);
	assert_eq!("RED".parse::<ConstantAttributes>(), Ok(ConstantAttributes::RED));
	assert_eq!("121212".parse::<ConstantAttributes>(), Ok(ConstantAttributes(121212)));
}

// Unusual types

#[named_constants]
#[repr(<Vec<i8> as IntoIterator>::Item)]
enum UnusualTypes1 { A = 14 }

#[named_constants]
#[repr(String)]
enum UnusualTypes2 { A = String::new() }
