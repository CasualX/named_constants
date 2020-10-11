use proc_macro::*;
use std::iter::FromIterator;

/// # Named constants
///
/// Makes enums behave like named constants in languages like C/C++ or C#.
///
/// Put this attribute on an enum and it will be rewritten as a newtype struct.
/// The enum variants are turned into associated constants.
///
/// # Examples
///
/// ```
/// use named_constants::named_constants;
///
/// #[named_constants]
/// // Derives are applied to the newtype wrapper
/// #[derive(Copy, Clone, Debug, Eq, PartialEq)]
/// // Required repr to specify the underlying type
/// #[repr(i32)]
/// pub enum CardSuit {
/// 	CLUBS,      // (= 0) Starts from zero
/// 	DIAMONDS,   // (= 1) Autoincrements the previous value
/// 	HEARTS = 4, // (= 4) Direct assignment
/// 	SPADES,     // (= 5) Autoincrements the previous value
/// }
///
/// let clubs = CardSuit::CLUBS;
/// let weird = CardSuit(14); // Legal!
/// ```
///
/// # Implementation notes
///
/// In the example above, `CardSuit` is transformed into:
///
/// ```
/// #[derive(Copy, Clone, Debug, Eq, PartialEq)]
/// #[repr(transparent)]
/// pub struct CardSuit(pub i32);
/// impl CardSuit {
/// 	pub const CLUBS: CardSuit = CardSuit(0);
/// 	pub const DIAMONDS: CardSuit = CardSuit(1);
/// 	pub const HEARTS: CardSuit = CardSuit(4);
/// 	pub const SPADES: CardSuit = CardSuit(4 + 1);
/// }
/// ```
#[proc_macro_attribute]
pub fn named_constants(attr: TokenStream, item: TokenStream) -> TokenStream {
	if !attr.is_empty() {
		panic!("the attribute macro does not support any arguments. {}", attr);
	}
	let named = parse_named(item);
	let mut tokens = Vec::new();
	render(&mut tokens, &named);
	tokens.into_iter().collect()
}

macro_rules! token_stream {
	($($e:expr),*$(,)?) => {
		TokenStream::from_iter(vec![$(TokenTree::from($e)),*])
	};
}

//================================================================
// Parser

fn to_vec(stream: TokenStream) -> Vec<TokenTree> {
	stream.into_iter().collect()
}
fn to_group(tt: TokenTree) -> Group {
	match tt {
		TokenTree::Group(group) => group,
		_ => panic!(),
	}
}
fn to_ident(tt: TokenTree) -> Ident {
	match tt {
		TokenTree::Ident(ident) => ident,
		_ => panic!(),
	}
}

/*
macro_rules! named_constants {
	(
		$(#[$meta:meta])*
		$vis:vis enum $name:ident {
			$(
				$(#[$cmeta:meta])*
				$key:ident $(= $value:expr)?,
			)*
		}
	) => {
		//...
	};
}
*/

struct NamedConstants {
	attributes: Vec<TokenStream>,
	repr_type: TokenStream,
	derives: Derives,
	visibility: Visibility,
	name: Ident,
	constants: Vec<Constant>,
}
struct Derives {
	reflection: bool,
	debug: bool,
	from_str: bool,
}
struct Visibility {
	keyword: Option<Ident>,
	group: Option<Group>,
}
struct Constant {
	attributes: Vec<TokenStream>,
	key: Ident,
	value: Option<TokenStream>,
}

fn parse_commalist(stream: TokenStream) -> Vec<Vec<TokenTree>> {
	let mut tokens = to_vec(stream);
	tokens.reverse();

	let mut result = Vec::new();
	let mut temp = Vec::new();
	while let Some(token) = tokens.pop() {
		match token {
			TokenTree::Punct(punct) if punct.as_char() == ',' => {
				result.push(std::mem::replace(&mut temp, Vec::new()));
			},
			other => temp.push(other),
		}
	}
	if !temp.is_empty() {
		result.push(temp);
	}
	result
}
fn join_commalist(tokens: Vec<Vec<TokenTree>>) -> TokenStream {
	tokens.into_iter().flat_map(|mut item| {
		item.push(TokenTree::Punct(Punct::new(',', Spacing::Alone)));
		item
	}).collect()
}
fn parse_attributes(stream: TokenStream) -> (Vec<TokenStream>, TokenStream) {
	let mut tokens = to_vec(stream);
	tokens.reverse();

	// $(#[meta:meta])*
	let mut attributes = Vec::new();
	loop {
		match &tokens[..] {
			[.., TokenTree::Group(group), TokenTree::Punct(punct)] if punct.as_char() == '#' && group.delimiter() == Delimiter::Bracket => {
				let _ = tokens.pop();
				let group = to_group(tokens.pop().unwrap());
				attributes.push(group.stream());
			},
			_ => break,
		}
	}
	(attributes, tokens.into_iter().rev().collect())
}
fn parse_visibility(stream: TokenStream) -> (Visibility, TokenStream) {
	let mut tokens = to_vec(stream);
	tokens.reverse();

	let vis = match &tokens[..] {
		[.., TokenTree::Group(group), TokenTree::Ident(keyword)] if keyword.to_string() == "pub" && group.delimiter() == Delimiter::Parenthesis => {
			let keyword = Some(to_ident(tokens.pop().unwrap()));
			let group = Some(to_group(tokens.pop().unwrap()));
			Visibility { keyword, group }
		},
		[.., TokenTree::Ident(keyword)] if keyword.to_string() == "pub" => {
			let keyword = Some(to_ident(tokens.pop().unwrap()));
			let group = None;
			Visibility { keyword, group }
		},
		_ => {
			let keyword = None;
			let group = None;
			Visibility { keyword, group }
		},
	};
	(vis, tokens.into_iter().rev().collect())
}
fn parse_constants(stream: TokenStream) -> Vec<Constant> {
	let mut tokens = to_vec(stream);
	tokens.reverse();

	// $($(#[#meta:meta])* $key:ident $(= $value:expr,)?)
	let mut constants = Vec::new();
	loop {
		let mut attributes = Vec::new();
		while let [.., TokenTree::Group(_), TokenTree::Punct(_)] = &tokens[..] {
			let _ = tokens.pop();
			let group = to_group(tokens.pop().unwrap());
			attributes.push(group.stream());
		}

		let key = match tokens.pop() {
			Some(TokenTree::Ident(key)) => key,
			None => break,
			_ => panic!("expected a list of constants `KEY $(= VALUE,)?`")
		};

		let value = match tokens.pop() {
			Some(TokenTree::Punct(punct)) if punct.as_char() == ',' => None,
			Some(TokenTree::Punct(punct)) if punct.as_char() == '=' => {
				let mut value = Vec::new();
				loop {
					match tokens.pop() {
						Some(TokenTree::Punct(punct)) if punct.as_char() == ',' => break,
						Some(tt) => value.push(tt),
						None => break,
					}
				}
				Some(TokenStream::from_iter(value))
			},
			None => None,
			_ => panic!("expected a list of constants `KEY $(= VALUE,)?`")
		};

		constants.push(Constant { attributes, key, value })
	}
	constants
}
fn parse_named(stream: TokenStream) -> NamedConstants {
	let (mut attributes, stream) = parse_attributes(stream);
	let repr_type = rewrite_repr(&mut attributes).expect("must have a repr attribute");
	let derives = rewrite_derives(&mut attributes);
	let (visibility, stream) = parse_visibility(stream);
	let tokens = to_vec(stream);
	match &tokens[..] {
		[TokenTree::Ident(keyword), TokenTree::Ident(name), TokenTree::Group(group)] => {
			match keyword.to_string().as_str() {
				"enum" => (),
				_ => panic!("named constants are only supported for enum declarations"),
			}
			match group.delimiter() {
				Delimiter::Brace => (),
				_ => panic!("named constants must use { } to declare the constants"),
			}
			let constants = parse_constants(group.stream());
			NamedConstants {
				attributes,
				repr_type,
				derives,
				visibility,
				name: name.clone(),
				constants
			}
		},
		[TokenTree::Ident(_), TokenTree::Ident(_), TokenTree::Punct(_), ..] => panic!("named constants do not support generics"),
		_ => panic!(),
	}
}
// Rewrite #[repr(TYPE)] into #[repr(transparent)]
fn rewrite_repr(attributes: &mut Vec<TokenStream>) -> Option<TokenStream> {
	let mut repr_type = None;
	for meta in attributes {
		let tokens = to_vec(meta.clone());
		match &tokens[..] {
			[TokenTree::Ident(repr), TokenTree::Group(group)] if repr.to_string() == "repr" => {
				repr_type = Some(group.stream());
				*meta = token_stream!(repr.clone(), Group::new(Delimiter::Parenthesis, token_stream!(Ident::new("transparent", group.span()))));
			},
			_ => (),
		}
	}
	repr_type
}
// Rewrite #[derive(..)] looking for Reflection, Debug and FromStr
fn rewrite_derives(attributes: &mut Vec<TokenStream>) -> Derives {
	let mut reflection = false;
	let mut debug = false;
	let mut from_str = false;
	for meta in attributes {
		let tokens = to_vec(meta.clone());
		match &tokens[..] {
			[TokenTree::Ident(derive), TokenTree::Group(group)] if derive.to_string() == "derive" => {
				let mut derives = parse_commalist(group.stream());
				derives.retain(|item| {
					match &item[..] {
						[TokenTree::Ident(ident)] => {
							let ident = ident.to_string();
							match ident.as_str() {
								"Reflection" => reflection = true,
								"Debug" => debug = true,
								"FromStr" => from_str = true,
								_ => return true,
							}
							false
						},
						_ => true,
					}
				});
				if reflection {
					*meta = token_stream!(derive.clone(), Group::new(Delimiter::Parenthesis, join_commalist(derives)));
				}
				else {
					debug = false;
					from_str = false;
				}
			},
			_ => (),
		}
	}
	Derives { reflection, debug, from_str }
}

//================================================================
// Render

fn render(tokens: &mut Vec<TokenTree>, named: &NamedConstants) {
	// Render attributes
	for meta in &named.attributes {
		render_attribute(tokens, meta);
	}
	// Render visibility
	render_visibility(tokens, &named.visibility);
	// Render the newtype
	render_newtype(tokens, &named.name, &named.visibility, &named.repr_type);
	// Render the constants
	render_constants(tokens, &named.name, &named.constants);
	// Render the reflection
	render_reflection(tokens, &named.name, &named.constants, &named.derives);
}
fn render_attribute(tokens: &mut Vec<TokenTree>, meta: &TokenStream) {
	tokens.push(TokenTree::Punct(Punct::new('#', Spacing::Alone)));
	tokens.push(TokenTree::Group(Group::new(Delimiter::Bracket, meta.clone())));
}
fn render_doc_comment(tokens: &mut Vec<TokenTree>, comment: &str) {
	tokens.push(TokenTree::Punct(Punct::new('#', Spacing::Alone)));
	render_group(tokens, Delimiter::Bracket, |tokens| {
		tokens.push(TokenTree::Ident(Ident::new("doc", Span::call_site())));
		tokens.push(TokenTree::Punct(Punct::new('=', Spacing::Alone)));
		tokens.push(TokenTree::Literal(Literal::string(comment)));
	});
}
fn render_string(tokens: &mut Vec<TokenTree>, string: &str) {
	tokens.extend(string.parse::<TokenStream>().unwrap());
}
fn render_impl(tokens: &mut Vec<TokenTree>, name: &Ident, items: impl FnMut(&mut Vec<TokenTree>)) {
	tokens.push(TokenTree::Ident(Ident::new("impl", Span::call_site())));
	tokens.push(TokenTree::Ident(name.clone()));
	render_group(tokens, Delimiter::Brace, items);
}
fn render_group(tokens: &mut Vec<TokenTree>, delimiter: Delimiter, mut contents: impl FnMut(&mut Vec<TokenTree>)) {
	tokens.push(TokenTree::Group(Group::new(delimiter, {
		let mut tokens = Vec::new();
		contents(&mut tokens);
		tokens.into_iter().collect()
	})));
}
fn render_visibility(tokens: &mut Vec<TokenTree>, visibility: &Visibility) {
	if let Some(keyword) = &visibility.keyword {
		tokens.push(TokenTree::Ident(keyword.clone()));
	}
	if let Some(group) = &visibility.group {
		tokens.push(TokenTree::Group(group.clone()));
	}
}
fn render_newtype(tokens: &mut Vec<TokenTree>, name: &Ident, vis: &Visibility, ty: &TokenStream) {
	tokens.push(TokenTree::Ident(Ident::new("struct", Span::call_site())));
	tokens.push(TokenTree::Ident(name.clone()));
	render_group(tokens, Delimiter::Parenthesis, |tokens| {
		render_visibility(tokens, vis);
		tokens.extend(ty.clone().into_iter());
	});
	tokens.push(TokenTree::Punct(Punct::new(';', Spacing::Alone)));
}
fn render_constants(tokens: &mut Vec<TokenTree>, name: &Ident, constants: &[Constant]) {
	render_string(tokens, "#[allow(non_upper_case_globals)]");
	render_impl(tokens, name, |tokens| {
		let mut last_value = (None, 0);
		for constant in constants {
			render_constant(tokens, name, constant, &mut last_value);
		}
	});
}
fn render_constant<'a>(tokens: &mut Vec<TokenTree>, name: &Ident, constant: &'a Constant, last_value: &mut (Option<&'a TokenStream>, i32)) {
	for attr in &constant.attributes {
		render_attribute(tokens, attr);
	}
	let value = {
		match &constant.value {
			// If an explicit value is assigned to this constant
			Some(value) => {
				// Set this constant as the base for iota and reset iota to 1 for the next constant
				last_value.0 = Some(value);
				last_value.1 = 1;
				value.clone()
			},
			// If no explicit value is assigned to this constant
			None => {
				// Then take the last known constant and add
				let mut tokens = Vec::new();
				if let Some(last_value) = last_value.0 {
					// Curious: Using Delimiter::None does not ensure proper precedence
					tokens.push(TokenTree::Group(Group::new(Delimiter::Parenthesis, last_value.clone())));
					tokens.push(TokenTree::Punct(Punct::new('+', Spacing::Alone)));
				}
				// The iota as its value
				tokens.push(TokenTree::Literal(Literal::i32_unsuffixed(last_value.1)));
				// Increment the iota for next constant
				last_value.1 += 1;
				tokens.into_iter().collect()
			},
		}
	};
	render_doc_comment(tokens, "");
	render_doc_comment(tokens, &format!("const `{}::{}` = `{}`.", name, constant.key, value));
	tokens.push(TokenTree::Ident(Ident::new("pub", Span::call_site())));
	tokens.push(TokenTree::Ident(Ident::new("const", Span::call_site())));
	tokens.push(TokenTree::Ident(constant.key.clone()));
	tokens.push(TokenTree::Punct(Punct::new(':', Spacing::Alone)));
	tokens.push(TokenTree::Ident(name.clone()));
	tokens.push(TokenTree::Punct(Punct::new('=', Spacing::Alone)));
	tokens.push(TokenTree::Ident(name.clone()));
	tokens.push(TokenTree::Group(Group::new(Delimiter::Parenthesis, value)));
	tokens.push(TokenTree::Punct(Punct::new(';', Spacing::Alone)));
}
fn render_reflection(tokens: &mut Vec<TokenTree>, name: &Ident, constants: &[Constant], derives: &Derives) {
	if derives.reflection {
		render_impl(tokens, name, |tokens| {
			render_string(tokens, "const fn _str(&self) -> ::core::option::Option<&'static str>");
			render_group(tokens, Delimiter::Brace, |tokens| {
				render_string(tokens, "match self");
				render_group(tokens, Delimiter::Brace, |tokens| {
					for constant in constants {
						render_string(tokens, "&Self::");
						tokens.push(TokenTree::Ident(constant.key.clone()));
						render_string(tokens, "=> ::core::option::Option::Some");
						tokens.push(TokenTree::Group(Group::new(Delimiter::Parenthesis, token_stream!(Literal::string(&constant.key.to_string())))));
						tokens.push(TokenTree::Punct(Punct::new(',', Spacing::Alone)));
					}
					render_string(tokens, "_ => ::core::option::Option::None,");
				});
			});
			render_string(tokens, "const fn _keys() -> &'static [&'static str]");
			render_group(tokens, Delimiter::Brace, |tokens| {
				tokens.push(TokenTree::Punct(Punct::new('&', Spacing::Alone)));
				render_group(tokens, Delimiter::Bracket, |tokens| {
					for constant in constants {
						tokens.push(TokenTree::Literal(Literal::string(&constant.key.to_string())));
						tokens.push(TokenTree::Punct(Punct::new(',', Spacing::Alone)));
					}
				});
			});
			render_string(tokens, "const fn _values() -> &'static [Self]");
			render_group(tokens, Delimiter::Brace, |tokens| {
				tokens.push(TokenTree::Punct(Punct::new('&', Spacing::Alone)));
				render_group(tokens, Delimiter::Bracket, |tokens| {
					for constant in constants {
						render_string(tokens, "Self::");
						tokens.push(TokenTree::Ident(constant.key.clone()));
						tokens.push(TokenTree::Punct(Punct::new(',', Spacing::Alone)));
					}
				});
			});
			render_string(tokens, "const fn _is_defined(&self) -> bool");
			render_group(tokens, Delimiter::Brace, |tokens| {
				render_string(tokens, "match self");
				render_group(tokens, Delimiter::Brace, |tokens| {
					for constant in constants {
						render_string(tokens, "&Self::");
						tokens.push(TokenTree::Ident(constant.key.clone()));
						render_string(tokens, "=> true,");
					}
					render_string(tokens, "_ => false,");
				});
			});
		});
	}
	if derives.debug {
		render_string(tokens, "impl ::core::fmt::Debug for");
		tokens.push(TokenTree::Ident(name.clone()));
		render_string(tokens, "{
			fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
				::core::write!(f, \"{} ({:?})\", self._str().unwrap_or(\"_\"), self.0)
			}
		}");
	}
	if derives.from_str {
		render_string(tokens, "impl ::core::str::FromStr for");
		tokens.push(TokenTree::Ident(name.clone()));
		render_group(tokens, Delimiter::Brace, |tokens| {
			render_string(tokens, "type Err = ::core::num::ParseIntError;");
			render_string(tokens, "fn from_str(s: &str) -> ::core::result::Result<Self, Self::Err> {
				let keys = Self::_keys();
				let values = Self::_values();
				for i in 0..keys.len() {
					if keys[i] == s {
						return Ok(values[i]);
					}
				}
				Ok(Self(s.parse()?))
			}");
		});
	}
}
