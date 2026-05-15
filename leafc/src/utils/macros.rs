#[macro_export]
macro_rules! bit_enum_impl_op {
	($name:ident, $trait:path => $fn_name:ident($op:tt)) => {
		impl $trait for $name {
			type Output = Self;

			fn $fn_name(self, rhs: Self) -> Self::Output {
				return $name(self.0 $op rhs.0);
			}
		}

		#[allow(unused)]
		impl $name {
			const fn $fn_name(self, rhs: Self) -> Self {
				return $name(self.0 $op rhs.0);
			}
		}
	};
}

#[macro_export]
macro_rules! bit_enum_impl_op_assign {
	($name:ident, $trait:path => $fn_name:ident($op:tt)) => {
		impl $trait for $name {
			fn $fn_name(&mut self, rhs: Self)
			{
				self.0 $op rhs.0;
			}
		}

		#[allow(unused)]
		impl $name {
			const fn $fn_name(&mut self, rhs: Self)
			{
				self.0 $op rhs.0;
			}
		}
	};
}

#[macro_export]
macro_rules! bit_enum {
	($vis:vis struct $name:ident : $ty:ty { $($fname:ident = $val:literal),* $(,)?}) => {
		#[derive(Clone, Copy,  Eq)]
		$vis struct $name($vis $ty);

		#[allow(unused)]
		impl $name {
			$vis const NONE: $name = $name(0);
			$($vis const $fname: $name = $name($val);)*
		}

		const _: () = {
			$(
				if ($val as $ty).count_ones() != 1 {
					panic!(concat!(
							"expexted exactly one bit set, error on: ",
							stringify!($fname),
							" (value: ",
							stringify!($val),
							")"
						));
				}
			)*
		};

		const _: () = {
			let mut answer: $ty = 0;

			$(
				if (answer & $val) != 0 {
					panic!(concat!(
							"value was already seen, second time on: ",
							stringify!($fname),
							" (value: ",
							stringify!($val),
							")"
						)); // maybe check how to also let it see the first one
				}
				answer ^= $val;
			)*
			let _: $ty = answer;
		};

		impl std::fmt::Debug for $name {
			fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
				let mut flags: Vec<&str> = vec![];
				$(
					if (self.0 & $val) != 0 {
						flags.push(stringify!($fname));
					}
				)*
				return f.debug_struct(stringify!($name))
					.field("data: ", &format_args!("{:#08b}", self.0))
					.field("flags: ", &flags)
					.finish();
			}
		}

		impl Default for $name {
			fn default() -> Self {
				return $name(0);
			}
		}

		impl PartialEq for $name {
			fn eq(&self, other: &Self) -> bool
			{
				return self.0 == other.0;
			}
		}

		impl $name {
			const fn eq(&self, other: &Self) -> bool
			{
				return self.0 == other.0;
			}

			const fn ne(&self, other: &Self) -> bool
			{
				return self.0 != other.0;
			}
		}

		#[allow(unused)]
		impl $name {
			$vis const fn new() -> Self {
				return $name::NONE;
			}

			$vis const fn contains_single(&self, flag: Self) -> bool
			{
				return self.bitand(flag).ne(&Self::NONE);
			}

			$vis const fn contains_all(&self, flag: Self) -> bool
			{
				return self.bitand(flag).eq(&flag);
			}

			/// Error value -> if flag already set
			$vis const fn add_flag(self, flag: Self) -> Result<Self, ()>
			{
				if self.contains_single(flag) {
					return Err(());
				}
				return Ok(self.bitor(flag));
			}
		}

		$crate::bit_enum_impl_op!($name, std::ops::BitAnd => bitand(&));
		$crate::bit_enum_impl_op!($name, std::ops::BitOr => bitor(|));
		$crate::bit_enum_impl_op!($name, std::ops::BitXor => bitxor(^));
		$crate::bit_enum_impl_op_assign!($name, std::ops::BitAndAssign => bitand_assign(&=));
		$crate::bit_enum_impl_op_assign!($name, std::ops::BitOrAssign => bitor_assign(|=));
		$crate::bit_enum_impl_op_assign!($name, std::ops::BitXorAssign => bitxor_assign(^=));
	};
}
