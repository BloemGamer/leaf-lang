# Syntax
> [!NOTE]
> This document is now mostly for notes for myself, and will be split up, and written better in the future

> [!NOTE]
> There sometimes wil be name mangeling, for if you want to call things with C code, this will be defined later

declarations:
```
function:			functionmodifier(optional) fn name<template(optional)>(arguments) -> type_modifier(optional) return_type(optional) {}
variable:			type_modifier(optional) type var_name = value;
macro's:			because of that this is preprocessor this wil be later defined, but probably something like functions, even though what is in it will be a little different
msg to compiler:	@msg
```

other things:
```
`var = { }` is allowed, and will take the last token, (defined as the one after the last `;`, like rust)
`struct.fn(args)` wil result in `fn_<struct>(&struct, args)`, so object oriented can be done, you want to call a function pointer like `struct.fn_p()` this should be done with the message `@no_fn`
the keywords new, delete and `resize` can be used for memory allocation and deallocation, intern this just will use malloc and fee, ect

extra for loop syntax
not all types work for this for supporting types
for(var in var_iterable) -> for(int _i = 0; (var = var_interable[_i]; _i++)
for(var in var_iterable, index) -> for(int index = 0; (var = var_interable[index]; incex++)
for(i in 0..10) -> for(int i = 0; i < 10; i++) note i may not be changed i the loop

string format can be done with @f"a = { a }, b = { b }, c = { c }" or @f("a = {}, b = {}, c = {}", a, b, c)

otherwise C syntax should be used
```

Keywords:
```
if:		same as in C
else:	same as in C
true:	same as in C
false:	same as in C
do:		same as in C
while:	same as in C
for:	same as in C, but can do more than in C, read other things for more info
fn:		defining a function, the same as in rust
macro:	defining a macro
struct:	same as in C
enum:	same as in C, but I maybe want to add more, but I don't know what yet, maybe like rust and call the C enum an c_enum
return: same as in C
```

Modifiers:
```
mut:	allows variables to be changed, otherwise all variables are const
static:	the same as in C
const:	compiler const, so constexpr in C

```


basic types:
```
isize:		ssize_t
usize:		size_t
i64:		int64_t
i32:		int32_t
i16:		int16_t
i8:			int8_t
u64:		uint64_t
u32:		uint32_t
u16:		uint16_t
u8:			uint8_t

f32:		float
f64:		double

char:		char
```

complexer types:
```
String:		typedef-ed String struct
Vec<T>:		typedef-ed Vec struct
Option<T>:	typedef-ed Option struct
// later there will be more defined, and will not be needed for syntax
```
