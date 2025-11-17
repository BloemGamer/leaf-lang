# Syntax
> [!WARNING]
> This document is now mostly for notes for myself, and will be split up, and written better in the future, and there will become a real syntax guilde at instead of this file

> [!NOTE]
> There sometimes wil be name mangeling, for if you want to call things with C code, this will be defined later, but in this document is a prototype for this already

> [!NOTE]
> Not everything will be there in the fist version, the first version will be a proof of concept, and that there will be improvements

## Declarations:
```yaml
function:			function_modifier(optional) fn name<template(optional)>(arguments) -> type_modifier(optional) return_type(optional) {}
variable:			type_modifier(optional) type var_name = value;
macro's:			because of that this is preprocessor this wil be later defined, but probably something like functions, even though what is in it will be a little different
msg to compiler:	@msg
```

## Remarks:
`var = { }` is allowed, and will take the last token, (defined as the one after the last `;`, like rust), this will also work for if statements ect, but not for functions
`struct.fn(args)` wil result in `<struct>_fn(&struct, args)`, so object oriented can be done, you want to call a function pointer like `struct.fn_p()` this should be done with the message `@no_fn`
the keywords `new`, `delete` and `resize` can be used for memory allocation and deallocation, intern this just will use `malloc` and `fee`, ect, or maybe the `fn.new`, `fn.delete` and `fn.resize`

you can have templates, they will be defined as `fn_name<T>(args)` and they result in `<type>_fn_name(args)`

extra for loop syntax
not all types work for this for supporting types
`for(var in var_iterable)` -> `for(int _i = 0; (var = var_interable[_i]; _i++)`
`for(var in var_iterable, index) -> for(int index = 0; (var = var_interable[index]; index++)`
`for(i in 0..10)` -> `for(int i = 0; i < 10; i++)` note i may not be changed i the loop

string format can be done with `@f"a = { a }, b = { b }, c = { c }"` or `@f("a = {}, b = {}, c = {}", a, b, c)`

otherwise C syntax should be used

## Keywords:
```yaml
if:		same as in C
else:	same as in C
true:	same as in C
false:	same as in C
do:		same as in C
while:	same as in C
for:	same as in C, but can do more than in C, read other things for more info
fn:		defining a function, the same as in rust
macro:	defining a macro
struct:	same as in C, but will always typedef
enum:	same as in C, but I maybe want to add more, but I don't know what yet, maybe like rust and call the C enum an c_enum
return: same as in C
```

## Modifiers:
```yaml
mut:	allows variables to be changed, otherwise all variables are const
static:	the same as in C # should only be used on local types
const:	compiler const, so constexpr in C # maybe later this will also work on functions
pub:	export this to be used in other files

```

## Extra notes over types
All types kan be a pointer `*` or a reference `&`, a reference is a const pointer, in the future maybe I will add that the first one never can be nullprt

## Basic types:
```yaml
isize:		ssize_t # this one does not always exsist, so I will ad this later, but it will be an signed int of the size of the OS, so for most it will be the same as i64
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
You can make tuples, these are going to be represented as a struct in C

## Complex types:
```yaml
String:		typedef-ed String struct
Vec<T>:		typedef-ed Vec struct
Option<T>:	typedef-ed Option struct
# later there will be more defined, and will not be needed for syntax
```
