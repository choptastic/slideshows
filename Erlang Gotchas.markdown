# Erlang Gotchas

### Things About Erlang That Will Surprise You 

Chicago Erlang User Group

Wednesday, February 5th, 2014

Jesse Gumm (@jessegumm)

---

# Prologue

## Erlang is great

---

* Functional Language
* Semantics that lend itself to clean, easy-to-read code
* Amazing Concurrency before it was popular

![HIPSTER ARMSTRONG](/slideshows/armstrong-hipster.jpg)

---

# but...

---

# It has its mysteries

![I want to believe](/slideshows/i-want-to-believe.jpg)

---

# Strings and Things

```erlang
> [87, 65, 84, 63, 0].
[87, 65, 84, 63, 0]
> [87, 65, 84, 63].
"WAT?"
```

---

## Erlang does not have strings

It has lists of integers that are valid Latin-1 characters. If the list contains only valid Latin-1 characters, the Erlang shell "assumes" you want to print it as a string.

![no-string](/slideshows/no-string.jpg)

---

# Tuple Modules

You're familiar with Erlang's `Module:Function(Args)` syntax, where `Module` is an atom.

---

## But it doesn't have to be...

# DUN DUN DUN...

---

## It can be a tuple

How `Module` as a tuple remaps to a normal module call.

![tuple modules](/slideshows/tuple_module.png)

---
Before we move on,

## A quick Record Rehash

Remember, Erlang records are syntactical suger for tuples


![record definition](/slideshows/record-definition.png)

---

With that out of the way...

## Using Records as Tuple Modules

The following calls are the same

![record tuple module](/slideshows/record_tuple_module.png)

---

## This leads us to this convenient, if possibly unclear (by Erlang conventions) syntactical construct

![convenience](/slideshows/record_tuple_module_convenience.png)

---

# This can make your source code rather difficult to follow

---

Which brings us to

### Erlang's most controversial, deprecated, experimental feature that's surprisingly but I guess not that surprisingly popular

---

## Parameter Modules

### Danger: Do Not Approach

![abom](/slideshows/chimera-pmod.jpg)

---


## P-mod definition

```erlang
-module(module_name, [Arg1, Arg2, ...]).
```

---

## What Erlang Compiler does to your pmod

* Creates and Exports `new(Arg1, Arg2, ...)` function
* Converts all `Mod/X` to Mod/X+1`, adding `{?MODULE, Arg1, Arg2...}` as last Arg.
* In your code, all variables from module definition are available in function code

---

## These modules are equivilant

![pmod](/slideshows/pmod-sbs.png)

---

# Warning!

### Using pmods will incur the wrath of the Erlang mailing list! 

---

## Recommendation!

If you're going to use pmods, be damn well sure you name your variables well, because it's much less obvious just from the code where the relevant module code is.

---

# A really fast way to crash the Erlang VM

(takes me a hair over a minute with default configuration)

```erlang
[list_to_atom(integer_to_list(N)) || N <- lists:seq(1,2000000)].
```

---

![atom table](/slideshows/atoms-dynamic-create.jpg)

---

# Atom Table

## Atoms are never garbage collected

---

# Atom Table (continued)

* Be very careful about creating atoms with `list_to_atom/1`.
* When in doubt, use `list_to_existing_atom/1`

---

# Atom Table (continued)

* Increase atom table limit with the `erl +t N`

---

# Erlang Binaries

* Large binaries stored in a separate "binary heap"
* Sub-binaries point to specific byte-ranges in the binary
* Fast for message passing - just passing a pointer

---

# But, it can lead to problems...

```erlang
get_first_100_bytes() ->
	{ok, Bin} = file:read_file("huge_file.bin"),
	binary:part(Bin, 100).
```
---

## Until all references of a binary have been release, a binary will not be garbage collected

---

## Solutions

* Use `binary:copy/1` to make an actual copy of a binary from the heap
* Use `binary:referenced_byte_size/1` to get the size of the full binary referenced

---

# Erlang structures are 1-indexed

* lists
* tuples
* string (`string` module)

---

# Except when they're not

* Array with the `array` module are 0-indexed
* Binaries are 0-indexed

---

# Leaky case expressions

## Any variables defined in a case expressions will be bound outside the case expressions.

---

## Y and Y2 will be available outside of the case

```erlang
MyThing = case X of
	Y when is_integer(Y) ->
		Y2 = Y + 1,
		Y2 * 10;
	Y when is_list(Y) ->
		Y2 = list_to_integer(Y),
		Y3 = Y2 + 1,
		Y3 * 10
end
```
## Trying to use Y3 will cause an error because its not in both case clauses

---

# Partial Solution, if this is a problem

* Wrap clauses with: `fun() -> ... end()` (don't do this)
* Cannot solve that variables are bound by the case match clauses.

---

# Ideal solution:

* Refactor into multiple function clauses (DO THIS)

---

# Equals Binary

What's wrong here?

```erlang
Bin=<<"my amazing binary">>.
```

---

# Magic Time

![Erlusion](/slideshows/erlusion-meme.jpg)

---

## See the issue?

```erlang
[
	#food{type="fruitcake"},
	#food{type="banana"}
	#food{type="watermelon"},
	#food{type="lemon"}
].
```
---


![tada](/slideshows/tada-its-gone.jpg)

---

# Chaining Records

```erlang
#record{field=Val}#record{otherfield=Val2}
```
is equivilant to
```erlang
(#record{field=Val})#record{otherfield=Val2}
```

Which is equivilant to

```erlang
R = #record{field=Val},
R#record{otherfield=Val2}
```

---

## So if you see records getting squashed, or outright removed from a list, now you know

If you leave off a trailing comma, you may encounter the undesired squashing of records.

---

# This confusion is common

It had Joe Armstrong thinking it was a compiler bug in 2011.

---

# Thanks!

## Questions? Comments? Suggestions? Horror Stories of your own?

If you thought this was the coolest thing you've ever seen and I'm the most bad dude (even bad enough to save the president), you should ping me on twitter at: [@jessegumm](http://twitter.com/jessegumm)
