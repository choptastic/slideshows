# Erlang Parse Transforms

## A simple introduction to metaprogramming in Erlang

Jesse Gumm (http://jessegumm.com)

Milwaukee Functional Programming Group

January 16th, 2018

---

# The setup

I primarly work with the [Nitrogen Web Framework](http://nitrogenproject.com), which depends *heavily* on "Erlang Records"

---

### Erlang Records?

Erlang Records are a syntactical sugar around normal every day tuples.

The following:

```erlang
#my_record{
   field1="Some Value",
   field2="Some Other Value"
}.
```

is really just:

```erlang
{my_record, "Some Value", "Some Other Value"}.
```

---

### Nitrogen uses Erlang Records to create HTML elements

The following:

```erlang
#span{
	body="Some Content",
	class=my_awesome_span
}
```

Is rendered by Nitrogen into:

```html
<span class="my_awesome_span">Some Content</span>
```


---

### I wanted to be able to extend records

I wanted to be able to take an existing record definition and extend it (like inheritance) to create a new record definition.

Example:

Take the record definition:

```erlang
-record(button, {text, class, click}).
```

And make a new #btn record, adding a type attribute without having to redefine all fields of #button[frag=2]

```erlang
#btn{
	text="Click me",
	class="awesome_button",
	click=#alert{text="You're just so awesome, dude!"},
	type=warning %% not a native attribute to #button
}
```
[frag=2]
---

## The problems

* There is no construct within Erlang to do this[frag=1]
* Records in Erlang are Compile-time constructs only[frag=2]

---

# Time to make a parse transform

---

## What is a Parse Transform?

### A Parse Transforms is a module that alters the parsed Erlang "abstract forms" before running the compiler.

They take this list of Erlang terms representing the parsed abstract forms as both input and output.[frag=1]

Read all about the Abstract format at: [frag=2]

http://erlang.org/doc/apps/erts/absform.html[frag=2]

---

### Make a parse transform

Define a module that exports the function parse_transform/2

```erlang
-module(my_pt).
-export([parse_transform/2]).

parse_transform(Forms, Options) ->
	Forms.
```

(Yes, the above parse transform doesn't change any of the forms)[frag=1]

---

### Use a parse transform

Add the following to run the parse transform on a module:

```erlang
-compile({parse_transform, my_pt}).
```

---

# Playtime

---

# rekt

http://github.com/nitrogen/rekt

Rekt is the Parse Transform I ended up making.

---

## Using rekt

By adding the following to a module


```erlang
-compile({parse_transform, rekt}).

-extend(record1, record2, [new_field1, new_field2]).
```

You've effectively defined a new record[frag=1]

* Called #record2[frag=1]
* Has all the fields of #record1[frag=2]
* And also has new_field1, new_field2[frag=3]

---

## Why this is good?

It cuts down on a tremendous amount of code for extending Nitrogen.

[Look at this commit as an example](https://github.com/choptastic/coldstrap/commit/bc83a2a14a4d71f9ec5ff804e41957f6325b990d#diff-6a0990b667313a0e1f0aaffd9b6ec2af)

---

## Thanks

Rekt can be found at:

https://github.com/nitrogen/rekt

I'm on twitter and never tweet ([@jessegumm](http://twitter.com/jessegumm)) but be my friend anyway.

