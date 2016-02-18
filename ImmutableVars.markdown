## Stop the Mutants

![mutant free america](/slideshows/senatorkelly.jpg)

#### Getting accustomed to Immutable Variables In Functional programming

[Jesse Gumm](http://jessegumm.com) | February 17th, 2016 | [mkefp.com](http://mkefp.com)

---

## Some Well-known Functional Languages with Immutable Variables

* Haskell
* Erlang and Elixir
* ML (Standard ML, Caml, OCaml)
* F# (mostly)
* Elm

---

## Problems with Mutability

A terribly contrived example

```javascript

function generator() {
	var a = 1;
	var new_fun = function() {
		return a;
	}
	a++;
	return new_fun;
}

var my_generator = generator();
my_generator();
```

What is the return value of `my_generator()`?

---

## With mutable variables

* You have to actually think about scope[frag=1]
* Did something within the same scope change the variable[frag=2]
* How are closures handled?[frag=3]
* What is the state of things when a crash happens?[frag=4]

---

## Concurrency with mutable variables and shared memory

* How do we control who can change what and when?
* Mutexes and Semaphores

---

## Immutable Variables encourage good functional programming practices

---

### Benefits of immutability

## State is always explicit

* State is in variables[frag=1]
* Variables can't change[frag=2]
* State must be passed around as a variable[frag=3]
* Makes debugging easier[frag=4]

---

### Benefits of immutability

## Shorter function length

Long functions are things that just happen when you can rebind variables mid-function.

---

### Benefits of immutability

## Scoping concerns are minimized

Since a variable can't change after being bound, you don't have to worry about "did something else changed this variable"

---

### Benefits of immutability

## Concurrency Simplified

Message passing instead of shared data

---

# Enough Propaganda

---

## Immutability means doing some stuff like this:

```erlang
Req1 = prepare_cookies(Req, Cookies),
Req2 = prepare_headers(Req1, Headers),
Req3 = prepare_body(Req2, Req2)
```

---

## How to embrace immutability? What about loops?

* Recursion
* Maps and folds
* List comprehensions

---

## Recursion

```javascript
function do_something_x_times(X) {
	for(i=0; i<X; i++) {
		do_something(i);
	}
}
```

```erlang
do_something_x_times(X) ->
	do_something_x_times(0, X).

do_something_x_times(I, X) when I =< X ->
	ok;
do_something_x_times(I, X) ->
	do_something(I),
	do_something_x_times(I+1, X).
```

---

## Maps

```javascript
function do_something_x_times(X) {
	for(i=0; i<X; i++) {
		do_something(i);
	}
}
```

```erlang
do_something_x_times(X) ->
	lists:map(fun(I) ->
		do_something(I)
	end, lists:seq(0, X-1)).
```
---

## List Comprehensions as Map

```javascript
function do_something_x_times(X) {
	for(i=0; i<X; i++) {
		do_something(i);
	}
}
```

```erlang
do_something_x_times(X) ->
	[do_something(I) || I <- lists:seq(0, X-1)].
```

---

## List Comprehension as Filter

```javascript
function filter_things(orig_arr) {
	var new_arr = [];
	foreach(var key in orig_arr) {
		if(some_condition(orig_arr[key])) {
			new_arr[] = orig_arr[key];
		}
	}
	return new_arr;
}
```

```erlang
filter_things(List) ->
	[X || X <- List, some_condition(X)].
```

---

## List Comprehension as Filter and Map

```javascript
function filter_things(orig_arr) {
	var new_arr = [];
	for(var key in orig_arr) {
		if(some_condition(orig_arr[key])) {
			new_arr[] = process_value(orig_arr[key]);
		}
	}
	return new_arr;
}
```

```erlang
filter_things(List) ->
	[process_value(X) || X <- List, some_condition(X)].
```

---

## Folding for accumulation

```javascript
function sum_of_squares(arr) {
	var total=0;
	for(var key in arr) {
		total += arr[key] * arr[key];
	}
	return total;
}
```

```erlang
sum_of_squares(List) ->
	lists:foldl(fun(X, Total) ->
		Total + X*X
	end, 0, List).
```

---

## Concurrency and interprocess communication


```erlang
{ok, Pid} = spawn(fun() ->
	receive
		Msg -> io:format("You received a message: ~p",[Msg])
	end
end),

Pid ! "Oh what a glorious day".
```

* Messages must be passed to do concurrency
* Debugging is easier because a variable you're using can't be changed by another process (because it can't be changed at all)

---

## Problems with Immutable Variables

* Not as straightforward - the computer doesn't actually work that way. Memory, but its nature is mutable[frag=1]
* Doing inline updates like arr[x] = some_new_value; not possible. Must instead change the single value and return a new list/array/etc.[frag=2]
* Var1, Var2, Var3, Var4...ad infinitum gets tiring. (Elixir solves this nicely)[frag=3]
* Wrapping your brain around lots of recursion takes time.[frag=4]

---

## Embracing Immutability in languages support mutation

Embrace `static` keyword.

---

## Conclusion

Getting used to immutable variables takes time, but as you get more comfortable
with recursion, it grows on your and helps make your code cleaner, easier to
read, and easier to debug.

---

# Questions?

I'm writing a book about Erlang Web Dev with Nitrogen: [builditwith.com/nitrogen](http://builditwith.com/nitrogen)

I'm on Twitter: [@jessegumm](http://twitter.com/jessegumm)

These slides are available online at: [slides.sigma-star.com](http://slides.sigma-star.com)
