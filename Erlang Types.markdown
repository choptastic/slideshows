# Introduction to Erlang Types

Chicago Erlang User Group

September 11th, 2013


Presented by Jesse Gumm

Twitter: [@jessegumm](http://twitter.com/jessegumm)
|
Github: [choptastic](http://github.com/choptastic)

---

# Erlang is Dynamically Typed


```erlang
HopefullyInteger = somefunction() + 5.
```


Let's hope `somefunction()` actually returns an integer.

---

# The core Erlang types

## Simple types:

  * integer
  * float
  * atom

---

# The core Erlang Types

## Specialized types:

  * pid
  * fun (function)
  * reference
  * port

---

# The core Erlang Types

## Compound types:

  * binary
  * list
  * tuple

---

## Other non-types worth mentioning

  * boolean
  * record
  * string
  * iolist
  * proplist
  * Module-specific types: dict, queue, etc

---

# Integers

   * -1
   * `456547568456874645865794678688769789768` (Big ints. Erlang Ints don't overflow)
   * `$F` (ASCII Ordinals)
   * `2#100101001` (Binary Numbers)
   * `16#faded` (Hexadecimal)
   * `36#erlang` (Base 36? Sure, why not)
   * guard: `is_integer`

---

# Floats

   * `2.5`
   * `3.99999999`
   * `10e5`
   * `10e-5`
   * guard: `is_float`

---

# Atoms

Atoms are like strings, but not like strings.

*Thanks, that helps a lot*

---

# Atoms, continued

  * More like C `enums`, 
  * All atoms are unique. No atom equals another atom, unless it is the *same* atom.
  * Commonly used in pattern matching.
  * **Very Fast**: O(1) comparison.
  * guard: `is_atom()`

---

# But atoms have their caveats...

### (dun, dun, DUUUUN!!!)

---

## The Atom table has a limit

![One does not simply...](/slideshows/atoms-dynamic-create.jpg)

---

## Atoms are not garbage collected

![...cannot be unmade](/slideshows/atoms-not-unmade.jpg)

---

## If you really need more atoms

Use `erl +t` 

*Default atom table limit is 1,048,576 (2^20)*

---

![...gonna have a bad time](/slideshows/atoms-bad-time.jpg)

---

# pid

  * Process Identifier
  * `self()` returns the pid of the current process
  * `spawn()` and similar functions return the pid of the spawned process.
  * `Pid ! Msg` sends `Msg` to the process `Pid`
  * guard: `is_pid()`
  * Looks like: `<0.39.0>`

---

# pid, continued

If the first number in a pid is `0`, then that means the pid is local to the current node.

---

## pids are node-agnostic

![good guy erl-greg](/slideshows/remote-node.jpg)

---

# pid example

```erlang
> Pid = spawn(fun() ->
> 	receive
> 		Msg -> io:format("Received: ~p~n",[Msg])
> 	end
> end).
<0.40.0>
> Pid ! a_message.
a_message.
Received: a_message
```

---

# fun

  * Functions have *arity* (number of arguments).

---

## Anonymous Function:
```erlang
> MyFun = fun(A) -> A * 2 end.
> MyFun(2).
4.
```

---

# fun

## Closure:

```erlang
> B = 4.
> MyFun = fun(A) -> A * B end.
> MyFun(2).
8.
```

---

# fun

## Named function in the same module:

```erlang
> MyFun = fun function/2.
> MyFun(X,Y).
```

---

# fun

## Named function in another module:

```erlang
> MyFun = fun module:function/2.
> MyFun(X,Y).
```
---

# reference

  * Created with `make_ref/0`
  * Unique for the age of the node.
  * Unique across nodes.
  * Used for making unique references.

---

#port

  * Port Identifier 
  * Used for communicating with other OS processes (making drivers)

---

# list

  * "A compound data type with a variale number of terms."
  * Each element can have any type.
  * Denoted with square brackets: `[ ]`.
  * Example: `[1, 50, Fun, [an, inner, list], "something else"]`

---

## Poorly Photoshopped Gandalf Armstrong Says:

![one indexed](/slideshows/one-indexed.jpg)

---

# list representation

![list](/slideshows/list.png)

```erlang
> List = [5, a, 3.5].
[5, a, 3.5]
> hd(List).
5
> tl(List)
[a, 3.5]
> hd(tl(List))
a
> tl(tl(tl(List))).
[]
```

---

## Adding an item to the front of a list

```erlang
> List = [5, a, 3.5].
[5, a, 3.5].
> List2 = [6 | List].
[6, 5, a, 3.5].
```
### O(1) Complexity

---

## Adding an item to the end of a list

```erlang
> List = [5, a, 3.5].
[5, a, 3.5].
> List3 = List ++ [bears].
[5, a, 3.5, bears].
```
### O(N) Complexity

---

## Deleting items from a list

```erlang
> List = [1, 2, 4, 8].
> ToDelete = [1, 3, 4].
> List2 = List -- ToDelete.
[2, 8].
```

### O(N * M) Complexity

Where:

  * N = `length(List)`
  * M = `length(ToDelete)`
---

## List BIFs Usable in guards:

 * `length(List)`
 * `hd(List)` - Head of the List
 * `tl(List)` - Tail of the List

---

## Commonly used functions from the lists module:
 * `lists:reverse(List).`
 * `lists:nth(N, List).`
 * `lists:nth_tail(N, List).`
 * `lists:sort(List)`

---

## Matching List Head and Tail

```erlang
> List = [a, b, c, d].
[a, b, c, d]
> [H | T] = List
[a, b, c, d]
> H.
a
> T.
[b, c, d]
```

---

## Matching List Elements

```erlang
> List = [a, b, c, d].
[a, b, c, d]
> [First, Second, Third, Fourth] = List.
[a, b, c, d]
> Second.
b
> Fourth.
d
```

---

## Matching List Elements and Tail in one match

```erlang
> List = [a, b, c, d].
[a, b, c, d]
> [First, Second | Rest] = List.
[a, b, c, d]
> Second.
b
> Rest.
[c, d]

```


---

# tuple

  * "Compound data type with a fixed number of terms" 
  * A tuple has a specific size.
  * Denoted with curly brackets: `{ }`
  * Example: `{coordinate, 5, 10}`

---

# tuple = fixed size

  * You don't resize a tuple like you do a list.
  * There are no append or prepend operations.
  * You change the value of specific elements

---

# tuple matching

Tuple matching works like lists, except no `head` or `tail`, meaning no pipe (`|`) operator.

```erlang
> ThreeD = {coordinate, 5, -10, 55}.
{coordinate, 5, -10, 55}
> {coordinate, X, Y, Z} = ThreeD.
{coordinate, 5, -10, 55}
> Y.
-10
> Z.
55
```
**Note:** the atom `coordinate` is called a `tag`

---

# tuple element and setelement

```erlang
> ThreeD = {coordinate, 5, -10, 55}.
{coordinate, 5, -10, 55}
> X = element(2, ThreeD)
5.
> NewThreeD = setelement(3, ThreeD, 200).
{coordinate, 5, 200, 55}
```

---

# binary

  * A binary is a list of bits and bytes.
  * `<<[5,10


---

# boolean

---

# record

---

# string

---

# iolist

---

# proplist


---

# Module-specific types: dict, queue, etc

---

# Type conversion

---

# Typespecs

---

# Custom Types

---

# Function Type specs

---

# Record Type specs

---

# Dialzyer
