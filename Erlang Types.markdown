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

# Other Erlang non-types worth mentioning

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

### If you need to increase the atom limit becayse you're creating atoms on the fly, you're only delaying the inevitable

![...gonna have a bad time](/slideshows/atoms-bad-time.jpg)

---

# pid

  * Process Identifier
  * `self()` returns the pid of the current process
  * `spawn()` and similar functions return the pid of the spawned process.
  * `Pid ! Msg` sends `Msg` to the process `Pid`
  * node-agnostic: Doesn't matter what node a Process is running on, if it's in the cluster, it can be communicated with.
  * guard: `is_pid()`
  * Looks like: `<0.39.0>`

---

# pid, continued

If the first number in a pid is `0`, then that means the pid is local to the current node.

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

  * A list is a linked-list of Erlang terms. 
  * Each element can have any type.
  * `[1, 50, Fun, [an, inner, list], "something else"]`

---

# tuple

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
