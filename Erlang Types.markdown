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
  * Commonly used in pattern matching
  * **Very Fast**: O(1) comparison
  * guard: `is_atom`

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

## If you're creating atoms on the fly, you're probably only delaying the inevitable

![...gonna have a bad time](/slideshows/atoms-bad-time.jpg)

---

# pid (Process Identifier)

  * `self()` returns the pid of the current process
  * `Pid ! Msg` sends `Msg` to the process `Pid`
  * node-agnostic: Doesn't matter what node a Process is running on, if it's in the cluster, it can be communicated with.
  * guard: `is_pid`

---

# fun (Anonyous Function)

---

# reference


---

#port

---

# binary

---

# list

---

# tuple

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
