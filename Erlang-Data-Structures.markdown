# Erlang Data Structures
### (with maps)

Milwaukee Functional Programmer User Group

Tuesday, July 1st, 2014

Jesse Gumm ([@jessegumm](http://twitter.com/jessegumm))

---

## Erlang has two fundamental data structures

# Lists and Tuples

Tuples = just a list with a static number of elements

```
List = [1, 2, "a string", some_atom, <<"binary">>],

Tuple = {1, 2, "a string", some_atom, <<"binary">>}
```

---

## Records

* Syntactical sugar for tuples with named items
* Converted to plain tuples during compilation
* Keys must be atoms

```
-record(person, {name, email, city}).

MyRecord = #person{
	name="Jesse",
	email="jesse@bracketpal.com",
	city="Milwaukee"
}
```
At compile-time, converted to:

```
{jesse, "Jesse", "jesse@bracketpal.com", "Milwaukee"}
```

---

### Lists, Tuples, and Records can all be pattern matched

```
%% Bind A and B to the first two elements of MyList
[A, B, | _] = MyList,

%% Bind C and D to the first and third elements of MyTuple
{C, _, D} = MyTuple

%% Bind Name and City to the values from the record:
#person{name=Name, city=City} = MyRecord.
```

---

## Compound data structures: Proplists

List of two-tuples

```
> MyProplist = [{name, "Billy"}, {employer, "ABC Corp"}].
> proplists:get_value(name, MyProplist).
"Billy"

```
---

## Proplist Pros

* Very simple and human readable
* Fast prepending: O(1) (`[{new_key, new_value} | MyProplist]`)
* Commonly used
* Trivial to traverse

---

## Proplist Cons

* Slow lookup: O(N) (`proplists:get_value(field, Proplist)`)
* Changing a value requires traversing the list
* No real way to optimize, even when sorting because still linked lists.
* Cannot be reliably pattern matched

---

## Compound data structures: dict and gb_trees

```
> Dict = dict:new().
> Dict2 = dict:store(my_key, some_value, Dict).
> {ok, Value} = dict:find(my_key, Dict).11
{ok, some_value}
```

(`gb_trees` interface exactly the same)
---

## Dict (gb_trees) Pros

* Fast: O(logN) insertions and lookups

---

## Dict (gb_trees) Cons

* Cannot be pattern matched
* Not human readable (must be converted with `to_list(Dict)`)

---

## Compound data structures: arrays

* 0-indexed (most erlang structures are 1-indexed)
* Fast and capable of multidimentaional indexing
* Not commonly used
* Despite name, not actually C-type arrays
* Also not pattern matchable

---

## Compound data structures: orddict

* Basically ordered proplists with an interface like `dict`

---

## Maps!!!

![I know maps](/slideshows/i_know_map_fu.jpg)

---

# Maps

### New Primitive Data Structure to Erlang 17.0

---

## Maps: The best of all worlds

* Less cumbersome variation of record syntax.
* Pattern matchable like records.
* Dynamically sized with dynamic keys (like dict)
* Dynamic Key lookup/insert (like dict - not possible with records)
* Keys can be any type

---

## Maps

### Less Cunmberson variation of record syntax

```
#{key => value}

vs

#recordtype{key = value}
```
---

## Maps

### Pattern matchable

```
my_function(#{some_key := SomeVal}) ->
```

`SomeVal` will be bound

---

## Maps

### Dynamically sized with dynamic keys

```
%% Assigns the value value to the key 
maps:put(Key, Value, Map)
```

Future versions will support variable Keys, like
```
MyKey = some_key,
#{MyKey => MyVal}
```
---

## Maps

### Dynamic Key lookup/insert (like dict - not possible with records)

```
maps:get(Key, Map)
```

Future versions will again support variable key matching:

```
MyKey = some_key,
#{MyKey := SomeVal} = Map
%% SomeVal will be bound
```
---

## Maps
### Keys can be any type

Not valid (records with tuples as keys)
```
#my_record{ {a,b,c}=Value }
```

Valid (maps)
```
#{ {a,b,c} => Value }.
```

---
## Maps: Cons

* Missing some parts of the original design (Variable Keys, Single Element lookup)
* Has some initial performance issues (not quite as performant as tuples yet)
* Not compatible with built-in datastores which rely on records (ets, dets, mnesia)

---

## Overview

* Maps are still under development, but improving
* Maps will slowly replace proplists, dicts, gb\_trees, and some instances of records

---

# Questions?

(these slides available on [slides.sigma-star.com](http://slides.sigma-star.com))

Get Erlang from [erlang.org](http://erlang.org) (Latest 17.1 released a few days ago)
