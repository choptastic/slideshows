# A Ninja-Quick intro Some Erlang Web Frameworks

Milwaukee Functional Programming Meetup

Jesse Gumm ([@jessegumm](http://twitter.com/jessegumm))

Wednesday, Sep 25th, 2013

---

## Three main "full" Erlang Web Frameworks

* Nitrogen
* ChicagoBoss
* Zotonic

These three frameworks provide additional abstractions like client-side enhancements, database integration, etc.

---

## Erlang Web Servers that are server-type frameworks

* Webmachine
* Yaws
* Cowboy
* Mochiweb
* Elli
* Inets

These are more server-only architectures. You can build capable sites with
them, but the client-side stuff is up to you.

---

## Simple Bridge

* Used by both Nitrogen and ChicagoBoss
* Server-abstraction to allow support of most Erlang Server backends behind the full framework.
* Example Uses:
	* Run Nitrogen on top of Yaws, Cowboy, Mochiweb
	* Run ChicagoBoss on top of Mochiweb or Cowboy

---

## A word about websockets

Websockets are currently supported by:

* Cowboy
* Yaws
* ChicagoBoss (with Cowboy as backend)
* N2O (fork of Nitrogen, also Cowboy-only)
* Websocket support coming to SimpleBridge soon
---

# So let's get to it

---

# ChicagoBoss

Author: Evan Miller ([@evmill](http://twitter.com/evmill))

Homepage: http://chicagoboss.org

---

# Features

* Rails for Erlang (MVC)
* Database Abstraction layer (`BossDB`)
* Integrated Mail server with (`gen_smtp`)
* Framework-specific Test Suite (`BossTest`)
* Django Templates

---

# Contraversy

* Heavy use of Parameter Modules
* Lots of parsing trickery
* Code Generation

---

## Useless Hello World

`src/controllers/mkefp_greeting_controller`:
```erlang
-module(mkefp_greeting_controller, [Req]).
-compile(export_all).
 
hello('GET', []) ->
    {output, "Hello Evan"}.
```

Open browser to http://127.0.0.1:8001/greeting/hello

---

## Slightly more useful Hello World

`src/views/greeting/hello.html`

```html
Hello, <b>{{ name }}</b>, my best of friends.
```

`src/controllers/mkefp_greeting_controller`:

```erlang
-module(mkefp_greeting_controller, [Req]).
-compile(export_all).

hello('GET', [Name]) ->
	{ok, [{name, Name}]}.
```

Open browser to http://127.0.0.1:8001/greeting/hello/Your+Name

---

# Resources for ChicagoBoss

* Tutorial PDF: http://www.chicagoboss.org/tutorial.pdf
* Twitter: [@chicagoboss](http://twitter.com/chicagoboss)
* Mailing List: https://groups.google.com/forum/#!forum/chicagoboss
* IRC: irc.freenode.com: #chicagoboss 

---

# Nitrogen Web Framework

Original Author: Rusty Klophaus ([@rustyio](http://twitter.com/rustyio))

Current Maintainer: (*Me!*) Jesse Gumm ([@jessegumm](http://twitter.com/jessegumm))

Homepage: http://nitrogenproject.com

---

# Features

* Event-driven development with Ajax postbacks
* Web DSL with the use of Nitrogen Elements
* Nitrogen HTML Templates (Django Templates coming soon)
* Customizable Handlers

---

# Contraversy

* Heavy use of Erlang Records
* Process dictionary for storing "context"
* No built-in database integration

---

## Web DSL with use the Nitrogen Elements
### What does it mean?!

Write HTML/Javascript with Erlang terms rather than HTML

```erlang
[
	#span{text="Some text in a span"},
	#button{text="A button", postback=button_was_clicked}
].
```

```erlang
%% When some_button is clicked, pop a javascript alert
wf:wire(some_button, #event{type=click, actions=[
	#alert{text="This will pop a Javascript Alert"}
}).
```
---

## Useless Hello World

```erlang
-module(index).
-compile(export_all).

main() -> "Hello Mike".
```

---

### A Slightly More Useful Hello World

```erlang
-module(index).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").

main() -> #template{file="./site/templates/bare.html"}.

title() -> "My Hello World".

body() ->
	#panel{body=[
		"Click below to see magic",
		#br{},
		#button{
			id=the_button,
			postback=click,
			text="Click Me, I dare you!"
		}
	]}.

event(event) ->
	NewBody = #panel{
		id=the_replacement,
		text="Congrats, you did it"
	},
	wf:replace(the_button, NewBody).
```

---

# Nitrogen Resources

* Tutorial Slideshow: http://nitrogenproject.com/doc/tutorial.html
* Twitter: [@nitrogenproject](http://twitter.com/nitrogenproject)
* Mailing List: https://groups.google.com/d/forum/nitrogenweb
* IRC: irc.freenode.net #nitrogen

---

# Zotonic

Author: Marc Worrell ([@mworrell](http://twitter.com/mworrell))

Homepage: http://zotonic.com

---

# Features

* Full-Featured CMS
* Websockets
* Integrated Email
* PostgreSQL
* Django Templates
* Multiple Sites from one Erlang Instance

---

# Contraversy

None that I'm aware of, except maybe that it's a full-featured CMS
