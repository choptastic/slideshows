## Rapid Web App Development with Nitrogen

aka "How to be RAD like this dude"

![rad dude](/slideshows/rad-dude.jpg)

### Chicago Erlang Conference 2014

Jesse Gumm ([@jessegumm](http://twitter.com/jessegumm))

---

## A Question:

### Who got into programming to make games? [frag=1]

# Why? [frag=2]

### We were kids and it's what kids understand [frag=3]

---

### If you want to make games, you need to make tools

## So we made our first apps. [frag=1]

* Easier [frag=2]
* Immediate feedback[frag=3]

---

## Interface development

### Great for new programmers to appreciate making something

* Logo [frag=1]
* Visual Basic [frag=1]
* Hypercard [frag=1]
* HTML Websites on Geocities [frag=1]
* Macromedia Authorware [frag=1]

&nbsp;

### Immediate feedback is powerful! [frag=2]

---

## Another Question:

### How many times have you heard the following statement?

"I have this great idea for an app! Let's build the front-end in Erlang!" [frag=1]

---

### Erlang is a great language for front-end development

## Nitrogen is the tool that makes that possible!

---

![spaceman](/slideshows/nitrogen.png)

## A Brief History of Nitrogen

* Created by Rusty Klophaus in 2008 [frag=1]
* I took over in 2011 [frag=2]
* Soon to be released: Version 2.3.0 (Adds websockets) [frag=3]
* Book soon to be released:<br>Build it with Nitrogen: [frag=4]
  http://builditwith.com/nitrogen [frag=4]

---

# Whet the appetite!

&nbsp;

## http://j.mp/nitro-bit

---


## Nitrogen Fundamental #1

# Nitrogen Elements [frag=1]

### Build your front-end with Erlang records [frag=1]

(Because building web applications with HTML and javascript is overrated) [frag=1]

---

## Nitrogen element example:

Instead of

```html
<input type="button" value="Save Data" id="save-button">
<script>
	$("#save_button").click(function(){ save_data() });
< /script>
```

Write: [frag=1]

```erlang
#button{
	text="Save Data",
	postback={save, Recordid}
}
```
[frag=1]

---

## Nitrogen Fundamental #2:

# Event Driven

* Postbacks generate asynchronous requests to the server
* Think in terms of UI interactions through postbacks

---

## Familiar Pattern for Erlang

#### `gen_server:call() :: handle_call()` [frag=1]

```erlang

gen_server:call(?MODULE, {some_action, Recordid}).

...

handle_call({some_action, Recordid}, _From, State) ->
	do_something(Recordid)
```
[frag=1]

#### `postback tag :: event()` [frag=2]

```erlang
#button{
	text="Save Data",
	postback={save, Recordid}
}.

...

event({save, Recordid}) ->
	save_that_record_to_the_database_yo(Recordid).
```
[frag=2]
---

## Nitrogen Fundamentals

1. Nitrogen Elements
2. Event Driven

&nbsp;

### No context-shifts. Build whole application with Erlang. [frag=1]

(Node brings client-side to the server, Nitrogen brings server-side to the client) [frag=1]

---

![live-coding](/slideshows/a-live-coding-session.png)

---
# Are you sufficiently blown away?

![blown-away-granny](/slideshows/blown-away-granny.jpg)

---

## Nitrogen Features

* 120+ built in Elements, Actions, and Validators [frag=1]
* Easily extendable with your own elements [frag=2]
* Plugin System [frag=3]
* Built-in AJAX, Comet, and Websockets [frag=4]
* jQuery Mobile [frag=5]
* REST Handler [frag=6]
* Runs on any Erlang Webserver with SimpleBridge [frag=7]

---

## What's on the horizon?

* Site Generator ("soup")
* Releases with relx
* Support for more webservers: elli and psycho

---

# Why you should choose Nitrogen

* Erlang reliability and scalability
* Code cleanliness enforced by Erlang's syntax and record validation
* Convenient client/server abstraction

---

# Resources

* Website: http://nitrogenproject.com
* Github: http://github.com/nitrogen/nitrogen
* Twitter: [@nitrogenproject](http://twitter.com/nitrogenproject)
* Mailing List: http://groups.google.com/group/nitrogenweb
* IRC: [#nitrogen @ freenode.net](irc://chat.freenode.net#nitrogen)

---

# The Book

## Build it with Nitrogen

### http://builditwith.com/nitrogen

* Free download of the in-progress version (updated periodically)
* Pre-order the ebook and/or physical copy (pay what you want)

---

### So why did I talk about games and visual basic earlier?

## Nitrogen makes interface development accessible in Erlang. [frag=1]

And that's huge for on-ramping new developers to Erlang.[frag=1]

---

# Questions?

Ping me on twitter: [@jessegumm](http://twitter.com/jessegumm)

These slides will be available on http://slides.sigma-star.com
