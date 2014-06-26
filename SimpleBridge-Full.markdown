# SimpleBridge
### Building Erlang apps with any Erlang Webserver

Chicago Erlang User Group

June 26th, 2014

Jesse Gumm (@jessegumm)

---

# Problem

Erlang has a large number of different webservers, each with its own API

---

# Solution

Simple Bridge provides single API for many of them:

* Cowboy
* Inets
* Mochiweb
* Webmachine
* Yaws
* (Elli coming soon)
* Misultin (deprecated)

---

# But why?

SimpleBridge was built when Inets, Mochiweb, and Yaws were it.

Before Cowboy, Elli, or Misultin's Rise and Fall.

---
# SimpleBridge's main purpose

## Abstraction layer between a framework and the webserver

(You probably won't be building an application directly on top of SimpleBridge)

---

# Currently used by

* Nitrogen Web Framework (http://nitrogenproject.com)
* ChicagoBoss (http://chicagoboss.org)

---

# What are websockets?

* Lightweight
* Bidirectional
* Asynchronous

Replacement for AJAX and Comet/Long polling

---

# A New Problem

Websockets are only natively supported by Cowboy and Yaws (and recently by Mochiweb)

Again, each with its own API

---

# A new solution!

Simple Bridge gives a unified API for websockets.

...even for webservers that don't support websockets natively

---

# How is this done?

Checks for the websocket headers, and hijacks the socket.

---

## How does the SimpleBridge API simplify this?

Let's use an example of simply retrieving a header from the request

---

### Getting a header
# Inets

```
Headers = Req#mod.parsed_header,
proplists:get_value(Headers, "content-type")
```

---
### Getting a header
# Cowboy
```
{Headers, Req} = cowboy_req:headers(Req),
proplists:get_value(Headers, <<"content-type">>)
```
---
### Getting a header
# Yaws
```
Headers = yaws_api:arg_headers(Req),
yaws_api:headers_content_type(Headers)
```
---
### Getting a header
# Mochiweb
```
Headers = mochiweb_request:get(headers, Req),
mochiweb_headers:get_value("Content-Type", Headers)
```
---
### Getting a header
# Webmachine
```
Headers = wrq:req_headers(Req),
mochiweb_headers:get_value("Content-Type", Headers)
```

---
## Getting a header
# SimpleBridge
```
sbw:header(content_type, Bridge),

%% or using tuple calls:

Bridge:header("Content-type"),
```
The Header can be a binary, string, or atom, and is not case sensitive
---

### And that's just something simple like getting a header

More complicated things: parsing cookies, parsing POST and GET variables, etc

---

## A note about inputs and outputs

* Generally, Type in, Type out
* Exception: Atom In, String/Lists out

---

# Using SimpleBridge

Add a SimpleBridge "Handler" (`-behaviour(simple_bridge_handler)`) which defines a few functions:

* `run(Bridge)` - Handle normal requests
* `ws_init(Bridge)`
* `ws_message(Msg, Bridge)`
* `ws_info(Msg, Bridge)`
* `ws_terminate(Reason, Bridge)`
---
# Hello World
```
-module(demo).
-behaviour(simple_bridge_handler).
-export([run/1]).

run(Bridge) ->
	Bridge1 = Bridge:set_response_data("Hello World"),
    Bridge1:build_response().
```
---

## Let's build something a little more substantial

* Takes our name
* Submits it as a POST to another page
* Prints it

---

## First, let's route our simple requests.
```
-module(demo2).
-behaviour(simple_bridge_handler).
-export([run/1]).

run(Bridge) ->
	case Bridge:path() of
		"/" -> index(Bridge);
		"/show" -> show(Bridge);
		_ -> four_oh_four(Bridge)
	end.
```
---

## Now, let's make the form:

```
index(Bridge) ->
	Body = "<h1>Enter your name, please</h1>
			<form action='/show' method=post>
				Name: <input type=text name=name>
				<input type=submit text='Go!'>
			</form>",
	Bridge2 = Bridge:set_response_data(Body),
	Bridge2:build_response().
```
---

## And now lets make the page that shows our name
```
show(Bridge) ->
	YourName = Bridge:post_param(name, "Nameless Person"),
	Body = "<h2>Welcome, ", YourName, "</h2>",
	Bridge2 = Bridge:set_response_data(Body),
	Bridge2:build_response().
```
---

## Finally, let's make our 404 page
```
four_oh_four(Bridge) ->
	Bridge2 = Bridge:set_status_code(404),
	Bridge3 = Bridge2:set_response_data("Not Found"),
	Bridge3:build_response().
```
---

# Let's test it out!

---

```
erl \
	-pa ebin \
	-pa deps/*/ebin \
	-config simple_bridge.config \
	-eval "simple_bridge:start(inets, demo)"
```
---

# A more advanced example: Websockets

From http://github.com/nitrogen/simple_bridge_demo

---

## Let's migrate to something a little better: Yaws

* Add Rebar Dependency
* Change simple_bridge.config or change `simple_bridge:start/2` call

---

# Each Bridge consists of three modules

---

## PLATFORM_simple_bridge.erl

Does the API translation

---

## PLATFORM_simple_bridge_sup.erl

A supervisor that SimpleBridge can use to set up and launch server, by loading the simple_bridge configuration and translating the configuration to server's configuration methods
---

## PLATFORM_simple_bridge_anchor.erl

The module that the server will use to handle requests.

---
# Questions?

Simple Bridge is Open Source

http://github.com/nitrogen/simple_bridge

Branch for this demo: **ws** (for Websocket)
