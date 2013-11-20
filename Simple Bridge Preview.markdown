# A Preview and Demo of Simple Bridge 2.0

Milwaukee Functional User Group

Nov 20th, 2013

Jesse Gumm (@jessegumm)

---

# Problem

Erlang has a large number of different webservers, each with its own API

---

# Solution

Simple Bridge provides unified API for many of them:

---

# Supported Servers

* Cowboy
* Inets
* Mochiweb
* Webmachine
* Yaws

---

# What about Websockets?

---

# What even are websockets?

* Lightweight
* Bidirectional
* Asynchronous

Replacement for AJAX and Comet/Long polling

---

# A New Problem

Websockets are only natively supported by Cowboy and Yaws.

Again, each with its own API

---

# A new solution!

Simple Bridge gives a unified API for websockets.

...even for webservers that don't support websockets natively

---

# How is this done?

Checks for the websocket headers, and hijacks the socket.

---

# Demo Time!

---

# Questions?

Simple Bridge is Open Source

http://github.com/nitrogen/simple_bridge

Branch for this demo: **ws** (for Websocket)
