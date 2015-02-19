## Web Application Testing with Nitrogen

a.k.a. Making your web applications scientifically **AWESOME**

![beakman](/slideshows/beakman.jpg)

Jesse Gumm ([@jessegumm](http://twitter.com/jessegumm))

February 19th, 2015

---

## Ninja-quick Nitrogen Overview

![ninja](/slideshows/ninja-nitrogen.png)


* Erlang Web Framework [frag=1]
* Event Driven [frag=2]
* Pure Erlang rather than HTML/Javascript [frag=3]

---

## Integrated Testing Suite

* Introduced in version 2.3 (just released) [frag=1]
* Drives the Browser, but validates results server-side [frag=2]

---

## Why not just use Selenium?

http://docs.seleniumhq.org

* Selenium requires a browser plugin (AFAIK) [frag=1]
* Selenium Web Driver for Erlang is not a completely FOSS license. [frag=2]

  From the License: [frag=3]

  *The software MAY NOT be used in combination with the Proper tool.*[frag=3]
* Nitrogen already has the necessary capabilities built-in [frag=4]

---

## Nitrogen's Testing Suite Overview

* Can test in multiple browsers automatically
* Simulate Clicks [frag=1]
* Verify postbacks [frag=2]
* Verify client-side things (like dimensions or placement) [frag=3]
* Update form fields [frag=4]
* Anything Nitrogen or Javascript can do. [frag=5]

---

## Three "Layers"

* Entire Application [frag=1]
* Each Test Page [frag=2]
* Individual tests [frag=3]

---

### Invocation

![invoke](/slideshows/invocation.jpg)

`wf_test:start_all(nitrogen)`

---

### Top Layer

## Whole Application


Defined in the Application Configuration

```erlang
{nitrogen, [
	...
	{test_browsers, [
		"google-chrome",
		"firefox"
		%% etc
	]},

	{tests, [
		"/some/test/page",
		"/a/different/test",
		"/a/querystring?test=yes"
	]}
]}
```
[frag=1]

Will test each page in each browser in order[frag=2]

Will generate an overall summary page[frag=3]

---

### Middle Layer

## Each Page to Test

Call either one from the `main()` function on page:

### Self-contained test page:[frag=1]

```erlang
wf_test:start(Fun)
```
[frag=1]


### Testing a different page:[frag=2]
```erlang
wf_test:start_other(PageModule, Fun)
```
[frag=2]

The Fun specified will be a function (arity=0) that runs individual tests[frag=3]

---

### Lower Layer

## Individual Tests

All follow a similar pattern:

* Test name[frag=1]
* A Setup Function[frag=2]
* An optional client-side javascript function[frag=3]
* An Assertion Function[frag=4]

---

### Lower Layer

## Individual Tests

### `?wf_test_auto`[frag=1]

Automatically postback after setup is complete.[frag=1]

### `?wf_test_manual`[frag=2]

Test needs to be manually confirmed (?wf_test_event(TestName))[frag=2]

### `?wf_test_js`[frag=3]

Run and return some javascript values, which are passed to the AssertionFun.[frag=3]

---

# Demo Time!

https://github.com/choptastic/nitrogen_test_example

---

## Full tests for NitrogenProject.com

https://github.com/nitrogen/NitrogenProject.com/tree/master/src/tests

```bash
git clone git://github.com/nitrogen/NitrogenProject.com
cd NitrogenProject.com
make test_all
```

Enjoy the fireworks!

---

# Documentation

http://nitrogenproject.com/doc/advanced/testing.html

---

# Questions?

Nitrogen Homepage: http://nitrogenproject.com

Github: http://github.com/nitrogen

Twitter: [@nitrogenproject](http://twitter.com/nitrogenproject)

Slides for this talk are up on http://slides.sigma-star.com
