# Lisp Evaluation Examples with Gendl

The Lisply MCP wrapper provides a `lisp_eval` tool that allows you to
evaluate Lisp code directly within the backend "Lisply-compliant"
environment. The default environment configured with lisply-mcp is
Gendl. This document provides examples of how to use this default
Gendl backend.

## Basic Lisp Evaluation

You can evaluate simple Lisp expressions:

```lisp
(+ 1 2 3)
```

Result: `6`

## Working with Gendl Objects

GDL (GENeral-purpose Declarative Language) is the core declarative
modeling language in Gendl. Here are examples of defining and using
GDL objects:

**Note for the below examples:**

`the` is a referencing macro used to access messages in the
current object (implicit `self`).

`theo` is a synonym for `the-object` and is used to access messages in
an object that you give as an expression, e.g. a variable, as the
first argument to `theo` e.g.: `(theo my-instance total-mass)`

`(the ...)` expands exactly to `(the-object self ...)`


```
(the message-name)            ; message to implicit self, message has no args.
(the (message-name arg1 arg2)) ; implicit self message with args
(theo object message-name)     ; explicit object, message without args.
(theo object (message-name arg1 arg2)) ; explicit object, message with args. 

```


### Defining a Simple Circle

```lisp
(define-object my-circle (base-object)
  :input-slots (radius)
  :computed-slots
  ((area (* pi (expt (the radius) 2)))
   (circumference (* 2 pi (the radius)))))
```

Result: `#<GDL-CLASS CIRCLE>`

### Creating and Using a Circle Instance

```lisp
(let ((my-circle (make-object 'circle :radius 5)))
  (list :area (theo my-circle area)
        :circumference (theo my-circle circumference)))
```

Result: `(:AREA 78.53981633974483 :CIRCUMFERENCE 31.41592653589793)`

### Defining a Box with Volume and Surface Area

```lisp
(define-object box (base-object)
  :input-slots
  (length width height)
  :computed-slots
  ((volume (* (the length) (the width) (the height)))
   (surface-area (* 2 (+ (* (the length) (the width))
                         (* (the length) (the height))
                         (* (the width) (the height)))))))
```

Result: `#<GDL-CLASS BOX>`

### Computing Box Properties

```lisp
(let ((box (make-object 'box :length 5 :width 3 :height 2)))
  (list :volume (theo box volume)
        :surface-area (theo box surface-area)))
```

Result: `(:VOLUME 30 :SURFACE-AREA 62)`

## Using Gendl via Claude

When interacting with Claude, you can ask it to evaluate Lisp code in Gendl using the `lisp_eval` tool. For example:

```
User: Could you calculate the volume of a box with dimensions 10x5x3?

Claude: I'll calculate the volume of a box with those dimensions using Gendl:

[Claude calls lisp_eval with appropriate code]

The volume of a box with dimensions 10x5x3 is 150 cubic units.
```

## Common Usage Patterns

1. **Define a GDL Object**: First define your GDL object with its properties
2. **Create an Instance**: Use `make-self` to create an instance with specific parameters
3. **Access Properties**: Use `the-object` to access computed properties

## Tips and Best Practices

- Use `define-object` to create reusable object templates
- Separate input slots (parameters) from computed slots (derived values)
- When evaluating expressions, wrap them in `let` bindings to create a clean scope
- Use `make-self` for creating instances in the evaluation context
- Access properties with `the-object` rather than `the` when in a separate context
