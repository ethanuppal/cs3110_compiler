# x86ISTMB Programming Language - Introduction and Tutorial

## Overview

x86ISTMB is a minimalistic, strongly typed programming language designed for simplicity and clarity. It focuses on providing a straightforward approach to programming by limiting its features to the core essentials. This document serves as an introductory guide and tutorial to the x86ISTMB programming language.

## Variables and Types

### Declaring Variables

In x86ISTMB, variables are declared using the `let` keyword followed by the variable name and an initial value. Currently, x86ISTMB supports only integer data types for variables.

Syntax:

```swift
let variableName = value
```

Example:

```swift
let x = 5
```

This statement declares a variable named `x` and initializes it with the value `5`.

## Operators

x86ISTMB supports three basic operators for performing arithmetic and assignment operations on integer values. These operators are:

1. `+` (Addition): Adds two integer values.
2. `=` (Assignment): Assigns a value to a variable.
3. `*` (Multiplication): Multiplies two integer values.

### Using Operators

Operators can be used in expressions to perform operations on variables and integer literals.

#### Addition (+)

Syntax:

```swift
let variableName = operand_A + operand_B
```

Example:

```swift
let x = 5
let y = x + 3 // y is now 8
```

#### Assignment (=)

Syntax:

```swift
variableName = newValue
```

Example:

```swift
let x = 5
x = 10 // x is now 10
```

#### Multiplication (*)

Syntax:

```swift
let variableName = operand_A * operand_B
```

Example:

```swift
let x = 5
let y = x * 2 // y is now 10
```

## Examples

Below are examples that demonstrate the use of variables and operators in x86ISTMB.

### Example 1: Simple Arithmetic

```swift
let a = 10
let b = 5
let sum = a + b // sum is now 15
let product = a * b // product is now 50
```

This example demonstrates declaring variables and performing basic arithmetic operations.

### Example 2: Reassignment

```swift
let x = 10
x = x + 5 // x is now 15
x = x * 2 // x is now 30
```

In this example, we see how a variable's value can be reassigned using the result of an arithmetic operation involving its current value.

## Conclusion

This document provides an introduction to the x86ISTMB programming language, detailing the syntax for declaring variables and using operators for arithmetic and assignment. With its minimalist design, x86ISTMB aims to offer a straightforward approach to programming, focusing on the core concepts of variable manipulation and basic operations.
