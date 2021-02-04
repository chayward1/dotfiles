+++
title = "Thinking in C++"
author = ["Christopher James Hayward"]
lastmod = 2021-02-03T14:08:28-05:00
draft = false
+++

## Coding Style {#coding-style}

**Eckel** justifies the decisions about the coding styles in the text[^fn:1]:

> All the decisions about coding style in this book have been deliberately considered and made, sometimes over a period of years.


### General {#general}

The coding style as described by **Eckel** is one that he came to after years of practice, and originates from Bjarne Stroustrup's style, the Author of `C++`[^fn:1]:

> I use a particular coding style for the examples in this book. It was developed over a number of years, and was partially inspired by Bjarne Stroustrup's style in his original The C++ Programming Language.

**Eckel** goes on to declare that it's more important to keep a consistent style, than to try to determine which is superior[^fn:1]:

> Because C++ is a free-form programming language, you can continue to use whatever style you're comfortable with. That said, I will note that it is important to have a consistent formatting style within a project.


### File names {#file-names}

**Eckel** describes the history of the naming conventions for `C/C++` files[^fn:1]:

> In C, it has been traditional to name header files (containing declarations) with an extension of .h and implementation files (that cause storage to be allocated and code to be generated) with an extension of .c.

Depending on the type of operating system, some files had different extensions as well[^fn:1]:

> DOS C++ vendors used extensions of hxx and cxx for header files and implementation files, respectively, or hpp and cpp. Later, someone figured out that the only reason you needed a different extension for a file was so the compiler could determine whether to compile it as a C or C++ file

| Name           | Extension | Description                 |
|----------------|-----------|-----------------------------|
| Header         | .h        | Header files for C/C++      |
| Header         | .hxx      | Header file for C++ on DOS  |
| Implementation | .c        | Implementation file for C   |
| Implementation | .cpp      | Implementaiton file for C++ |


### Begin and end comment tags {#begin-and-end-comment-tags}

Most of the code examples in the text book have comment tags at the beginning and end, which is part of a system used by **Eckel** to verify all of the code examples[^fn:1]:

> A very important issue with this book is that all code that you see in the book must be verified to be correct (with at least one compiler).

**Eckel** further elaborates how each example has a custom `makefile` and some program information included in the tags[^fn:1]:

> Because ExtractCode.cpp also creates a makefile for each subdirectory, information about how a program is made and the command-line used to test it is also incorporated into the listings.


### Parentheses, braces, and indentation {#parentheses-braces-and-indentation}

The section begins with, what is in my opinion, an accurate statement by **Eckel**, although not without irony[^fn:1]:

> Of course, everyone thinks their own style is the most rational. However, the style used here has a simple logic behind it, which will be presented here mixed in with ideas on why some of the other styles developed.

On addressing indentation[^fn:1]:

> Everyone seems to agree that code inside braces should be indented.

**Eckel** addresses braces in his coding style[^fn:1]:

> ... the opening brace should always go on the same line as the "precursor" (by which I mean "whatever the body is about: a class, function, object definition, if statement, etc."). This is a single, consistent rule I apply to all of the code I write, and it makes formatting much simpler.


### Identifier names {#identifier-names}

**Eckel** explains the naming conventions of identifiers[^fn:1]:

> Those familiar with Java will notice that I have switched to using the standard Java style for all identifier names. However, I cannot be completely consistent here because identifiers in the Standard C and C++ libraries do not follow this style

| Type                | Style       |
|---------------------|-------------|
| Class               | Pascal Case |
| Function / Variable | Camel Case  |
| Const / Definition  | Snake Case  |

```cpp
class FrenchVanilla : public IceCream { }
```

```cpp
const MAX_SCOOPS = 3;
```

```cpp
FrenchVanilla myIceCreamCone(MAX_SCOOPS);
```


### Order of header inclusion {#order-of-header-inclusion}

**Eckel** broadly defines the order of inclusion[^fn:1]:

> Headers are included in order from "the most specific to the most general." That is, any header files in the local directory are included first, then any of my own "tool" headers, such as require.h, then any third-party library headers, then the Standard C++ Library headers, and finally the C library headers.

```cpp
#include <myheader.h>  // Local directory headers.
#include <mytools>     // Personal tool headers.
#include <thirdparty>  // Third party library headers.
#include <bits/stdc++> // C++ library headers.
#include <iostream>    // C library headerts
```


## Resources {#resources}

[^fn:1]: Eckel, Bruce. Thinking in C++. 2nd ed, Prentice Hall, 2000, <https://online.vitalsource.com/books/9781269392440>.
