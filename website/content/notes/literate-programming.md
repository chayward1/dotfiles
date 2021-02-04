+++
title = "Literate Programming"
author = ["Christopher James Hayward"]
lastmod = 2021-02-01T16:14:03-05:00
draft = false
+++

-   Programming paradigm
-   Programs expressed in natural language with code snippets
-   Produces machine readable code and human readable documentation


## Introduction {#introduction}

Described in its introduction[^fn:1] as a

> Programming paradigm in which a computer program is given an explanation of its logic in a natural language, such as English, interspersed with snippets of macros and traditional source code, from which compilable source code can be generated.

Knuth describes a **practitioner** in the introduction of his 1984 paper[^fn:1] as

> An essayist concerned with exposition and excellence of style. Someone who carefully selects the name for each variable and describes their meaning. They will strive for a program that is comprehensible because concepts are introduced in a manner best for human understanding.


## Concept {#concept}

```text
                     _____
                    |     | -> Send to Boss
 _____              | doc | -> Copy to Team
|     | -> Weave -> |_____| -> Copy to Support
| org |               _____
|_____| -> Tangle -> |     | -> Test on CI
                     | src | -> Copy to CDN
                     |_____| -> Send to Customer
```

Illustrated above we see the process of **weaving** and **tangling** the literate source file, and how each of the produced components is handled respectively.

| File | Description                  |
|------|------------------------------|
| org  | Literate document / file     |
| src  | Machine readable source code |
| doc  | Human readable documentation |


## Resources {#resources}

[^fn:1]: Knuth, D. E. (1984). Literate Programming. The Computer Journal, 27(2), 97–111. <https://doi.org/10.1093/comjnl/27.2.97>