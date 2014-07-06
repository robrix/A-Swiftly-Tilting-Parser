# A SWIFTLY TILTING PARSER

## (in memory of Madeleine L’Engle)

### https://github.com/robrix/A-Swiftly-Tilting-Parser <br/> rob.rix@github.com ❧ @rob_rix

---

# THE DERIVATIVE of PARSERS
## in
# OBJECTIVE-C & SWIFT

---

# PARSER COMBINATORS are NOT SCARY

### We’ll use “parser” as a synonym.

- Executable LEGOs for parsing text

	- Each one is a tiny program

	- Some parse input directly
	
	- Some combine other parsers

- Together, they match specific patterns

---

# THE DERIVATIVE of PARSERS is ALSO NOT SCARY

- Might, Darais, & Spiewak’s 2011 paper *[Parsing with Derivatives—a Functional Pearl](http://matt.might.net/papers/might2011derivatives.pdf)*

- *Recognizes* and *parses* programming languages*

	- Recognizing: “is my input valid?”

	- Parsing: “how is the input structured?”

- Validity and structure are defined by the grammar, which is made of parser combinators

*Technically, context-free languages

---

# BREAKING it DOWN

- Parsing
- Derivative
- Nullability
- Compaction
- Parse forest

---

# PARSING with DERIVATIVES: NOT SCARY

- Go through the input character by character

- At each step, compute the derivative of the parser

- Return the parsed input as a parse tree*

\*Technically, parse *forest*

---

# *parsing in Objective-C and Swift*

---

# DERIVATIVE is SLIGHTLY SCARY

- Returns the parser that would match *after* the current one

- Stores matched input in parse trees

- On failure, returns the empty parser

- Holds eye contact slightly longer than comfortable 

---

# *derivative in Objective-C and Swift*

---
