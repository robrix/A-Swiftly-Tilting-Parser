# A SWIFTLY TILTING PARSER

(in memory of Madeleine L’Engle)

# PARSER COMBINATORS and the DERIVATIVE THEREOF

The derivative of parser combinators is one of many algorithms for parsing context-free languages. It’s one of only a few which can parse *all* context-free grammars, whether left-recursive, right-recursive, mutually-recursive, or completely ambiguous. It is small, it is simple, and it has decent performance characteristics (unless you write ambiguous grammars; but as I understand it nothing deals with that well, “so don’t do that”).

When I say it’s *small*, I mean you can define the entire thing in about a page. It is *small*. Actually implementing it takes a *bit* more doing: the ObjC implementation is about 2200 lines of code (counting inline tests but not counting frameworks), while the (incomplete, and much less well-tested) Swift implementation clocks in just under 700.

When I say it’s *simple*, I mean almost the same as I did by “small”—there’s very few concepts at play, and they can have a small number of well-defined interactions. This belies the subtlety of it, however.

The algorithm was (eventually) published in a 2011 paper called Parsing with Derivatives: A Functional Pearl, by Matthew Might, David Darais, and Daniel Spiewak. As the title suggests, the paper describes the algorithm in a pure functional style, going so far as to define the core operations algebraically (as well as informally, in Scheme).

Of course, if Scheme can do it, so can we. But as we’ll see, it’s not without its challenges. We’ll get to these in a moment.

“Derivative parsers” are, to be precise, parser combinators. In general, a combinator a kind of higher-order function which combines its argument functions together into a new function. You can define parser combinators to do more or less whatever you want, but for our purposes we’ll look at a few simple ones, and we’ll refer to all of them simply as “parsers”—there’s no single `NSXMLParser` instance overseeing the algorithm, just lots of tiny parser combinators, so the term is unambiguous.

Here’s our cast:


# CHARACTERS

A character parser matches a specific character. It’s like seeing the letter 'x' in a regular expression: it will match the letter 'x' in the input.


# ALTERNATIONS, or UNIONS

Alternations are `OR` operators, the same as the `|` operator in regular expressions. The alternation of two languages will match whenever either of them matches.

Speaking formally, languages are sets, and alternation is the same as the union, but `OR` is the easiest way to think about them.


# CONCATENATIONS

The concatenation of two languages will match them both in sequence. It’s not *quite* the same as `AND`, but you can think of it as `AND THEN`. In regular expressions, there is no operator for concatenation; instead, it’s what you get implicitly when you put two patterns next to each other: `xy` matches `x` *and then* `y`.

We’ll use `x ++ y` to mean the concatenation of `x` and `y` instead; it’s a little less direct, but we can use it both informally and in Swift code.


# REPETITIONS

Regular expressions have a variety of different ways to repeat, but none of them—literally none!—are necessary in context-free grammars: you can use alternation and concatenation to represent repetition instead.

But… that can be kind of annoying. For our purposes, we’ll allow ourselves a single kind of repetition parser: one which repeats another parser zero or more times. (This is the same as the Kleene star; `*` in regular expressions.)


# REDUCTIONS

Reductions are a little bit non-obvious at first; there’s no analogue for them in regular expressions.

A reduction has a parser and a function, but unlike alternation, concatenation, and repetition, it doesn’t change what you can parse at all. Instead, it changes the *results*.

At the end of the day, we need to know what was matched, and what structure it had; that’s why we’re parsing after all. Reductions apply their function to their parser’s output, letting us turn lists of input characters into simple abstract syntax tree nodes, and lists of simple abstract syntax tree nodes into relatively complex ones.


# NULL PARSERS and NULL PARSES

Null parsers are an interesting case: they match only the null, or empty, string. The paper uses a lowercase epsilon (`ε`) to mean the null string, and null parsers by extension. You could use this when you’re describing a language’s grammar if you want to explicitly allow some structure to have some contents or none; but it’s generally more common to use a repetition instead.

The regular expression operator `?` which matches a pattern 0 or 1 times works sort of like this: `x?` is the same as `x | ε`.

In derivative parsers, null parsers are a little more complicated: they can also hold on to parsed input, so that at the end, when all of the input has been parsed, the algorithm can return it. They’re like little functional–style notes that the algorithm is using to keep track of what it’s done.


# EMPTY

The empty parser is contrarian—it refuses to match anything. This isn’t generally very useful when you’re defining a parser—except maybe for unit testing!—but it turns out to be *extremely* useful in the derivative’s operations.

For now, think of it as a marker that indicates that some part of the grammar failed to match something. This doesn’t mean an error case, however; if your language can parse `x` or `y`, and you pass it `x`, the `y` branch will obviously fail.


# COMBINING PARSERS

The key detail is that on their own, each parser combinator is very simple; even boring. Combine them, however, and you can describe any context-free language you want—often in many different ways.

We’ve referred to regular expressions several times, but it’s important to note that context-free languages are more expressive—they can match more sophisticated kinds of patterns. This is why you can’t use a regular expression (on its own) to parse arbitrary HTML, for example.

The difference is that context-free languages are recursive, whereas regular expressions are not. What does that mean, exactly? It means that this is a valid definition of a language:

	S -> S | 'x'

(You can read `->` as “is matched by matching…”, i.e. “S is matched by matching S or an 'x'.”) This is one of many—infinite!—context-free grammars you can construct to match the empty string.

This is an example of a left-recursive grammar. If you were writing a naïve recursive descent parser, you’d be stuck in an infinite loop!

Here’s a right-recursive grammar:

	S -> ε | S

(“S is matched by matching an 'x', or S.”) Since this is right-recursive, a recursive descent parser can match this just fine.

And here’s a mutually-recursive grammar:

	S -> T | ε
	T -> ε | S

Mutual recursion is the key to interesting context-free grammars.

Now let’s look at an informal grammar for addition and multiplication with integers:

	let expression =
		integer
	|	addition
	|	'(' ++ expression ++ ')'
	
	let integer = '-'? ++ (0...9)+
	
	let addition = multiplication ++ '+' ++ multiplication
	let multiplication = expression ++ '*' ++ expression

(This isn’t actually valid Swift code, incidentally, but it’s not that far off!)

An expression is an integer, or addition expression, or a parenthesized expression.

An integer is an optionally negative series of digits.

Addition is two multiplications separated by a `+`.

Multiplication is two expressions separated by a `*`.

Here are some expressions described by this grammar:

	0
	1 + 1
	1 + 2 * 3
	(1 + 1) * 3

It also accepts some expressions that make somewhat less sense:

	-0
	000000000000000
	(1)
	(1 * 1)
	((((((1))))))

As long as the parentheses are balanced, it can match an arbitrary number of them.

