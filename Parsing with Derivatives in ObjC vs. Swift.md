# A SWIFTLY TILTING PARSER

(in memory of Madeleine L’Engle)

# PARSER COMBINATORS and the DERIVATIVE THEREOF

The derivative of parser combinators is one of many algorithms for parsing context-free languages. It’s one of only a few which can parse *all* context-free grammars, whether left-recursive, right-recursive, mutually-recursive, or completely ambiguous. It is small, it is simple, and it has decent performance characteristics (unless you write ambiguous grammars; but as I understand it nothing deals with that well, “so don’t do that”).

When I say it’s *small*, I mean you can define the entire thing in about a page. It is *small*. Actually implementing it takes a *bit* more doing: the ObjC implementation is about 2200 lines of code (counting inline tests but not counting frameworks), while the (incomplete, and much less well-tested) Swift implementation clocks in just under 700.

When I say it’s *simple*, I mean almost the same as I did by “small”—there’s very few concepts at play, and they can have a small number of well-defined interactions. This belies the subtlety of it, however.

The algorithm was (eventually) published in a 2011 paper called Parsing with Derivatives: A Functional Pearl, by Matthew Might, David Darais, and Daniel Spiewak. As the title suggests, the paper describes the algorithm in a pure functional style, going so far as to define the core operations algebraically (as well as informally, in Scheme).

Of course, if Scheme can do it, so can we. But as we’ll see, it’s not without its challenges. We’ll get to these in a moment.

“Derivative parsers” are, to be precise, parser combinators. In general, a combinator a kind of higher-order function which combines its argument functions together into a new function. You can define parser combinators to do more or less whatever you want, but for our purposes we’ll look at a few simple ones, and we’ll refer to all of them simply as “parsers”—there’s no single `NSXMLParser` instance overseeing the algorithm, just lots of tiny parser combinators, so the term is unambiguous.