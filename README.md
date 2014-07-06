# A Swiftly Tilting Parser

[My talk](https://github.com/robrix/A-Swiftly-Tilting-Parser/blob/master/Parsing%20with%20Derivatives%20in%20ObjC%20vs.%20Swift.md) about the [derivative of parser combinators](http://matt.might.net/articles/parsing-with-derivatives/), to be given at [TACOW](https://tacow.org/) on July 8th, 2014.

### References

- *[Parsing with Derivatives—A Functional Pearl][PwD]*, by Matthew Might, David Darais, and Daniel Spiewak. Definitely a gem.
- [Context][] for the algorithm and its development
- [Hammer][Hammer.objc], my implementation of derivative parsers in Objective-C
- [Hammer.swift][Hammer.swift], my implementation of derivative parsers in Swift
- (Informal) reasoning on [whether nullability will always stabilize][δ] within a single iteration
- A similar argument [for `parseNull`][parseNull] (of which I am less certain)

[PwD]: http://matt.might.net/papers/might2011derivatives.pdf
[Context]: http://matt.might.net/articles/parsing-with-derivatives/
[Hammer.objc]: https://github.com/robrix/Hammer
[Hammer.swift]: https://github.com/robrix/Hammer.swift
[δ]: https://gist.github.com/robrix/a197f268a54813d3e9d6
[parseNull]: https://gist.github.com/robrix/566c38e3ecc849e303d7
