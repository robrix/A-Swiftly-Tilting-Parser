# **A SWIFTLY TILTING PARSER**

## _in memory of Madeleine L’Engle_

### https://github.com/robrix/A-Swiftly-Tilting-Parser <br/> rob.rix@github.com 🚀 @rob_rix

^This URL and my contact information will be repeated at the end.

^We’re going to be talking about the Derivative of Parsers. There’s going to be a _small_ amount of theory, and then we’ll explain the algorithm by contrasting its implementation in Objective-C and Swift.

^Don’t worry if you miss some of the theory: the code will explain, and there won’t be a test.

---

# **THE DERIVATIVE of PARSERS**

- Might, Darais, & Spiewak’s 2011 paper *[Parsing with Derivatives—a Functional Pearl](http://matt.might.net/papers/might2011derivatives.pdf)*

- Recognizes and parses context-free languages

	- Recognizing: “is my input valid?”

	- Parsing: “how is the input structured?”

- Validity and structure are defined by the grammar, which is made of parser combinators

^The derivative of parsers, defined in the listed paper, is an algorithm for recognizing & parsing context-free languages.

^(“Context-free” is a mathematical notion the opposite of “context-sensitive”; for our purposes, we can think of “context-free languages” as meaning “programming languages” while “context-sensitive languages” would be “natural languages.” So the takeaway is that this algorithm will work on Ruby or C, but not Chinese or English.)

^Recognizing a language means testing whether or not a given input is valid.

^Parsing returns the structure of the input with respect to the language’s grammar; e.g. the tree structure of nested expressions. If it parses, it’s valid, and if it doesn’t parse, it’s invalid, so we’re only going to consider parsing from here on in.

^Both parsing and recognition are defined by a grammar, which is made up of parser combinators.

---

# **PARSER COMBINATORS**

### (We’ll use “parser” as a synonym)

- Executable LEGOs for parsing text

	- Each one is a tiny program

	- Some parse input directly
	
	- Some combine other parsers

- Put together, they match patterns in text

^Parser combinators, or parsers, are kind of like executable LEGOs for parsing text.

^Each one parses one very simple kind of pattern, either parsing input directly or combining other parsers—hence _combinators_.

^When you assemble simple parsers together like this, the result is a more complex parser which parses a grammar. For our purposes, the word “grammar” refers to the specific combination of parsers with which we’re trying to parse our language.

^You can invent new parsers to do all kinds of fun things, but we’re only going to consider a few common ones.

---

# **KINDS of PARSERS**

- _Literal_: match a specific character

- **Alternation:** match _x_ **or** _y_

- **Concatenation:** match _x_ & **then** _y_

- **Repetition:** match _x_ **zero or more** times

- **Reduction:** match _x_ & **map** with a function

- _Null_: match the empty string; hold parse trees

- _Empty_: never ever match

^The italicized ones (literal, null, and empty) are “terminal”—they don’t involve other parsers.

^The bold ones (alternation, concatenation, repetition, and reduction) are “nonterminal”—defined in terms of other parsers.

^Now we’ll jump into the code for a look at the terminal parsers in Objective-C.

---

# **TERMINAL PARSERS in OBJC**

```objectivec
@interface HMRLiteral : HMRTerminal
+(instancetype)literal:(id)object;
@property (readonly) id object;
@end

@interface HMREmpty : HMRTerminal
@end

@interface HMRNull : HMRTerminal
+(instancetype)captureForest:(NSSet *)forest;
@property (readonly) NSSet *parseForest;
@end
```

^This is pretty standard fare. Each kind of parser combinator is a class which (ultimately) inherits from `HMRCombinator`.

^With the exception of `HMREmpty` (which is effectively a singleton), each class has a factory method to produce new instances and a property to access its contents.

^Now let’s look at nonterminal parsers in Objective-C.

---

# **NONTERMINAL PARSERS in OBJC**

```objectivec
@interface HMRAlternation : HMRNonterminal
+(instancetype)alternateLeft:(HMRCombinator *)left right:(HMRCombinator *)right;
@property (readonly) HMRCombinator *left;
@property (readonly) HMRCombinator *right;
@end

@interface HMRConcatenation : HMRNonterminal
+(instancetype)concatenateFirst:(HMRCombinator *)first second:(HMRCombinator *)second;
@property (readonly) HMRCombinator *first;
@property (readonly) HMRCombinator *second;
@end

@interface HMRRepetition : HMRNonterminal
+(instancetype)repeat:(HMRCombinator *)combinator;
@property (readonly) HMRCombinator *combinator;
@end

@interface HMRReduction : HMRNonterminal
+(instancetype)reduce:(HMRCombinator *)combinator usingBlock:(HMRReductionBlock)block;
@property (readonly) HMRCombinator *combinator;
@property (readonly) HMRReductionBlock block;
@end
```

^This is pretty much the same as the terminal parsers: each is a class with a factory method and properties. You can see that these parsers are made from other parsers: their properties are `HMRCombinator`s.

^While we’re looking at these classes grouped together, the actual project uses the normal Objective-C conventions for organization: each of these interfaces is in its own header file, with the implementations all in separate `.m` files.

^This can make for a lot of noise and searching around to find all of the different pieces.

^Even grouped together like this, it makes for a lot of boilerplate, and we haven’t even seen the implementations: every one of those factory methods has an implementation and a corresponding `-init` method.

^Everything here would be trivial to translate to Swift 1:1. However, it wouldn’t be very interesting to just review a mechanical translation of the same solution with Swift syntax.

^Fortunately, it turns out that Swift has some interesting tricks up its sleeve.

---

# **PARSERS in SWIFT**

```swift
enum Language<Alphabet : Alphabet, Recur> {
  case Literal(Box<Alphabet>)
  
  case Alternation(Delay<Recur>, Delay<Recur>)
  case Concatenation(Delay<Recur>, Delay<Recur>)
  case Repetition(Delay<Recur>)
  case Reduction(Delay<Recur>, Alphabet -> Any)
  
  case Empty
  case Null(ParseTree<Alphabet>)
}
```

^In Swift, we declare an `enum` representing all of the different kinds of languages we care about. This is the entire thing, by the way—I haven’t removed anything except for comments.

^Note in particular that there isn’t a single `-init` or factory method. The `case`s in Swift’s `enum`s are what Haskell would call “constructors”—each one of them lets you construct an instance of this `enum` with the parameters specified in the parentheses.

^Note also that there aren’t any properties. Instead, you retrieve the values by pattern matching.

^You’ll note that this isn’t entirely free of implementation detail—`Delay` is significant (we’ll come back to it later), but both it and `Box` are only used here as workarounds for (current) deficiencies in the compiler. (I don’t know if they’ll be fixed in 1.0 or not.)

^Similarly, this is `Language` instead of `Combinator` or `Parser` 

^Even so, with this definition alone, we have an order of magnitude less boilerplate. Even better, it’s _declarative_: instead of saying _how to make parsers_, I say _what parsers are_; it’s the _compiler’s_ job to know how to construct & access them.

^Now let’s look at how these parsers are used.

---

# **OPERATIONS**

1. Parsing
2. Derivative
3. Nullability
4. Parse forest
5. Compaction

^These are the basic, high-level operations of the algorithm. You can define other ones—like pretty-printing—but these are the necessary and sufficient ones to parse.

---

# **OPERATIONS**

1. **Parsing**
2. Derivative
3. Nullability
4. Parse forest
5. Compaction

^First, we’ll look at parsing itself—at a very high level, how to go from input to results.

---

# **PARSING**

- Go through the input character by character

- At each step, compute the derivative of the parser

- Compact it

- Use it for the next step

- Return the parsed input as a parse forest

^The parsing operation goes through the input one character at a time. At the end, it returns a parse forest—the structured results we’re looking for.

^Starting with the parser you give it—the grammar for your language—it computes the derivative with respect to that input character, and compacts it.

^This derivative is a new parser, which it uses for the next step.

^Finally, it returns the last parser’s parse forest.

^Let’s take a quick glance at it in Objective-C.

---

# **PARSING in OBJC**

```objc
NSSet *HMRParseCollection(HMRCombinator *parser, id sequence) {
  parser = [sequence reduce:parser combine:^(HMRCombinator *parser, id each) {
    return [parser derivative:each];
  }];
  return parser.parseForest;
}
```

^This is a function taking a parser and a sequence, and returning the `parseForest`, a set containing the resulting parse trees.

^It doesn’t explicitly compact the parser itself; instead, this is handled in the `-derivative:` method.

^(Why a function? This is some historical dandruff of the implementation. Initially combinators were a protocol, rather than a class. Since you can’t have implementations of methods in Objective-C, expressing parsing as a method would have required me to implement at least seven `-parse:` methods, which would probably all call out to this function anyway.)

^It uses this `reduce:combine:` method (which is the same as the `reduce` function in Swift, or a left fold) to iterate over the input and return the (compacted) derivative, which is then passed to the next iteration of the block.

^And in Swift?

---

# **PARSING in SWIFT**

```swift
extension Combinator {
  func parse<S : Sequence where S.GeneratorType.Element == Alphabet>
    (sequence: S) -> ParseTree<Alphabet> {
    return reduce(sequence, self) { parser, term in
      derive(parser, term).compact()
    }.parseForest
  }
}
```

^The Swift version is almost 1:1 with the Objective-C one with some caveats:

^It’s a method instead of a function; it does compaction here instead of in `derive()`; it’s using generics to specify the types of the input and parse tree; and the return type is different.

^We returned `NSSet` in Objective-C because if the grammar is ambiguous—i.e. if there’s more than one way to successfully parse the input—then we want to return all of the alternatives.

^The Swift implementation instead represents ambiguity in the `ParseTree` type itself, using `Choice` nodes.

^All of this is to say that the `NSSet` and the `ParseTree` represent the same thing; the Swift version is just defined more precisely, and therefore the compiler has more information at its disposal for ensuring safety, and for optimization.

---

# **OPERATIONS**

1. Parsing
2. **Derivative**
3. Nullability
4. Parse forest
5. Compaction

^Next, let’s look at the derivative.

---

# **DERIVATIVE**

- Returns the parser that would match *after* the current one

- Stores matched input in parse trees

- On failure, returns the empty parser

- Different definition for each kind of parser

^Since each step of parsing uses a new parser—the derivative of the previous one with respect to the current input character—it stands to reason that the parser returned has to be ready for the character after _that_.

^For example, if our grammar matched “hi” or “ha”, and our input began with “h”, the derivative couldn’t be expecting an “h”, or it couldn’t match. It would have to match “i” or “a” instead.

^Further, since it returns the parse forest at the end, it stands to reason that each parser contains the input which it matched so far—otherwise it would have nothing to return.

^And finally, since the kinds of parsers match in different ways, we’d expect the derivative to be defined differently for each kind of parser.

---

# **TERMINAL DERIVATIVE in OBJC**

```objectivec
// Literal
-(HMRCombinator *)derivative:(id)object {
  return [self evaluateWithObject:object]?
    [HMRCombinator captureTree:object]
  : [HMRCombinator empty];
}

// Null
-(HMRCombinator *)derivative:(id)object {
  return [HMRCombinator empty];
}

// Empty
-(HMRCombinator *)derivative:(id)object {
  return self;
}
```

^Since each kind of parser is a different class in ObjC, these are all separate methods in those classes.

^The derivative of terminal parsers is straightforward. Literals derive to a null parser containing the input when they match, and to empty otherwise. Null and empty both derive to empty.

^Next we’ll look at the derivative of nonterminal parsers.

---

# **NONTERMINAL DERIVATIVE in OBJC**

```objectivec
// Alternation
-(HMRCombinator *)deriveWithRespectToObject:(id)object {
  return [[self.left derivative:object] or:[self.right derivative:object]];
}
// Reduction
-(HMRReduction *)deriveWithRespectToObject:(id)object {
  return [[self.combinator derivative:object] mapSet:self.block];
}
// Repetition
-(HMRCombinator *)deriveWithRespectToObject:(id)object {
  return [[self.combinator derivative:object] concat:self];
}
// Concatenation
-(HMRCombinator *)deriveWithRespectToObject:(id)object {
  return HMRCombinatorIsNullable(first)?
    [[[first derivative:object] concat:second]
      or:[[HMRCombinator capture:first.parseForest] concat:[second derivative:object]]]
  : [[first derivative:object] concat:second];
}
```

^The derivative of alternation is simple: it’s the alternation of its parsers’ derivatives.

^Reduction too: it’s the reduction of its parser’s derivative by its block.

^Repetition concatenates the derivative of its parser with itself, peeling off copies behind it.

^Concatenation is complicated by the fact that some parsers can be skipped.

^We’ll take a closer look at this in Swift.

---

# **DERIVATIVE in SWIFT**

```swift
func derive(c: Alphabet) -> Recur {
  switch self.language {
  case let .Literal(x) where x == c:
    return Combinator(parsed: ParseTree(leaf: c))
    
  case let .Alternation(x, y):
    return derive(x, c) | derive(y, c)
    
  case let .Reduction(x, f): return derive(x, c) --> f
    
  case let .Repetition(x): return derive(x, c) ++ self
    
  case let .Concatenation(x, y) where x.value.nullable:
    return
      derive(x, c) ++ y
    | Combinator(parsed: x.value.parseForest) ++ derive(y, c)
  case let .Concatenation(x, y): return derive(x, c) ++ y
    
  default: return Combinator(.Empty)
  }
}
```

^Because parsers use an `enum` in Swift, we can use `switch`/`case` to pattern match against them. (This is why we don’t need properties.) In fact, every operation on different kinds of parsers is performed with pattern matching in Swift.

^The code is all in one place, so you don’t have to search and risk missing key details. You can contrast the derivatives of various parsers at a glance.

^There’s that much less syntax between us and what we’re trying to express. This makes Concatenation much easier both to write and to understand.

^We see that Concatenation has two cases, differing in whether its first parser is nullable. (For the moment, we can think of “is nullable” as “can be skipped.”)

^If it _can’t_ be skipped—the second case—then the derivative is the derivative of first concatenated with second.

^If it _can_ be skipped—the first case—the derivative is either the same as if it cannot, or it’s the concatenation of a null parse containing whatever parse trees first parsed with the derivative of second.

^For example, when we parse the “h” in “ha”, it gets stored in a null parse and concatenated with the literal parser for “a”; then, when we parse the “a”, the null parse is skipped (for the purpose of matching further input), but its parse trees are retained (for the purpose of producing the correct parse forest at the end).

^Unfortunately, since both the Objective-C and Swift code given here is recursive, we’re confronted with a serious problem as soon as we try to parse anything more complex than a regular expression: nontermination.

---

# **RECURSION 🌀 & NONTERMINATION 🔄**

- Context-free languages & grammars are recursive

- NB: Not just the *types*: the object graph is cyclic!

- Key to why you can’t parse arbitrary HTML with a regexp

- Regexps can be matched with a list, but context-free languages need a stack

- Naïve implementations will infinite loop 🔄

^Stepping back for a moment, we need to understand the relationship between the _language_ we are parsing and the _grammar_ that is parsing it.

^Context-free languages are a superset of regular languages: All regular languages are context-free, but not all context-free languages are regular.

^Context-free grammars correspond to context-free languages, while regular expressions correspond to regular languages.

^The differentiating factor between the two is that context-free grammars are recursive, while regular expressions are not. This is why you need a parser instead of regular expressions to parse arbitrary HTML or many other programming languages: regular expressions cannot express arbitrary recursion, and arbitrary recursion is the key to arbitrary nesting.

^Since this means that the grammar—not the types of parsers, but the actual object graph at runtime—is cyclic, a naïve recursive implementation of the derivative like we saw before will immediately fall into an infinite loop when we use it for any language which is context-free but not regular.

^Now what?

---

# **PROTECTING your PARSERS from NONTERMINATION 😎**

1. **Laziness 😴**

^Now, we deal with it.

^First up, we employ laziness.

---

# **LAZINESS 😴**

- Alternations, concatenations, repetitions, & reductions use closures to delay evaluation

- Avoids nontermination when constructing the derivative

- Necessary to even *construct* cyclic grammars!

^Nonterminal parsers use closures to delay their traversal of the object graph.

^This allows us to avoid nontermination when computing the derivative, but it’s actually necessary even to construct a cyclic grammar in the first place—both implementations are immutable, so a closure allows us to postpone evaluation and therefore to close the loop.

---

# **LAZINESS 😴 in OBJC**

```objectivec
@implementation HMRDelayCombinator

-(HMRCombinator *)forced {
	HMRCombinator *(^block)(void) = _block;
	_block = nil;
	if (block) _forced = block();
	return _forced;
}

-(NSString *)description {
	return [@"λ." stringByAppendingString:[self.forced description]];
}

-(id)forwardingTargetForSelector:(SEL)selector {
	return self.forced;
}

@end

#define HMRDelay(x) ((__typeof__(x))[HMRDelayCombinator delay:^{ return (x); }])
…
HMRDelay([self derivativeWithRespectToObject:c]);
```

^I’ve elided most of the class. I override & manually forward many of the methods myself so I can have finer-grained control over evaluation, in order to avoid accidental infinite loops when looking up method signatures if `forwardingTargetForSelector:` is skipped (as it frequently will be due to the target being another `HMRDelay` instance).

^In short, I can’t trust it. Coping with nontermination has been the single biggest headache with the Objective-C implementation.

^I added the description method because I started explicitly logging delays so as to find more ways to aggressively remove them in order to try to avoid causing infinite loops. I was somewhat successful, but the scars are visible in the byzantine implementation that remains.

---

# **LAZINESS 😴 in SWIFT**

```swift
@final class Delay<T> {
	var _thunk: (() -> T)?
	
	@lazy var value: T = {
		let value = self._thunk!()
		self._thunk = nil
		return value
	}()
	
	init(_ thunk: () -> T) {
		_thunk = thunk
	}
	
	@conversion func __conversion() -> T {
		return value
	}
}
```

^By contrast, this is an extremely trustworthy little class. It’s less automatic, but also less magical, while still allowing conveniences such as `@auto_closure` to wrap the construction of new delays.

^I have hash & equality & a convenience constructor as functions, but this is a sufficient description of the class itself.

---

# **PROTECTING your PARSERS from NONTERMINATION 😎**

1. Laziness 😴
2. **Memoization 📎**

---

# **MEMOIZATION 📎**

- The first time you call a memoized function with a set of arguments, it stores the results

- The next time, it looks them up; memoize ≅ cache

- Can store results in a dictionary, ivar, etc.

- Allows the derivative to “tie the knot” when building a cyclic grammar *from* a cyclic grammar

---

# **MEMOIZATION 📎 in OBJC**

```objectivec
#define HMRMemoize(x, start, body) ((x) ?: ((x = (start)), (x = (body))))

// HMRNonterminal.m
-(HMRCombinator *)derivative:(id<NSObject, NSCopying>)object {
  return HMRMemoize(_derivativesByElements[object],
    [HMRCombinator empty],
    [self deriveWithRespectToObject:object].compaction);
}
```

^In Objective-C, I memoize into an instance variable using a macro. (There’s also another mechanism which we’ll see later.)

^All the nonterminal parsers inherit from `HMRNonterminal`, which memoizes their implementations of the derivative; this avoids me having to do it again and again in every subclass.

---

# **MEMOIZATION 📎 in SWIFT**

```swift
func derive(c: Alphabet) -> Recur {
  let derive: (Recur, Alphabet) -> Recur = memoize { recur, parameters in
    let (combinator, c) = parameters
    switch combinator.language {
    case let .Literal(x) where x == c:
      return Combinator(parsed: ParseTree(leaf: c))
      
    case let .Alternation(x, y):
      return recur(x, c) | recur(y, c)
      
    case let .Concatenation(x, y) where x.value.nullable:
      return recur(x, c) ++ y
        | Combinator(parsed: x.value.parseForest) ++ recur(y, c)
    case let .Concatenation(x, y): return recur(x, c) ++ y
      
    case let .Repetition(x): return recur(x, c) ++ combinator
      
    case let .Reduction(x, f): return recur(x, c) --> f
      
    default: return Combinator(.Empty)
    }
  }
  return derive(self, c)
}
```

^cf WWDC Session 404 Advanced Swift

---

# **OPERATIONS**

1. Parsing
2. Derivative
3. **Nullability**
4. Parse forest
5. Compaction

---

# **NULLABILITY**

- “Is this grammar nullable?” = “Will it match an empty string?”

- Equivalent: “Can it match at the end of the input?”

- Equivalent: “Can it be skipped?”

---

# **NULLABILITY in OBJC**

```objectivec
bool HMRCombinatorIsNullable(HMRCombinator *combinator) {
  return [HMRMemoize(cache[combinator], @NO, HMRMatch(combinator, @[
    [[[HMRBind() concat:HMRBind()] quote] then:^(HMRCombinator *fst, HMRCombinator *snd) {
      return @(recur(fst) && recur(snd));
    }],
    
    [[[HMRBind() or:HMRBind()] quote] then:^(HMRCombinator *left, HMRCombinator *right) {
      return @(recur(left) || recur(right));
    }],
    
    [[[HMRBind() map:REDIdentityMapBlock] quote] then:^(HMRCombinator *combinator) {
      return @(recur(combinator));
    }],
    
    [[[HMRAny() repeat] quote] then:^{ return @YES; }],
    [[HMRNull quote] then:^{ return @YES; }],
  ])) boolValue];
}
```

---

# **NULLABILITY in SWIFT**

```swift
var nullable: Bool {
  let nullable: Combinator<Alphabet> -> Bool = memoize { recur, combinator in
    switch combinator.language {
    case .Null: return true
      
    case let .Alternation(left, right):
      return recur(left) || recur(right)
      
    case let .Concatenation(first, second):
      return recur(first) && recur(second)
      
    case .Repetition: return true
      
    case let .Reduction(c, _): return recur(c)
      
    default: return false
    }
  }
  return nullable(self)
}
```

---

# **NULLABILITY and NONTERMINATION 🔄**

- Nullability walks the grammar eagerly, defeating laziness 😴

- Nullability computes pass/fail, not a structure, defeating memoization 📎

- Thus: 🔄

---

# **PROTECTING your PARSERS from NONTERMINATION 😎**

1. Laziness 😴
2. Memoization 📎
3. **~~*Math*~~ Fixed points 🔨☝️**

---

# **~~*MATH*~~ FIXED POINTS 🔨☝️**

- If `𝑓(𝑥) = 𝑥`, `𝑓` is fixed at `𝑥`; `𝑥²` is fixed at `0` and `1`

- If `𝐿` is nullable, `δ(𝐿)` is null, otherwise empty

- Any fixpoints of `δ` are likewise either null or empty

- Interpret `δ(𝐿) = δ(𝐿) α | ϵ` as a fixpoint of `δ`

- Iterate `δⁿ(𝐿)` from `δ⁰(𝐿) = false` until `δⁿ(𝐿) = δⁿ⁻¹(𝐿)`  (Kleene fixpoint theorem)

---

# **~~FIXPOINTS 🔨☝️~~ in OBJC**

```objectivec
bool HMRCombinatorIsNullable(HMRCombinator *combinator) {
  return [HMRMemoize(cache[combinator], @NO, HMRMatch(combinator, @[
    [[[HMRBind() concat:HMRBind()] quote] then:^(HMRCombinator *fst, HMRCombinator *snd) {
      return @(recur(fst) && recur(snd));
    }],
    
    [[[HMRBind() or:HMRBind()] quote] then:^(HMRCombinator *left, HMRCombinator *right) {
      return @(recur(left) || recur(right));
    }],
    
    [[[HMRBind() map:REDIdentityMapBlock] quote] then:^(HMRCombinator *combinator) {
      return @(recur(combinator));
    }],
    
    [[[HMRAny() repeat] quote] then:^{ return @YES; }],
    [[HMRNull quote] then:^{ return @YES; }],
  ])) boolValue];
}
```

---

# **FIXPOINTS 🔨☝️ in OBJC**

```objectivec
bool HMRCombinatorIsNullable(HMRCombinator *combinator) {
  NSMutableDictionary *cache = [NSMutableDictionary new];
  bool (^__weak __block recur)(HMRCombinator *);
  bool (^isNullable)(HMRCombinator *) = ^bool (HMRCombinator *combinator) {
    return [HMRMemoize(cache[combinator], @NO, HMRMatch(combinator, @[
      [[[HMRBind() concat:HMRBind()] quote] then:^(HMRCombinator *fst, HMRCombinator *snd) {
        return @(recur(fst) && recur(snd));
      }],
      
      [[[HMRBind() or:HMRBind()] quote] then:^(HMRCombinator *left, HMRCombinator *right) {
        return @(recur(left) || recur(right));
      }],
      
      [[[HMRBind() map:REDIdentityMapBlock] quote] then:^(HMRCombinator *combinator) {
        return @(recur(combinator));
      }],
      
      [[[HMRAny() repeat] quote] then:^{ return @YES; }],
      [[HMRNull quote] then:^{ return @YES; }],
    ])) boolValue];
  };
  recur = isNullable;
  return isNullable(combinator);
}
```

---

# **~~FIXPOINTS 🔨☝️~~ in SWIFT**

```swift
var nullable: Bool {
  let nullable: Combinator<Alphabet> -> Bool = memoize { recur, combinator in
    switch combinator.language {
    case .Null: return true
      
    case let .Alternation(left, right):
      return recur(left) || recur(right)
      
    case let .Concatenation(first, second):
      return recur(first) && recur(second)
      
    case .Repetition: return true
      
    case let .Reduction(c, _): return recur(c)
      
    default: return false
    }
  }
  return nullable(self)
}
```

---

# **FIXPOINTS 🔨☝️ in SWIFT**

```swift
var nullable: Bool {
  let nullable: Combinator<Alphabet> -> Bool = fixpoint(false) { recur, combinator in
    switch combinator.language {
    case .Null: return true
      
    case let .Alternation(left, right):
      return recur(left) || recur(right)
      
    case let .Concatenation(first, second):
      return recur(first) && recur(second)
      
    case .Repetition: return true
      
    case let .Reduction(c, _): return recur(c)
      
    default: return false
    }
  }
  return nullable(self)
}
```

---

# **OPERATIONS**

1. Parsing
2. Derivative
3. Nullability
4. **Parse forest**
5. Compaction

---

# **PARSE FOREST**

- Construct and return any matched parse trees

- Apply reductions

	- This is how you construct *your* objects

- If > 1 parser matched the input, > 1 parse tree in the parse forest

	- This means there’s ambiguity in the grammar 😨

---

# **PARSE FOREST in OBJC**

```objectivec
-(NSSet *)parseForest {
  return cache[combinator] = HMRMatch(combinator, @[
    [[[HMRBind() or:HMRBind()] quote] then:^(HMRCombinator *left, HMRCombinator *right) {
      return [parseForest(left, cache) setByAddingObjectsFromSet:parseForest(right, cache)];
    }],
    
    [[[HMRBind() concat:HMRBind()] quote] then:^(HMRCombinator *fst, HMRCombinator *snd) {
      return [parseForest(fst, cache) product:parseForest(snd, cache)];
    }],
    
    [[[HMRBind() map:REDIdentityMapBlock] quote]
        then:^(HMRCombinator *c, HMRReductionBlock f) {
      return [[NSSet set] f(parseForest(c, cache))];
    }],
    
    [[[HMRAny() repeat] quote] then:^{
      return [NSSet setWithObject:[HMRPair null]];
    }],
    [[HMRNull quote] then:^{
      return combinator.parseForest;
    }],
  ]));
}
```

---

# **PARSE FOREST in SWIFT**

```swift
var parseForest: ParseTree<Alphabet> {
  let parseForest: Combinator<Alphabet> -> ParseTree<Alphabet> =
      fixpoint(ParseTree.Nil) { recur, combinator in
    switch combinator.language {
    case let .Null(x): return x
      
    case let .Alternation(x, y): return recur(x) + recur(y)
      
    case let .Concatenation(x, y): return recur(x) * recur(y)
      
    case let .Repetition(x): return .Nil
      
    case let .Reduction(x, f): return map(recur(x), f)
      
    default: return .Nil
    }
  }
  return parseForest(self)
}
```

---

# **OPERATIONS**

1. Parsing
2. Derivative
3. Nullability
4. Parse forest
5. **Compaction**

---

# **WITHOUT COMPACTION**

> *“The implementation is brief. The code is pure. The theory is elegant. So, how does this perform in practice? In brief, it is awful.”*

- Derivative of concatenation *doubles* grammar size

- Worst case: O(2²*ⁿ**G*²) : *G* = grammar size, *n* = input length 💥

---

# **COMPACTION**

- Replace complex parsers with simpler equivalents

- Enables better performance

	- Worst case still terrible

	- Expected case (unambiguous grammars) is O(*nG*)

	- Quite reasonable in practice; *no* algorithm is fast under ambiguity

---

# **COMPACTION in OBJC**

```objectivec
// HMRAlternation
-(HMRCombinator *)compact {
  HMRCombinator *left = self.left.compacted, *right = self.right.compacted;
  if ([left isEqual:[HMRCombinator empty]]) return right;
  else if ([right isEqual:[HMRCombinator empty]]) return left;

  else if ([left isKindOfClass:[HMRNull class]]
    && [right isKindOfClass:[HMRNull class]]) {
    NSSet *all = [left.parseForest setByAddingObjectsFromSet:right.parseForest];
    return [HMRCombinator capture:all];
  }
  else if ([left isKindOfClass:[HMRConcatenation class]]
    && [left.first isKindOfClass:[HMRNull class]]
    && [right isKindOfClass:[HMRConcatenation class]]
    && [left.first isEqual:right.first]) {
    HMRCombinator *innerLeft = left.second;
    HMRCombinator *innerRight = right.second;
    alternation = [innerLeft or:innerRight];
    return [left.first concat:[innerLeft or:innerRight]];
  }
  else return [left or:right];
}
```

---

# **COMPACTION in OBJC**

```objectivec
// HMRConcatenation
-(HMRCombinator *)compact {
  HMRCombinator *fst = self.first.compaction, *snd = self.second.compaction;
  if ([fst isEqual:[HMRCombinator empty]] || [snd isEqual:[HMRCombinator empty]])
    return [HMRCombinator empty];
  else if ([fst isKindOfClass:[HMRNull class]] && [snd isKindOfClass:[HMRNull class]])
    return [HMRCombinator capture:[fst.parseForest product:snd.parseForest]];
  else if ([fst isKindOfClass:[HMRNull class]]) {
    NSSet *parseForest = fst.parseForest;
    if (parseForest.count == 0) return snd;
    else return [snd map:^(id each) {
        return HMRCons(parseForest.anyObject, each);
      }];
  }
  else if ([snd isKindOfClass:[HMRNull class]]) {
    NSSet *parseForest = snd.parseForest;
    if (parseForest.count == 0) concatenation = fst;
    else return [fst map:^(id each) {
        return HMRCons(each, parseForest.anyObject);
      }];
  }
  else return [fst concat:snd];
}
```

---

# **COMPACTION in OBJC**

```objectivec
-(HMRCombinator *)compact {
  HMRCombinator *combinator = self.combinator.compaction;
  return [combinator isEqual:[HMRCombinator empty]]?
    [HMRCombinator captureTree:[HMRPair null]]
  : (combinator == self.combinator? self : [combinator repeat]);
}
```

---

# **COMPACTION in OBJC**

```objectivec
// HMRReduction
-(HMRCombinator *)compact {
  HMRCombinator *combinator = self.combinator.compaction;
  if ([combinator isEqual:[HMRCombinator empty]])
    return [HMRCombinator empty];
  else if ([combinator isKindOfClass:[HMRReduction class]])
    return HMRComposeReduction(combinator, self.block);
  else if ([combinator isKindOfClass:[HMRNull class]])
    return [HMRCombinator capture:[self map:combinator.parseForest]];
  else return [combinator mapSet:self.block];
}
```

---

# **COMPACTION in SWIFT**

```swift
func compact() -> Combinator<Alphabet> {
  let compact: Recur -> Recur = fixpoint(self) { recur, combinator in
    switch combinator.destructure(recur) {
    /// Alternations with Empty are equivalent to the other alternative.
    case let .Alternation(x, .Empty): return Combinator(x)
    case let .Alternation(.Empty, y): return Combinator(y)
      
    /// Concatenations with Empty are equivalent to Empty.
    case .Concatenation(.Empty, _), .Concatenation(_, .Empty):
      return Combinator.empty
      
    /// Repetitions of empty are equivalent to parsing the empty string.
    case .Repetition(.Empty): return Combinator(parsed: .Nil)
    case let .Repetition(x): return Combinator(x)*
      
    /// Reductions of reductions compose.
    case let .Reduction(.Reduction(x, f), g): return Combinator(x --> compose(g, f))

    default: return combinator
    }
  }
  return compact(self)
```

---

# **COMPACTION in the FUTURE 🚀**

- Generally must compact after derivative, or else 🔄

	- Can we avoid complex parsers altogether in some cases?

- Enables better features

	- Incremental results: 🔢 vs. 1️⃣…2️⃣…3️⃣…4️⃣…

	- (Good) error reporting?

	- Disambiguation? ✨

---

# **CHALLENGES in OBJC & SWIFT**

- Understanding the paper is hard 😖

- ObjC & Swift are reference counted

	- Cyclic grammars = refcycles (unless handled specially)

	- Potential solution: a refcycle-breaking combinator

- Pattern matching cyclic grammars is tricky

---

# **CHALLENGES UNIQUE to OBJC**

- Language/algorithm impedance mismatch

- Verbose; dense; splits functions across many files

- Pattern matching cyclic grammars is *really* tricky

	- The language doesn’t have pattern matching 😭

	- Implemented pattern matching for parsers *using* parsers 💫🌀

- Nontermination is much harder to solve, e.g. `-isEqual:` for equal cyclic grammars

---

# **CHALLENGES UNIQUE to SWIFT**

- Beta (& evolving!) compiler & IDE 😱

	- No codegen yet for some features

	- Crash-happy 😂💥 (as of Xcode 6b3)

	- Bad error reporting (ProTip™: extract nested expressions into constants to isolate issues)

- Some language design/prioritization choices need workarounds

	- Making it up as I go ✈️💺👖

---

# **BENEFITS of SWIFT vs. OBJC**

- Much better tool:job match

	- `enum` is a better fit than classes for parsers 👍

	- Pattern matching 😍

	- Operator overloading for constructing parsers ✨

- Stronger typing → safer, better program 💪

- Solve *my* problems more, incidental ones less 🙌

- Make mistakes faster & with greater confidence 🎢

^I don’t have to come up with class prefixes.

---

# **BENEFITS of OBJC vs. SWIFT**

- ObjC is stable

- `clang` is stable

- Familiarity

- Unlikely to break my code on the day of the talk 😆

---

# **SUBTLETIES: OBJC > SWIFT…?**

- It was initially hard describing parse trees’ type in Swift

	- ObjC: sets, pairs, input, & AST are all `id`

	- However, easy ≠ *good*: 💥

- Can use macros & dynamic proxies in ObjC

	- No real equivalents in Swift 😕

	- *Had* to use macros & dynamic proxies in ObjC 😞

---

# **SUBTLETIES: SWIFT > OBJC…?**

- *Much* more readable with `enum`/pattern matching

	- Wasn’t sure this approach would work 1w ago 😰

	- If not, same solution as ObjC, with beta tools 😡

- `@auto_closure` & operators are ✨ for grammars

	- Potentially masks refcycles

	- Hard to break cycles automatically *or* manually

---

# **SWIFT ❤️**

- Objective-C is the wrong tool for the job

- Much more sound theoretically

	- Inheritance is holding us back

	- Better type system → more flexibility, less effort

- Much more sound practically

	- Safer & more productive

	- Types enable better optimizations → _fast!_
	
---

# **¿Q&A!**

---

# **_THANK YOU!_ 🙇**

### David Darais, Matt Might, Kelly Rix, David Smith,<br/>Daniel Spiewak, the Swift team, @DecksetApp,<br/>& especially you 💟

### https://github.com/robrix/A-Swiftly-Tilting-Parser<br/> rob.rix@github.com 🚀 @rob_rix