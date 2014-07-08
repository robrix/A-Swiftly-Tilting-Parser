# **A SWIFTLY TILTING PARSER**

## _in memory of Madeleine L’Engle_

### https://github.com/robrix/A-Swiftly-Tilting-Parser <br/> rob.rix@github.com ❧ @rob_rix

---

# **THE DERIVATIVE of PARSERS**

- Might, Darais, & Spiewak’s 2011 paper *[Parsing with Derivatives—a Functional Pearl](http://matt.might.net/papers/might2011derivatives.pdf)*

- Recognizes and parses context-free languages

	- Recognizing: “is my input valid?”

	- Parsing: “how is the input structured?”

- Validity and structure are defined by the grammar, which is made of parser combinators

---

# **PARSER COMBINATORS**

### (We’ll use “parser” as a synonym)

- Executable LEGOs for parsing text

	- Each one is a tiny program

	- Some parse input directly
	
	- Some combine other parsers

- Put together, they match patterns in text

---

# **KINDS of PARSERS**

- Literal: match a specific character

- Alternation: match x or y

- Concatenation: match x and then y

- Repetition: match x zero or more times

- Reduction: match x & map with a function

- Null: match the empty string; hold parse trees

- Empty: never ever match

^Literal, null, and empty are “terminal”—they don’t involve other parsers.

^Alternation, concatenation, repetition, and reduction are “nonterminals”—defined in terms of other parsers.

---

# **TERMINAL PARSERS in OBJC**

```objectivec
@interface HMRLiteral : HMRPredicateCombinator
+(instancetype)literal:(id)object;
@property (readonly) id<NSObject, NSCopying> object;
@end

@interface HMREmpty : HMRTerminal
@end

@interface HMRNull : HMRTerminal
+(instancetype)captureForest:(NSSet *)forest;
@property (readonly) NSSet *parseForest;
@end
```

^Standard fare; factory methods and properties.

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

^More factory methods and properties. A bunch of monotonous lines declaring the kinds of parsers we are interested in.

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

^This is the whole thing. I don’t have to write a single init method. I don’t have to write properties. I don’t even have to inject delays into the graph when I discover it needs them.

---

# **OPERATIONS**

1. Parsing
2. Derivative
3. Nullability
4. Parse forest
5. Compaction

---

# **OPERATIONS**

1. **Parsing**
2. Derivative
3. Nullability
4. Parse forest
5. Compaction

---

# **PARSING**

- Go through the input character by character

- At each step, compute the derivative of the parser

- Compact it

- Use it for the next step

- Return the parsed input as a parse forest

---

# **PARSING in OBJC**

```objc
NSSet *HMRParseCollection(HMRCombinator *parser, id<REDReducible> sequence) {
  parser = [sequence reduce:parser combine:^(HMRCombinator *parser, id each) {
    return [parser derivative:each];
  }];
  return parser.parseForest;
}
```

^The ObjC version is a function taking a parser and a sequence, and returning a set containing parse trees.
It doesn’t explicitly compact the grammar like we discussed before; instead, this is done in the `-derivative:` method.

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

^The Swift version is a method taking a sequence and returning a parse tree. It returns a parse tree instead of a set because sets are used to represent ambiguity, and it represents that explicitly within the parse tree as a Choice node.

---

# **OPERATIONS**

1. Parsing
2. **Derivative**
3. Nullability
4. Parse forest
5. Compaction

---

# **DERIVATIVE**

- Returns the parser that would match *after* the current one

- Stores matched input in parse trees

- On failure, returns the empty parser

- Different definition for each kind of parser

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

^Each kind of parser is a different class in ObjC, so these are all separate methods from separate classes—`HMRLiteral`, `HMRNull`, `HMREmpty`, and so on.

^The derivative of terminals—parsers which aren’t built from other parsers—is straightforward. Literals derive to a null parse containing the input they match it, or else empty. Null and empty both derive to empty.

---

# **NONTERMINAL DERIVATIVE in OBJC**

```objectivec
// Alternation
-(HMRCombinator *)deriveWithRespectToObject:(id)object {
  return [[self.left derivative:object] or:[self.right derivative:object]];
}
// Concatenation
-(HMRCombinator *)deriveWithRespectToObject:(id)object {
  return HMRCombinatorIsNullable(first)?
    [[[first derivative:object] concat:second]
      or:[[HMRCombinator capture:first.parseForest] concat:[second derivative:object]]]
  : [[first derivative:object] concat:second];
}
// Repetition
-(HMRCombinator *)deriveWithRespectToObject:(id)object {
  return [[self.combinator derivative:object] concat:self];
}
// Reduction
-(HMRReduction *)deriveWithRespectToObject:(id)object {
  return [[self.combinator derivative:object] mapSet:self.block];
}
```

^The derivative of alternation is simple: it’s the alternation of its parsers’ derivatives.

^Reduction too: it’s the reduction of its parser’s derivative by its block.

^Repetition concatenates the derivative of its parser with itself, peeling off copies behind it.

^Concatenation is more complicated. If first is nullable, the derivative of the concatenation is the derivative of first, concatenated with second, *or* the parse trees of first concatenated with the derivative of second. If first is not nullable, then the derivative of the concatenation is just the derivative of first, concatenated with second.

---

# **DERIVATIVE in SWIFT**

```swift
func derive(c: Alphabet) -> Recur {
  switch self.language {
  case let .Literal(x) where x == c:
    return Combinator(parsed: ParseTree(leaf: c))
    
  case let .Alternation(x, y):
    return derive(x, c) | derive(y, c)
    
  case let .Concatenation(x, y) where x.value.nullable:
    return derive(x, c) ++ y
      | Combinator(parsed: x.value.parseForest) ++ derive(y, c)
  case let .Concatenation(x, y): return derive(x, c) ++ y
    
  case let .Repetition(x): return derive(x, c) ++ self
    
  case let .Reduction(x, f): return derive(x, c) --> f
    
  default: return Combinator(.Empty)
  }
}
```

^Because languages in Swift are an enum, we can just pattern match against them. Every operation on different kinds of parsers is pattern matching in Swift.

---

# **RECURSION 🌀 & NONTERMINATION 🔄**

- Context-free languages & grammars are recursive

- NB: Not just the *types*: the object graph is cyclic!

- Key to why you can’t parse arbitrary HTML with a regexp

- Regexps can be matched with a list, but context-free languages need a stack

- Naïve implementations will infinite loop 🔄

---

# **PROTECTING your PARSERS from NONTERMINATION 😎**

1. **Laziness 😴**

---

# **LAZINESS 😴**

- Alternations, concatenations, repetitions, & reductions use closures to delay evaluation

- Avoids nontermination when constructing the derivative

- Necessary to even *construct* cyclic grammars!

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

# **COMPACTION in the FUTURE**

- Generally must compact after derivative, or else cyclic → 🔄

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

	- Crash-happy 😂💥 (as of Xcode 6b2)

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

### https://github.com/robrix/A-Swiftly-Tilting-Parser<br/> rob.rix@github.com ❧ @rob_rix