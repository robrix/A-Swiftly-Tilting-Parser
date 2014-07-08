# **A SWIFTLY TILTING PARSER**

## _in memory of Madeleine L’Engle_

### https://github.com/robrix/A-Swiftly-Tilting-Parser <br/> rob.rix@github.com ❧ @rob_rix

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

---

# **THE DERIVATIVE of PARSERS**

- Might, Darais, & Spiewak’s 2011 paper *[Parsing with Derivatives—a Functional Pearl](http://matt.might.net/papers/might2011derivatives.pdf)*

- *Recognizes* and *parses* context-free languages

	- Recognizing: “is my input valid?”

	- Parsing: “how is the input structured?”

- Validity and structure are defined by the grammar, which is made of parser combinators

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
  HMRCombinator *first = self.first;
  HMRCombinator *second = self.second;
  HMRCombinator *derivativeAfterFirst = [[first derivative:object] concat:second];
  return HMRCombinatorIsNullable(first)?
    [derivativeAfterFirst or:[[HMRCombinator capture:first.parseForest] concat:[second derivative:object]]]
  : derivativeAfterFirst;
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
func derive<Alphabet : Alphabet>(combinator: Combinator<Alphabet>, character: Alphabet) -> Combinator<Alphabet> {
  let derive: (Combinator<Alphabet>, Alphabet) -> Combinator<Alphabet> =
    fixpoint(combinator, { HashablePair($0, $1) }) { recur, parameters in
    let (combinator, character) = parameters
    switch combinator.language {
    case let .Literal(c) where c == character:
      return Combinator(.Null(ParseTree(leaf: c)))
    
    case let .Alternation(x, y):
      return Combinator(.Alternation(delay(recur(x, character)), delay(recur(y, character))))
      
    case let .Concatenation(x, y) where x.forced.nullable:
      return recur(x, character) ++ y | Combinator(parsed: x.forced.parseForest) ++ recur(y, character)
    case let .Concatenation(x, y):
      return recur(x, character) ++ y
      
    case let .Repetition(x):
      return recur(x, character) ++ combinator
      
    case let .Reduction(x, f):
      return recur(x, character) --> f
      
    default:
      return Combinator(.Empty)
    }
  }
  return derive(combinator, character)
}
```

^Because languages in Swift are an enum, 

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

# **LAZINESS in OBJC**

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
return _compaction ?: HMRDelay([self compact]);
```

---

# **PROTECTING your PARSERS from NONTERMINATION 😎**

1. Laziness 😴
2. **Memoization 📎**

---

# **MEMOIZATION** 📎

- The first time you call a memoized function with a set of arguments, it stores the results

- The next time, it looks them up; memoize ≅ cache

- Can store results in a dictionary, ivar, etc.

- Allows the derivative to “tie the knot” when building a cyclic grammar *from* a cyclic grammar

---

# **MEMOIZATION in OBJC**

```objectivec
#define HMRMemoize(x, start, body) ((x) ?: ((x = (start)), (x = (body))))

-(HMRCombinator *)derivative:(id<NSObject, NSCopying>)object {
  return HMRMemoize(_derivativesByElements[object],
    [HMRCombinator empty],
    [self deriveWithRespectToObject:object].compaction);
}
```

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

# **FIXPOINTS in OBJC**

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

- I solve *my* problems more; incidental ones less 🙌

- Enables me to make mistakes faster, & with greater confidence 🎢

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