------------
title: Adventures in Dependently Typed Accounting
author: Arnaud Bailly
date: 2019-09-06
------------

> This article is the [third installment](/posts/dependently-typed-date.html) of a series of articles exploring connection between [Domain Driven Design](https://en.wikipedia.org/wiki/Domain-driven_design) and [Dependent types](https://en.wikipedia.org/wiki/Dependent_type) as implemented in [Idris](http://idris-lang.org). It's been written as prepaatory work to a talk given at the [Okiwi meetup](https://www.meetup.com/software-craftsmanship-bdx/events/264578542/) in Bordeaux.

Although it took me more than 2 years to write a sequel to my previous post on the subject of _DDD + T(ype)DDD_, it's a subject that I keep having in the back of my mind and something I try to apply in my day job wherever it's possible. This time, I am investigating how to write a double-entry bookkeeping module in Idris, in the spirit of [ledger](https://www.ledger-cli.org/) and [hledger](https://hledger.org/). Of course, what I wrote is nowhere near as full-featured as hledger, it's mostly a proof-of-concept that only allows one to _parse_ a ledger-formatted file and _pretty-prints_ back its content.

What's interesting in double-entry accounting is that it's a "business-domain" that is relatively simple to explain and understand, grounded on some basic _invariants_ that should be enforced to guarantee transactions and ledger stay consistent. This post won't go into the details of the code which can be found in a [github repository](https://github.com/abailly/xxi-century-typed/tree/master/idris/src/Accounting). It's more of a narrative on the design choices I made and what it entails to write such code in Idris, trying to highlights both the strengths of the language and its weaknesses, or its darker corners.

I would like to say a big "Thank you!" to the smart and friendly people from the `#idris` channel who have helped me write most of the proofs in this code, providing advices and direction on how to approach proof-writing in such a language.


# Basic Concepts

Here is an excerpt from [Wikipedia page on double-entry bookkeeping](https://en.wikipedia.org/wiki/Double-entry_bookkeeping_system):

> In the double-entry accounting system, at least two accounting
> entries are required to record each financial transaction. These
> entries may occur in asset, liability, equity, expense, or revenue
> accounts. Recording of a debit amount to one or more accounts and
> an equal credit amount to one or more accounts results in total
> debits being equal to total credits for all accounts in the general
> ledger. If the accounting entries are recorded without error, the
> aggregate balance of all accounts having Debit balances will be
> equal to the aggregate balance of all accounts having Credit
> balances. Accounting entries that debit and credit related accounts
> typically include the same date and identifying code in both
> accounts, so that in case of error, each debit and credit can be
> traced back to a journal and transaction source document, thus
> preserving an audit trail. The accounting entries are recorded in
> the "Books of Accounts". Regardless of which accounts and how many
> are impacted by a given transaction, the fundamental accounting
> equation of assets equal liabilities plus capital will hold.

From this description we get some basic information about the "domain" that we'll want to implement:

* A _transaction_ comprises at least 2 _entries_
* An _entry_ records a _debit_ or _credit_ amount in an _account_
* An _account_ can fall into 5 different categories: _asset_, _liability_, _equity_, _expense_, or _revenue_
* The aggregate debit and credit _balance_ of all accounts should be equal
* A _book of accounts_ should preserve a _fundamental equation_ that ensures `asset = liability + equity`[^1]

From these elements, we can start to code and first of all define the _types_ we'll need. The whole point of this series of post is to apply DDD principles to Type-Driven Development which means we want our _domain concepts_ to be reflected directly into the _core domain_ of our code. So we end up needing the following types:

* An `Entry` which contains an `Account` and some _amount_ (more on this later) with a `Direction`, `Debit` or `Credit`,
* A `Transaction` has a date, a `String` label and a list of `Entry` which must be `Balance`d,
* A `BookOfAccounts` is a list of `Transaction`s such that the _fundamental equation_ holds at all time.

# Take 1

Equipped with all this information, I started implementing the various data types, embedding the needed invariants within the definition of the types. My initial version looked like the following (see [Core.idr](https://github.com/abailly/xxi-century-typed/blob/2c81760819f1ab1d9ba0bc101b64a168d2817bb8/idris/src/Accounting/Core.idr) for details).

## Core Domain

The `Balance` is a simple alias for a tuple of a `Nat` and a `Direction` (debit or credit) and the other core types are straightforward:

~~~~idris
Balance : Type
Balance = (Nat, Direction)

data Account : Type where
  MkAccount : String -> { type : AccountType } -> Account

record Entry where
  constructor MkEntry
  amount : Balance
  account : Account
~~~~

Then a `Transaction` contains `Entries` of length at least 2 and with a `Balance` of `(0, Cr)`.

~~~~idris
data Entries : Type where
  MkEntries : (entries : Vect n Entry) ->
              { auto need2Entries : LTE 2 n } ->
              { auto balanced : balance entries = (0, Cr) } ->
              Entries

record Transaction where
  constructor Tx
  label : String
  date : Date
  entries : Entries
~~~~

The `balance` function computes the aggregated balance of a list of entries, taking advantage of the fact our `Balance` type is a `Monoid`:

~~~~idris
balance : Vect n Entry -> Balance
balance =  normalise . concat . map amount
~~~~

The `normalise` function is needed because a 0 balance can be either a `Dr` or `Cr`. More on this later...

~~~~idris
  where
    normalise : Balance -> Balance
    normalise (Z, Dr) = neutral
    normalise bal     = bal
~~~~

And we are then ready to define our `BookOfAccounts` type to group a sequence of transactions:

~~~~idris
data BookOfAccounts : Type where
  BookTransactions : (txs : Vect k Transaction) ->
                     { auto fundamentalEquation : invert (assets txs <+> expenses txs) = liabilities txs <+> capital txs <+> revenues txs } ->
                     BookOfAccounts
~~~~

Note that we have modified the `fundamentalEquation` to take into account `expenses` and `revenues`. It is actually the case that `asset = liability + equity` only when taking into account the profit or deficit that is the difference between revenues and expenses. In actual accounting practices, a "normal" transaction always involves an expense or revenue account and one or more balance sheet account.

## Testing

One of the benefits of using a Type-Driven Approach in a language like Idris is that we can use the compiler/typechecker to run _tests_, instead of having to define separate test suites.

Given some `Account`s definitions:

~~~~idris
Capital : Account
Capital = MkAccount "Capital" {type = Equity}

Bank : Account
Bank = MkAccount "Bank" {type = Asset}
~~~~

We can use propositional equality, that is the `Type` asserting that two expressions are the same, as a way to assert some property holds, and then _prove_ the property actually holds by providing an implementation for it that will be typechecked:

~~~~idris
valid1 : balance [ MkEntry (100, Dr) Bank,
                  MkEntry (100, Cr) Capital ] = (0, Cr)
valid1 = Refl

valid2 : balance [ MkEntry (100, Cr) Bank,
                  MkEntry (100, Dr) Capital ] = (0, Cr)
valid2 = Refl
~~~~

Interestingly we can also state _negative_ properties by proving there can never be an implementation for this type:

~~~~idris
invalid : Not (balance [ MkEntry (100, Cr) Bank,
                         MkEntry (101, Dr) Capital ] = (0, Cr))
invalid = \ Refl impossible
~~~~

And those tests found a bug in my code! The order in which the entries were given in a transaction mattered as I had forgotten to recursively call the accumulation function in one case.

## Parsing

Once we have our core model's types defined, we can try to talk to the outside world. Here, we'll simply parse a list of transactions in the _ledger_ format:

```
2019-01-01 Some transaction
  Asset:Bank  D 100
  Equity:Capital C 100

2019-01-02 Another transaction
  Asset:Bank C 90
  Expense:Foo D 80
  Liability:Tax D 10

```

and ensure we are able to pretty-print it in the same format.

I use the [lightyear](https://github.com/ziman/lightyear) parser combinators library which is pretty-much a clone of Haskell's parsec and is similar to every other parser combinators library out there. What's more relevant to our purpose is the fact that the types I have defined _enforce_ their invariant at the _constructor level_ which means they require the calling context to provide the _proofs_ those invariants are indeed valid.

In the `parseEntries` function for example, we need to make sure provide a sequence of the correct length (at least 2) and a proof that the entries are _balanced_ in order to build an `Entries` value.

~~~~idris
parseEntries : Parser Entries
parseEntries = do
  e1 <- parseEntry
  endOfLine
  e2 <- parseEntry
  endOfLine
  es <- sepBy parseEntry endOfLine
  let entries = e1 :: e2 :: fromList es
  case decEq (balance entries) Zero of
    (Yes prf) => pure $ MkEntries entries
    (No  _)   => fail "Entries are not balanced, total debits minus total credits should be 0"
~~~~

In other words, there is no way[^2] to build an invalid value and the Idris type system guarantees our _core model_ will stay consistent _at compile time_. Applying a _Ports and adapters_ or _Hexagonal Architecture_ strategy leaves no room for introducing errors in our core model, completely alleviating the need to have such validation concerns (eg. checking values range, lengths, validate data...) leak into the model's code.

## Evaluation

I was pretty happy with myself, patting me in the back for having succeeded in modelling double-entry bookkeeping in a nice simple type-safe model. Then I realised that _implementing_ `Monoid Balance` was the first step, I still needed to _prove_ the [monoid laws](https://en.wikibooks.org/wiki/Haskell/Monoids#Monoid_laws) hold for my type and `<+>` operation. This is when things started to go awry...

The proof for neutral element is short but already highlights the main issue:

~~~~idris
rightNeutralBalance : (x : Balance) -> (x <+> (0, Cr) = x)
rightNeutralBalance (n, Cr) = rewrite plusZeroRightNeutral n in Refl
rightNeutralBalance (Z, Dr) = believe_me "special case for zero debit"
rightNeutralBalance (S n, Dr) = Refl
~~~~

The problem is that there really are _two_ neutral elements, a 0 debit and a 0 credit. I have chosen to consider the canonical neutral element to be `(0, Cr)` but that's just a convention which comes bite me in the back when trying to prove things: I am forced to use the magic `believe_me` function to lure the typechecker in the `(0, Dr)` case.

Things get even worse when trying to prove associativity:

~~~~idris
associativeBalance : (x : Balance) -> (y : Balance) -> (z : Balance) -> (x <+> (y <+> z) = (x <+> y) <+> z)
associativeBalance (a, Dr) (b, Dr) (c, Dr) = rewrite plusAssociative a b c in Refl
associativeBalance (a, Cr) (b, Cr) (c, Cr) = rewrite plusAssociative a b c in Refl
associativeBalance (a, Dr) (b, Cr) (c, Cr) with (order {to=LTE} a (plus b c))
  | (Left l) with (order {to=LTE} a b)
    | (Left x) = rewrite minusPlusPlusMinus b c a in Refl
    | (Right r) with (order {to=LTE} (a - b) c)
      | (Left x) = rewrite minusPlusMinusMinus b c a in Refl
      | (Right x) = ?hole_4
~~~~

What happens here is that I got caught in a maze of lemmas involving various combinations of plus and minus operations, which unvoidably would end in the need to prove that `Dr = Cr` which, obviously, would fail or need some magic axiom.

After much struggling with the typechecker and discussion on slack, I realised this proof was problematic because the underlying types were fatally flawed. As brillantly stated by Ohad Kammar:

> Since it's going to be impossible to get Idris to recognise all false statements automatically, the best we can hope for is that proving false statements is going to get more and more difficult until the programmer gives up

The types are wrong because I am conflating two concepts: The _Balance_ resulting from aggregating one or more entries for some account(s), and the _Amount_ of each entry, along with a direction (Debit or Credit). Although they both are integral values with a "sign", they have different meaning in the domain and different behaviour: A _Balance_ can be 0, whereas an entry's Amount cannot.

Moreover, the structure of the `Balance` type is also wrong and some form of premature optimisation: Instead of having a proper data type to represent the concept along with the constraints it supports, I am trying to "abuse" existing types.

# Take 2

So I went for a deep refactoring of my code to cleanly separate the concepts of an `Amount` and a `Balance` along with their proper constraints. This was also a good opportunity to start splitting my code in more manageables parts.

## Amounts

An `Amount` is basically a non-zero `Nat`ural integer so let's embed that property in our type's constructor:

~~~~idris
data Amount : Type where
  MkAmount : (n : Nat) -> { auto notZero : LTE 1 n } -> Amount
~~~~

We want our `Amount` to be straightforward to use so I've decided I would like to equip it with a `Num` implementation:

~~~~idris
Num Amount where
  (MkAmount n {notZero=nz}) + (MkAmount k {notZero=nz'}) =
    MkAmount (n + k) { notZero = plusRightIsLte nz }

  (MkAmount n {notZero=nz}) * (MkAmount k {notZero=nz'}) =
    MkAmount (n * k) { notZero = lteOneMult nz nz' }

  fromInteger = fromIntegerAmount
~~~~

Although the actual _code_ for addition and multiplication is trivial and it is _obvious_ that adding and multiplying non-zero integers yields non-zero integers, it was non-trivial (at least for me) to build the needed proofs. The addition case relies on a couple of properties from [Nat](https://www.idris-lang.org/docs/current/prelude_doc/docs/Prelude.Nat.html) defined in Idris' prelude, namely that adding a number to the right of a `LTE` comparison preserves it, and that `LTE` is transitive:

~~~~idris
plusRightIsLte : LTE j k -> LTE j (k + n)
plusRightIsLte x   {k} = lteTransitive x (lteAddRight k)
~~~~

The multiplication case is similar except that the standard prelude does not provide a `lteMultRight` proof so I need to build it myself. This proofs is of course slightly different from the addition case because we need the factor `k` to be non-zero:

~~~~idris
lteMultRight : (n : Nat) -> { auto nz : LTE 1 k } -> LTE n (mult n k)
lteMultRight Z               = LTEZero
lteMultRight n     {k = Z}   impossible
lteMultRight (S j) {k = S k} =
  rewrite plusCommutative k (j * S k)
  in LTESucc (plusRightIsLte $ lteMultRight j {k = S k})
~~~~

This is done by induction on `n` while prooving the case `k = 0` is indeed `impossible`, and here is a detailed step-by-step implementation:

Start with the signature for the _proposition_. Note that we can already use that signature in our implementation of `Num Amount`, as an additional hypothesis we'll need to prove for our implementation to satisfy the typechecker:

~~~~idris
lteMultRight : (n : Nat) -> { auto nz : LTE 1 k } -> LTE n (mult n k)
~~~~

Hitting `C-c C-s` in Emacs and then case-splitting (with `C-c C-c` on `n` gives us the two base cases:

~~~~idris
lteMultRight : (n : Nat) -> { auto nz : LTE 1 k } -> LTE n (mult n k)
lteMultRight Z = ?lteMultRight_rhs_1
lteMultRight (S j) = ?lteMultRight_rhs_2
~~~~

The first case is directly solvable by hitting `C-c C-a` on the provided hole, while we need to bring in scope the (implicit) `k` and case-split on it for the second case:

~~~~idris
lteMultRight Z = LTEZero
lteMultRight (S j) {k = Z} = ?lteMultRight_rhs_1
lteMultRight (S j) {k = (S k)} = ?lteMultRight_rhs_3
~~~~

The second case is clearly `impossible` yet the typechecker fails to infer it automatically so we have to fill it by hand. This leaves us with the following hole to fill:

~~~~idris
 `--                                     k : Nat
                                         j : Nat
                                        nz : LTE (fromInteger 1) (S k)
     ------------------------------------------------------------------------------
      Accounting.Amount.lteMultRight_rhs_3 : LTE (S j) (S (plus k (mult j (S k))))
~~~~

This type looks like something we can recurse over inductively because we know that:

```
λΠ> :t LTESucc
LTESucc : LTE left right -> LTE (S left) (S right)
```

So peeling one layer

~~~~idris
lteMultRight (S j) {k = (S k)} =
  LTESucc ?hole
~~~~

and looking for the type of the hole we have

~~~~idris
 `--                       k : Nat
                           j : Nat
                          nz : LTE (fromInteger 1) (S k)
     --------------------------------------------------------
      Accounting.Amount.hole : LTE j (plus k (mult j (S k)))
~~~~

gives us some more information. We can call `lteMultRight` inductively on `j` (which is reducing `n`) and see what's needed:

~~~~idris
lteMultRight (S j) {k = (S k)} =
  LTESucc (?hole $ lteMultRight j {k = S k})
~~~~

which gives us the `hole`

~~~~idris
  k : Nat
  j : Nat
  nz : LTE 1 (S k)
--------------------------------------
Accounting.Amount.hole : LTE j (mult j (S k)) -> LTE j (plus k (mult j (S k)))
~~~~

We now notice that the type of `hole` looks like the type of our `plusRightIsLte`:

~~~~idris
plusRightIsLte : LTE j n         -> LTE j (n + k)
hole           : LTE j (j * S k) -> LTE j (k + (j * S k))
~~~~

but with `n = j * S k` and the order of arguments in the addition on the right reversed. We can pull in the fact that addition is commutative:

~~~~idris
lteMultRight (S j) {k = (S k)} =
  rewrite plusCommutative k (j * S k)
  in LTESucc (?hole $ lteMultRight j {k = S k})
~~~~

and now our `hole` has the right type:

~~~~idris
      Accounting.Amount.hole : LTE j (j * S k) -> LTE j ((j * S k)  + k)
~~~~

which allows us to conclude our proof:

~~~~idris
lteMultRight (S j) {k = (S k)} =
  rewrite plusCommutative k (j * S k)
  in LTESucc (plusRightIsLte $ lteMultRight j {k = S k})
~~~~

The real process was much more hectic and I needed quite a lot of help from slack's people!

## Balance

We can now turn our attention to the `Balance` type and provide an implementation that takes into account the possibility for a balance to be _null_ while preserving the chances to make our `Balance` type a proper monoid:

~~~~idris
data Balance : Type where
  Zero : Balance
  Bal : (n : Amount) -> (d : Direction) -> Balance
~~~~

The key insight here is to introduce a special constructor to denote 0 which will remove the need to do "magical" things when 2 `Balance` gets to compensate each other depending on the ordering of the operands. The definitions for the various algebraic structure our `Balance` is supposed to implement (semigroup, monoid and group) are simple once we introduce the `compensate` operation relating `Amount`s and `Direction`s to yield a `Balance`:

~~~~idris
compensate : (n : Amount) -> (d : Direction)
          -> (n' : Amount) -> (d' : Direction)
          -> { auto notEqDir : Not (d = d') }
          -> Balance
compensate (MkAmount n) d (MkAmount n') d' with (decEq n n')
    | (Yes prf) = Zero
    | (No contra) with (order {to=LTE} n n')
      | (Left l)  = Bal (MkAmount (n' - n)
                        { notZero = notEqualMinusGTOne n n' l contra }) d'
      | (Right r) = Bal (MkAmount (n - n')
                        { notZero = notEqualMinusGTOne n' n r (notEqReflexive contra) }) d
~~~~

This definition is however made more complex than we'd hoped because:
1. When substracting 2 `Nat`ural numbers, we need to prove the first number is greater than or equal to the second one, which entails the need to call `order` here to case-split on the order of the 2 numbers with the relevant proof,
2. We also need to ensure the difference is still greater than or equal to 1 in order to build an `Amount`.

This is a tribute to the fact our `Amount` type is "complex", eg. its constructor is not only a _structure_ to aggregate some other datatypes but also carries with it some _proofs_ which need to be maintained at all time. This will come bite us again later on...

## Refactoring

Changing our 2 core types' definition ripples through our system but without entailing any deep changes in our top-level types. The structure and the properties of our `Entry`, `Transaction`, `Entries` and `BookOfAccounts` types stay mostly the same except for the fact we don't use `Balance` for defining an `Entry` but separate the `Amount` and the `Direction`. We only have to fix the compiler's  errors one at a time and end-up with pretty much the same code than we had before.

The introduction of `Amount` forces us however to make the numbers explicit in our sample entries. Although `Amount` is a `Num`ber and the compiler automatically introduces a conversion `fromInteger` when we use a literal in place of an `Amount`, writing

~~~~idris
valid1 : balance [ MkEntry 100 Dr Bank,
                   MkEntry 100 Cr Capital ] = Zero
valid1 = Refl
~~~~

yields the following typechecker error:

```
             Type mismatch between
                     Zero
             and
                     compensate (fromIntegerAmount 100) Dr (fromIntegerAmount 100) Cr
```

because the typechecker cannot see "past" `fromIntegerAmount`. We need to either explicitly call `MkAmount` or turn on `public export` visibility on `fromIntegerAmount` to allow the typechecker to reduce expressions usinge the definition of the function.

## Evaluation

The situation is definitely better than with our first model: We have a clear separation of two key concepts in our system and have strengthened our understanding of the domain and the relationship between the domain and the code by baking more properties.

Turns out it's not all bright and shinny, which becomes apparent when we try (again) to prove the `Group Balance` properties. I won't go into the tedious details in this already long post but this ended up being a long, interesting but painful journey in the realm of proofs writing. Interested reader is referred to the [Proofs.idr](https://github.com/abailly/xxi-century-typed/blob/master/idris/src/Accounting/Proofs.idr) file which contains the current (unfinished) state of the proofs of neutral element, inverse and associativity of `Balance` with `<+>` operation.

I gave up trying to prove associativity at some point, when it became clear the proof would be a long and tedious enumeration of all possible cases depending on the respective ordering of the `Balance`s and the values of their `direction`. This is caused by the fact the definition of `<+>` involves a case-split on the `direction`s and then uses `compensate` which itself involves 2 case-splitting: on the the equality of the values, and on their relative ordering. The Idris typechecker works by replacing each function call by its definition and trying to reduce the resulting expression to some ground types. This means that when one wants to provide a proof that `(a <+> b) <+> c = a <+> (b <+> c)`, the applications of `<+>` are only reduced if the typechecker can follow the various branches in `<+>` definition to conclude. This can only happen if we provide the relevant proofs which basically means the structure of our proof necessarily follows from various _paths_ leading to a conclusion in our function's implementation.

# Conclusion and Takeaways

I have not yet undertaken _Take 3_ but it's pretty clear I am not there yet: The fact the proof for associativity of `Balance` is still daunting and painful is certainly a sign the involved types are more complex than they should be and need some more refactoring.

There are a couple of other options that would be worth exploring:
* The embedding of the `notZero` proof does not seem like a good idea as it brings more proof obligations in scope at each use of an `Amount`. A better solution could be to have `Amount` use directly `Nat` but consider it to represent it's successor, and then provide smart constructor and operations to manipulate those numbers while preserving this internal property,
* There exists a [ZZ](https://github.com/idris-lang/Idris-dev/blob/master/libs/contrib/Data/ZZ.idr) module to represent _relative_ integers. I could either get some inspiration from it or use it directly for `Amount`s and to hold a `Balance`, with some encoding to extract the direction (debit or credit) from the sign of the number,
* Rather than trying to make `Balance` into a `Monoid`/`Group`, I could define a `Vect n Entry` to be a `Monoid` action _on_ a `Balance`, eg. provide a specialised operation that applies an `Entry` on a `Balance` to give another `Balance`.

I am currently leaning towards exploring a combination of option 2 (use `ZZ` to contain an `Entry`'s amount and drop `Amount`) and option 3 (keep a separate `Balance` type but only use it in conjuction with `Entry`) as it seems to be both simpler than the current solution yet still close to the domain.

All in all, this experience has confirmed my initial intuition about Idris and the relevance of dependently-typed languages in providing "better" business domain models, something which I have been convinced of since I discovered Idris thanks to Edwin Brady's [book](https://www.manning.com/books/type-driven-development-with-idris). Working through some involved (for me) proofs was an eye opener on both how difficult this process can be, and how much insight in one's design it can provide. Some key takeaways I would like to share are:

* "Proving" propositions by coding their implementation is really fun, and at the same time immensely frustrating: You see something that's obvious, and you try to convince the typechecker this is indeed obvious armed with a bunch of functions, types and rewriting rules, and it sometimes does not align and you don't know why...
* Proving negative statements (eg. types of the form `foo -> Void`) is often, somewhat unsurprisingly, harder and less intuitive than proving positive statements
* Thanks to the help provided by various people on slack, I had some _Ah-ah_ moments on the inner workings of the typechecker and gathered some more insights on how to write proofs:
  * For a function call involved in a proof to be reduced, it needs to which case to chose which implies one need to either provide explicitly pattern-matching on arguments or `with` clauses (which really act like "local" arguments),
  * The order of case-splitting on arguments matters and can make a proof more or less complicated,
  * It's generally easier to prove general statements and then use that in a specialised way, than the converse,
  * _Implicit_ values are not rewritten by `rewrite ... in ...` statements, so they need to be provided in some way in the context
  * As already stated, it's better to not have _implicit_ proofs be part of one's types definitions: Only export the type itself and provide smart constructors when needed.
* Leveraging the REPL and the interactions it permits is very important as it permits one to isolate the problems to solve, step-by-step, until all is left are simple lemmas or possibly axioms,
* Relating domain-level concepts to more abstract mathematical or computer-related concepts can be a source of insights on the domain itself, for example relating a `Balance` to `Monoid` or `Group`. And trying to prove the relevant properties gives more insights on the _representation_ used and whether or not it suits our purpose.

This last point in particular is interesting as it provides a way to start a dialog between the _domain_ and the _code_ that is not one-way. We often develop software considering the "business domain"  _ex abstracto_, as if the fact we were designing a _software system_ for that domain was irrelevant. But this is not true, the fact it _runs on software_ changes the domain itself hence it's perfectly legitimate we can gather insights about the domain from the code implementing it. In the case of _accounting_ this is pretty obvious as we are manipulating numbers and it's a well-studied domain, but I think most if not all domains would strongly benefit from such dialog, transforming as hodge-podge of ad-hoc rules into something simpler, more regular and probably more efficient.



[^1]: It's interesting to note the original sentence seems to imply capital and equity are one and same thing, which is not true.

[^2]: Short of cheating using something like [believe_me](https://www.idris-lang.org/docs/current/prelude_doc/docs/[builtins].html#believe_me) of course that will _subvert the typechecker_.
