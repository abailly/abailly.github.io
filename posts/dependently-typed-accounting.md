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

# Stage 1

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

Note that we have modified the `fundamentalEquation` to take into account `expenses` and `revenues`. It is actually the case that `asset = liability + equity` only when taking into account the profit or deficit that is the difference between revenues and expenses. In actual accounting practices, a "normal" transaction always involves an expense or revenue account and or more balance sheet account.

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

# Stage 2

I was pretty happy with myself

[^1]: It's interesting to note the original sentence seems to imply capital and equity are one and same thing, which is not true.

[^2]: Short of cheating using something like [believe_me](https://www.idris-lang.org/docs/current/prelude_doc/docs/[builtins].html#believe_me) of course that will _subvert the typechecker_.
