------------
title: DDD, meet TDD
subtitle: More powerful types for better domain modeling
author: Arnaud Bailly - @dr_c0d3
date: 2020-02-07
theme: serif-compact
transition: none
------------

## Double-Entry Accounting Domain

> In the double-entry accounting system, at least two accounting entries are required to record each financial transaction. [..] Recording of a debit amount to one or more accounts and  an equal credit amount to one or more accounts results in total debits being equal to total credits for all accounts in the general ledger.
> ...

## Business rules

* A _transaction_ comprises at least 2 _entries_
* An _entry_ records a _debit_ or _credit_ amount in an _account_
* An _account_ can fall into 5 different categories: _asset_, _liability_, _equity_, _expense_, or _revenue_
* The aggregate debit and credit _balance_ of all accounts should be equal
* A _book of accounts_ should preserve a _fundamental equation_ that ensures `asset = liability + equity`

## First Model

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

-----

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

-----

~~~~idris
data BookOfAccounts : Type where
  BookTransactions :
      (txs : Vect k Transaction) ->
      { auto fundamentalEquation :
         invert (assets txs <+> expenses txs) =
                 liabilities txs <+> capital txs <+> revenues txs }
                 -> BookOfAccounts
~~~~

## Testing with the Compiler

~~~~idris
valid2 : balance [ MkEntry (100, Cr) Bank,
                  MkEntry (100, Dr) Capital ] = (0, Cr)
valid2 = Refl

invalid : Not (balance [ MkEntry (100, Cr) Bank,
                         MkEntry (101, Dr) Capital ] = (0, Cr))
invalid = \ Refl impossible
~~~~

## Enforcing Invariants at the Hexagon's Sides

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

## A Flaw in the Model

Trying to prove the `Monoid` properties for a `Balance` forced me to realize my model was wrong:

* I am conflating two concepts: The _Balance_ and the _Amount_ of each entry
* `Balance` should have a proper domain-relevant representation
* Idris' compiler has a good way to tell you your design is wrong: Proving properties becomes increasingly difficult

## Better Types

~~~~idris
data Amount : Type where
  MkAmount : (n : Nat) -> { auto notZero : LTE 1 n } -> Amount
~~~~

~~~~idris
data Balance : Type where
  Zero : Balance
  Bal : (n : Amount) -> (d : Direction) -> Balance
~~~~

## The Dark Side of the Moon

Things start to get tricky when trying (again) to prove `Monoid` properties on a `Balance`:

* [Proofs](https://github.com/abailly/xxi-century-typed/blob/master/idris/src/Accounting/Proofs.idr) of *associativity* quickly became bogged down into a maze of various cases
* Embedding proofs in _data types_ means we have to _discharge_ those proofs to construct values, which leds to more work
* The `Balance` representation is probably still wrong and needs to be _distilled_

## Conclusion

* Proving things about your domain's model is _fun_ and _rewarding_
* Working with a "smart" compiler providing powerful type system and REPL helps you refine your domain _incrementally_ and _iteratively_
* Relating domain-level concepts to more abstract concepts can be a source of insights on the domain itself
* Trying to prove the relevant properties gives more insights on the _representation_ used

## The Book

![](https://images.manning.com/720/960/resize/book/1/453215a-afa1-443f-9f2d-3b6bf24c34db/Brady-TDDI-HI.png)

## References

* This talk is actually from a blog post: [http://abailly.github.io/posts/dependently-typed-accounting.html](http://abailly.github.io/posts/dependently-typed-accounting.html)
* You can find the code on Github: [https://github.com/abailly/xxi-century-typed](https://github.com/abailly/xxi-century-typed/blob/master/idris/src/Accounting.idr)
* I am [dr_c0d3](https://twitter.com/dr_c0d3) on Twitter
