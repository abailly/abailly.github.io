---
title: Understanding Existential Types
author: Arnaud Bailly 
date: 2017-03-31
---

Existential types are a less well-known extension to Haskell's type system and Damas-Hindley-Milner type theory, although they are a logical consequence of the identification of type theory with predicate logic: If type variables can be universally quantified then it seems logical they can also be existentially quantified. I have used existential types in Haskell on several occasions but my brain always struggled to really understand what was going on and how to fix the dreaded error messages I got back from the compiler.

While working on [my talk](/slides/xxi-century-typed.html) on type-driven development I used existentials to model one of the evolution step of the quizz program I was working on and had some sort of epiphany. This short post is an attempt to capture the insights I gathered in the hope it might be useful for other people.

# The Problem

The problem is pretty simple and quite common. A quizz is made up from different *types* of questions which are part of a quizz, say open questions, multiple choice questions, rating questions. Each question expects a different type of answer: A free text for open questions, selected option for MCQ, a number for rating questions, but the answer is always initially given by the user as a text which must be converted to the right form.

So we have the following (abridged) types of questions:

~~~~haskell
data OpenQuestion 
data MCQuestion
data RatingQuestion
~~~~

We expose the API of questions as a *typeclass* in order to be future-proof (we would like to make it easy to introduce new types of questions):

~~~~~haskell
class Questionable q where
    type Answer q :: *
    -- other methods omitted....
    isCorrectAnswer :: Answer q -> q -> Bool
~~~~~

and then we need to define a `Quizz` as a sequence of questions and some functions to compute the answers to the questions and assess the user's result. The `User` is simply modelled as a function that provides an answer (or no answer) as a string, given any question.

~~~~~haskell
type User = Text -> Maybe Text

data Quizz = Quizz { questions :: [ Question ] }

answerQuestion :: User -> Question -> Quizz -> Quizz
answerQuestion user question quizz = ...

answers :: User -> Quizz -> Quizz
answers user quizz =
 foldr (answerQuestion user) quizz (questions quizz)
~~~~~ 

The key issue is then to define `Question` in such a way that it allows us to work with current (and future) question's types in a uniform way. 

# Using Existential type

A very simple solution would be to wrap each possible type in a specialized constructor, which in essence amounts to *tag* each possible `Question` with its type:

~~~~~haskell
data Question = 
    MCQ MCQuestion
  | Open OpenQuestion
  | Rating RatingQuestion
~~~~~

This solution, while simple to use as it is amenable to direct pattern-matching, suffers from an obvious drawback: It closes the range of available types of questions, or rather makes it difficult to add a new one. 

Given that each type of question is assumed to be an instance of the `Questionnable` interface, we can wrap them using existential quantification in the type `Question`, using one of the two available syntaxes. The classic data type declaration syntax:

~~~~~haskell
data Question =
  forall q . Questionable q => Question q
~~~~~

or the GADT-style syntax which explicitly exposes the constructor `Question` as a function

~~~~~haskell
data Question where
  Question :: Questionable q => q -> Question
~~~~~

The effect of an existential type construction is to limit the scope of the type variable `q` appearing in the constructor, thus in effect ensuring the question itself, whatever its type, must stay within the scope of its appearance. For example, while it is possible to pattern match on `Question` to get whatever `q` is packed inside it, this `q` cannot be returned. Writing 

~~~~haskell
getQ (Question question _) = question
~~~~

yields the following, somewhat dreadful, compiler error: 

~~~~
error:
    • Couldn't match expected type ‘t’ with actual type ‘q’
        because type variable ‘q’ would escape its scope
      This (rigid, skolem) type variable is bound by
        a pattern with constructor:
          Question :: forall q.
                      Questionable q =>
                      q -> Question,
        in an equation for ‘getQ’
        at /Users/arnaud/projects/xxi-typed/haskell/.stack-work/intero/intero2342CZJ.hs:11:7-18
    • In the expression: question
      In an equation for ‘getQ’: getQ (Question question) = question
    • Relevant bindings include
        question :: q
          (bound at /Users/arnaud/projects/xxi-typed/haskell/.stack-work/intero/intero2342CZJ.hs:11:16)
        getQ :: Question -> t
          (bound at /Users/arnaud/projects/xxi-typed/haskell/.stack-work/intero/intero2342CZJ.hs:11:1)
~~~~

This is so because the type of the result `question` being `question :: q` implies that the type variable `q` becomes *free* in any context where `getQ` is used: It escapes the scope of the constructor. 

Interestingly, the [ScopedTypeVariables](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#lexically-scoped-type-variables) GHC extension gives us the capability to use that `q`. We can write the following (somewhat contrived) function:

~~~~haskell
slug (Question (quest :: q) _) = slugify quest
  where
    slugify :: q -> Text
    slugify = T.take 3 . question
~~~~

In the type of `slugify` we are not forced to expose the constraint `Questionable q` because the type variable `q` at this point is the one brought in scope with the expression `q :: q`.

# Understanding Existential Types

What's somewhat confusing for the average programmer who has not had a PhD in type theory is: Why are those types called *existential*? Especially given the fact they are introduced by `forall` keyword? [This StackOverflow](http://stackoverflow.com/questions/10753073/whats-the-theoretical-basis-for-existential-types) does a great job at explaining how (intuitionistic) logic rules relate $\forall$ and $\exists$ quantifiers in the case of type constructors.

In short, it comes from the fact that 
$$
\forall x. Q(x) \implies P  =  (\exists x. Q(x)) \implies P.
$$

# References 

There are already a number of resources on the topic:

* Roman Cheplyaka's [24 Days of GHC Extensions](https://ocharles.org.uk/blog/guest-posts/2014-12-19-existential-quantification.html) does a great job at explaining how existentials work in Haskell
* Benjamin Pierce's [Types and Programming Languages](http://www.cis.upenn.edu/~bcpierce/tapl/) has chapter 24 dedicated to the subject of existential types, whose main application is the modelling of object-oriented programming
* [GHC manual](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#existentially-quantified-data-constructors) of course, gives the fine prints

