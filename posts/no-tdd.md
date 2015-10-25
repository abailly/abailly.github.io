------------
title:  Ten "Good" Reasons Not to Do TDD
author: Arnaud Bailly 
date: 2014-01-31
------------

When trying to introduce *Test-Driven Development* in a team or company, it is normal to encounter resistance: Change is always
hard, whatever it may be, for *everybody*. It is a transgression of the current norm and rules that most people would not accept
readily. 

Here is a small compendium of ten "good" reasons I have been given for not doing TDD while introducing that technique to
new teams.

## "It Is going to slow us down!"

If we deliver code faster, we have more times to discover bugs and fix them using real testing conditions, eg. in production. 

## "It takes a lot of time to master!"

We would rather employ youngsters having just graduated that will put 12+ hours of work per day and ship code faster than take
the time to train people on TDD
   
## "Managers and Business people keep setting tight deadlines"

We cannot or do not know how to say "no" to our customers, so we keep promising features and given we work hard and fast, we can
deliver and fix bugs later. Anyway, it has always worked that way so the customer does not even expect first versions to be
correct so everybody knows and expect there will be bugs (and more overtime to fix them after delivery...). Besides, our
contracts fix scope, deadline and price with high penalty in case of failure to deliver so we would rather ship it as planned.

## "We already tried writing unit tests and it became a maintenance nightmare"

Given we already have QA team that takes care of testing, and the complexity of the code, it is better to invest in large,
complex but well-known functional or system-level tests than doubling the codebase with unit tests. And by the way, when I
write unit tests for my code, it happens to be 3-4 times bigger than the code I am testing so what's the point?
   
## "We have too much legacy code, it would be a drop in the ocean"

Let's concentrate on shipping new features and fixing bugs touching existing code in the least invasively possible way, given we
do not know if touching anything won't break our software in possibly horrible ways
  
## "I do not want to be told how I have to code"

I have enough experience to find my way in the code so I can change it quickly enough without the need of TDD. Besides I have my
own set of techniques and tools that I have polished over the years so why bother changing?

## "Our test suite takes too long to run and is broken half of the time"

Given the aforementioned reasons, we do not have the time to fix it properly so let's move on and find other ways to deal with
bugs. 
   
## "Our code does not lend itself to TDD because we do much (GUI/Embedded/Mobile/Legacy Pick one) code"

We are special here so we cannot do something that's been invented for startupers or simple web apps. 

## "All these unit tests do not guarantee functional relevance"

So why bother writing them given we could invest the time in functional-level tests? 

## "What's the point of writing tests for code that will never change? They won't catch any regression"

OK, it might be changed but surely not in the near future so given I know what I am doing, I would rather get rid of those tests 
