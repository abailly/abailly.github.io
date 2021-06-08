------------
title: CodeMesh 2017 - Full Notes
author: Arnaud Bailly
date: 2017-11-10
------------

Here is a summary and some quick takeaways from this year's [CodeMesh](http://www.codemesh.io/codemesh2017) conference. I must say I never had such an intense conference, with lot of fascinating and mind-blowing sessions which brood lots of new or old ideas in my mind. Thanks a lot to the organisers and the amazing speakers which made this event memorable.

# Day 1 - Wednesday

## [Margo Seltzer's Keynote](http://www.codemesh.io/codemesh2017/margo-seltzer)

Margo Seltzer's work starts from an observation: Humans get better with practice ; which leads to a question: "Could computers get faster with practice?" This question leads to a research program called Automatically Scalable Architecture which tries to extract information from repeated runs of the same program (fragment) to speed it up. The basic idea goes along the following lines, with lots of details omitted:

* Consider the program's code as a state machine whose language is the instruction set of the assembly language and whose states are bit-vectors

-   could computer gets faster with practice ?
    -   programs don't learn while they run&#x2026;
    -   online training
    -   offline training  -> more improvements
-   state space of 1 proc = register + memory
    -   state transition per each insrtuction
    -   run == path in state space
    -   how to parallelize ?
    -   predict future location in state space and run parallel
        experiments -> fast forward when hitting a location reached by
        one // experiement
    -   half of theoretical max
-   automatically scalable arch (ASC)
    -   speculative execution entered in a cache
    -   applied to parts of the programs, big chunks of side-effect free
        parts of hte program
-   ScaleyASC: runs on VM, interpret all instructions, very slow but
    large scalability
    -   predictors are stupid
    -   speculators are copies of the software on other VMs
    -   trajectory cache
    -   transform state space into a bit vector -> execute instr -> get a
        new bit vector
    -   recognizer : map Instr Pointer to state
    -   predictors: very simple, use weatherman/mean/log
        regression/linear regression on 32 bit features
        -   weighted average of all predictors to get next bit
        -   update weights according to accuracy of predictions
            -   -> back prop ? gradient descent
    -   speculators:
        -   use read/write masks and actual read/w values as cache entry
        -   checks only part of the state for cache hit/miss
-   actual hardware
    -   predictors: decision trees
    -   speculators run on same hardware
    -   Pin: dynamic instrumentation tool -> pinned program, instrument
        some instructions. Overhead depends on how many instructions are
        instrumented
    -   offline pin jumps -> count frequency -> select those above some
        threshold
    -   ptrace tracks normal program, speculators run pinned -> snapshot
        of state when reaching selected branch instructions
    -   dependency tracking between dependent part of the states
        -   pintool -> instrument load/stores
    -   trajectory cache:
        -   keys are parts of  bit vectors, different in each cache entry
        -   hash content of the live registers as keys
    -   allocator
        -   speculators are running slower than main program -> cannot
            achieve max speedup
        -   overhead when dispatching speculations
        -   matrix multiply : much less speedup, lot of load/store
            instucctions to instrument
        -   transaction memory could bring HW support to R/W sets ->
            achieve speedups
-   Qs:
    -   PIN has a load of overhead -> improvement in hardware
    -   what is the class of programs we can speedup? depends on loss
        function of state space trnasformation
    -   what is the best learner?

-   using this in compiler

### Flying Spaghetti Monster<a id="sec-0-1-2" name="sec-0-1-2"></a>

-   FSM cult
-   evolution is a fact of life for us&#x2026; => no intelligent design
-   Idris -> philosophy of development
-   microservices
    -   do things in small pieces: small pieces of data, change code a
        little bit at a time, understand systems a bit at a time
    -   definition fuzzy
    -   trading freedom for strong consistency
    -   greenspun's tenth rules of programming => incrementally evolving
        a system with breaking abstractions leads to pain
    -   ford's tenth rule: any sufficiently complicated mservice arch
        contains a fucked up implementation of a distributed protocol
    -   breakthrough abstractions
    -   narrative is not enough -> going through happy path is not enough
        -> needs good observability
-   finite-state machines:
-   indexed monads -> dominads ~~ like dominos :) when you place
    dominos you must match the last one
-   quote: "types  can't help once network is involved"
    -   fallability : dealing with failure of components IRL
    -   distribution: communicating across heterogeneous networks, how to
        actually distirbteu protoocol?
    -   reciprocity
-   fallability
    -   each failure is different, each system can fail in different ways
    -   non deterministic state machines -> door might jam when opening
        it, multiple output for a given state/label => type of output is
        a function of actual result
    -   => enforces error checking
    -   type providers -> borrow from F#, hook into compiler and generate
        types from external sources
    -   needs to allow IO at compile time in Idris -> helps distributing
        protocols, use a single authoritative model/schema
-   session types
    -   multiparty interaction as a type
    -   project a global type into a local representation for each
        participant
    -   verify global proto from local properties (??)
    -   ex. SessionState, needs to model branch (deterministic or non det
        -> chosen by other party)
    -   dual : Session -> Session
    -   write type once, use it on both sides of a conversation
    -   Communicating state machines
-   Conclusion:
    -   types **can** help us understand how the network works
    -   <https://github.com/ctford/flying-spaghetti-monster>

### FSM<a id="sec-0-1-3" name="sec-0-1-3"></a>

-   Oskar Wickström
-   journey: valley of programmer death -> ADTs -> MTL -> type classes
    -> Indexed monads
-   State
    -   implicit state -> program does not explicitly define set of legal
        states, scattered across mutable vars
    -   -> explicit -> FSMs
    -   State x Event -> Action x State
-   State as Data types
    -   ex: shopping cart checkout flow
    -   transitions coupled with IO
    -   legal transitions not enforced
-   MTL (~~ extensible-effects)
    -   typeclass encodes state transition
    -   events are typeclass methods
    -   state type is abstract
    -   instance performs side-effects
    -   states are empty Data types, use associated type in the typeclass
        defining state machine
    -   select event is more complex, several input/output states -> use
        a dedicated type rather than encoding in either
    -   instance type : overload abstract type names as constructor names
    -   CheckoutT transformer -> implement instance usign concrete State
    -   pattern matching in methods is limited to input type thansk to
        abstraction barrier
    -   example with timeouts
    -   encode valid state transitions
    -   pbs: not necessarily safe, side-effects can be performed
        illegally, does not enforce transition to final state, state
        values can be reused&#x2026; -> double spending
    -   move state value inside the monad ->
-   indexed monads
    -   <https://gist.github.com/abailly/02dcc04b23d4c607f33dca20021bcd2f>
    -   Purescript has row kinds, e.g. type-level record -> indices for
        record types and effects
-   Idris
    -   Control.ST library -> "named" resources
    -   <http://docs.idris-lang.org/en/latest/st/>

Entered on <span class="timestamp-wrapper"><span class="timestamp">[2017-11-08 Wed 09:24]</span></span>

[<file:///Users/arnaud/projects/aleryo/homomorphic-event-sourcing/sources/server/src/server.hs> :: String -> Application](file:///Users/arnaud/projects/aleryo/homomorphic-event-sourcing/sources/server/src/server.hs)

## Code Mesh - Wed afternoon<a id="sec-0-2" name="sec-0-2"></a>

### Infinite LAmbda Calculues<a id="sec-0-2-1" name="sec-0-2-1"></a>

-   a conversation on stage with the interpreter&#x2026;
-   ill-typed programs considered useful
-   basic lambda rules
-   church numerals
-   proper programming -> little schemer
-   extend basic language with syntactic sugar
-   recursion? -> Kleene and rosser proved inconsistent -> get infinite loops
-   types were added to avoid infinite loops => find an expression that
    type checker rejects => gives us a good candidate for what we are
    looking&#x2026;
-   (\\ x -> x) (\\ x -> x) &#x2026;->  (\\ x -> x) (\\ x -> x)
-   :install real-world
-   to land a job, implement FizzBuzz with pure lambda calculus
-   <http://llama-the-ultimate.org/lambdas.html>

### The Making of an IO<a id="sec-0-2-2" name="sec-0-2-2"></a>

-   "IO Monad is boilerplate Haskell to appease Phil Wadler" -> it's
    really about Effects
-   Effect = anything you can't "just" do twice (or you can't undo)
    -   hard to test
    -   concurrency is hard
    -   foo(42) + foo(42) -/-> val x = foo(42); x + x
    -   no referential transparency
    -   should be controlled &#x2013;> IO Monad
-   IO Monad = build a description of the program to be run ->
    description can be changed before it is evaluated -> code as data
    -   separate composition from declaration &#x2013;> Concurrency, dynamic
        scaling (backpressure, dynamic allocation&#x2026;)
    -   sequential composition is guaranteed by parametric polymorphism
        of bind (flatMap in Scala)
-   trait Monad in scala
-   Haskell
    -   all roads lead to IO
    -   all FFI is encapsulated in IO
    -   only way to construct an IO is through return
    -   concurrency managed throuhg IO monad
-   Scala
    -   can run any effect at any time
    -   native threads, explicit async IO, cannot block other things ->
        in Haskell we have green threads, we can block it, very inexpensive
    -   no tail-call elimination
-   IO monad in scala
    -   stack safety
    -   strict/lazy/asynchronous evaluation modes
    -   FUture -> memory leaks, memoizing, doesn't encapsulate async
        execution -> leaky
    -   Scalaz 7 IO -> encourages thread blocking whcih does not work on JVM
    -   Scalaz Task -> name too long (!!)
        -   now/delay/async/fail
        -   quite ancient implementation, very slow, baroque actor-based code
        -   still no abstraction
-   Thread BP
    -   Thread best practices from @djspiewak : Computation =
        work-stealing non daemon, cpus bounded ; blocking IO -> unbounded
        and cacheing ; event dispatchers -> very small, high priority
        daemon
    -   Task -> no resource management -> one can run tasks in parallel
        without requiring proper resource cleanup
-   fs2 and monix
-   cats-effect -> simpler, no actors, no concurrency, abstract
    typeclasses and laws
    -   shift allows moving effect among threads -> relocates
        computations after sequencing
    -   the only thread related function!
    -   ASycn request callbacks
-   parametricity not enough to contain beahviour of typeclass
    -   define set of laws as actual implementations, kind of TCKs ->
        users can rely on implementation that have passed the tck

### Collapsing Towers of Interpreters<a id="sec-0-2-3" name="sec-0-2-3"></a>

-   Nada Amin
-   challenge: collapse a tower of interpreters into a single pass
    compiler
    -   L0 <- I1 <- L1 <-&#x2026;. <- Ln
    -   partial collapsing if needed (e.g. ship optimized code to the browser)
    -   ex. Regexp -> Regex Matcher -> Evaluator -> &#x2026; -> Low Level
    -   Modified Evaluator -> &#x2026; -> Low Level
    -   consider reflective and infinite tower of languages
        -   can inspect and modify program at runtime at anyt level
        -   HL -> LL == reify, going meta
        -   LL -> HL == reflect, going to object
        -   L0 == most high level language, conceptually infinite number of
            interpreters => no bound on n
        -   can go up and down dynamically, consider finite exeuction
            because of default semantics
    -   Black language: <https://github.com/readevalprintlove/black>
        -   changes the way I evaluate variables at the Meta-level
        -   EM == execute at metalevel (with same syntax)
        -   clambda == compiled function
        -   collapsing level at compilation
-   solving the challenge:
    -   partial evaluation (Futumara projection)
    -   specialization step for a given program
    -   sdtaged intepreter -> compiler
    -   multilevel language: annotates lambda terms with the level n
    -   metaML: quote/splice/unquote/run quote
    -   in Scala: use types Rep[T] to distinguish stage (.. TH?)
-   tower: stage each interpreter -> composition of compilers&#x2026;
    -   pb with one-pass compiler : prevents reflection ?
-   stage polymorphism
    -   base language is multilevel lambda calc
    -   staged interpreter is the last stage
    -   pink = stage-polymorphic multilevel lamdba calculus
    -   purple = polytipic progra via type classes
    -   controlling collapse: compilation unite or explicit lifting
-   multi level lambda calc:
    -   Lift operator
    -   Let insertion &#x2013;> control order of operations
    -   stage polymorphism
    -   &#x2014;> online partial evaluation
    -   result of definitional evaluator can be a constant or some code
        -> pass down expression to later stage reified
    -   Lift(Clo(..)) &#x2013;> reflectc(Lam(..))
-   Stage polymorphic metacircular evaluator
    -   environment can contain reified/reflected values
    -   maybe-lift: function to chose to lift or not
-   potential applications:
    -   towers in the wild w/ existing levels of interpreters Python ->
        MAchine code
    -   modified semantics
    -   non standard interpretations -> program analysis, verification,
        interpreter for one paradigm on top of another, abstracting
        abstract machines
    -   collapsing not only for performance but also for
    -   <https://github.com/TiarkRompf/collapsing-towers/tree/master/popl18>

### Blockchains<a id="sec-0-2-4" name="sec-0-2-4"></a>

-   blockweaves: a blockchain that shards
-   proof of access
    -   to mine the next block you need to have the last copy
-   distributed storage: distribute blocks among nodes in the network
-   building an internet archive: putting history on a blokckweave to
    prevent tampering
    -   extension to the browser, walking through the history of a page
-   apps on archain
    -   no application on the blockchain people want to use
    -   different model: use archain to store the data

entered on <span class="timestamp-wrapper"><span class="timestamp">[2017-11-08 Wed 13:38]</span></span>

<file:///Users/arnaud/projects/aleryo/homomorphic-event-sourcing/sources/server/src/server.hs>

# 2017-11-09 Thursday<a id="sec-1" name="sec-1"></a>

## [More: Systems Programming with Racket](https://download.racket-lang.org/releases/6.11/doc/more/index.html)<a id="sec-1-1" name="sec-1-1"></a>

Captured On: <span class="timestamp-wrapper"><span class="timestamp">[2017-11-09 Thu 08:09]</span></span>

## CodeMesh - Thursday Morning<a id="sec-1-2" name="sec-1-2"></a>

### David Turner - History of PL<a id="sec-1-2-1" name="sec-1-2-1"></a>

-   1936: lambda-calculus
-   polymorphic typed high-order pure lazy functionning PL

-   theory of pure functions, constructive representation λ-calculus
    -   church-rosser theorem: uinque convergence of reductions to normal
        forms if they exist
    -   reducing through leftmost redex leads to normal form => does not
        matter which one (innermost/outermost) you chose
    -   Böhm's theorem: if 2 terms have distinct normal forms, you can
        define a predicate to separate them => lam is THE theory of
        normalising terms and pure functions

-   lazy evaluation: 2nd church-rosser theorem -> to find normal form,
    we meust substitute in unreduced form
    -   efficiency advantage of call by value
    -   normal order recuction inefficiency can be overcome by reducing
        graphs (e.g. pointerS)
    -   compilation of λ-calculus to SK combinators
    -   extract programs specific combinators from source -> λ-lifting
    -   SPJ -> Spineless TAgless G-machine -> GHC
    -   efficiency gain for lazy evaluation
-   LISP 1960 McCarthy
    -   S-expressions
    -   Garbage collection -> not to burden the progframmer with
        mgmt of memory -> most important thing in lisp
    -   M-language: first order FP language
    -   pure LISP does not exist
    -   LISP was not baed on λ-calculus -> Kleene's work on first-order recursrion
    -   in M-language you can pass functions as their text
        reprensentation -> dynamic scoping of free variables
-   static binding/invention of closures
    -   Algol 60 : precise tehcnical writing
    -   could pass procs as parameters -> maintains stack discipline
    -   default parameter rule : call by name -> handle α-conversion
        explcitly in manual
    -   dynamic (link to caller) vs. static (definitino of variable)
        chain of stack frames
    -   Landin: returning procs as values -> closures in the heap
    -   The next 700 programming languages: <https://archive.alvb.in/msc/11_infomtpt/papers/the-next-700_Landin_dk.pdf>
    -   sugar: let, where, and, rec
        -   offside rule, indentation to structure code
    -   assignment
    -   J operator: capture continuation -> any kind of weird control structure
    -   ISWIM = λ-calc + assignment + control
    -   first appearance of ADTs
    -   ISWIM -> PAL (68) GEDANKEN (70)
-   PAL
    -   applicative layer == sugared λ-calc, shallow pattern matching
    -   imperative layer = mutable vairables, labels and jumps ->
        everything has to leave in the heap
    -   data types
    -   fringe problem: 2 different trees have same leaves -> implement
        with coroutines
-   St Andrews Static Language
    -   simple denotational language, applicative subset of PAL ->
        implementing in LISP over a week-end
    -   multilevel pattern matching, string as a list of chars
    -   using for teaching FP -> no imperative features. correct scoping
        rules, multilevel pattern matching
    -   runtime typing  -> needed by computation over symbolic data
    -   evolution: drop rec to be recursive by default, multiline
        equational pattern matching to define functions, lazy evaluation
    -   laziness:
        -   better for equational reasoning
        -   allows to write programs using infinite lists -> interactive IOs
        -   makes exotic control structures unnecessarily -> fringe
            problem's lists evaluation is done has needed
        -   replacing failures by a list of successes -> Wadler 85
    -   SASL had 27 implememntations!
-   69: formal definition for recursive types definition -> declaring ADTs
    -   NPL had set expressions
    -   HOPE: higher order purely functional polymorphic
    -   ML -> meta-language for LCF, sugared λ let letrec, had types,
        polymorphism, static typing
    -   Standard ML = HOPE + ML, not pure
-   KRC: SASL w/o where (programming w/o local variables), drop
    conditional in favor ofguards -> line oriented with line editor
-   Miranda: guards + where, lexical distinction between constructors
    and functions: <http://miranda.org.uk/>
-   Haskell:
    -   guards are switched to left-hand side
    -   typeclasses, monadic IOs, a module system
    -   much richer syntax
    -   case for typeless languages&#x2026; -> more complex to write some
        stuff

### Would alien understand λ-calculus?<a id="sec-1-2-2" name="sec-1-2-2"></a>

-   Tomas Petricek
    -   how do we do what we do and why?
    -   are pure functions invented or discovered?
        -   I wonder if there is a paper about that?
        -   there is a whole discipline about that! -> philosophy of mathematics
-   crash course in phil of math
    -   platonism: math objects exist independently of our thoughts ->
        there will still be number 5 if universe explodes
    -   problem: great story but very intimidating and helps maintaining
        an elite (Lakoff & Nunez) => looks like a perfect universal
        truth, elitist
    -   Lakatos: maths does not grow with more theorems, but by method of
        proofs and refutations -> community, social process, exemple of polyhedra
    -   counterexamples causes refinement -> "monstrosity" of some constructs
    -   culture dependent: (Lakoff) mathematical ideas have some cultural
        aspects in it
    -   western culture <- ancient greeks
        -   notiion of essence
        -   human reason is  a form of logic
        -   foundations for a subject matter
    -   "Where maths come from" -> embodied mathematics -> we only know
        brain and mind maths (Kant's phenomenon)
        -   need to apply cognitive sciences to better understand maths
-   Cognitive science of maths
    -   metaphors are central to thought (Lakoff) -> abstract concepts
        are understaood via metaphors in terms of more concrete concepts
    -   ex. abstract cat th concepts need to be understood through
        concrete things
    -   math ideas are acually mathematicizing ordinary ideas =>
        derivatives is math equivalent of instantaneous change
        -   some basic math concepts in babies -> innate arith
        -   conceptual metapohores
        -   layering metaphors
    -   experiments w/ babies
    -   arithmetics is object collection: object collection -> arithmetic
        -   building arith concepts on top of objects collections manipulation
        -   linguistic examples: use "math" words for common things
        -   limits of metaphor: 0 ? -> need other metaphor to understand 0
-   "type theory and λ-calculus are eternal"
    -   "λ-calculus is discovered, Angular is invented"
    -   λ-calculus linked to proofs and categories -> deep truth in the
        universe !!??
    -   1 answer: category mistakes -> can't relate physical stuff
        (programs) to abstract stuff (proofs)
        -   Fetzer 1988 : program verification is non-sense
    -   sociologist's answer: analogy between all three is carefully
        constructed through process of proofs and refutations -> human
        work made them fit
        -   all 3 are the product of the work of similar people in the same
            network -> community effect
    -   cognitive scientist argument: all 3 are derived from the same
        embodied experience -> what would it be for λ-calculus? -> what
        aliens would need to understand it?
-   aliens
    -   "any intelligent species is bound to have logic" -> computer
        scientist answe w/o evidence
    -   "they'd also run into program-proof duality"
    -   cognitive science and λ-calculus/logic:
        -   embodied experiement of membership in set through Container
            schema -> construct math because this is how the way we
            perceive the world
        -   transitivity of containment &#x2013;> modus ponens &#x2013;> function application
    -   looking at the language we use:
        -   β-\*\*reduction\*\* -> requires a sense of direction
    -   Arrival movie: aliens w/ circular languages affecting perception
        of time -> no notion of directiveness &#x2013;> only reversible
        computations ?
    -   Solaris: planet itself is sentient -> would there be more numbers
        than one?
    -   interstellar dust cloud aliens: there is no boundaries in chaos,
        no inside/outside, no container schema metaphors

### Haxl - Simon Marlow<a id="sec-1-2-3" name="sec-1-2-3"></a>

-   IO: slow, hard to test, hard to debug
    -   all 3 problems are related -> concurrency
    -   every language can do this
    -   what's wrong?
        -   got to remember explicitly
        -   might wait too  early -> limit amount of concurrency
        -   have to fix awaits when refactoring
        -   concurrency clutters the code -> makes refactoring harder
            becuase of extra structure
    -   what bout side effects?
        -   there are no side effects in gather data + make decisions part
            of the process
        -   technically, reading DB is a side-effect but usually you dno't
            care -> ignore the pb. cache things
-   when there are no side-effects, concurrency is a better default
-   how to test?
    -   reserve a small part of the world for testing only?
    -   fake the world -> Mock, hard to write manually, replay/record is
        hard, different for each kind of IO
    -   I want record/replay mock without effort
-   what about debugging?
    -   logging &#x2026; meh:
        -   i have to remembe to do it
        -   might not log enough stuff
        -   feedback loop too long
        -   clutters the code
    -   let me reproduce exactly what happened to diagnose it
-   big hammers = a technology that solves one or more problems  for good
    -   non-trivial to adopt
    -   examples:
        -   distributed source control
        -   garbage collection
        -   language independent RPC -> write code in heterogeneous
            environments, sharing common data types
        -   Haxl!
-   Haxl
    -   ApplicativeDo
-   example: Update script
-   exploiting parallelism from dependency graph in computation
-   how does it work?
    -   data dependencie are first class -> compiler support
    -   we don't want to modify compiler
    -   do notation -> bind operator expanded -> every monad implemetns >>=
    -   pb: we have already lost, dependency explicitly baked in the bind
        operator => monads can only be sequential
    -   Applicative : <\*> combines things concurrently -> don't want to
        write it by hand
    -   ApplicativeDo: analyzes dependencies between statements and use
        applicative wherever it can, e.g. when there are no dependencies
        between computations
    -   user implements `fetch` only
    -   computations happen in rounds
    -   pb: what if one of the operation in a round take much longer than
        others? -> might be inherent in the way we write code
    -   refine dependency graph to maximimze //ism
-   HAxl 2
    -   drop requirement to complete everything in a round
    -   IO can be arbitrarily overlapped
    -   chnage contract w/ provider -> passs a variable where provider
        can write the result to
    -   tradeoff: possibly less batching, things might happen in
        different times where they would have been batched before
    -   -> latency reductions in production
-   testing & debugging
    -   to use Haxl: write a data type to represent your IO, add some
        instances, implement fetch
    -   => turn IO into data
    -   HAxl stores all IOs in internal cache -> memoizing IO computations
    -   improves modularity: same fetch in different parts of the program
        are referentially transparent
    -   caching => running again same program yields same behaviour
    -   dunmping cache in the form of a haskell program -> can load it again to populate cache
    -   => make a unit test
    -   can synthesize cache by prepopulating requests results
    -   debugging -> persist the cache on failure then reload for analysis
-   Why Haxl =>
    -   fighting spam
    -   Sigma rule engine for abuse detection/remediation
    -   serving > 1M req/sec
    -   hundreds of changes -> deployed immediately
    -   fraxl

## CodeMesh - Thursday Afternoon<a id="sec-1-3" name="sec-1-3"></a>

### Relational interpreters<a id="sec-1-3-1" name="sec-1-3-1"></a>

-   Will Byrd
-   miniKanren
    -   TRS-8000 -> first program "draw a square 2'' by 2''" -> Fail!
    -   Perlis, epigram in programming
        -   if someone wants to program saying what to do, give him a lollipop
        -   => lollipop driven development
    -   make inference rules executable
    -   miniKanren == executable computable science metanotation
    -   math aquarium on Amiga
-   environment passing interpreter in minikanren
-   relational evaluator -> evalo : expr -> value -> expr
    -   unification variables
-   generate scheme trees from targets
-   99 ways to say I love you
-   quine -> find an expr e s.t. (eval e == e
-   write the program in scheem -> get relationality from evaluator
-   <https://github.com/webyrd/Barliman> -> quickspec <https://hackage.haskell.org/package/quickspec>
-   generate scheme expression from examples
-   geenrate implementation from CS papers!
-   <http://minikanren.org>

### COde and architecture<a id="sec-1-3-2" name="sec-1-3-2"></a>

-   why do we need scripting?
    -   design = form generation and pattern exploration
    -   how to construct complex forms? => panelization, how to make the
        small pieces of a curved surface
    -   data extraction
    -   budget
-   visual programming tools

### Building Distr Systems<a id="sec-1-3-3" name="sec-1-3-3"></a>

-   PEter Van Roy
-   big systems are complicated -> making systems self managing -> use
    feedback loops
    -   combining feedback loops ?
    -   feedback structures
    -   bigger systems -> weekly interacting feedback structures
-   system == a set of components connected together to form a coherent
    whole -> Wiener's cybernetics
    -   dist syst -> nodes, connected by network single coherent
    -   autnomic computing -> one loop
-   feedback loops
    -   actuating agent <-> monitor agent
    -   low level concets
    -   combining them , connecting to specificaitnos
    -   metastable systems ?
-   example: hotel lobby
    -   a primitive tribesman in a hotel lobby : 2 feedback loops, one
        with air conditioner and one with tribesman making fire ->
        unstable situation
    -   how to debug this?
    -   management: one loops manages the other
    -   stigmergy: loops interact through the environment
    -   TCP uses same approach: adaptive change of size of packets
-   example: erlang's supervisor tree
    -   linked processes
    -   supervisor trees
-   obstacle avoiding robot
-   human respiratory system
    -   huge number of examples in biological systems
    -   4 feedback loops
    -   conscious control loop is very smart -> sandwiched between 2
        simpler ones -> very powerful but may go wacko -> needs
        protection mechanism
    -   state diagram
-   feedback structures = a combination of feedback loops to maintain a
    desirable property
-   scalable archiectur ein 4 steps
    -   concurrent components
    -   feedback loops
    -   feedback strucutre -> specification to maintain a property, loops
        manages each others
    -   weeakly interacting feedback structures -> each property is
        maintained by 1 FS, some interaection between each FS, dependency
        graph
-   Scalaris: HP self-managing K/V stores overlay network
    -   conjunction of 6 properties, use 5 feedback structures: connect,
        route, replica, tx mgr, load
    -   connect: detect nodes failing, do something
    -   route: adapt to nodes topology
    -   dependency DAG
    -   structured overlay network
        -   neighbours connectivity feedback loop
        -   fingers provide efficient routing
-   smart components are essential
    -   smart components solve a problem in a specific part of system
    -   outside of their opearting space they should be disabled
    -   power is built in, not added
    -   power of a system depends on the strength of its smart components
    -   intelligence is combination of many smart components
    -   "deep learning is a tsunami, you have to have it in your system"
