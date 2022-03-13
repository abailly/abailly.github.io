---
title: Modern Emacs for Haskell
subtitle: Setting up a more modern development environment with LSP
author: Arnaud Bailly
date: 2020-10-12
---

For a couple of years now there has been an upsurge of activity on the front of Haskell Tooling and IDE support. As explained in [this blog post](https://neilmitchell.blogspot.com/2020/01/one-haskell-ide-to-rule-them-all.html) from Neil Mitchell, there is a need, and now a will, to equip Haskell with a more  modern development environment based on the now standard [Language Server Protocol](https://microsoft.github.io/language-server-protocol/) introduced by Visual Studio Code. This initiative gave rise to the [Haskell Language Server](https://github.com/haskell/haskell-language-server) which aims at fully supporting LSP for the Haskell eco-system and becoming the standard "IDE".

I have used various solutions for hacking Haskell in Emacs which all are now either extinct or on the path towards extinction:

- [haskell-mode](http://haskell.github.io/haskell-mode/) is the oldest one and the one I of course started with,
- At some point I switched to [ghc-mod](https://github.com/DanielG/ghc-mod) which at the time, ca. 2013-2014 was offering the best experience. It was the first tool to provide basic things like autocompletion, jump to definition and other niceties we've come to appreciate from our IDEs,
- Then [intero](https://chrisdone.github.io/intero/) came out as [stack](https://docs.haskellstack.org/en/stable/README/) usage grew and I have used happily for a few years. It was a breeze to install in Emacs and integrated nicely with stack, reusing its configuration files to automatically provide all needed dependencies and enabling fast turnaround from a properly configured REPL. Sadly intero development stopped a couple years ago,
- I have also used [ghcid](https://github.com/ndmitchell/ghcid) which is pretty much editor or IDE-agnostic, it was (is?) great for providing ultra fast edit-compile-test cycle and it was the only tool that enable me to do [Test-Commit-Revert](https://medium.com/@kentbeck_7670/test-commit-revert-870bbd756864) in Haskell,
- I am not doing anymore Haskell development in my day job so I have switched back to using haskell-mode which at least worked and provided basic support. I never spent the time however to delve into its configuration details hence my environment was pretty simple and I mostly used syntax highlighting and REPL interaction.

This week-end, following the announcement in the Haskell Weekly News that HLS supported case-splitting and hole resolution, I decided to bite the bullet and reconfigure my Emacs to use it. Here are the steps I went through in the hope it will be useful for someone else.

First, I spent a lot of time fiddling with my `.emacs` and packages: I somehow lost a bunch of packages when I tried to install the relevant packages needed for LSP, possibly as a consequence of running `package-autoremove` without really understand what I was doing, so I add to fix my configuration one `require` at a time to ensure packages would be resolved if non existent.

I ended with the following configuration with is mostly copied verbatim from [ghcide](https://github.com/haskell/ghcide/#using-with-emacs)'s configuration, except for the variables related to process execution in the `lsp-haskell` package

```
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t))
(use-package yasnippet
  :ensure t)
(use-package lsp-mode
  :ensure t
  :hook (haskell-mode . lsp)
  :commands lsp)
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)
(use-package lsp-haskell
  :ensure t
  :config
 (setq lsp-haskell-server-path "haskell-language-server-wrapper")
 (setq lsp-haskell-server-args ())
   ;; Comment/uncomment this line to see interactions between lsp client/server.
  (setq lsp-log-io t))
```

I then proceeded to install [ghcide](https://github.com/haskell/ghcide) but it turned out to not be a good idea.

Then I installed [haskell-language-server](https://github.com/haskell/haskell-language-server). I opted for the "install from source" option as it seemed the simplest and safest to me. Cloning, building and isntalling it was pretty straightforward:

```
> git clone https://github.com/haskell/haskell-language-server
> cd haskell-language-server
> stack install.hs hls-8.8.4
```

As this is done through `stack`, it will install the relevant GHC version automatically in its own directory, and the binary for the server will be installed in `~/.local/bin` by default.

```
> ls -l ~/.local/bin/haskell-*
-rwxr-xr-x  3 arnaud  staff  124958940 Oct 11 11:20 /Users/arnaud/.local/bin/haskell-language-server
-rwxr-xr-x  3 arnaud  staff  124958940 Oct 11 11:20 /Users/arnaud/.local/bin/haskell-language-server-8.8
-rwxr-xr-x  3 arnaud  staff  124958940 Oct 11 11:20 /Users/arnaud/.local/bin/haskell-language-server-8.8.4
-rwxr-xr-x  1 arnaud  staff    3168996 Oct 11 11:20 /Users/arnaud/.local/bin/haskell-language-server-wrapper
```

As I understand it, the executable that needs to be pointed to is the `xxx-wrapper` which is responsible for starting the server and delegates the actual parsing and compiling to the correct backend:

![](/images/emacs-lsp-process-tree.png)


Emacs needs to be able to find the executable. In my case, I have added the directory top the `exec-path` variable of Emacs and to the `PATH` environment variable:

```
(setenv "PATH" (concat (getenv "HOME") "/.local/bin:" "/usr/local/bin:" (getenv "PATH")))

(setq exec-path
      (reverse
       (append
        (reverse exec-path)
        (list (concat (getenv "HOME") "/.local/bin")  "/usr/local/bin" ))))
```

It did not worked the first time I tried to open an Haskell source file and it was useful to be able to got to `*lsp-log*` and other log buffers generated by the various processes to try to understand why the LSP server was crashing at start. After some tests and poking around on the web, I realised I was missing **the critically important part**: Correct configuration for [hie-bios](https://github.com/mpickering/hie-bios) which is a low-level library that's responsible for providing the correct GHC configuration for a given project or set of files.

[hie-bios](https://github.com/mpickering/hie-bios) is configured through a `hie.yaml` file which it locates somewhere up the path from where it's started. If it does not find one, it uses a default configuration which is most probably incorrect for the project, unless it's something very simple like a single file. So I ended up writing a `hie.yaml` at the **root** of the project which is considered to be where the `.git` directory lives. I am using stack and have no plans to switch, but this implies GHC might be installed by stack and not in the PATH, so we need to tell the server to use a stack-based configuration

```
cradle:
  stack:
```

This is however not enough if you have more than one component and the `stack.yaml` file is not located in the current directory, so I needed to add relevant configuration to ensure it finds all the dependencies, both for the library _and_ for the tests. So here is how a configuration for a library with tests look like:

```
cradle:
  stack:
  - path: "./src"
    component: "hstore:lib"
  - path: "./test"
    component: "hstore:test:hstore-test"
```

It's a little bit annoying one has to maintain this file by hand as it could easily be generated from the `package.yaml` or `.cabal` files. An [issue](https://github.com/mpickering/hie-bios/issues/122) is open in hie-bios and shall be resolved for [cabal](https://github.com/haskell/cabal/pull/6241) soon.

So after a couple hours, I was finally able to open a Haskell file and enjoy the pleasure of being able to case-split, introduce lambdas and resolve holes automatically from within Emacs, thanks to the universal `s-l a a` keys (where `s-l` stand for `Super + l`, with `Super` being `Command` on a Mac).

This can give surprising results when, for example, give the following code:

~~~~~~~~~ {.haskell}
data Foo = Bar Int | Baz String

frobnicate :: Foo -> String
frobnicate = _
~~~~~~~~~

one uses `Attempt to fill hole` command:

~~~~~~~~~ {.haskell}
data Foo = Bar Int | Baz String

frobnicate :: Foo -> String
frobnicate = (\ f
   -> case f of
        (Bar i) -> []
        (Baz l_c)
          -> case l_c of
               [] -> l_c
               ((:) c l_c3)
                 -> (:)
                      (case l_c3 of
                         [] -> c
                         ((:) c4 l_c5) -> c4)
                      (case l_c3 of
                         [] -> l_c3
                         ((:) c4 l_c5) -> l_c5))

~~~~~~~~~

More interestingly, case-split is particularly useful to cover all constructors of a datatype. Given:

~~~~~~~~~ {.haskell}
fun :: Foo -> String
fun x = _
~~~~~~~~~

Typing `s-l a a` then selecting `Case...` when the cursor is on the hole yields:

~~~~~~~~~ {.haskell}
fun :: Foo -> String
fun x = (case x of
   (Bar i) -> _
   (Baz l_c) -> _)
~~~~~~~~~

Then I can `Replace with []` on the first hole and `Homomorphic case-split...` on the second hole, yielding:

~~~~~~~~~ {.haskell}
fun :: Foo -> String
fun x = (case x of
   (Bar i) -> []
   (Baz l_c) -> (case l_c of
      [] -> []
      ((:) c l_c3) -> (:) _ _))
~~~~~~~~~

I have yet to test this new setup on a significant project but I am already quite pleased by the speed of the provided feedback which matches ghcid and the integration in Emacs.
