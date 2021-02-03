------------
title: Haskell + Emacs + Nix
subtitle: Making of sense of Nix for Haskell development
author: Arnaud Bailly
date: 2021-02-02
------------


In a [previous post](a-modern-haskell-env.html) I detailed how to set up a LSP server for Haskell using Emacs as client. Now, despite my good friend Sam Halliday's [advice](https://levelup.gitconnected.com/delivering-with-haskell-a347d8359597), I wanted to add Nix in the mix. Both [standardized and shared development environment](common-dev-environment.html) and reproducibility of such environments are relevant and important. But I could argue that the wealth of virtualization tools available nowadays, from the humble `chroot` to full-blown virtual machines through containers of all kind, makes it much easier to produce and reproduce identical environments than when nix was started 15 years ago.

There are [teams](https://www.tweag.io/blog/2020-08-20-how-nix-grew-a-marketing-team/) and people in the Haskell community that support and use nix, and I wanted to get my feet wet and taste the water, to see how it feels to develop using nix. This short essay reports on my experience so far trying to:

* Set up a development environment for Haskell code based on Nix, Emacs and LSP,
* Nixify a cabal-based Haskell project.

I wouldn't have succeeded in this endeavor but for the support of fine people from the  [Functional Programming Slack](https://fpchat-invite.herokuapp.com/) community, the [FP Zulip](https://funprog.srid.ca/) community, and online documentation:

* IOHK's [Haskell & Nix Tutorial](https://input-output-hk.github.io/haskell.nix) which covers the [haskell.nix](https://github.com/input-output-hk/haskell.nix/) infrastructure,
* nixpkgs [Haskell infrastructure](https://haskell4nix.readthedocs.io/) guides, although they are somewhat conflicting with the former,
* [Nix Pills](https://nixos.org/guides/nix-pills) which are invaluable to better understand how nix is working.

Please note the source code for provisioning a virtual machine with such an environment is available on [GitHub](https://github.com/abailly/nix-haskell-dev-vm).

# Configure Emacs and LSP

The tricky bits for me was to configure Emacs in such a way that when it opens a `*.hs` file it automatically fires up `lsp-mode` and connects to the _right version_ of the  [Haskell Language Server](https://github.com/haskell/haskell-language-server). As explained on HLS's GitHub page, the LSP client must connect to a LSP server that's compiled with the correct GHC version and uses the correct dependencies. In Emacs' `lsp-mode` this is normally done through the use of the binary program `haskell-language-server-wrapper` which will itself spawn the correct version of `haskell-language-server` binary depending on the project's configuration which can be given by a `stack.yaml` file or a `xxx.cabal` file.

The Nix way of providing such a configuration is to set the dependencies in a context-specific way, using a `default.nix` which will be picked up by all nix tools when we don't provide them a specific file containing a nix expression to evaluate. Then a `shell.nix` file references the `default.nix` as its sources for packages and gives the user a customized shell updated with whatever packages it exposes. Note that in this case, there is no need to provide a wrapper over `haskell-language-server` because, by virtue of Nix providing a customised environment through a fixed set of packages, the "correct" HLS version will be installed, as explained in this [PR Comment](https://github.com/input-output-hk/haskell.nix/pull/1015#issuecomment-768160999). There is a section on [configuring Emacs](https://input-output-hk.github.io/haskell.nix/tutorials/development/#emacs-ide-support) in haskell.nix doc but it applies to Dante and not LSP.

So we need to configure Emacs to:

1. Use `haskell-language-server` as the name of the executable for Haskell LSP server,
2. And more importantly, use the environment provided by `nix-shell`.

The latter could be achieved by wrapping the HLS invocation in `nix-shell` but [direnv](https://direnv.net/) seems to be the way to go as it provides a declarative way of setting up Nix on a per-directory basis. In my case, it amounts to:

* Write a `.envrc` file containing a single line, `use nix`, at the top level of the project's directory,
* Configure Emacs to use `direnv-mode`:
  ```
  (use-package direnv
    :ensure t
    :config
    (direnv-mode))
  ```

When emacs now visits a file located in the project's directory or one of its sub-directories, `direnv-mode` will kick in and set the current environment, and most notably the `exec-path` according to the instructions given in `.envrc` which here means executing `nix-shell`. However, this did not work out-of-the-box and took me some time to understand why. The LSP client that `lsp-mode` runs kept saying it could not find an LSP server implementation for my language, even though I could assert that:

1. `direnv` was working and ran nix-shell to setup the environment,
2. `haskell-language-server` was installed in the shell and available from the ambient `PATH`.

It turned out the problem seemed to be caused by a _race condition_ between the LSP client and the `direnv` setup: The LSP client tries to connect to the server before the environment is properly setup which happens because entering `nix-shell` takes a few seconds. _Deferring_ the connection attempt until the point where the file is properly loaded fixed this issue, leading to this LSP configuration:

```
(use-package lsp-mode
  :ensure t
  :hook ((haskell-mode . lsp-deferred))
  :commands (lsp lsp-deferred))

(use-package lsp-haskell
  :ensure t)
```

# Speed-up Nix

So I have a nice and working Nix/Haskell/Emacs/LSP configuration setup for my project, but there's a major issue: `haskell.nix` does not provide a cache of the packages it exposes derivation for, which means everything must be rebuilt from scratch every time I destroy and recreate the VM. And as the `default.nix` configuration retrieves its packages from the `master`, every time we enter `nix-shell` we run the risk of having to update some depedencies which might take ages.

[Pinning down](https://input-output-hk.github.io/haskell.nix/tutorials/getting-started/#pinning-the-haskellnix-version) the version of the packages used remediates the second problem, so we are left with the question of caching the binaries built by Nix in such a way as to be able to share them across different VMs. Enters [cachix](https://cachix.org/) which is a hosted service with a free 500MB tier specifically built to cache Nix derivations' output. After having created an account and a cache instance called `hydra-sim`, I installed and configured `cachix` on the development environment, and then could push/pull binaries produced.

Local configuration requires the following steps:

Install cachix which is most easily done through nix:

```
nix-env -iA cachix -f https://cachix.org/api/v1/install
```

Assuming nix is installed globally and runs as a daemon, the user running `cachix` must be authorized to create and manipulate caches. This is defined in `/etc/nix/nix/.conf` which looks like:

```
max-jobs = 6
cores = 0
trusted-users = root curry
substituters = https://cache.nixos.org https://hydra.iohk.io https://iohk.cachix.org
trusted-public-keys = iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
```

Retrieve an authentication token for the `hydra-sim` cache from cachix and configure local environment to use it:

```
cachix authtoken <the token>
```

 Finally, use the cache

```
cachix use hydra-sim
```

Pushing to the cache can be done from the output of the project's build
```
$ nix-build -A hydra-sim | cachix push hydra-sim
```
and using the [nix-shell configuration](https://fzakaria.com/2020/08/11/caching-your-nix-shell.html). This is important as it means the tools, and most notably `haskell-language-server`, will be part of the cache:
```
nix-store --query --references $(nix-instantiate shell.nix) | \
     xargs nix-store --realise | \
     xargs nix-store --query --requisites | \
     cachix push hydra-sim
```

# Conclusion

This is the beginning of my journey in Nix-land and it's a bit early to say whether I like the tool or not. Right now, it seems like a bit of time-waste as I have spent several hours scattered over a week to get my environment "right" using Nix on a dedicated VM, where doing this using the standard tools provided by Haskell to install packages and utilities, namely [ghcup](https://www.haskell.org/ghcup/) and [cabal](https://cabal.readthedocs.io/en/3.4/index.html), took me approximately twenty minutes.

As is often the case with non-mainstream open source tools, there is a lot of information "out there" written by enthusiastic people like tutorials, guides, and blog entries. This information is often fragmentary, dependent on a specific environment, operating system, component of the stack, or specific flavor of the tools. Hence one has to invest time to recombine those fragments in a way that suits his or her needs and taste. This implies investing time in understanding _how_ those tools work in order to be able to tweak configuration and parameters, which might gives one that [yak shaving](https://www.urbandictionary.com/define.php?term=yak%20shaving) feeling.

Yet when I compare that experience with my past year working mostly with proprietary or semi-proprietary language and tools (C#, Windows, Visual Studio, Citrix, [Powerbuilder](https://www.appeon.com/products/powerbuilder)), I wouldn't want to go back at any price. When something is wrong in proprietary land, you don't even get a chance to understand _what_ is wrong, you are dependent on the whims of a software publisher. The time that's gained in shrinkwrapped tooling and environments, pre-packaged components, guided processes, is valuable only in the short-term at the onset of a project. As soon as _essential complexity_ of the business domain creeps in, the abstraction barriers the proprietary tooling carefully built to hide implementation details breaks, leaving the hapless developer struggling with patches, workarounds, opaque procedures to get things done.
