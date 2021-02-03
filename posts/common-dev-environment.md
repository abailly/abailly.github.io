------------
title: "Team Practices: Shared Development Environment"
author: Arnaud Bailly
date: 2020-02-01
------------

This is another post in a series I plan to write on interesting team-wide practices I have found and still find to be effective to further and support eXtreme Programming. In this post, I present how to promote _Collective Code Ownership_ and _Coding Standards_ to a new level through the use of a _Shared Development Environment_. The goal is to ensure as much as possible standardization in the process and tools from design to deployment, in order to avoid the all too common “Works on my machine” syndrome that plagues software development teams.

# Standardizing Development Environment

The _intention_ behind _Collective code ownership_ and _Coding standards_ practices is to increase the _Standardization_ of the development process within a specific team (or group of teams working closely enough together), but _why_ should we want to standardize the development process, practices and tools in the first place?

After all, each developer has his or her own unique set of skills, backgrounds, habits, and idiosyncrasies that make him or her productive ; and each of us has honed his or her coding katas, carefully crafted keyboard shortcuts, personal information and note taking system, favorite editor, specialised configuration, preferred packages, OSes, laptop brands, keyboard layout, color themes and whatnot, not speaking of languages, idioms, patterns, or architecture preferences.

This is all fine and good in a context where the goal is to maximise each developer's productivity, and each developer's work has little to no impact on others'. This is most common in organisations where each developer is responsible for a  _component_, whatever that actually means, and works mostly in isolation on his or her part of the system, where the system's functionalities are designed and planned in advance, then broken down in various development tasks pertaining to various components, which are picked up by each responsible developer, then assembled, QCed, and released by other teams.

But this ideal situation never actually happens except in the extreme case of a _single developer software development_. Even the most siloed organisations feel the need to standardize _some part_ of the development process, a need that leads to increased formalism, bureaucracy, and organisational bloat in order to provide a sense of control on what's going on, which is what gives standardization, understood as top-down imposed norms and rules, a bad reputation among developers and, in return, leads to even more individualism and customization in order to alleviate the feeling one is but a cog in a gigantic machine.

XP proposes another route: Instead of stratifying the development process by tying together highly productive but siloed individualistic development practices with "scientific management", it aims to maximise productivity of the _whole process_ which means maximising productivity at the _team level_. Moreover, XP seeks to maintain or even increase that productivity as time passes and the software grows and evolves, the team changes, new versions of the technology or even new technologies emerge, new or changing requirements put pressure on the software.

Maximizing team productivity as a whole means, among many other things:

* Ensuring smooth and fast training and onboarding for newcomers, and transitions from senior to junior developers or from leaving to incoming team members,
* Maintaining an adequate level of understandability of the overall system, ie. not only the code itself but also all the supporting components that are needed to make the code releasable,
* Fostering [Psychological safety](https://hbr.org/2017/08/high-performing-teams-need-psychological-safety-heres-how-to-create-it) within team by making sure everyone can work with everyone else comfortably and fearlessly,
* Increasing the [bus factor](https://en.wikipedia.org/wiki/Bus_factor) to reduce the risks a project grinds to halt because a single member suddenly is missing,
* ...

What Kent Beck promoted more than 20 years is now backed by a lot of scientific studies, including the famous one Google conducted to determine [what makes a team effective](https://rework.withgoogle.com/guides/understanding-team-effectiveness/steps/introduction/).

In a sense _Standardization_ is just what happens when people actually collaborate effectively together on a continuous basis: They settle on common practices, idioms, languages, tools that they feel provide the more benefits to the team as _whole_. And conversely, standards that emerge from negotiations _within_ the team pave the route towards effectively collaborating together as a _whole_.

[Coding Standards](http://wiki.c2.com/?CodingStandard) and [Collective code ownership](http://wiki.c2.com/?CollectiveCodeOwnership) are XP's ways of promoting standardization of the development process across the team.

## Coding Standards

According to [Wadler's Law](https://wiki.haskell.org/Wadler's_Law), the hottest discussions among developers happen on the smallest and least significance matters of syntax. It is therefore "natural" to address the issue of _code formatting_  first. But of course, coding standards do not stop at syntax and the team should, step by step, increase the coverage of its standards to include more and more semantics.

### Formatting Code

The code’s formatting rules is one particular area where standardization, across a team or even a whole programming environment, can and _should_ be automatically enforced: . There have been, and will be, endless discussions among developers on such critically important topics as where to put curly braces, whether or not to use tabulations or spaces for indentation, how much space to put in various places of the code, etc. These discussions are a source of considerable bikeshedding, waste of time, and contribute exactly zero value to the end product; automating application of standard code format [before commit](https://prettier.io/docs/en/precommit.html) or, even better, before [saving files](https://www.gnu.org/software/emacs/manual/html_node/elisp/Standard-Hooks.html).

However, this is dependent on the particular setup of each development environment and various problems might arise:

* The rules might be encoded in different tools to support different IDEs, which necessarily will lead to differences cropping up,
* There might not be an overall agreed upon set of rules for the language which makes it hard for the team to settle the question,
* The language itself might be hardly amenable to [parsing](http://modernperlbooks.com/mt/2009/08/on-parsing-perl-5.html) and pretty-printing,
* ...

I thus view it as a great step forward that some formatting tools decide to provide no configuration options, like the [google-java-format](https://github.com/google/google-java-format) for Java, [Ormolu](https://hackage.haskell.org/package/ormolu), [elm-format](https://github.com/avh4/elm-format) for Elm, or [gofmt](https://golang.org/cmd/gofmt/) for Go. The next logical step would be for the compiler itself to enforce the formatting rules1 and only accept programs in _Normal Form_[^1]: This would guarantee a uniform layout across all code bases using this language and would have the added advantage of greatly simplifying parsing and more generally development of tools to manipulate the language.

### Beyond Code Formatting

In the past, these took the form of extensive [Coding style guides](http://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines) but as our tools mature, more and more standards can and are implemented as automated checks that are run as part of the software deployment pipeline. [SonarQube](https://www.sonarqube.org/) is one of the many tools that automates coding standards, best practices, and rules. One thing to note is that coding standards encompass a wide range of different activities and code artefacts, from lexical and syntactic rules to design principles, through naming conventions and prescriptions. There are probably things which are not amenable to automation but there’s nevertheless a relentless effort from practitioners and researchers alike to Automate All The Things, and verify things like [software architecture](https://innig.net/macker/guide/) or [security](https://lgtm.com/).

## Collective Code Ownership

Collective Code Ownership is that XP practice which strives to make all parts of the code collectively owned, meaning that every team member has the right and the duty to work on the whole software stack. There is no “preserve” that would be exclusive to some team members for reasons of skills, purpose, sensitivity, or seniority. A corollary to this strong statement is that every team member should feel at ease in every part of the code, and be able to understand what’s going on and make relevant changes and improvements in a way that any other team member can vouch for it.

Pair Programming or Mob Programming are XP practices which are conducive to building such a collectively shared understanding of the code, much more so than Pull Requests or Code Reviews[^2]. But they are only effective if all the people coding together can actually do so effectively and efficiently: If I pair program with an expert Vim golfer and I drive, then chances are I will be very slow and clumsy, at least initially; it will take time for me to get accustomed to my mate’s development environment, to use his or her keyboard shortcuts, commands, links, productively. And this is not specific to Vim of course, cognitive dissonance can come from every part of the environment: The keyboard (Dvorak/Colemak/Qwerty/Azerty/Bépo), the OS (Mac/Windows/flavors of Linux), the language settings, the window manager (tiling/non-tiling), the shell (Bash/fish/zsh), the IDE (VS/VS Code/IDEA/Eclipse), the color settings (dark vs. light colors, small vs. large screens) are all potential sources of pain for the newcomer.

Collective Code Ownership should be understood not only as an imperative to share understanding of the code but also to share understanding, knowledge, and skills of the tools to produce and work on that code. Everyone has his or her own idiosyncrasies, preferences, disgusts, personal history that accumulate over time and crystallize in specific tools and environments we feel comfortable with. This is of course totally fine. But this should not come in the way of building an inter-subjective experience of the code: if we all insist on viewing and manipulating the code through our own private lenses, chances are we will fail in developing any form of shared understanding, vision, or comprehensive knowledge of the code[^3].

# Building a Shared Environment

Working remotely is actually a great opportunity to overcome particularism and standardize the team's practices through a _Shared Development Environment_, that is some virtual machines (or containers) whose configuration is versioned alongside the code the team is working on. The idea hinges on [Infrastructure as Code](https://en.wikipedia.org/wiki/Infrastructure_as_code) principles: Manage the infrastructure supporting the process using the same tools and principles with which we manage the product, but one step beyond. Do not only manage the production infrastructure, but also the development infrastructure, so that the whole stack of tools used to produce the code is shared by everyone, versioned in the source code, deployable at will.

Here is a description of the environment we have built in my previous teams on a Linux system.

## Defining the image

The first step is to define the virtual machine image. This we do using Hashicorp’s [Packer](https://packer.io) which provides a way to build VM images deployable to most major clouds (at least GCP and AWS). Packer is configured with a JSON file that looks the following (I have shown only the configuration for building a GCP image):

```
{
    "builders": [
        {
            "type": "googlecompute",
            "account_file": "google-keys.json",
            "project_id": "my-project",
            "source_image": "ubuntu-1804-bionic-v20190212a",
            "ssh_username": "curry",
            "image_family": "build",
            "image_name": "build-{{timestamp}}",
            "zone": "eu-west1-b"
        }
    ],
    "provisioners": [{
    "type": "shell",
        "inline": [
            "sleep 30"
        ]
    },{
        "type": "shell",
        "script": "build-env.sh"
    }]
}
```

There are much more sophisticated options to configure the image than using a plain shell script but this is sufficient for our needs. I won’t go into the details of the script here but it is pretty straightforward: it installs all the tools we need for development, which in this case includes docker, gcloud tools, and a whole bunch of utilities like curl, git, tmux, graphviz, wget, jq, python3, bzip2, neovim, ag… We settled on using Emacs as our code editor so it was also part of the installed packages.

Running `packer build build.json` will build a new image with the given name ready to be deployed over the configured infrastructure providers.

## Defining Infrastructure

The most interesting part is the definition of the infrastructure itself, which uses Terraform. The set of .tf files is interpreted by the terraform tool, it checks the consistency of the defined infrastructure, and compares it with the latest known persistent state drawn from cloud storage, to decide what to do: create, delete or modify various components of the infrastructure.

There is a module defining the standard configuration of a development VM:
```
variable "name" {
  description = "The name of the machine."
}

variable "ssh_keys" {
  description = "The file containing ssh_keys."
}
```

The variable ssh_keys contains the name of a file which lists the public keys of people who can access the machine. This makes it possible to control who can access the development environment through a file committed to the version control repository.

```
output "address" {
  value = "${google_compute_address.dev-address.address}"
}

resource "google_compute_address" "dev-address" {
  name = "${var.name}-address"
}
```

This output variable will show the IP address of the VM once it’s deployed, so we need to allocate a public IP address for this machine.

```
resource "google_compute_instance" "dev" {
  name                      = "${var.name}"
  machine_type              = "n2-standard-8"
  zone                      = "eu-west1-c"
  allow_stopping_for_update = true

  tags = [ "dev"]

  boot_disk {
    auto_delete = true

    initialize_params {
      size  = 200
      image = "build/build-1546597455"
    }
  }

  network_interface {
    network = "default"

    access_config {
      nat_ip = "${google_compute_address.dev-address.address}"
    }
  }

  metadata {
    sshKeys = "${file(var.ssh_keys)}"
  }

  service_account {
    email  = "build@my-project.iam.gserviceaccount.com"
    scopes = ["compute-rw", "storage-rw", "https://www.googleapis.com/auth/cloudkms"]
  }
```

This sets the core features of our VM: Machine type, location, boot disk size… The sshKeys metadata reads its content from the file we defined earlier, and the service_account definition requests the creation of a new service account with the given authorizations.

```
provisioner "file" {
    source      = "dev/bash_aliases"
    destination = "/home/curry/.bash_aliases"

    connection {
      type = "ssh"
      user = "curry"
    }
  }
# ...
# more filea uploaded
# ...
  provisioner "file" {
    source      = "dev/configure.sh"
    destination = "/home/curry/configure.sh"

    connection {
      type = "ssh"
      user = "curry"
    }
  }
```

We then upload configuration files (aka. dotfiles) which are also versioned and stored in the source tree, including configuration for the shell, the tmux, emacs, and a configuration script.

```
 provisioner "remote-exec" {
    inline = [
      "chmod +x /home/curry/configure.sh",
      "/home/curry/configure.sh",
    ]

    connection {
      type = "ssh"
      user = "curry"
    }
  }
}
```

And finally we execute the configuration script that’s responsible for retrieving the source code, possibly cached build artifacts, and last minute configuration steps. There are a lot of possible variations on this baseline but the end goal is to ensure the team has at its disposal development environments which everyone can use and which are guaranteed to be consistently configured, thus making pair and mob programming sessions simple and easy to set up.

# Conclusion

In this post I have motivated why a shared development environment is desirable and useful, and demonstrated how easily it can be set up using basic IAAC tools. This practice can also be very useful in more complicated settings. I am working in a team which has to maintain a legacy piece of software built using a 4GL IDE called PowerBuilder, which is only available on Windows. One of the first things I did when joining the team was to create a couple of VMs on Azure cloud with several tools installed, including the PowerBuilder IDE, SQL Management Studio, Git, in order to make it straightforward to work collaboratively on the code. The experience is not quite as smooth as sharing a terminal through tmux or tmate, but it still only takes a few seconds to switch driver/navigator roles: Simply log into the VM and share screen. Not being an expert in Windows automation I have not yet gone beyond documenting the manual steps needed to configure the machine but this is only a matter of investing some time or finding the right person to help me.

Sharing the development environment is something that requires negotiation within the team, and that takes time to grow. Team members need to overcome their personal preferences and accept the discomfort of coding in an environment to which they might not be, at least initial, a hundred percent atuned to. The goal is not to impose The One True Way but on the contrary to share the knowledge and take the best that each setup has to offer. A versioned and pre-configured VM can perfectly host both Emacs and Vim, or can expose a VNC server that makes it easier to share graphical tools, or basically offer any kind of environment.


[^1]: I thought this was the case for Elm and Go but it seems they do not still fail to compile ill-formatted source code.

[^2]: As my friend Christophe Thibaut used to say: "Code reviews is Pair programming for people who don't fully trust each other"

[^3]: What is developer productivity is still something that needs to be defined. Does writing more lines of code than other developers make you more productive?
