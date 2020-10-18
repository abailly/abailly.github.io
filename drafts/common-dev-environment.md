------------
title: "Team Practices: Shared Development Environment"
author: Arnaud Bailly
date: 2020-10-15
------------

This is another post in a series I plan to write on interesting team-wide practices I have found and still find to be effective to further and support _eXtreme Programming_. In this post, I present how I try to promote _Collective Code Ownership_ and _Coding Standards_ to a new level through the use of a _Shared Development Environment_. The goal is to ensure as much standardization in the process and tools from design to deployment, in order to avoid the all too common _"Works on my machine"_ syndrom that plagues software development team.

# The need for standardization

## Coding Standards

The usual approach to _Coding Standards_ is to provide guidelines to the code writing part of the process. In the past, these took the form of extensive [Coding style guides](http://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines) but as our tools mature, more and more standards can and are implemented as automated checks that are run as part of the software deployment pipeline. [SonarQube](https://www.sonarqube.org/) is one of the many tools that encodes coding standards, best practices, and rules. One thing to note is that coding standards encompass a wide range of different activities and code artefacts, from lexical and syntactic rules to design principles, through naming conventions and prescriptions. There are probably things which are not amenable to automation but there's nevertheless a relentless effort from practitioners and researchers alike to _Automate all The Things_, and verify things like [software architecture](https://innig.net/macker/guide/) or [security](https://lgtm.com/).

There is one particular area where standardization, across a team or even a whole programming environment, should be enforced: The code's formatting rules. There has been, and will be, endless discussions between developers on such critically important topics as where to put curly braces, whether or not to use tabulations or spaces for indentation, how much space to put in various places of the code, etc. These discussions are a source of considerable bikeshedding, waste of time, and contribute exactly _zero_ value to the end product; automating application of standard code format [before commit](https://prettier.io/docs/en/precommit.html) or, even better, before [saving files](https://www.gnu.org/software/emacs/manual/html_node/elisp/Standard-Hooks.html).

However, this is dependent on the particular setup of each development environment and various problems might arise:

* The rules might be encoded in different tools to support different IDEs, which necessarily will lead to differences cropping up,
* There might not be an overall agreed upon set of rules for the language which makes it hard for the team to settle the question,
* The language itself might be hardly amenable to [parsing](http://modernperlbooks.com/mt/2009/08/on-parsing-perl-5.html) and pretty-printing,
* ...

I thus view as a great step forward that some formatting tools decide to provide no configuration options, like the [google-java-format](https://github.com/google/google-java-format) for Java, [Ormolu](https://hackage.haskell.org/package/ormolu), [elm-format](https://github.com/avh4/elm-format) for Elm, or [gofmt](https://golang.org/cmd/gofmt/)  for Go. The next logical step would be for the compiler itself to enforce the formatting rules[^1] and only accept programs in _Normal Form_: This would guarantee a uniform layout across all code bases using this language and would have the added advantage of greatly simplifying parsing and more generally development of tools to manipulate the language.

## Collective Code Ownership

_Collective Code Ownership_ is that XP practice which strives to make _all_ parts of the code collectively owned, meaning that every team member has the right and the duty to work on the whole software stack, there is no "preserve" that would be exclusive to some team members for reasons of skills, purpose, sensitivity, or seniority. A corollary to this strong statement is that every team member should feel at ease in every part of the code, and be able to understand what's going on and make relevant changes and improvements in a way that any other team member can vouch for it.

_Pair Programming_ or _Mob Programming_ are XP practices which are conducive to building such a collectively shared understanding of the code, much more so than _Pull Requests_ or _Code Reviews_[^2]. But they are only effective if all the people coding together can actually do so effectively and efficiently: If I pair program with an expert Vim golfer and I drive, then chances are I will be very slow and clumsy, at least initially; it will take time for me to get accustomed to my mate's development environment, to use his or her keyboard shortcuts, commands, links, productively. And this is not specific to Vim of course, cognitive dissonance can come from every part of the environment: The keyboard (Dvorak/Colemak/Qwerty/Azerty/Bépo), the OS (Mac/Windows/flavors of Linux), the language settings, the window manager (tiling/non-tiling), the shell (Bash/fish/zsh), the IDE (VS/VS Code/IDEA/Eclipse), the color settings (dark vs. light colors, small vs. large screens) are all potential sources of pain for the newcomer.

_Collective Code Ownership_ should be understood not only as an imperative to share understanding of the code but also to share understanding, knowledge, and skills of the _tools_ to produce and work on that code. Everyone has his or her own idiosyncrasies, preferences, disgusts, personal history that accumulate over time and cristallize in specific tools and environment we feel comfortable with. And this is of course totally fine. But this should not come in the way of building an _inter-subjective experience_ of the code: If we all insist on viewing and manipulating the code through our own private lenses, chances are we will fail in developing any form of shared understanding, vision, comprehensive knowledge of the code.

# Building Shared environment

Working remotely is actually a great opportunity to outflank the obstacle of particularism and build a _Shared Development Environment_ that is pre-configured _virtual machines_ (or _containers_) that provide a standardised development environment shareable across the team. The idea hinges on [Infrastructure as Code](https://en.wikipedia.org/wiki/Infrastructure_as_code) principles: Manage the infrastructure supporting the  _process_ using the same tools and principles with which we manage the _product_, but one step beyond. Do not only manage the production infrastructure, but also the development infrastructure, so that the whole stack of tools used to produce the code is shared by everyone, versioned in the source code, deployable at will.

Here is a description of the environment we have built in my previous teams.

## Defining the image

The first step is to define the virtual machine image. This we do using Hashicorp's [Packer](https://www.packer.io/) which provides a way to build VM images deployable to most major clouds (at least I have tried GCP and AWS). Packer is configured with a JSON file that looks the following (I have shown only the configuration for building a GCP image):

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

There are much more sophisticated options to configure the image than using a plain shell script but this is sufficient for our needs. I won't go into the details of the script here but it is pretty straightforward: It installs all the tools we need for development, which in this case includes docker, gcloud tools, and a whole bunch of utilities like curl, git, tmux, graphviz, wget, jq, python3, bzip2, neovim, ag... We were using Emacs (or rather [spacemacs](https://www.spacemacs.org/)) so it was also part of the installed packages.

Running `packer build build.json` will build a new image with the given name ready to be deployed over the configured infrastructure providers.

## Defining infrastructure

The most interesting part is the definition of the infrastructure itself, which uses [Terraform](https://www.terraform.io/). The set of `.tf` files is interpreted by the terraform tool, it checks the consistency of the defined infrastructure, and compares it with the latest known persistent state drawn from cloud storage, to decide what to do: Create, delete or modify various components of the infrastructure.

There is a module defining the standard configuration of a development VM:

```
variable "name" {
  description = "The name of the machine."
}

variable "ssh_keys" {
  description = "The file containing ssh_keys."
}
```

The variable `ssh_keys` contains the name of a file which lists the public keys of people who can access the machine. This makes it possible to control who can access the devlopment environment through a file _committed_ to the version control repository.

```
output "address" {
  value = "${google_compute_address.dev-address.address}"
}

resource "google_compute_address" "dev-address" {
  name = "${var.name}-address"
}
```

This `output` variable will show the IP address of the VM once it's deployed, so we need to allocate a public IP address for this machine.

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

This sets the core features of our VM: Machine type, location, boot disk size... The `sshKeys` metadata reads its content from the file we defined earlier, and the `service_Account` definition requests the creation of a new service account with the given authorizations.

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

We then upload configuration files (aka. _dotfiles_) which are also versioned and stored in the source tree, including configuration for the shell, the [tmux](https://github.com/tmux/tmux/wiki), emacs, and a configuration script.

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

And finally we execute the configuration script that's responsible for retrieving the source code, possibly cached build artifacts, and last minute configuration steps. There are a lot of possible variations on this baseline but the end goal is to ensure the team has at its disposal development environments which everyone can use and which are guaranteed to be consistently configured, thus making pair and mob programming session simple and easy to setup.

# Conclusion

In this post I have motivated why a shared development environment is desirable and useful, and demonstrated how easily it can be set up using basic IAAC tools. This practice can also be very useful in more complicated settings. I am working in a team which has to maintain a legacy piece of software built using a 4GL IDE called [PowerBuilder](https://en.wikipedia.org/wiki/PowerBuilder), which is only available on Windows. One of the first things I did when joining the team was to create a couple of VMs on Azure cloud with several tools installed, including the PowerBuilder IDE, SQL Management Studio, Git, in order to make it straightforward to work collaboratively on the code. The experience is not quite as smooth as sharing a terminal through tmux or tmate, but it's still only take a few seconds to switch driver/navigator roles: Simply log into the VM and share screen. Not being an expert in Windows automation I have not yet go beyond documenting the manual steps needed to configure the machine but this is only a matter of investing some time or finding the right person to help me.

Sharing the development environment is something that requires negotiation within the team, and that takes time to grow. Team members need to overcome their personal preferences and accept the discomfort of coding in an environment to which they might not be, at least initiall, a hundred percent atuned to. The goal is not to impose _The One True Way_ but on the contrary to share the knowledge and take the best that each setup has to offer. A versioned and pre-configured VM can perfectly host _both_ Emacs and Vim, or can expose a VNC server that makes it easier to share graphical tools, or basically offer any kind of environment.

The key benefit of standardisation of the development tools can also be extended to _Continuous Integration_ by sharing configuration of the machines between development and CI VMs.

[^1]: I thought this was the case for Elm and Go but it seems they do not still fail to compile ill-formatted source code.

[^2]: As my friend Christophe Thibaut used to say: "Code reviews is Pair programming for people who don't fully trust each other"
