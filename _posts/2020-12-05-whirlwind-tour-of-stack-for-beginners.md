---
layout: post
author: School Of FP
excerpt_separator: <!--end_excerpt-->
---

In this post, we will take a look at [`stack`](https://www.haskellstack.org/). It is the 3rd in a series of posts about getting started with Haskell. The first post looked into the basics of [Setting Up Haskell Development Environment](https://schooloffp.co/2020/07/25/setting-up-haskell-development-environment-the-basics.html), while the second post was an introduction to [Cabal for beginners](https://schooloffp.co/2020/08/17/whirlwind-tour-of-cabal-for-beginners.html). This post presents `stack` which is an alternate build tool to `cabal-install`. 

This post contains the following sections:

- [Clarifying Terms](#clarifying-terms)
- [What Stack is](#what-stack-is)
- [Moving Parts](#moving-parts)
- [Installing Stack](#installing-stack)
- [Creating a new project](#creating-a-new-project)
- [Configuring Stack](#configuring-stack)
- [Building and running executables](#building-and-running-executables)
- [Adding dependencies](#adding-dependencies)
- [Having a project with both library and executable](#having-a-project-with-both-library-and-executable)
- [Configuring visibility of modules in a library](#configuring-visibility-of-modules-in-a-library)
- [Using different GHC compiler across different projects](#using-different-ghc-compiler-across-different-projects)
- [Using the REPL](#using-the-repl)
- [How to execute a Haskell file as a script](#how-to-execute-a-haskell-file-as-a-script)
- [How to install packages](#how-to-install-packages)
- [Some interesting Stack paths](#some-interesting-stack-paths)

<!--end_excerpt-->

> This post is going to be a quick tour of the essential aspects of `stack` targeted at beginners. It will not only list commands that should be executed but would try to provide contextual explanations as much as possible. The aim is to give an introduction to `stack` that is not filled with commands to be memorized but one that can help the beginner to start forming a better understanding of `stack` so they can explore the tool further on their own.

## Clarifying terms

It is important to have an understanding of some basic terms and concepts when it comes to working and building Haskell programs. This includes understanding what is meant when we say things like _Cabal_, the difference between _Modules, Libraries, Packages_, etc. These terms were already explained in the previous post, so instead of reproducing it again here, please take a moment to read the [Clarifying Terms section](https://schooloffp.co/2020/08/17/whirlwind-tour-of-cabal-for-beginners.html#clarifying-terms) in the Whirlwind Tour Of Cabal For Beginners post.

One thing to point out regarding the concepts of packages and projects is that `stack` provides configuration for projects via a file called `stack.yaml`, while packages are configured via either `package.yaml` file or `.cabal` file. These files are covered in more depth in [Creating a new project](#creating-a-new-project) section.

## What Stack is

`stack` is a build tool for Haskell. It is an alternative to `cabal-install`, but still makes use of cabal the package format and Cabal the library. It has a strong focus on reproducible build plans. It was conceived to solve dependency problems (commonly referred to as cabal hell) that used to plague `cabal-install`. It does this by providing a curated package set that is guaranteed to always compile.

> It should be stressed that the cabal hell issue that used to plague cabal-install has mostly been resolved. The solution took a different approach than what `stack` employed. It made use of an approach similar to what is obtained in Nix. The details of this are not important, just that using either `cabal-install` or `stack`is now a matter of preference and UX choices.


## Moving parts

`stack` also comes with some moving parts and terms that could be confusing to a beginner. In this section, these terms are explained before moving to how to install `stack`.

### Curated Package Set

Hackage is Haskell’s central package archive of open-source software. Anyone can upload (and also download) packages from Hackage hence it contains different versions of packages that are not guaranteed to be compatible. A curated package set is then a subset of packages from Hackage which are regularly tested for compatibility. `stack` makes use of the idea of a curated package set to resolve the problem of incompatible dependencies.

### Stackage

[Stackage](https://www.stackage.org/) is the central package archive of open-source software of Haskell packages **that have been curated and tested for compatibility**. Hence the curated package set used by `stack` is hosted on Stackage. Stackage is spearheaded by folks at [fpcomplete](http://www.fpcomplete.com/), but is still a community effort, hence anyone can also [submit packages](https://github.com/commercialhaskell/stackage/blob/master/MAINTAINERS.md#adding-a-package) for inclusion in the curated package set that is hosted on Stackage.

### Resolver

It is easy to imagine the need for versioning when working with these curated package sets hosted on Stackage. Where a particular version uniquely identifies a particular curated package set and the packages it contains. In `stack`, the version of a curated package set is referred to or specified as a _resolver_.

A resolver uniquely identifies the GHC version and the other Haskell packages contained within a package set. Package sets that have been thoroughly tested and would be maintained for a long period are identified by LTS (long term support) resolver version numbers, while package sets that are still in flux are identified by nightly resolvers. The available LTS resolvers can be found [here](https://www.stackage.org/lts) while nightly resolvers can be seen [here](https://www.stackage.org/nightly). For example [lts-16.22](https://www.stackage.org/lts-16.22), is a LTS Resolver, while [nightly-2020-11-19](https://www.stackage.org/nightly-2020-11-19) is a nightly Resolver.

> Based on its name, it might be tempting to think of resolver as a piece of software that performs the act of resolution. A better way is to see the resolver as an identifier or a tag. It is a way to refer to a particular curated package set.

### hpack

As explained in [Clarifying Terms section](https://schooloffp.co/2020/08/17/whirlwind-tour-of-cabal-for-beginners.html#clarifying-terms) Haskell makes use of cabal package format, a text-based, key-value format used to describe a Haskell package. `hpack` is an alternate format to cabal package format. Instead of a custom key-value text-based format, it makes use of [`yaml`](https://yaml.org/). 

So while the cabal package format uses a custom key-value text-based format specified in a file ending with `.cabal` extension, `hpack` uses `yaml` with package descriptions specified in `package.yaml` file. `stack`has inbuilt support for `hpack`. This means `stack`can parse `hpack`s’ `package.yaml` and use it to generate `.cabal` file.

In summary, you can view `stack`as a Haskell build tool, that makes use of curated package sets, identified by resolvers and hosted on Stackage, with inbuilt support for `hpack` as an alternative package specification format.

## Installing Stack

Installing `stack`is straight forward. On Un*x operating systems, run: 

```bash
curl -sSL https://get.haskellstack.org/ | sh
```

On windows, the provided [Windows Installer](https://get.haskellstack.org/stable/windows-x86_64-installer.exe) can be used

To confirm `stack`is properly installed run `stack --version` which will give an output similar to this:

```bash
$ stack --version
Version 2.3.3, Git revision cb44d51bed48b723a5deb08c3348c0b3ccfc437e x86_64 hpack-0.33.0
```

> Note that you do not need to install GHC separately when using `stack`. `stack`handles getting the version of GHC needed for each project created. This is also one of the ways in which `stack`differs from `cabal-install` which requires GHC to be separately downloaded and installed. So `stack`not only takes care of resolving dependencies, compiling Haskell code, it also handles the management of the toolchain required to do this, and GHC is included in the toolchain.


## Creating a new project

Now we have `stack`installed, the next thing to do is to create a new Haskell project. Doing this is straightforward. To create a new project, run `stack new`. 

Doing so will produce an output similar to this:

```bash
$ stack new firstproject
Downloading template "new-template" to create project "firstproject" in firstproject/ ...

The following parameters were needed by the template but not provided: author-name
You can provide them in /Users/schooloffp/.stack/config.yaml, like this:
templates:
  params:
    author-name: value
Or you can pass each one as parameters like this:
stack new firstproject new-template -p "author-name:value"

The following parameters were needed by the template but not provided: author-email, author-name, category, copyright, github-username
You can provide them in /Users/schooloffp/.stack/config.yaml, like this:
templates:
  params:
    author-email: value
    author-name: value
    category: value
    copyright: value
    github-username: value
Or you can pass each one as parameters like this:
stack new firstproject new-template -p "author-email:value" -p "author-name:value" -p "category:value" -p "copyright:value" -p "github-username:value"

Looking for .cabal or package.yaml files to use to init the project.
Using cabal packages:
- firstproject/

Selecting the best among 18 snapshots...

* Matches lts-16.22

Selected resolver: lts-16.22
Initialising configuration using resolver: lts-16.22
Total number of user packages considered: 1
Writing configuration to file: firstproject/stack.yaml
All done.
/Users/schooloffp/.stack/templates/new-template.hsfiles:    3.72 KiB downloaded...
```

The above command will create a directory named `firstproject` with will contain the source files for the project.

> The above command creates a project based on a default template which dictates the default files and configuration that will be created. It should be noted that it is possible to pass an extra option together with `new` that specifies the template that `stack`should use to create a new project. When the template option is left out, `stack`uses the `new-template` template by default. Hence `stack new firstproject` is same as running `stack new firstproject new-template`. Run `stack templates` to see the available templates.

The state of the directory after running `stack new firstproject` can be seen:

```bash
$ tree firstproject/
firstproject/
├── ChangeLog.md
├── LICENSE
├── README.md
├── Setup.hs
├── app
│   └── Main.hs
├── firstproject.cabal
├── package.yaml
├── src
│   └── Lib.hs
├── stack.yaml
└── test
    └── Spec.hs
```
Let’s take a pause to talk about the files generated after running the `stack new` command.

### ChangeLog.md

As is evident in the name, this is a markdown file that should be used to capture the revision history for the project.

### LICENSE

This is where the license governing the project is specified, as also evident in the name.

### README.md

The well-known README file.

### Setup.hs
The `Setup.hs` file comes in handy in the scenario where one needs to circumvent tools like `cabal-install` or `stack`for direct usage of the Cabal library when compiling Cabal packages. The `Setup.hs` is, in essence, a runnable Haskell program that can be further configured and used to compile Cabal packages.

This is an exceptional use case. Everyday Haskell development would probably never require the need for `Setup.hs` and hence it can be ignored or even safely deleted.

### app/Main.hs

A Haskell package can contain both an executable (to be run) or a library, (to be used when writing Haskell code). It is customary to put the source files that generate executable and library in separate directories. When `stack new` is used, it puts the executables into a directory named `app`. `Main.hs` is the source file for the default executable `stack` generates, hence why it is put in the `app` directory.

### firstproject.cabal

This is the cabal package format file. `stack`maintains this file: generates it and updates it when necessary, hence it should never be the case that you need to update this file directly.

### package.yaml

This is the `hpack` package format file, which has already been explained to be an alternative to the cabal package format. This is the file that you will need to update to provide meta-information like dependencies, information about the developer(s) maintaining the package, directories where the executable and libraries can be found etc. `stack` uses this as input to generate the `.cabal` file for the project. That means the `firstproject.cabal` is generated from the contents of `package.yaml`. 

> consult [`hpack` quick reference](https://github.com/sol/hpack#quick-reference) for an overview of properties that can be set within the package.yaml file.

### src/Lib.hs

A Haskell package can contain both an executable (to be run) or a library, (to be used when writing Haskell code). It is customary to put the source files that generate executable and library in separate directories. When `stack new` is used, it puts the libraries into a directory named `src`. `Lib.hs` is a default library source `stack` generates, hence why it is put in the `src` directory.  

### stack.yaml

`stack.yaml` contains contents that are used to configure `stack` itself and how it should build your project. It contains information like which resolver version `stack`should use, the list of packages `stack`should build etc. 
 
### test/Spec.hs

This contains the default test cases `stack`generates.

## Configuring Stack

Before we go-ahead to start creating and building projects in `stack`, it is important to understand how to configure `stack`and where the various configuration files can be found. This knowledge will help in a better understanding of which configurations to change and where to find them when there is the need to tweak how `stack`works.

`stack`works with the idea of a project which is a directory that contains a `stack.yaml` configuration file. A `stack`project then contains one or more Haskell packages, which is depicted by the presence of one or more `package.yaml` (or the corresponding `.cabal` files). These packages are often referred to as local packages, in contrast to packages hosted on either Hackage or Stackage.

`stack`can be used either in the context of a project or outside of a project. When used in the context of a project, `stack`is executed from within a directory that contains `stack.yaml`. Using `stack`outside of a project means executing `stack`from a location where there is no `stack.yaml`` file.

Hence there are three files that should be noted when configuring `stack`, and these include:

- <project_dir>/stack.yaml
- ~/.stack/global-project/stack.yaml
- ~/.stack/config.yaml 

> `config.yaml` can also be found in `/etc/stack/config.yaml`

So what do these configurations contain and when do they get applied?

**`<project_dir>/stack.yaml`** contains local package related configuration, **`~/.stack/global-project/stack.yaml`** also contains package related configuration but it is _only_ consulted in the cases where `stack`is executed outside of a directory with a `stack.yaml` file. 

**`~/.stack/config.yaml`** contains a non-project configuration that can be used to change how `stack`does what it does. For example which `hpack` binary `stack`should use, or where `stack`should put the executables it builds.

Put another way, when `stack`is executed from within a project, configuration is picked from `<project_dir>/stack.yaml` together with whatever default that is specified in `~/.stack/config.yaml`.

When `stack`is executed outside of a project, `stack` still needs a `stack.yaml`, hence the one specified in `~/.stack/global-project/stack.yaml` is used together with whatever default is specified in `~/.stack/config.yaml`.

`<project_dir>/stack.yaml` and `~/.stack/global-project/stack.yaml` are said to contain project specific configuration options, while `~/.stack/config.yaml` contains default non project specific options.

One might ask, what `stack`commands can be executed outside of a directory where no `stack.yaml` file exists? 

A very good example of such is `stack new` which creates the very directory where a `stack.yaml` file will be created. Configurations found within `~/.stack/global-project/stack.yaml` (and also defaults within `~/.stack/config.yaml`) is then consulted. 

> To see the documentation for project-specific configuration options, check [here](https://docs.haskellstack.org/en/stable/yaml_configuration/#project-specific-config). For non project configurations, check [here](https://docs.haskellstack.org/en/stable/yaml_configuration/#non-project-specific-config)

## Building and running executables

To get started with using `stack`, we will take a look at how to build and run an executable with it. To get the ball rolling we would compile and run the default project created when `stack new firstproject` was run since it contains all necessary files needed to build a Haskell project.

To build, run `stack build`, this would lead to output similar to:

```bash

$ stack build
Preparing to install GHC to an isolated location.
This will not interfere with any system-level installation.
Downloaded ghc-8.8.4.

---snipped logged ---

```

This would compile and build the executable in the project. To do this, `stack`will first download the required GHC version for the project. This can be seen from initial log snippets. This is a unique feature of `stack`. `stack`not only helps in building Haskell projects, but it also manages the compiler version, that is the GHC version that would be used.

To run the executable generated above, after the `stack build` command finishes run `stack run`

```bash
$ stack run
someFunc
```

> The `stack run` command can be executed without first running `stack build`. If that is done, `stack`will make sure to first build in order to generate the executable that would be run.

So far so good, we have a project that contains an executable package which we were able to build and run. The next thing to look at is how `stack`helps with dependency management.

## Adding dependencies

`stack`works with curated package sets hosted on Stackage, hence naturally `stack`consults Stackage for resolving external third-party dependencies. As Stackage is a subset of packages, it may so happen that a required dependency is not on Stackage but found in the uncurated Hackage repository. For this situation, `stack`still makes it possible to make use of packages found on Hackage. In this section, we see how to include dependencies from both Stackage and Hackage.

To illustrate how to add dependencies from Stackage to a package we will be modifying the package we got when we ran `stack new` and we will be adding two packages. The packages are [emojis](https://hackage.haskell.org/package/emojis) an emoji utility, and [haskell-say](https://hackage.haskell.org/package/haskell-say) which decorates texts printed to the console with ASCII art of a callout from the Haskell logo. 

`emojis` can be found on Stackage, while `haskell-say` is only available on Hackage.

To add these packages as dependencies edit the `dependencies` section in `package.yaml` to be:

```yaml
dependencies:
- base >= 4.7 && < 5
- emojis
- haskell-say
- text
```

We also add [`text`](https://www.stackage.org/lts-16.24/package/text-1.2.4.0) as a dependency in other to be able to convert from `Text` which the `emojis` package used to the `String` which `haskell-say` uses.

Now that we have added these dependencies, we can use them. Update the `Main.hs` file as follow:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Emoji
import HaskellSay (haskellSay)
import Data.Text hiding (map)

main = mapM_ (haskellSay . ("You want some " <>) . unpack) (emojiFromAlias "pizza")
```

Now attempt to build and run by executing the command `stack run` This will fail, with an error message similar to this:

```bash
$ stack run

Error: While constructing the build plan, the following exceptions were encountered:

In the dependencies for firstproject-0.1.0.0:
    haskell-say needed, but the stack configuration has no specified version  (latest matching version is 1.0.0.0)
needed since firstproject is a build target.

Some different approaches to resolving this:

  * Recommended action: try adding the following to your extra-deps
    in /Users/schooloffp/delete/stack/firstproject/stack.yaml:

- haskell-say-1.0.0.0@sha256:654ed7ff571d62fb03dfda576aa0a89410fe403b0d84695b5151e3d026f33099,1330

Plan construction failed
```

So what is going on here?

The problem is, the `haskell-say` package is not present on Stackage. When the `package.yaml` was edited to include `emojis` and `haskell-say`, `stack`was able to resolve the needed `emojis` package as this is included in Stackage as can be seen [here](https://www.stackage.org/lts-16.22/package/emojis-0.1). 

This means that even though `haskell-say` has been specified as a dependency via its inclusion in `package.yaml`, `stack`will need extra help to be able to retrieve the `haskell-say` package, this time from Hackage, since it is not present in Stackage. To do this, the `extra-deps` configuration in `stack.yaml` is used.

Edit `stack.yaml` and update the `extra-deps` configuration as follows:

```yaml
extra-deps:
 - haskell-say-1.0.0.0
``` 

> Note that the exact version of the package needs to be specified. This can also be done by using the commit hash, as suggested in the error message.

Now that `haskell-say-1.0.0.0` in included as an `extra-deps`, executing `stack run` again should lead to the following output:

``` bash
$ stack run
Building all executables for `firstproject' once. After a successful build of all of them, only specified executables will be rebuilt.
firstproject> configure (lib + exe)
Configuring firstproject-0.1.0.0...
firstproject> build (lib + exe)
Preprocessing library for firstproject-0.1.0.0..
Building library for firstproject-0.1.0.0..
[1 of 2] Compiling Lib
[2 of 2] Compiling Paths_firstproject
Preprocessing executable 'firstproject-exe' for firstproject-0.1.0.0..
Building executable 'firstproject-exe' for firstproject-0.1.0.0..
[1 of 2] Compiling Main
[2 of 2] Compiling Paths_firstproject
Linking .stack-work/dist/x86_64-osx/Cabal-3.0.1.0/build/firstproject-exe/firstproject-exe ...
firstproject> copy/register
Installing library in /Users/schooloffp/delete/stack/firstproject/.stack-work/install/x86_64-osx/f7400eb5a06ef73a5cb417d56f25fb1b3c83d9c5e948a55a2c6f6c51f1bfd4fe/8.8.4/lib/x86_64-osx-ghc-8.8.4/firstproject-0.1.0.0-Kbd7jwgcvpB4EvHQfLN8fa
Installing executable firstproject-exe in /Users/schooloffp/delete/stack/firstproject/.stack-work/install/x86_64-osx/f7400eb5a06ef73a5cb417d56f25fb1b3c83d9c5e948a55a2c6f6c51f1bfd4fe/8.8.4/bin
Registering library for firstproject-0.1.0.0..
  ________________________________________________________
 /                                                        \
| You want some 🍕                                          |
 \____       _____________________________________________/
      \    /
       \  /
        \/
  _____   _____
  \    \  \    \
   \    \  \    \
    \    \  \    \
     \    \  \    \  \-----------|
      \    \  \    \  \          |
       \    \  \    \  \---------|
       /    /  /     \
      /    /  /       \  \-------|
     /    /  /    ^    \  \      |
    /    /  /    / \    \  \ ----|
   /    /  /    /   \    \
  /____/  /____/     \____\
```

## Having a project with both library and executable

Right now our package has only an executable. The next thing we are going to do is to introduce a library. To do this we are going to turn our project into one, which prints out quotes written in Emoji when executed. To achieve this, we make the following modifications:

- Add configuration of a library module to `package.yaml`
- Add code that implements the library module
- Add code to the executable to make use of the library module.

Putting these into code will look as follows:

### Adding configuration of a library module to package.yaml

Edit `package.yaml` to include the library configuration as follows:

```yaml
library:
  source-dirs: src
  exposed-modules: EmojiQuotes
```

The `source-dirs:src` is added by default when `stack new` is executed and it points to the directory that contains the source code for modules making up the library. The `exposed-modules: EmojiQuotes` indicates that `EmojiQuotes` is a library module and should be exposed for usage in other modules.

### Adding code that implements the library module

The next thing to do is to create the `EmojiQuotes` module in the `src` directory:

```bash
$ touch touch src/EmojiQuotes.hs
```

Then edit `EmojiQuotes.hs` to have the following content:

```haskell
module EmojiQuotes where
-- content of the module goes in here
```

Run `stack build`. If no error then you have everything all set up. The next thing would be to edit both `EmojiQuotes.hs` and `Main.hs` to implement our little Emoji Quotes program. The following code snippets show how that would look like.

In `EmojiQuotes.hs` add a list of quotes together with a function that selects one from the list. This code looks like this:

```haskell
{-# LANGUAGE OverloadedStrings #-}


module EmojiQuotes where

import Text.Emoji
import Data.Text hiding (length)

quotes =
 [(pure "Do not beat a "
   <> (emojiFromAlias "coffin")
   <> (pure " ")
   <>  (emojiFromAlias "horse")),
 (pure "Every "
   <> (emojiFromAlias "dog")
   <> (pure " ")
   <> (pure "has its ")
   <> (emojiFromAlias "calendar")),
 (emojiFromAlias "heart")
   <> (pure " is ")
   <> (emojiFromAlias "dark_sunglasses"),
 (pure "Don't ")
   <> (emojiFromAlias "sob")
   <> (pure " over spilt milk"),
 (emojiFromAlias "hammer")
   <> (pure " the ")
   <> (emojiFromAlias "round_pushpin")
   <> (pure " over the ")
   <> (emojiFromAlias "face_with_head_bandage")
 ]

selectQuote :: Int -> Maybe String
selectQuote i = if  i > (length quotes)
                then Nothing
                else (fmap unpack (quotes !! i))


numberOfQuotes = length quotes

```

### Adding code to the executable to make use of the library module

Then in `Main.hs` edit to make use of the `EmojiQuotes` module:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Emoji
import HaskellSay (haskellSay)
import Data.Text hiding (map)
import EmojiQuotes

main = do
     putStrLn ("Type in a number between 1 and " ++ show numberOfQuotes)
     index <- getLine
     case selectQuote (read index :: Int) of
       Just s -> putStrLn s
       _      -> putStrLn ("No quote found at index " ++ show index)
```

With all the above code changes, a library module `EmojiQuotes` has been created, which is then used in the executable module `Main`. Running `stack run` would build and run the executable, which leads to a prompt where a number can be inputted, which then returns a quote. You should have something similar to this:

```bash
$ stack run
Type in a number between 1 and 5
1
Every 🐶 has its 📆
```

This demonstrates the ability to have a library and an executable in the same project. In the case above, the emoji quotes are placed in the library, which the executable made use of.

## Configuring visibility of modules in a library

Most of the time, not all modules that make up a library are intended to be accessible. Some modules are implementation details and should be hidden from other developers who make use of the library.

Modules that serve as implementation details can be configured as hidden while modules meant to be used by other developers can be configured to be exposed modules.

To illustrate this, we will split the `EmojiQuotes` module into two. We will extract the list of quotes into another module named `QuotesStore.hs`. `QuotesStore.hs` would be marked as hidden while `EmojiQuotes.hs` would remain pubicly accessible.

To do this, edit `project.yaml` to introduce the `QuotesStore.hs`. The library section should look like this:

```yaml
library:
  source-dirs: src
  exposed-modules: EmojiQuotes
  other-modules: QuotesStore
```

`QuotesStore` has been added as `other-modules`, while `EmojiQuotes` remains as `exposed-modules`. Modules specified as `other-modules` are hidden, hence only `EmojiQuotes` would be accessible.

Add the `QuotesStore.hs` to the `lib` directory with the following content:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module QuotesStore where

import Text.Emoji

quotes =
 [(pure "Do not beat a "
   <> (emojiFromAlias "coffin")
   <> (pure " ")
   <>  (emojiFromAlias "horse")),

 (pure "Every "
   <> (emojiFromAlias "dog")
   <> (pure " ")
   <> (pure "has its ")
   <> (emojiFromAlias "calendar")),

 (emojiFromAlias "heart")
   <> (pure " is ")
   <> (emojiFromAlias "dark_sunglasses"),

 (pure "Don't ")
   <> (emojiFromAlias "sob")
   <> (pure " over spilt milk"),

 (emojiFromAlias "hammer")
   <> (pure " the ")
   <> (emojiFromAlias "round_pushpin")
   <> (pure " over the ")
   <> (emojiFromAlias "face_with_head_bandage")]
```

Update `EmojiQuotes.hs` to use `QuotesStore` module as follows:

```haskell
{-# LANGUAGE OverloadedStrings #-}


module EmojiQuotes where

import Text.Emoji
import Data.Text hiding (length)
import QuotesStore

selectQuote :: Int -> Maybe String
selectQuote i = if  i > (length quotes)
                then Nothing
                else (fmap unpack (quotes !! i))

numberOfQuotes = length quotes

```

With the following changes, executing `stack run` would work as it did before.

To confirm that `QuotesStore` is indeed hidden, an attempt to use it from within Main.hs, should lead to a compilation error. That is:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.Emoji
import HaskellSay (haskellSay)
import Data.Text hiding (map)
import EmojiQuotes

-- attempting to use a hidden module
import QuotesStore

main = do
     putStrLn ("Type in a number between 1 and " ++ show numberOfQuotes)
     index <- getLine
     case selectQuote (read index :: Int) of
       Just s -> putStrLn s
       _      -> putStrLn ("No quote found at index " ++ show index)

```

When compiled would lead to a compilation error similar to what is reproduced below:

```bash
/Users/schooloffp/delete/stack/firstproject/app/Main.hs:11:1: error:
    Could not load module ‘QuotesStore’
    it is a hidden module in the package ‘firstproject-0.1.0.0’
    Use -v (or `:set -v` in ghci) to see a list of the files searched for.
   |
11 | import QuotesStore
   | ^^^^^^^^^^^^^^^^^^


--  While building package firstproject-0.1.0.0 using:
      /Users/schooloffp/.stack/setup-exe-cache/x86_64-osx/Cabal-simple_mPHDZzAJ_3.0.1.0_ghc-8.8.4 --builddir=.stack-work/dist/x86_64-osx/Cabal-3.0.1.0 build lib:firstproject exe:firstproject-exe --ghc-options " -fdiagnostics-color=always"
    Process exited with code: ExitFailure 1
```

So far, we have been able to create a package with both an executable and library and also have external packages and local packages as dependencies. We were also able to configure the visibility of modules within the library. We compiled, built, and ran the executable in the package and everything works as expected.

The next thing to look at is one of the stated benefits of using a build tool like cabal/stack in [Setting Up Haskell Development Environment: The Basics](https://schooloffp.co/2020/07/25/setting-up-haskell-development-environment-the-basics.html) and that is how to work with various version of GHC across different projects.

## Using different GHC compiler across different projects

The goal is to be able to have two (or more) projects and be able to specify that they use different GHC versions. This is trivial to achieve with `stack`based on how `stack`works. As stated earlier, `stack`does not only take care of building Haskell projects, it also takes care of downloading the necessary version of GHC needed. 

This means to have a project use a specific version of GHC, the only thing needed to be done is to instruct `stack`appropriately. And this is done by updating the `resolver` key in the `stack.yaml` file, and specifying the appropriate LTS version of the package curated set that corresponds to the GHC version needed.

The version of GHC being used in a project can be confirmed by checking what the corresponding GHC version is to the LTS version specified in `stack.yaml`. This can be done by looking it up on [Stackage.org](https://www.stackage.org/) which always specify the GHC version that would be downloaded alongside the LTS version. Another way to check is to run  `stack ghc -- --version` from within the directory containing the project. If this is run from the directory of the project created earlier on, the output will be:

```bash
$ stack ghc -- --version
The Glorious Glasgow Haskell Compilation System, version 8.8.4
```

To illustrate, specifying a different GHC version, another project will be created and set to use a different version of GHC. The resolver key for this new project will use `LTS 16.11` which translates to GHC version 8.8.3.

Navigate out of the `firstproject` directory and create a new project. 

```
stack new secondproject
```

Edit `secondproject/stack.yaml` and have the `resolver` key set to `LTS 16.11`

```
resolver: lts-16.11
```

To confirm the GHC version using GHC run `stack ghc -- --version`

```
stack ghc -- --version
-- gazillion lines while ghc is downloaded
The Glorious Glasgow Haskell Compilation System, version 8.8.3
``` 

> If commands like `stack run` or `stack build` were not run on the new project, running  `stack ghc -- --version` will first download and install the required ghc before printing out the version.

## Using the REPL

The REPL which stands for Read-Eval-Print-Loop, is an interactive computer programming environment that takes user inputs, executes them, and returns the result to the user. Haskell also provides a REPL environment. This is powered by `GHCi`.

> For an overview of GHCI and the other components of the Haskell Tool chain, check [Setting Up Haskell Development Environment: The Basics](https://schooloffp.co/2020/07/25/setting-up-haskell-development-environment-the-basics.html)

`stack`provides the opportunity to interact with the Haskell REPL. Below is a run-through of some of the common ways of interacting with the REPL via stack.

### Starting the REPL

The simplest way to use the REPL is to start it and start interacting with it. To do this run `stack repl`.

```bash
$ stack repl

-- Snipped logs --

Prelude> 1 + 1
2
```

> Running `stack ghci` would produce the same result as it is an alias for `stack repl`

### Loading a file into the REPL

To demonstrate this, create a file, for example, `Hello.hs` and update it with the following code snippet:

```haskell
module Hello where

main = print "Hello world"
```

Now start the REPL and from there load the Hello.hs file:

```bash
$ stack repl

-- snipped log output


Prelude> :l Hello.hs
[1 of 1] Compiling Hello            ( Hello.hs, interpreted )
Ok, one module loaded.
*Hello> main
"Hello world"
*Hello>
```

It is also possible to load the file while starting the REPL:

```bash
$ stack repl Hello.hs

-- snipped log output

*Hello> main
"Hello world"
```

### Loading an external module into the REPL session

`stack`can also download and make an external package available for use in the REPL. This can be done by using the `--package` to specify the package `stack`should load into the REPL session. For example the following loads and uses the `titlecase` package in the REPL:

```bash
$ stack repl --package titlecase Hello.hs

-- Snipped log output --

*Hello> Data.Text.Titlecase.titlecase "turn this to title case"
"Turn This to Title Case"

```  

A question that is reasonable to ask is what is the version of the package that is loaded into the REPL in this way? Apart from the version of the package, what about the version of the GHC used in the REPL?

`stack repl` is another example of `stack`commands that can be run outside of a directory containing `stack.yaml`. As earlier mentioned, such commands will have `stack`picking up its configuration from `~/.stack/global-project/stack.yaml` and `~/.stack/config.yaml`. For example the GHC version to be used in the REPL session will be determined from these configurations. 

Another way of specifying version information when starting the REPL is to use the `--resolver ` parameter to set the version of the curated package set used. This would not only specify the GHC version used, but also the version of the package. 

For example this:

```bash
stack repl --package titlecase --resolver lts-16.24 Hello.hs
```

Sets the resolver version to `lts-16.24` which inadvertently also sets the GHC version to ghc-8.8.4 and the version of `titlecase` to version 1.0.1. 

### Loading a local package into the REPL session

It is also possible to load a local package into a REPL session and be able to interact with all the exposed modules in that package. For example to load the `firstproject` that was created earlier, navigate into the directoru containing the project and run `stack repl`:

```bash
$ cd firstproject/
$ stack repl

-- snipped log output --

*Main EmojiQuotes QuotesStore> main
Type in a number between 1 and 5
1
Every 🐶 has its 📆
```

## How to execute a Haskell file as a script

In [Setting Up Haskell Development Environment: The Basics](https://schooloffp.co/2020/07/25/setting-up-haskell-development-environment-the-basics.html) we saw how to execute a file containing Haskell source code as a script. We can achieve the same with `stack`. Running Haskell source as script via `stack`has the additional benefit of allowing dependencies to be specified and used within the script!

For example the following code snippet in a file named `haskellsayscript.hs` shows how to have a script that prints out text to the console using the `titlecase` and `haskell-say` package.

In `haskellsayscript.hs`:

```haskell
#!/usr/bin/env stack
{- stack
  script
  --resolver lts-16.24
  --package haskell-say
  --package titlecase
-}

module HaskellSayScript where

import HaskellSay (haskellSay)
import Data.Text.Titlecase (titlecase)

main :: IO ()
main =
  haskellSay $ titlecase "running as a script"

```

The preambles at the top specify `stack`as the interpreter. It also specifies the dependencies the script uses. This would be downloaded and made available before the script runs.

Make the script executable:

```bash
chmod +x haskellsayscript.hs
```

And then running it should give an output similar to this:

```bash

$ ./haskellsayscript.hs
  ________________________________________________________
 /                                                        \
| Running As a Script                                      |
 \____       _____________________________________________/
      \    /
       \  /
        \/
  _____   _____
  \    \  \    \
   \    \  \    \
    \    \  \    \
     \    \  \    \  \-----------|
      \    \  \    \  \          |
       \    \  \    \  \---------|
       /    /  /     \
      /    /  /       \  \-------|
     /    /  /    ^    \  \      |
    /    /  /    / \    \  \ ----|
   /    /  /    /   \    \
  /____/  /____/     \____\
```

## How to install packages

`stack`can also be used to install executables. It should be noted that `stack`is not a full-fledged package manager and should not be seen as a replacement for tools like homebrew, chocolatey, or even the native package manager that comes with Gnu/Linux systems. If the choice exists, prefer to manage installed software via these established package managers instead of using cabal-install. Having said that, software, i.e. executable created in Haskell can be installed via `stack`.

To illustrate this, we will install the titlecase executable which provides a command-line utility that converts English words to title case.

To install it run the following command:

```bash
stack install titlecase
```

After which you can then use it:

```bash
$ titlecase this turns sentences to title case
This Turns Sentences to Title Case
```

It should be noted that what the `stack install` command does is to compile an executable either from a local package or from Stackage and then move the created executable into the `$HOME/.local/bin` path, which by the way is a directory that should be added to `Path` environment. In fact, `stack install` is an alias for `stack build --copy-bins`. 

To confirm that running `stack install` builds and copies executable into stack local bin path. Run the following command:

```bash
$ which titlecase
/Users/schooloffp/.local/bin/titlecase
```

Which shows that the `titlecase` executable is indeed in the `stack`local bin path.

## Some interesting Stack paths

The `stack`local bin path is just one of the various paths that `stack`uses. In this concluding section of this post, some other interesting paths are outlined below:

#### Global stack root directory

This is the path to the directory where all `stack`related assets (files, executable, configuration etc) can be found. By default this is located in `$HOME/.stack`. To show the location of Stack's root directory run:

```bash
$ stack path --stack-root
/Users/schooloffp/.stack
```

#### Install location for GHC and other core tools

This is the path to the directory that contains core tools GHC depends on. It is by default located under `Stack`s root directory. To show the location run:

```bash
$ stack path --programs
/Users/schooloffp/.stack/programs/x86_64-osx
```

Listing the content of the directory would give an output similar to:

```bash
$ ls /Users/schooloffp/.stack/programs/x86_64-osx
ghc-7.10.3           ghc-8.2.2.tar.bz2    ghc-8.4.3.installed  ghc-8.6.1            ghc-8.6.4.tar.bz2    ghc-8.8.3.installed
ghc-7.10.3.installed ghc-8.4.2            ghc-8.4.3.tar.bz2    ghc-8.6.1.tar.bz2    ghc-8.6.5            ghc-8.8.3.tar.bz2
ghc-7.10.3.tar.bz2   ghc-8.4.2.installed  ghc-8.4.4            ghc-8.6.1.temp       ghc-8.6.5.installed  ghc-8.8.4
ghc-8.2.2            ghc-8.4.2.tar.bz2    ghc-8.4.4.installed  ghc-8.6.4            ghc-8.6.5.tar.bz2    ghc-8.8.4.installed
ghc-8.2.2.installed  ghc-8.4.3            ghc-8.4.4.tar.bz2    ghc-8.6.4.installed  ghc-8.8.3            ghc-8.8.4.tar.bz2
```

Showing that indeed this is where the different versions of GHC stacks needs are placed.

#### Configuration location (where the stack.yaml file is)

This is the path to the directory where the `stack.yaml` is located. To see the value of this path run `stack path --config-location`. Running this in a directory that is not a `stack`project gives the following results:

```bash
$ stack path --config-location
/Users/schooloffp/.stack/global-project/stack.yaml
```

while running the same command in a directory containing a `stack`project gives the following:

```bash
cd firstproject/
$ stack path --config-location
/Users/schooloffp/delete/stack/firstproject/stack.yaml
```

Confirming that `stack.yaml` found in a directory containing a `stack`project would take effect over the default. 

#### Compiler binary (e.g. ghc)

This is the path to the exact GHC binary used. To see the value of this path run `stack path --compiler-exe`. Running this in a directory that is not a `stack`project gives the following results:

```bash
$ stack path --compiler-exe
/Users/schooloffp/.stack/programs/x86_64-osx/ghc-8.6.5/bin/ghc-8.6.5
```

while running the same command in a directory containing a `stack`project gives the following:

```bash
cd firstproject/
$ stack path --compiler-exe
/Users/schooloffp/.stack/programs/x86_64-osx/ghc-8.8.4/bin/ghc-8.8.4
```

Indicating that the version of GHC used may be different per project.

To view all the available paths, run `stack path`.

This ends the whirlwind tour of `stack` for beginners. The information presented in this post should be enough to get started with working with `stack`. The [Stack User Guide](https://docs.haskellstack.org/en/stable/GUIDE/) is a good resource to check out to further explorer how to use `stack`. The [`hpack` README](https://github.com/sol/hpack#hpack-a-modern-format-for-haskell-packages) is also another resource to consult to futher explore how `package.yaml` can be confifigured.