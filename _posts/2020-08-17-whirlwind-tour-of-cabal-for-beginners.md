---
layout: post
author: School Of FP
excerpt_separator: <!--end_excerpt-->
---

In this post, we will take a look at Cabal. It is a follow up on the previous post: [Setting Up Haskell Development Environment: The Basics](https://schooloffp.co/2020/07/25/setting-up-haskell-development-environment-the-basics.html) where we took a step back to see how to compile Haskell programs without the use of any build tool. The reason for doing that is to create an appreciation for the fundamental moving parts involved in setting up and developing with Haskell. In this post, we move forward and we introduce Cabal, which is a build tool that can be described as a system for building and packaging Haskell libraries and programs.

This post contains the following sections:

- [Clarifying terms](#clarifying-terms)
- [Installing (or upgrading) cabal-install](#installing-cabal-install)
- [Creating a new project](#creating-a-new-project)
- [Building and running executables](#building-and-running-executables)
- [Adding dependencies](#adding-dependencies)
- [Having a project with both library and executable](#having-a-project-with-both-library-and-executable)
- [Configuring visibility of modules in a library](#configuring-visibility-of-modules-in-a-library)
- [Using different GHC compiler across different projects](#using-different-ghc-compiler-across-different-projects)
- [How to execute a Haskell file as a script](#how-to-execute-a-haskell-file-as-a-script)
- [How to install packages](#how-to-install-packages)
- [Historical Notes](#historical-notes)


<!--end_excerpt-->

> This post is going to be a quick tour of the essential aspects of Cabal targeted at beginners. It will not only list commands that should be executed but would try to provide contextual explanations as much as possible. The aim is to give an introduction to Cabal that is not filled with commands to be memorized but one that can help the beginner to start forming a better understanding of Cabal.

---

## Clarifying terms

To begin, we first take some time out to clarify some ambiguity regarding some terms that come up when discussing building and packing software in Haskell. 

### What do we mean when we say Cabal?.

Cabal is an acronym that stands for _Common Architecture for Building Applications and Libraries_. Unfortunately, the term Cabal by itself is ambiguous and It could refer to one of three things: a package format, a library, or a binary.

#### Cabal the Package Format. 

This is the format that specifies the content of Haskell packages (not sure exactly what Haskell packages mean? Do not fret, that will be clarified soon). It contains information that drives the compilation and building of Haskell packages. It is a text-based, key-value format, that is divided into subsections called _stanzas_. It is  denoted with a file with the `.cabal` extension. See [package.cabal fields](https://cabal.readthedocs.io/en/latest/cabal-projectindex.html#cap-package.cabal%20fields) for the fields that are defined within the Cabal Package format.

#### Cabal the Library

This is the library that provides functionality that allows the information in the `.cabal` files to be put to use. Without the `Cabal` library, the Cabal package format is just that, a text file. The `Cabal` library contains the implementations that allow for the parsing and operations based on the content of a `.cabal` file. `Cabal` the library by convention is written with a Capitalization case. It can be found on Hackage [here](https://hackage.haskell.org/package/Cabal)   

#### cabal the binary

`cabal` (lowercase) or more accurately `cabal-install` is the command-line tool that provides a user interface for dealing with Haskell packages. `cabal-install` makes use of `Cabal` the library to do its job. It can be found on Hackage [here](https://hackage.haskell.org/package/cabal-install)

> it is worth mentioning here that `stack` is also a command-line tool that depends on the Cabal Library, and hence also consumes the information specified in Cabal Package format found in the `.cabal` files.

### What do we mean when we say Module, Package, Library, Executable and Project

Module, Package, Library, and Executable are terms that could also be highly ambiguous. Especially given the fact that they are also used in other programming language ecosystems where they might mean something different compared to their meaning in Haskell. Also, some of these terms are often used interchangeably adding to the confusion regarding their precise definition. Hence there is some benefit in defining what exactly they mean when used within the context of Haskell.


#### Module

The Haskell language is made up of elementary constructs that can be viewed as forming the building blocks of the language. Things like data type declarations, function declarations, type aliases, type classes, etc.. The grouping of these elementary constructs into a namespace inside a single file, is a _Module_. For example, if you have the following code in Quotes.hs:

```haskell
module Quotables where

data Quote = LameQuote String | InspirationalQuote String deriving (Show)

isLameQuote :: Quote -> Bool
isLameQuote (LameQuote _) = True
isLameQuote (InspirationalQuote _) = False
```

This defines a `Quotables` module within which the `Quote` data type and `isLameQuote` function are defined.


#### Library

When one or more modules are grouped together, that makes for a library. The grouping together could be for whatever reason, for example, the modules when taken together provide the implementation for a certain task. It is worth mentioning that the visibility of the modules contained in a library can be configured. With _hidden_ modules being modules that serve as implementation details, not meant for public consumption, while _exposed_ modules can be used. These kind of information are the kind of metadata specified via the Cabal package format in the `.cabal` files. Later in this post, we would see how to [configure the visibility of modules within a library](#configuring-visibility-of-modules-in-a-library). 

#### Executable

An executable, on the other hand, is a module with the `main :: IO ()` function defined. It is a module that when compiled creates a binary that can be executed. The `main :: IO ()` function it contains designates the start of the program when the binary is executed. It is safe to think of it as being similar to the main method in C++ and Java. 

#### Package

When Libraries or executables are assembled together turning them into a distributable unit, you have a package. A Cabal package is then a collection of one or more Haskell libraries or executable, and also providing a mechanism via which they can be shared and used by others. Packages provide not just grouping functionality, but it is also the delivery mechanism. Hence the accurate word in Haskell is Packages (and not library) when talking about the distributable unit of functionality that other developers can use (as a library) or that can be executed (on a computer).

> it should be noted that packages in Haskell could refer to Cabal packages or GHC packages. A Cabal package is a collection of uncompiled Haskell module in source form, while the GHC package is a collection of compiled Haskell modules. GHC packages are created from Cabal packages using build tools like `cabal-install` or `stack`. For now, we can safely ignore the existence of GHC packages as we will only be dealing with Cabal packages

A package may contain one or more libraries, one or more executables or it could contain both libraries and executables. This information is also specified in the `.cabal` files that define the content of packages.

#### Project
It is best to think of _project_ has a workspace in which the Haskell developer works when creating packages. It is the space, the directory that contain all the necessary artifacts: source files, configuration files, etc that are required to create, compile, and build Haskell packages.

Now that we have some disambiguation under our belt, let us get to the business of installing `cabal-install` and using it to create projects.

---

## <a name="installing-cabal-install"></a> Installing (or upgrading) cabal-install

A convenient way to install `cabal-install` is to use `ghcup`. As was mentioned in [Setting Up Haskell Development Environment: The Basics](https://schooloffp.co/2020/07/25/setting-up-haskell-development-environment-the-basics.html) The installation process be found [here](https://www.haskell.org/ghcup/). It is also possible to install `cabal-install` via an operating system’s package manager, for example, via [`homebrew`](https://formulae.brew.sh/formula/cabal-install) on a mac. 

Using `ghcup` is still recommended and perhaps arguable the most convenient way.

If you already have cabal installed, you can upgrade by running these two commands:

```
cabal update
cabal install cabal-install
```

Once the installation is completed, confirm by, checking the version:

```
$ cabal --version
cabal-install version 3.2.0.0
compiled using version 3.2.0.0 of the Cabal library
```

## Creating a new project

Creating a new project involves creating a directory, and running `cabal init` to initialize the project. This is shown in the following series of commands:

Create and change into directory:

```
$ mkdir firstproject
$ cd firstproject/
```

Run `cabal init`

```
$ cabal init

Guessing dependencies...

Generating LICENSE...
Warning: unknown license type, you must put a copy in LICENSE yourself.
Generating Setup.hs...
Generating CHANGELOG.md...
Generating Main.hs...
Generating firstproject.cabal...

Warning: no synopsis given. You should edit the .cabal file and add one.
You may want to edit the .cabal file and add a Description field.
```

The state of the directory after running `cabal init` can be seen:


```
$ tree
.
├── CHANGELOG.md
├── Main.hs
├── Setup.hs
└── firstproject.cabal

0 directories, 4 files
```

Let’s take a pause to talk about the files generated by running the `cabal init` command.

> Note that cabal initialization can be run in an interactive mode, to do that, pass the interactive flag. i.e by running `cabal init -i` or `cabal init --interactive`

#### CHANGELOG.md

As is evident in the name, this is a markdown file that should be used to capture the revision history for the project.

#### Main.hs

The `Main.hs` file is the default module created by the initialization process. It contains the following content:

```haskell
module Main where

main :: IO ()
main = putStrLn "Hello, Haskell!"
```
As can be seen, it contains a `main :: IO ()` which means when this module is compiled it would create an executable that when run will print out `"Hello, Haskell!"`

#### Setup.hs

Remember in the section above on Package, the difference between Cabal packages and GHC packages was mentioned; specifically that GHC packages are compiled versions of Cabal packages and the fact that tools like `cabal-install` or `stack` make use of the `Cabal` library when compiling down Cabal packages into GHC packages.

The `Setup.hs` files come in handy in the scenario where one needs to circumvent tools like `cabal-install` or `stack` for direct usage of the `Cabal` library when compiling Cabal packages. The `Setup.hs` is, in essence, a runnable Haskell program that can be further configured and used to compile Cabal packages.

This is an exceptional use case. Everyday Haskell development would probably never require the need for `Setup.hs` and hence it can be ignored or even safely deleted.

#### firstproject.cabal

The `firstproject.cabal` file contains the package description specified using the Cabal description format.  There are a couple of things to note. The content of the `.cabal` file is in a key-value pair format. These key-value pairs are then grouped together based on their purpose. These grouping form sections referred to as _stanzas_. Each grouping/section or more accurately stanza starts with a label which is a stanza type and name. The default grouping has no stanza label and is usually referred to as the meta-data. By default `.cabal` file also includes entries that are commented out. `--` precedes the commented out entries. All these can be seen in the content of `firstproject.cabal` shown below:



```
--- META-DATA SECTION STARTS HERE

cabal-version:       >=1.10
-- Initial package description 'firstproject.cabal' generated by 'cabal
-- init'.  For further documentation, see
-- http://haskell.org/cabal/users-guide/

name:                firstproject
version:             0.1.0.0
-- synopsis:
-- description:
-- bug-reports:
-- license:
license-file:        LICENSE
author:              School of FP
maintainer:          theschooloffp@gmail.com
-- copyright:
-- category:
build-type:          Simple
extra-source-files:  CHANGELOG.md

--- META-DATA SECTION ENDS HERE

executable firstproject -- stanza label
  main-is:             Main.hs
  -- other-modules:
  -- other-extensions:
  build-depends:       base >=4.13 && <4.14
  -- hs-source-dirs:
  default-language:    Haskell2010
```
 
The stanza label also come in pairs as can be seen above, the only difference is that there is no ":" between the stanza type and name  

## Building and Running Executables

The default files created by `cabal init` is a perfectly complete package that can be built and run without any modification.

To build, run `cabal build`:

```
$ cabal build
Resolving dependencies...
Build profile: -w ghc-8.8.4 -O1
In order, the following will be built (use -v for more details):
 - firstproject-0.1.0.0 (exe:firstproject) (first run)
Configuring executable 'firstproject' for firstproject-0.1.0.0..
Warning: The 'license-file' field refers to the file 'LICENSE' which does not
exist.
Preprocessing executable 'firstproject' for firstproject-0.1.0.0..
Building executable 'firstproject' for firstproject-0.1.0.0..
[1 of 1] Compiling Main             ( Main.hs, /Users/schooloffp/delete/firstproject/dist-newstyle/build/x86_64-osx/ghc-8.8.4/firstproject-0.1.0.0/x/firstproject/build/firstproject/firstproject-tmp/Main.o )
Linking /Users/schooloffp/delete/firstproject/dist-newstyle/build/x86_64-osx/ghc-8.8.4/firstproject-0.1.0.0/x/firstproject/build/firstproject/firstproject ..
```

This would compile and build the executable in the project.

To run, execute the command `cabal run`:

```
$ cabal run
Up to date
Hello, Haskell!
```

The `cabal run` command can be executed without first running `cabal build`, doing that, `cabal-install` will make sure to first build in order to generate the executable that would be run.

So far so good, we have a project that contains an executable package which we were able to build and run. Thing is, this is not really that much different from what we ended the [Setting Up Haskell Development Environment: The Basics](https://schooloffp.co/2020/07/25/setting-up-haskell-development-environment-the-basics.html) post with. Where we were able to compile a Haskell source, which generated a binary that we were able to execute.

In that same post, it was stated that one of the reasons why build tools are needed in Haskell development is that they help with various tasks that are related/needed to the tasks of compiling, building, and running Haskell software. One such is the task of dependency management and this is what we are going to look at next.


## Adding dependencies

Haskell packages are distributed as Cabal packages and can be hosted and made available via a software repository. [Hackage](https://hackage.haskell.org/) is the central package archive of open-source software. Packages hosted on Hackage are publicly available to everyone and anyone and can be downloaded freely. By default `cabal-install` fetches packages from Hackage, although this can be configured to use another repository: for example a private company-wide repository.

To illustrate how to add dependencies to a package we will be modifying the package we got when we ran `cabal init` and we will be adding two packages, from Hackage as a dependency. The packages are [emoji](https://hackage.haskell.org/package/emoji) an emoji utility, and [haskell-say](https://hackage.haskell.org/package/haskell-say) which decorates texts printed to the console with ASCII art of a callout from the Haskell logo.

To add these packages as dependencies edit the executable stanza in the `firstproject.cabal` file to be:

```
executable firstproject
  main-is:             Main.hs
  -- other-modules:
  -- other-extensions:
  build-depends:       base >=4.13 && <4.14,
                       -- adds emoji as dependency
                       emoji == 0.1.0.2, 
                       -- adds haskell-say as dependency
                       haskell-say ^>= 1.0.0.0
  -- hs-source-dirs:
  default-language:    Haskell2010
```

> A quick note about how the dependencies are specified, especially their version numbers. Cabal description format allows the specification of version ranges. This means it is possible to not only specify the exact version you need, but you can also specify multiple or ranges of versions that can satisfy your requirement. `cabal-install` will then take this information and use it to find the appropriate versions that would satisfy the dependency requirement and that can be successfully compiled. Operators are used in specifying these version ranges. Some of the operators can be seen in the code snippet above. For example,`>=`, `==` and `^>=`. These are mostly equality operators. The `>=` is used to specify versions greater or equal to, while `==` is used to specify an exact version etc. The `^>=` operator can be seen as a shorthand for >= x.y.z && < x.(y + 1). This shows that logical operators like && and \|\| can also be used. Finally, it should be noted that Haskell versioning follows, PVP which is the Haskell [Package Versioning Policy](https://pvp.haskell.org/)    

Now that we have added these two dependencies, we can use them.  Update the `Main.hs` file as follow:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Emoji
import HaskellSay (haskellSay)

main = mapM_ (haskellSay . ("You want some " <>)) ( unicodeByName "pizza")
```
 
Now build and run by executing `cabal run`

It is possible you run into the following error:

```
firstproject.cabal:26:47: error:
unexpected major bounded version syntax (caret, ^>=) used. To use this syntax the package need to specify at least 'cabal-version: 2.0'. Alternatively, if broader compatibility is important then use: >=1.0.0.0 && <1.1
expecting "." or "-"

   25 |                        emoji == 0.1.0.2,
   26 |                        haskell-say ^>= 1.0.0.0
      |                                               ^  

```
That is fine. It will give an opportunity to touch on the meta-data section in the `.cabal` file. 

As can be seen from the error message, this error is due to the fact that a version operator is being used, namely `^>=` which is not supported by the version number of the Cabal format description that `cabal init` creates. The `cabal-version` field in the meta-data section of the `firstproject.cabal` needs to be updated to use version 2.0 or above. Like so:

```
cabal-version:       2.0
``` 

Once this has been updated, running `cabal run` again should give output similar to this:

```
$ cabal run
Resolving dependencies...
Build profile: -w ghc-8.8.4 -O1
In order, the following will be built (use -v for more details):
 - firstproject-0.1.0.0 (exe:firstproject) (configuration changed)
Configuring executable 'firstproject' for firstproject-0.1.0.0..
Warning: The 'license-file' field refers to the file 'LICENSE' which does not
exist.
Preprocessing executable 'firstproject' for firstproject-0.1.0.0..
Building executable 'firstproject' for firstproject-0.1.0.0..
[1 of 1] Compiling Main             ( Main.hs, /Users/schooloffp/delete/firstproject/dist-newstyle/build/x86_64-osx/ghc-8.8.4/firstproject-0.1.0.0/x/firstproject/build/firstproject/firstproject-tmp/Main.o )
Linking /Users/schooloffp/delete/firstproject/dist-newstyle/build/x86_64-osx/ghc-8.8.4/firstproject-0.1.0.0/x/firstproject/build/firstproject/firstproject ...
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

This shows we were able to successfully add and use dependencies! 

But before we move on, let’s make a quick little modification to our package structure. 

It is always a good practice to put the source files that would be used to create an executable in a directory, instead of being in the root directory of the project. 

Hence create a `bin` directory (you can name this anything you want, by the way, nothing special about the usage of _bin_) and move the `Main.hs` file there:

```
mkdir bin
mv Main.hs bin/
```

Now update the `firstproject.cabal` file to indicate that the source for the executable can be found in the `bin` directory. This is done using the `hs-source-dirs` field within the executable stanza. After the update, the content of `firstproject.cabal` should look like this:

```
$ cat firstproject.cabal
cabal-version:       2.0
-- Initial package description 'firstproject.cabal' generated by 'cabal
-- init'.  For further documentation, see
-- http://haskell.org/cabal/users-guide/

name:                firstproject
version:             0.1.0.0
-- synopsis:
-- description:
-- bug-reports:
-- license:
license-file:        LICENSE
author:              School of FP
maintainer:          theschooloffp@gmail.com
-- copyright:
-- category:
build-type:          Simple
extra-source-files:  CHANGELOG.md

executable firstproject
  main-is:             Main.hs
  -- other-modules:
  -- other-extensions:
  build-depends:       base >=4.13 && <4.14,
                       -- adds emoji as dependency
                       emoji == 0.1.0.2,
                       -- adds emoji as dependency
                       haskell-say ^>= 1.0.0.0
  -- specifies the directory where the source can be found
  hs-source-dirs:      bin
  default-language:    Haskell2010
```

Execute `cabal run` to confirm everything still works fine.

## Having a project with both library and executable

Right now our package has only an executable. The next thing we are going to do is to introduce a library. To do this we are going to turn our project into one, which prints out quotes written in Emoji when executed. To achieve this, we make the following modifications:

Edit `firstproject.cabal` to include the library stanza as follows:

```
library
  exposed-modules:     EmojiQuotes
  build-depends:       base >=4.13 && <4.14,
                     -- adds emoji as dependency
                       emoji == 0.1.0.2
  hs-source-dirs:      lib
  default-language:    Haskell2010
```

The full updated `firstproject.cabal` should not look like this:

```
cabal-version:       2.0
-- Initial package description 'firstproject.cabal' generated by 'cabal
-- init'.  For further documentation, see
-- http://haskell.org/cabal/users-guide/

name:                firstproject
version:             0.1.0.0
-- synopsis:
-- description:
-- bug-reports:
-- license:
license-file:        LICENSE
author:              School of FP
maintainer:          theschooloffp@gmail.com
-- copyright:
-- category:
build-type:          Simple
extra-source-files:  CHANGELOG.md

executable firstproject
  main-is:             Main.hs
  -- other-modules:
  -- other-extensions:
  build-depends:       base >=4.13 && <4.14,
                       -- adds haskell-say as dependency
                       haskell-say ^>= 1.0.0.0
  hs-source-dirs:      bin
  default-language:    Haskell2010

library
  exposed-modules:     EmojiQuotes
  build-depends:       base >=4.13 && <4.14,
                       -- adds emoji as dependency
                       emoji == 0.1.0.2
  hs-source-dirs:      lib
  default-language:    Haskell2010
```

 This tells `cabal-install` that the package not only contains an executable but also a library. The module contained in the library, for now, will be called `EmojiQuotes`. This is specified using the `exposed-modules` field. The `hs-source-dirs` field also specifies that the source for the library would be found in a directly named `lib`. 

The next thing to do is to create the `lib` directory and add the `EmojiQuotes` module:

```
$ mkdir lib
$ touch lib/EmojiQuotes.hs
```

Then edit `EmojiQuotes.hs` to have the following content:

```haskell
module EmojiQuotes where
-- content of the module goes in here
```

Run `cabal build`. If no error then you have everything all set up. The next thing would be to edit both `EmojiQuotes.hs` and `Main.hs` to implement our little Emoji Quotes program. The following code snippets show how that would look like.

In `EmojiQuotes.hs` add a list of quotes together with a function that selects one from the list:

```haskell
module EmojiQuotes where

import Data.Emoji

quotes =
 [(pure "Do not beat a "
   <> (unicodeByName "coffin")
   <> (pure " ")
   <>  (unicodeByName "horse")),
 (pure "Every "
   <> (unicodeByName "dog")
   <> (pure " ")
   <> (pure "has its ")
   <> (unicodeByName "calendar")),
 (unicodeByName "heart")
   <> (pure " is ")
   <> (unicodeByName "dark_sunglasses"),
 (pure "Don't ")
   <> (unicodeByName "sob")
   <> (pure " over spilt milk"),
 (unicodeByName "hammer")
   <> (pure " the ")
   <> (unicodeByName "round_pushpin")
   <> (pure " over the ")
   <> (unicodeByName "face_with_head_bandage")
 ]

selectQuote :: Int -> Maybe String
selectQuote i = if  i > (length quotes)
                then Nothing
                else (quotes !! i)


numberOfQuotes = length quotes
```

Then in `Main.hs` edit to make use of the `EmojiQuotes` module:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import EmojiQuotes

main = do
     putStrLn ("Type in a number between 1 and " ++ show numberOfQuotes)
     index <- getLine
     case selectQuote (read index :: Int) of
       Just s -> putStrLn s
       _      -> putStrLn ("No quote found at index " ++ show index)
```

What this does is to read an integer value from stdout and retrieve a quote from the list at that index.

Running `cabal build` or `cabal run` at this point will lead to a compilation error with an error message that should look like this:

```
bin/Main.hs:5:1: error:
    Could not load module ‘EmojiQuotes’
    It is a member of the hidden package ‘firstproject-0.1.0.0’.
    Perhaps you need to add ‘firstproject’ to the build-depends in your .cabal file.
    Use -v (or `:set -v` in ghci) to see a list of the files searched for.
  |
5 | import EmojiQuotes
  | ^^^^^^^^^^^^^^^^^^
cabal: Failed to build exe:firstproject from firstproject-0.1.0.0.

```

The reason for this error is that the executable package is now dependent on the `‘EmojiQuotes’`, but we did not update the `firstproject.cabal` file to reflect this. This means we need to specify the package as a dependency on itself since the required module is contained in the project. To fix this update the executable stanza in `firstproject.cabal` as follows:

```
executable firstproject
  main-is:             Main.hs
  -- other-modules:
  -- other-extensions:
  build-depends:       base >=4.13 && <4.14,
                       -- adds emoji as dependency
                       emoji == 0.1.0.2,
                       -- adds emoji as dependency
                       haskell-say ^>= 1.0.0.0,
                       -- adds itself as dependency 
                       -- to be able to access the library
                       firstproject 
  hs-source-dirs:      bin
  default-language:    Haskell2010
```

Now running `cabal run` would build and run the executable, which leads to a prompt where a number can be inputted, which then returns a quote. You should have something similar to this:


```
$ cabal run
Up to date
Type in a number between 1 and 5
1
Every 🐶 has its 📆
```

This demonstrates the ability to have a library and an executable in the same project. In the case above, the emoji quotes are placed in the library, which the executable made use of.

## Configuring visibility of modules in a library

Most of the time, not all modules that make up a library are intended to be accessible. Some modules are implementation details and should be hidden from  other developers who make use of the library. 

As stated in the introductory section Cabal provides the ability to configure the visibility of modules in a library. Modules that serve as implementation details can be configured as hidden while modules meant to be used by other developers can be configured as exposed modules.

To illustrate this, we will split the `EmojiQuotes` module into two. We will extract the list of quotes into another module named `QuotesStore.hs`. `QuotesStore.hs` would be marked as hidden while  `EmojiQuotes` would remain accessible.

To do this, edit `firstproject.cabal` to introduce the `QuotesStore.hs`. The library section should look like this:

```
library
  exposed-modules:     EmojiQuotes
  other-modules:       QuotesStore
  build-depends:       base >=4.13 && <4.14,
                       -- adds emoji as dependency
                       emoji == 0.1.0.2
  hs-source-dirs:      lib
  default-language:    Haskell2010
```

`QuotesStore` has been added as _other-modules_, while `EmojiQuotes` remains as `exposed-modules`. Modules specified as _other-modules_ are hidden, hence only `EmojiQuotes` would be accessible.

Add the `QuotesStore.hs` to the `lib` directory with the following content:

```haskell
module QuotesStore where
import Data.Emoji

quotes = 
 [(pure "Do not beat a " 
   <> (unicodeByName "coffin") 
   <> (pure " ") 
   <>  (unicodeByName "horse")), 
 (pure "Every " 
   <> (unicodeByName "dog") 
   <> (pure " ") 
   <> (pure "has its ") 
   <> (unicodeByName "calendar")),
 (unicodeByName "heart") 
   <> (pure " is ") 
   <> (unicodeByName "dark_sunglasses"),
 (pure "Don't ") 
   <> (unicodeByName "sob") 
   <> (pure " over spilt milk"),
 (unicodeByName "hammer") 
   <> (pure " the ") 
   <> (unicodeByName "round_pushpin") 
   <> (pure " over the ") 
   <> (unicodeByName "face_with_head_bandage")
 ]
```

Update `EmojiQuotes.hs` to use `QuotesStore` module as follows:

```haskell
module EmojiQuotes where

import QuotesStore

selectQuote :: Int -> Maybe String
selectQuote i = if  i > (length quotes)
                then Nothing
                else (quotes !! i)
                   
numberOfQuotes = length quotes
```

Now execute `cabal run` and everything should work as before.

To confirm that `QuotesStore` is indeed hidden, add it as an import in `Main.hs`, that is:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

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

Executing `cabal run` should lead to a compilation error with the following error message:

```
bin/Main.hs:27:1: error:
    Could not load module ‘QuotesStore’
    it is a hidden module in the package ‘firstproject-0.1.0.0’
    Use -v (or `:set -v` in ghci) to see a list of the files searched for.
   |
27 | import QuotesStore
   | ^^^^^^^^^^^^^^^^^^

```

> it should be noted that hidden modules are not limited to libraries, executables can also have hidden modules.

So far, we have been able to create a package with both an executable and library and also have external packages and local packages as dependencies. We were also able to configure the visibility of modules within the library. We compiled, built, and ran the executable in the package and everything works as expected. 

The next thing to look at is one of the stated benefits of using a build tool like cabal/stack in [Setting Up Haskell Development Environment: The Basics](https://schooloffp.co/2020/07/25/setting-up-haskell-development-environment-the-basics.html) and that is how to work with various version of GHC across different projects. 

## Using different GHC compiler across different projects

The goal is to be able to have two (or more) projects and be able to specify that they use different GHC versions. To illustrate this we will create another project by running `cabal init` and set that to use a different version of GHC from the one used in the `EmojiQuote` project.

Make a new directory called `secondproject` and run `cabal init`. You should see an output similar to the following

```
$ mkdir secondproject
$ cd secondproject/
$ cabal init

Guessing dependencies...

Generating LICENSE...
Warning: unknown license type, you must put a copy in LICENSE yourself.
Generating Setup.hs...
Generating CHANGELOG.md...
Generating Main.hs...
Generating secondproject.cabal...

Warning: no synopsis given. You should edit the .cabal file and add one.
You may want to edit the .cabal file and add a Description field.
$ tree
.
├── CHANGELOG.md
├── Main.hs
├── Setup.hs
└── secondproject.cabal
```

### Installing multiple versions of GHC

The different GHC version needs to be installed first in order to be able to specify them per project. 

To do that we use `ghcup` which can also help in installing multiple versions of GHC. 

First of all, let us check the current version of GHC installed:

```
$ ghc --version
The Glorious Glasgow Haskell Compilation System, version 8.8.4
```

To see the various version `ghcup` can install run `ghcup list`. You should see output similar to the following:

```
$ ghcup list
[ Warn  ] New GHCup version available: 0.1.9. To upgrade, run 'ghcup upgrade'
[ Warn  ] New GHC version available: 8.10.2. To upgrade, run 'ghcup install ghc 8.10.2'
   Tool  Version Tags                                 Notes
✗  ghc   7.10.3  base-4.8.2.0
✗  ghc   8.0.2   base-4.9.1.0
✗  ghc   8.2.2   base-4.10.1.0
✗  ghc   8.4.1   base-4.11.0.0
✗  ghc   8.4.2   base-4.11.1.0
✗  ghc   8.4.3   base-4.11.1.0
✗  ghc   8.4.4   base-4.11.1.0
✗  ghc   8.6.1   base-4.12.0.0
✗  ghc   8.6.2   base-4.12.0.0
✗  ghc   8.6.3   base-4.12.0.0
✗  ghc   8.6.4   base-4.12.0.0
✗  ghc   8.6.5   base-4.12.0.0
✗  ghc   8.8.1   base-4.13.0.0
✗  ghc   8.8.2   base-4.13.0.0
✗  ghc   8.8.3   base-4.13.0.0
✔✔ ghc   8.8.4   recommended,base-4.13.0.0
✗  ghc   8.10.1  base-4.14.0.0
✗  ghc   8.10.2  latest,base-4.14.1.0
✗  cabal 2.4.1.0
✗  cabal 3.0.0.0
✔✔ cabal 3.2.0.0 latest,recommended
✗  ghcup 0.1.9   latest,recommended
```

This not only shows the available GHC versions but also cabal and ghcup versions that can be installed. It also indicates the versions that have already been installed.

We will install GHC-8.8.3 which is one version behind the currently installed version. This is done by running the following command

```
$ ghcup install ghc 8.8.3
``` 

This should take a while. After installation run `ghcup list` again to confirm that GHC version 8.8.3 has been installed. `ghcup` installs GHC’s in `~/.ghcup/ghc/` so another way to confirm is to check that directory:

```
$ ls ~/.ghcup/ghc/
8.8.3 8.8.4
```

> it should be noted that stack takes a different approach for specifying different GHC versions per project than `cabal-install`. `cabal-install` requires the developer to download the needed GHCs versions, `stack` on the other hand takes care of downloading and setting the required GHC person per project. How to do this will be covered in the next post which will introduce stack.

Now that we have two versions of GHC, we will explicitly specify the `firstproject` to use GHC 8.8.4, while we specify `secondproject` to use GHC 8.8.3

We do that by introducing the `cabal.project` file, which is a file that allows explicit specification of the configurations to be applied to packages found in a cabal project, properties like which GHC version to use.

Within the `firstproject` directory create a `cabal.project` file with the following content:

```
-- Specifies the location of package source
-- The . specifies the package source is to be found in the current directory
packages: .

-- specifies the binary of the ghc version to be used
-- Full path can also be used if binary not in program path
-- with-compiler: /Users/schooloffp/.ghcup/bin/ghc-8.8.4
with-compiler: ghc-8.8.4
```

Notice that versions of GHC specified via the `with-compiler` field. This is how a specific GHC version can be configured for different projects on the same system.

Within the `secondproject` directory create a `cabal.project` file with content similar to the following:

```
packages: .

with-compiler: ghc-8.8.3
```

To now build, you can run `cabal build`. When building the package in the `firstproject` you see lines similar to the following in the output:

```
$ cabal build
Resolving dependencies...
Build profile: -w ghc-8.8.4 -O1
```

Indicating GHC version 8.8.4 is used, while when building the package in the `secondproject` you see lines similar to the following in the output:

```
$ cabal build
Resolving dependencies...
Build profile: -w ghc-8.8.3 -O1
```

Indicating GHC version 8.8.4 is used


In the preamble section, a Haskell _project_ is described as the workspace (or the directory) in which the Haskell developer works when creating packages. By default, there is one package, implicitly created per project. And this package is defined by all the files that are created in the root of the directory where `cabal init` is executed. But a Haskell project can contain multiple packages. In such a case, subdirectories are created per package.

For example, the directory structure for a project with 2 packages would look like this:

``` 
$ tree -L 2
.
└── packageone
    ├── CHANGELOG.md
    ├── bin
    ├── packageone.cabal
    └── lib
└── packagetwo
    ├── CHANGELOG.md
    ├── bin
    ├── packagetwo.cabal
    └── lib
- cabal.project
```

And the `cabal.project` would have to contain a configuration that points out where the packages are located, that is something like this:


```
packages: packageone/
          packagetwo/
```

[summoner](https://github.com/kowainik/summoner) is an example of a real-life Haskell project that uses this pattern. The `cabal.project` file for the project can be found [here](https://github.com/kowainik/summoner/blob/master/cabal.project) with its content as:

```
packages:
    summoner-cli/
    summoner-tui/

tests: true
```

Which defines  `summoner-cli/` and `summoner-tui/` as the two packages contained in the project.

In such a scenario where there are multiple packages defined in `cabal.project`, the command `cabal build all` is used to build all the packages at once.

## How to execute a Haskell file as a script

In [Setting Up Haskell Development Environment: The Basics](https://schooloffp.co/2020/07/25/setting-up-haskell-development-environment-the-basics.html) we saw how to execute a file containing Haskell source code as a script. We can achieve the same with Cabal. Running Haskell source as script via Cabal has the additional benefit of allowing dependencies to be specified and used within the script!

For example the following code snippet in a file named `haskellsay.hs` shows how to have a script that prints out text to the console using the `haskell-say` package.

In `haskellsay.hs`:

```haskell
#!/usr/bin/env cabal
{- cabal:
       build-depends: base >=4.13 && <4.14, haskell-say
-}

module Main where

import HaskellSay (haskellSay)

main :: IO ()
main =
  haskellSay "Running as a script"
``` 

The preambles at the top specify cabal as the interpreter. It also specify the dependencies the script uses. This would be downloaded and made available before the script runs.

Make the script executable:

```
chmod +x haskellsay.hs
```

And then running it should give an output similar to this:

```
$ ./haskellsay.hs
Resolving dependencies...
Build profile: -w ghc-8.8.4 -O1
In order, the following will be built (use -v for more details):
 - fake-package-0 (exe:script) (first run)
Configuring executable 'script' for fake-package-0..
Preprocessing executable 'script' for fake-package-0..
Building executable 'script' for fake-package-0..
[1 of 1] Compiling Main             ( Main.hs, /var/folders/fd/809yl44x5fz1qrjqj77g4gkc0000gp/T/cabal-repl.-7614/dist-newstyle/build/x86_64-osx/ghc-8.8.4/fake-package-0/x/script/build/script/script-tmp/Main.o )
Linking /var/folders/fd/809yl44x5fz1qrjqj77g4gkc0000gp/T/cabal-repl.-7614/dist-newstyle/build/x86_64-osx/ghc-8.8.4/fake-package-0/x/script/build/script/script ...
  ________________________________________________________
 /                                                        \
| Running as a script                                      |
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

`cabal-install` can also be used to install executables. It should be noted that `cabal-install` is not a full-fledged package manager and should not be seen as a replacement for tools like [homebrew](https://brew.sh/), [chocolatey](https://chocolatey.org/), or even the native package manager that comes with Gnu/Linux systems. If the choice exists, prefer to manage installed software via these established package managers instead of using `cabal-install`. Having said that, software, i.e. executable created in Haskell and distributed via Hackage can be installed via `cabal-install`.

To illustrate this, we will install [titlecase](https://hackage.haskell.org/package/titlecase), a command-line utility that converts English words to title case.

To install it run the following command:

```
cabal install titlecase
```

> note if you encounter an output that contains _"cabal: installdir is not defined. Set it in your cabal config file or use --installdir=<path>"_ then the installation did not go well. You will have to first run the following command `cabal user-config update` and then rerun the `cabal install` command again. This is a known issue, which has not been fixed at the time of writing this post. For more, see the issue <a href="https://github.com/haskell/cabal/issues/5973">here</a> on Github.

Run `which titlecase` to confirm the installation was successful. The command should show an output similar to the following:

```
$ which titlecase
/Users/schooloffp/.cabal/bin/titlecase
```    

Using the installed titlecase utility:

```
$ titlecase this turns sentences to title case
This Turns Sentences to Title Case
```

The next question might be, how might one uninstall the `titlecase` package? Well, unfortunately `cabal-install` does not provide any automated way to remove installed binaries, and one needs to track down and manually remove installed packages. This is part of the reasons why a proper package manager should be used instead of `cabal-install`. 

## Historical Notes

To wrap this post up we would touch on a couple of key historical details that should help appreciate the current landscape of Cabal as a build tool and help clarify some details that are remnant of historical artifacts.  

#### Cabal hell

For a long time, `cabal-install` was plagued with often cryptic version incompatibility issues that lead to compilation errors that were often hard to debug. This situation is often referred to as _cabal hell_ and it hampered usability and productivity. In fact, `stack` sprung up as an alternative build tool largely as a response to the frequent issues users face when using `cabal-install`. 

Fortunately over the years work has been done to improve the state of things with `cabal-install` and it can be confidently stated that  _cabal hell_ is more or less a resolved issue. Interestingly enough `stack` took a different approach to prevent version incompatibility issues when compared to the approach `cabal-install` took. `stack` took the approach of having a curated package set, while `cabal-install` employed an approach similar to what is obtained in Nix. As a beginner knowing the details of how the issue was fixed is not needed to be able to use either `cabal-install` or `stack`, but it is good to know that going with either `cabal-install` or `stack` is now a matter of preference and UX choices. 
 

#### Cabal Sandbox has been deprecated 

cabal sandboxes were one of the attempts that were proposed and implemented as a solution to the cabal hell problem.  Any cursory search on cabal would probably lead to cabal sandboxes turning up. The thing to note though is that even though it helped in managing the cabal hell problem, it was not the most optimal approach. In fact, since version 3.4 of `cabal-install` ability to create sandboxes has been removed. See this [Github issue](https://github.com/haskell/cabal/issues/6445#issuecomment-629146662) for more details. 

#### A word about cabal command prefixes 

Another thing one will quickly encounter when approaching `cabal-install`, are the command prefixes. That is the  `v2-*` and `new-*` prefixes. For example instead of just running `cabal build` it is possible to run `cabal v2-build` or `cabal new-build`.

What exactly does this prefix mean? And are they necessary? 

This is another artifact of history. 

When the new commands that make use of the nix-style approach to fix the cabal hell issue were being developed, they were prefixed by `new-*` prefix and later by `v2-*`. All these while the non prefixed version of these commands remained problematic. With version 3 of `cabal-install`, the default commands, that is, commands without the prefix now use the implementations that fix the cabal hell problem. Although the prefix `v2-*` and `new-*` still works, it is best practice to no longer use them.

This ends the whirlwind tour of cabal for beginners. Next up we would take a look at `stack`.


