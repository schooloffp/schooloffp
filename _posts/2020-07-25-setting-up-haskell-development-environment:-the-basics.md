---
layout: post
author: School Of FP
excerpt_separator: <!--end_excerpt-->
---

This post is going to outline the procedure for setting up a Haskell development environment.  

This post differs from most posts around setting up a Haskell development environment in the sense that it does not directly jump into Cabal or Stack. Instead, it first provides some background information that makes it possible to understand the basics of the development environment in Haskell, the different moving parts, and how those come together in turning Haskell source code into an executable which can then be run.  

<!--end_excerpt-->

A separate post will build on the information provided here and show how to make use of Stack or Cabal.

# The Big Picture

Haskell programs are compiled native executable binaries. Haskell programs are compiled. Software is written in Haskell, which by the way is a high-level programming language that is first converted into machine language which then gets executed. In this sense Haskell is different from languages like JavaScript and Python, which are interpreted language, but similar to languages like Java and Go, which are also compiled. It should be noted that it is also possible to run Haskell programs like an interpreted language. We get to this pretty soon, but most of the time, Haskell programs are compiled. 

The Haskell compilation process produces native executive binaries. The keyword here is native. This means once a Haskell program is compiled, it does not need anything else to be able to run. This is similar to languages like Rust and Go, which also produce native executive binaries, but differ from languages like Java and C#, which requires a runtime to be first installed on the target computer before the compiled program can be executed.

The job of converting software written in a high-level programming language to machine language is done by a compiler. In the case of Haskell, the compiler is known as [GHC (The Glasgow Haskell Compiler)](https://www.haskell.org/ghc/). Sometimes GHC is referred to as The Glorious Glasgow Haskell Compiler or Glorious Haskell Compiler. GHC is not the only Haskell compiler in existence; others exist. Some other ones are [UHC (Utrecht Haskell Compiler)](https://github.com/UU-ComputerScience/uhc), [LHC (LLVM Haskell Compiler)](https://github.com/Lemmih/lhc), and [HUGS (Haskell User´s Gofer System)](https://wiki.haskell.org/Hugs). Even though other compilers exist, GHC has evolved to be the one true Haskell compiler, as it is by far the most used Haskell compiler. The other compilers are not as used or they are no longer supported. Haskell Wiki [lists](https://wiki.haskell.org/Implementations) all the other compilers for Haskell. 

To setup, a Haskell development environment is then essentially setting up GHC, and be able to use it to compile Haskell source code to executable binaries.

# How to get GHC

The method for getting GHC installed varies depending on the operating system and the installation methods. This post will only focus on the installation on macOS and Linux (Ubuntu).

> If you are already familiar with Haskell, this is the point where you probably expect Cabal or Stack to be introduced. But on the contrary, Cabal/Stack won't be mentioned yet. The idea is to install GHC without these tools in other to get a feeling of how things work behind the scenes. Doing this will also help appreciate the motivation for tools like Cabal and Stack exist within Haskell. 

For installation on macOS [`ghcup`](https://www.haskell.org/ghcup/) will be used while installation will be done using the Software repository on Ubuntu (Linux).

## What is ghcup

`ghcup` describes itself as the Haskell (GHC) toolchain installer. Similar in scope to [rustup](https://github.com/rust-lang/rustup) with Rust. It provides the mechanism to install all the necessary components required for developing Haskell. Together these components are sometimes referred to as the Haskell toolchain, of which GHC is a constituent.

## What is Haskell Toolchain

Haskell toolchain is often used to refer to all the necessary components required for developing Haskell. It comprises of the compiler together with other auxiliary tools. As at the writing of this post, here is a list of what makes up the toolchain:

- GHC: The Haskell batch compiler. This is used for compiler source files into either libraries and executables.  Libraries are used as dependencies in other Haskell projects while executable are binaries that are run.

- GHCi: An interactive Haskell interpreter, which allows making use of GHC in an interactive environment. GHCi is what powers the Haskell REPL

- runghc: A shell script that allows running Haskell as a script. Find more information on how to use `runghc` [here](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/runghc.html) 

- runhaskell: Similar to `runghc` but allows the ability to swap the Haskell compiler from `ghc` to something else.

- Haddock: a tool for automatically generating documentation from annotated Haskell source code. See [here](https://www.haskell.org/haddock/) for more information about Haddock.

- hp2ps: a profile tool for Haskell programs. See [here](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/profiling.html) for more on how to use the Haskell profiler.

- hpc: a code coverage tool for Haskell programs. See [here](https://wiki.haskell.org/Haskell_program_coverage) for more information.

- hsc2hs: A preprocessor that helps with writing Haskell bindings to C code. See [here](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/utils.html#writing-haskell-interfaces-to-c-code-hsc2hs) for more information.

> I will like to note that the official download page is quite confusing. It refers to a Haskell platform which in reality no longer exists. It also presents 3 different classes of Haskell installation: Minimal Installation/Stack/Haskell Platform which is also really not the best representation of the process of installing Haskell. It is best to view installing Haskell as the process of getting the Haskell toolchain. This can be done by building from source, using a tool like `ghcup` or `Cabal` or `Stack`. The confusing state of the Download page is known, as can be seen from these two issues that have been created to resolve the problem: [Download page is confusing](https://github.com/haskell-infra/www.haskell.org/issues/12) and [Download Page: Rearrangement of sections](https://github.com/haskell-infra/www.haskell.org/pull/14). As of the writing of this post, these issues have not been resolved.

Now that we know what the Haskell toolchain comprises of, let us look at some of the ways of getting it installed. First, we start by looking at how to install it on a Mac using `ghcup`.

# Installing Haskell Toolchain on macOS via ghcup

There are a couple of ways to install `ghcup` but the most straightforward approach is to follow the instructions found at [https://www.haskell.org/ghcup/](https://www.haskell.org/ghcup/).

The installation process involves executing a command in the terminal and then following the prompts.

After installation is done, you will see a final prompt that may look like this:

```
Installation is done!

To run `ghc` and cabal, you need to adjust your PATH variable.
You may want to source '/Users/schooloffp/.ghcup/env' in your shell
configuration to do so (e.g. ~/.bashrc).

Detected bash shell on your system...
If you want ghcup to automatically add the required PATH variable to "/Users/schooloffp/.bashrc"
answer with YES, otherwise with NO and press ENTER.
```

This is about making the related binaries `ghcup` installed available in the terminal, so it is a good idea to type YES and have the `.bashrc` modified.

Note that on macOS, the `.bashrc` file is not run automatically. See this [stackoverflow answer](https://apple.stackexchange.com/a/13019). This means the above process of `ghcup` modifying the `.bashrc` will not suffice to get the needed binary available in the terminal. There are a couple of ways to remedy this. One way, as also noted in the StackOverflow thread is to add `source ~/.bashrc` to the `~/.profile`.

Once this is done, you can then execute the installed binaries from the terminal. For example:

```
ghc --version
The Glorious Glasgow Haskell Compilation System, version 8.8.4

ghci --version
The Glorious Glasgow Haskell Compilation System, version 8.8.4

haddock --version
Haddock version 2.23.0, (c) Simon Marlow 2006
Ported to use the GHC API by David Waern 2006-2008

```

> An inquisitive reader would have also found that apart from the binaries listed as a constituent of the Haskell Toolchain, a cabal binary has also been installed as part of the `ghcup` installation process. This is indeed the case. `ghcup` also installs cabal, but we will ignore cabal for now and talk about it in subsequent posts. 


# Installing Haskell Toolchain on Ubuntu (Linux) via Software Repository

It should be noted that the above method of using `ghcup` to install the Haskell toolchain will also work on a Linux machine. The main objective of the installation process is to get necessary binaries unto the system, and most Linux distribution has a procedure for this already using their software package management. Hence it is also possible to use those to install the Haskell toolchain.

The only drawback is that most of the time, the Haskell version found in the official software repositories of most Linux distribution would be out of date. 

At least that is the case with Ubuntu. Running the command below: 

```
 apt-cache policy haskell-platform
haskell-platform:
Installed: (none)
Candidate: 2014.2.0.0.debian4
Version table:
2014.2.0.0.debian4 500
500 http://nl.archive.ubuntu.com/ubuntu bionic/universe amd64 Packages
500 http://nl.archive.ubuntu.com/ubuntu bionic/universe i386 Packages
```

Shows that the Debian package version found for Haskell is 2014.2.0.0.debian4. And if we find this on [Launchpad](https://launchpad.net/ubuntu/+source/haskell-platform/2014.2.0.0.debian4) we see this was uploaded 2016.

On installation by running `sudo apt-get install haskell-platform`, we can then check the version of `ghc` that is installed:

```
ghc --version
The Glorious Glasgow Haskell Compilation System, version 8.0.2
```

This shows version 8.0.2. Even though, at the time of writing this post, the most recent version is 8.8.4

Therefore to get the latest Haskell Toolchain installed on Ubuntu, [hpv's PPA](https://launchpad.net/~hvr/+archive/ubuntu/ghc) can be used.

> For some background information about what PPA (Personal Package Archive) is, see [Packaging/PPA](https://help.launchpad.net/Packaging/PPA)

Adding the PPA:

```
sudo add-apt-repository ppa:hvr/ghc
sudo apt-get update
```

Installing Haskell:

```
// sudo apt-get install ghc-x.y.z where x.y.z is the version to install

sudo apt-get install ghc-8.8.3
```

> At the moment of writing this, even though 8.8.4 is the recent version, the latest version available via install via the PPA is version 8.8.3

The installation processes put the binaries in `/opt/ghc`

```
ls /opt/ghc
8.8.3 bin 
```

Hence to have the binary available in the terminal, updating the path is one approach:

```
export PATH=$PATH:/opt/ghc/bin
```

After which the installation can be confirmed:

```
ghc --version
The Glorious Glasgow Haskell Compilation System, version 8.8.3 
```

# Installing Haskell Toolchain on Windows

Window users can use [chocolatey](https://chocolatey.org/) to install Haskell. In summary, once you have `chocolatey` installed the command to be run is:

```
choco install haskell-dev
refreshenv
```

# Using the Haskell compiler

Now that we have the Haskell toolchain installed we would go ahead and show how to compile Haskell source to an executable binary. This is done using `ghc`.

In a file located at `~/hello.hs`, we have the following code:

```haskell
main :: IO ()
main = putStrLn "Hello, World!"
```

To compile:

```
ghc hello.hs -o hello_bin
[1 of 1] Compiling Main             ( hello.hs, hello.o )
Linking hello_bin ...
```

This compiles and generates the binary which is redirected to `hello_bin`. To execute the generate binary:

```
 ./hello_bin
Hello, World!
```

And that shows how `ghc` can compile the Haskell source, generate a binary that can be executed.

# Using the Haskell REPL
Haskell comes with a REPL environment. This is implemented in `ghci`. Starting the REPL is as simple as running the `ghci` binary:

```
ghci
GHCi, version 8.8.4: https://www.haskell.org/ghc/  :? for help
Prelude> 1 + 1
2
```

`ghci` supports a lot of functionalities. For example, it is possible to load a Haskell file into the REPL and execute it from there. Check the [GHCi section](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html) of the GHC user guide to learn more about the various things that can be done within the REPL.

```
ghci
GHCi, version 8.8.4: https://www.haskell.org/ghc/  :? for help
Prelude> :l ~/hello.hs
[1 of 1] Compiling Main             ( ~/hello.hs, interpreted )
Ok, one module loaded.
*Main> main -- the main function can then be called in the REPL
Hello, World!
*Main>
```

# Using Haskell as an interpreter.

`ghc` also supports having Haskell interpreted. This means instead of first compiling, and then run, it is also possible to run Haskell source code directly. This is done using either `runghc` or `runhaskell`. For example:

```
runghc hello.hs
Hello, World!
```

or 

```
runhaskell hello.hs
Hello, World!
```

And as stated before, the difference between `runghc` and `runhaskell` is that `runhaskell` provides the ability to swap the Haskell compiler from `ghc` to something else. Although in practical terms, since `ghc` has grown to become the de facto Haskell compiler, `runhaskel` is somewhat redundant.

The interesting bit about `runghc` (and `runhaskell`) is that they allow the possibility to use Haskell as a scripting language. For instance, to run the hello world as a script, update the `hello.hs` file to include a `shebang` directive that points to `runghc`. This looks like this:

```haskell
#!/usr/bin/env runghc

main :: IO ()
main = putStrLn "Hello, World!"
```

Then make the file executable:

```
chmod +x hello.hs
```

And then run the script:

```
./hello.hs
Hello, World!
```

# Why we need Cabal or Stack.

So far so good we have been able to use a single installed version of `ghc` to compile Haskell source code. What about the scenario where there exist multiple Haskell projects, with the requirement for different versions of `ghc`. What do we do then? Manually install multiple `ghc` versions? And then manually update the paths to ensure the right version required by a particular project is available when compiling that project?  

What about dependency management? How does one handle the situation where there is the need to use code package in other external Haskell libraries? So far, the code used to show a compilation of Haskell is simplistic and self-sufficient and does not depend on any external Haskell package. In real life, this won't probably be the case. How then should dependency management be dealt with? Manually download the dependent package and make use of low-level tools like [`ghc-pkg`](https://wiki.haskell.org/Ghc-pkg)?

Having to manually manage different installed versions of `ghc` is problematic and error-prone, as is the case of trying to manually manage external dependent Haskell packages. It is for this reason tools are built to help with these concerns, and much more. Cabal and Stack are an example of such tools.

In the [next post](https://schooloffp.co/2020/08/17/whirlwind-tour-of-cabal-for-beginners.html), we would take a look at Cabal. Understand more what it is, what it does, and how to use it. Following that, we will have another [post](https://schooloffp.co/2020/12/05/whirlwind-tour-of-stack-for-beginners.html) where we do the same for Stack.


