---
layout: post
author: School Of FP
excerpt_separator: <!--end_excerpt-->
---

Haskell is notoriously famous for having a steep learning curve. A situation that frustrates newcomers to the language, especially newcomers who are experienced developers using other programming languages. 

It is not uncommon to see seasoned developers express their frustration by making the point that all of the other languages they have picked up, they have been able to get productive within a reasonable amount of time, but Haskell only manages to remain an impenetrable wall of Egyptian glyphs after the same period of time.

A reason often cited for Haskell's steep learning curve is the fact that _"Haskell is different"_. The argument goes: Haskell is not all that difficult, it is just different, and because of this, it is unfamilair. But most of the time, when this argument is made, it is not mentioned how exactly Haskell is different.

Sure, there could be other reasons why learning Haskell may be considered difficult, but we think the _difference_ argument is an interesting one worth exploring. This is exactly what this post is about. The idea is to do more than just mention that Haskell is different but to show how. The hope is that doing this, will prepare newcomers to the language, hence preventing Haskell's difference from being a stumbling block to learning the language. 

So in what ways is Haskell different?

<!--end_excerpt-->

## Two Axes of Difference: Syntax and Computation Model

There are two axes where Haskell is fundamentally different: _The syntax and the computation model_. 

Haskell has a syntax that is different from languages most experienced developers are familiar with. Also, Haskell's computation model is based on a different paradigm when compared with most mainstream languages. Understanding this will help in developing a better learning strategy.

Most mainstream programming languages inherit the C-like syntax and have imperative semantics. Haskell syntax is not derived from C, and it is not an imperative language, instead, it is a functional language. 

This means a programmer who is experienced with an imperative language with a C-like syntax will find it easy to pick up another imperative language with C-like syntax. Attempting to pick up Haskell in such a similar fashion will be harder and if not aware of these differences, Haskell will feel needlessly hard. 

### Syntax Difference: Haskell is not a C-like language

Most, if not all current mainstream programming languages have a syntax inspired or derived from C. To illustrate this, let’s take a coding problem from [Codewars](https://www.codewars.com/) and see the solutions in the first top [5 programming languages](https://www.tiobe.com/tiobe-index/) as rated by the Tiobe programming index, which at the moment of writing this post are: C, Java, Python, C++ and C#. <br/>

- - -  
<br/>
_Problem Statement: Multiples of 3 or 5_ [source](https://www.codewars.com/kata/514b92a657cdc65150000006/)

If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6, and 9. The sum of these multiples is 23.

Finish the solution so that it returns the sum of all the multiples of 3 or 5 below the number passed in.

Note: If the number is a multiple of both 3 and 5, only count it once. Also, if a number is negative, return 0(for languages that do have them)
<br/>
- - -
<br/>

**Solution in C**. [source](https://www.codewars.com/kata/reviews/59721a02e3383171bf000057/groups/5974d6018a6e59f62b0001a9)

```c
int solution(int number) {
    int total = 0;
    for (int i = 0; i < number; i++) {
      if (i % 3 == 0 || i % 5 == 0) {
        total = total + i;
      }
    }
    return total;
}
```

**Solution in Java**. [source](https://www.codewars.com/kata/reviews/553a8e47f3cc94c58c000123/groups/553adaa02d1cacbea9000048)

```java
public class Solution {

  public int solution(int number) {
    int sum=0;
    for (int i=0; i < number; i++) {
      if (i%3==0 || i%5==0) {
        sum+=i;
      }
    }
    return sum;
  }
}
```

**Solution in Python**. [source](https://www.codewars.com/kata/reviews/54a5ebd237f4350faf00006c/groups/54a7a58272ad2b8b9600055c)

```python
def solution(number):
    sum = 0
    for i in range(number):
        if (i % 3) == 0 or (i % 5) == 0:
            sum += i
    return sum
```

**Solution in C++**. [source](https://www.codewars.com/kata/reviews/578538457e3a78630c000166/groups/5799efd74be91294190003ac)


```c++
int solution(int number) 
{
  int sum = 0;
  for (int n = 3; n < number; n++) {
    if ((n%3 == 0) || (n%5 == 0))
      sum += n;
  }
  return sum;
}
```

**Solution in C#**. [source](https://www.codewars.com/kata/reviews/550b09270681519ec1001768/groups/550b0c865951388d45000bd1)

```c#
public static class Kata
{
  public static int Solution(int value)
  {
    var sum = 0;
    for(int i = 3; i < value; i++)
    {
      if(i % 3 == 0 || i % 5 == 0) sum += i;
    }
    return sum;
  }
}
```

You see the pattern here? Even in Python, a language that uses whitespace instead of curly braces, the structure of the solutions are all similar. It is not then difficult to see how someone who is proficient in one of the languages can easily make sense of any of the other languages because the structure of their syntax are the same.

Note that some of these imperative languages above now have constructs that makes it possible to emulate functional syntax. For example iterations and operations usually performed via `for loops` can now be done using construncts like streams and lambdas. This only shows the influence of functional programming on mainstream imperative languages. Infact these functional style is often seen as unnatural to the original languages. The creator of Python, Guido van Rossum, shared a similar sentiment about functional programming style in Python:

> Python probably has the reputation of supporting functional programming based on the inclusion of lambda, map, filter, and reduce in the language, but in my eyes these are just syntactic sugar, and not the fundamental building blocks that they are in functional languages.

- [source](https://books.google.nl/books?id=yB1WwURwBUQC&pg=PA26&lpg=PA26&dq=Python+probably+has+the+reputation+of+supporting+functional+programming+based+on+the+inclusion+of+lambda,+map,+filter,+and+reduce+in+the+language,+but+in+my+eyes+these+are+just+syntactic+sugar,+and+not+the+fundamental+building+blocks+that+they+are+in+functional+languages&source=bl&ots=-FON4zmkcC&sig=ACfU3U0t3fD8IgnwdRoOLWbMvdRsqmMMOg&hl=en&sa=X&ved=2ahUKEwizwPvo0O_tAhVH4aQKHWrJDuUQ6AEwAHoECAEQAg#v=onepage&q=Python%20probably%20has%20the%20reputation%20of%20supporting%20functional%20programming%20based%20on%20the%20inclusion%20of%20lambda%2C%20map%2C%20filter%2C%20and%20reduce%20in%20the%20language%2C%20but%20in%20my%20eyes%20these%20are%20just%20syntactic%20sugar%2C%20and%20not%20the%20fundamental%20building%20blocks%20that%20they%20are%20in%20functional%20languages&f=false)

Now let us see the solution in Haskell:  

**Solution in Haskell**. [source](https://www.codewars.com/kata/reviews/5546614c0240a76900000188/groups/5a42ff9c2d59e1e85a0035e0)

```haskell
module MultiplesOf3And5 where
import Data.List

solution :: Integer -> Integer
solution n = sum $ [3,6..n-1] `union` [5,10..n-1]
```

This looks very different from the previous solutions. The syntax is different and that is because Haskell's syntax is not inspired by C. **More importantly, this syntax is native to Haskell, and not just a style**. This highlights how Haskell is different when it comes to the axis of syntax.


> An analogy:
Imagine you understand British English, and you want to learn how to write American English. Such a task would be pretty straight forward since you can more or less apply the bulk of the knowledge you already have. What you will then need to learn consists of knowing where to tweak things like the grammar and spelling and idioms that are peculiar to American English. Now contrast that to learning how to write Russian. To do that successfully you will need to be able to set aside what you already know in English and be ready to pick up new grammar rules, new alphabets, new spellings, new idioms, etc.<br/><br/>
Learning yet another of the mainstream C-family like languages once you know one, is like learning how to speak American English if you already know any variant of English Languages. Learning Haskell on the other hand is similar to already knowing English but wanting to learn Russian. You have to appreciate and understand the fact that the process cannot be approached with a strategy that involves applying what you already know and tweaking one or two things here and there, it has to be approached with the intention to pick up a totally different set of rules about computation and programming. If you are not aware of this fact, then it would become easy to get frustrated and make statements like: I am a senior programmer and in my 10 years of programming I have been able to pick up a new language in 2 weeks tops! I can’t do that with Haskell, hence Haskell is impenetrable. Nope Haskell is not impenetrable, you have just been applying the wrong learning technique. Being aware of this is part of the first steps in learning Haskell successfully.

## Computation Model Difference: Haskell is not an imperative language

The other axis where Haskell is different is its computation model it is based on. This difference is not as obvious as the syntax difference but is a more important difference to appreciate.

Programming languages are based on [Models of computation](https://en.wikipedia.org/wiki/Model_of_computation). This is not difficult to appreciate, as programming languages can be seen as a mechanism to express computations.

The theory of computation and the different possible models have a rich mathematical and academic background, with sequential models, functional models, and concurrent models being the three broad categories.

Most mainstream programming languages are imperative in nature. This means their semantic is derived from the sequential model of computation. To be specific, most imperative languages are implementations of [Random access machines](https://en.wikipedia.org/wiki/Random-access_machine)/[Turing machines](https://en.wikipedia.org/wiki/Turing_machine), which are just two examples of the sequential model of computation. 

The essence of this model involves expressing computation as a sequential step that involves data mutation together with control structures that control how data is accessed and updated as part of the computation.

This model is the basis of the imperative programming paradigm.

Haskell on the other hand is not based on a sequential model of computation hence it is not an imperative programming language. Haskell is based on the functional model of computation. To be specific, Haskell is an implementation of the [Lambda calculus](https://en.wikipedia.org/wiki/Lambda_calculus), which is one example of a functional model of computation.

The essence of Lambda Calculus involves expressing computation based on function abstraction and application using variable binding and substitution. Haskell, at its core is nothing but an implementation of Lambda Calculus. 

These computation models then dictate the mental model that would be required when working with a programming language based on them. A sequential model that involves data mutation requires a different model than a functional model that is an implementation of lambda calculus which requires function abstraction and application.

Hence this is why it is beneficial, especially for seasoned developers picking up Haskell to understand a bit of Lambda Calculus, not to the level required of a mathematician or a computer scientist, but enough to understand why Haskell is the way it is. If not for any other reason but the fact that it will help to appreciate what we mean by function in a mathematical sense and how that is different from the notion of function as popularised in most mainstream language.

> An analogy:
Imagine you live in Colorado, USA and you have a Map of the country. You can easily find your way when moving around within Colorado, and if you want to explore a different city, you can easily make use of the Map. As the map provides a conceptual model you can apply when navigating. Now Imagine you find yourself in Seoul in South Korea, but for some strange reason, you are not aware that you are still using the same Colorado map. You can imagine the frustration, the chaos, how much stuff won’t make sense, and the danger of trying to still make use of the map of Colorado to navigate in Seoul.<br/><br/>
Learning yet another imperative programming language once you know one is similar to exploring cities in the USA, once you live in one and understands how the mapping system in the USA works. The conceptual models are more or less the same. What most people do is to attempt to pick up Haskell, but unknowingly still want to apply the various conceptual models they have picked up from knowing one or more imperative programming languages. It is not hard to imagine that this would lead to the same disorientation and frustration as trying to navigate the city of Seoul without being aware that the map of the United States is still being used. <br/><br/>
Haskell is a purely functional programming language with a model of computation based on Lambda Calculus, this is different from imperative languages which have Turing machines and RAM Machines as their foundational model of computation. Hence if you have only been exposed to imperative languages, learning Haskell will require not just learning how to write Haskell, but how to also think in a different paradigm. This is why we usually recommend learning functional programming with any of the purely functional programming languages out there eg Haskell, Elm, PureScript instead of doing it with natively imperative languages like Scala, JavaScript, Kotlin, etc with functional features. Picking a functional language would make it starkly obvious that one is dealing with a different programming paradigm. While using an imperative language, with functional features, run the risk of not making the need to learn a new paradigm obvious.

Hopefully being aware of these two ways in which Haskell is different will help set the right expectation and help develop a better strategy when seeking to learn Haskell. We plan to continually share such strategies and insights on here, so feel free to subscribe via [RSS](https://schooloffp.co/feed.xml) of follow on [twitter](https://twitter.com/schooloffp)


