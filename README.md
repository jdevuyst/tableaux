# Dynamic Tableaux for Public Announcement Logic

This Github project contains the Clojure implementation of the 'dynamic' tableau system for public announcement logic that I describe in my [PhD thesis](https://jdevuyst.appspot.com/publications/2013/jdevuyst-phd-thesis.pdf). I briefly touch on some keywords below. For details and documentation, please see my PhD thesis.

## What is public announcement logic?

Public announcement logic is a logic for reasoning about knowledge and public announcements. Public announcements are a form of learning where all agents simultaneously acquire common knowledge of a particular fact.

[Introductory slides by Jan Plaza](http://faculty.plattsburgh.edu/jan.plaza/research/logic/public-slides.pdf)

## What are tablau systems?

Tableau systems are formal systems for constructing models that satisfy a given formula. They can also be used for proving theorems on the assumption that a formula is a theorem iff no model is found using the tableau system (which is the case here).

## What are dynamic tableaux?

In my dissertation I describe tableau systems for various dynamic modal logics, including public announcement logic. These tableau systems have rules that closely mirror the 'dynamic' semantics of dynamic modal logics.

## Usage instructions

To get started, perform the following steps.

1. Download the [Java Development Kit (JDK)](http://www.oracle.com/technetwork/java/javase/downloads/bag.html). Beware that installing the Java Runtime Environment (JRE) might not suffice, as it does not add the command line Java app to the system path on some platforms.
2. Download [Leiningen](http://leiningen.org). This download includes the Clojure environment.
3. Go to the project root folder (the one containing project.clj) in your terminal.
4. Run "lein test".

Among other things, this will test some well-known validities (Hilbert axioms, S5 axioms, PAL reduction axioms). These tests are located at the bottom of [test/dyntab/cascade_test.clj](test/dyntab/cascade_test.clj).

If you're interested in using my theorem prover as a library, you will probably want to look at [src/dyntab/cascade.clj](src/dyntab/cascade.clj) and [src/dyntab/syntax.clj](src/dyntab/syntax.clj) first.

There currently is no end user interface for the theorem prover.

## Addendum

In section 7.7 of my PhD thesis I discussed performance when using multiple threads. I wrote that on a quad core machine CPU utilization of 370% was observed and that execution time was nearly halved compared to when one thread was used. It turns out the machine was dual core, however. The fact that more than 200% CPU utilization was reported appears to be due to [hyper-threading](http://en.wikipedia.org/wiki/Hyper-threading). Thus, it appears that the theorem prover scales better than originally stated.

In the same section I also wrote that the theorem prover code would benefit if the `TupleBag` data structure was replaced by a more general solution. This remark later prompted me to create [Comprehend](https://github.com/jdevuyst/comprehend), a Clojure library for pattern matching on sets. It would be an interesting exercise to reimplement the theorem prover using the Comprehend library.