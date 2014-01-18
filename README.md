# Dynamic Tableaux for Public Announcement Logic

This Github project contains the Clojure implementation of the 'dynamic' tableau system for public announcement logic that I describe in my [PhD thesis](https://jdevuyst.appspot.com/publications/2013/jdevuyst-phd-thesis.pdf). I briefly touch on some keywords below. For details and documentation, please see my PhD thesis.

## What's Public Announcement Logic?

Public announcement logic is a logic for reasoning about knowledge and public announcements. Public announcements are a form of learning where all agents simultaneously acquire common knowledge of a particular fact.

[Introductory slides by Jan Plaza](http://faculty.plattsburgh.edu/jan.plaza/research/logic/public-slides.pdf)

## What are Tablau Systems?

Tableau systems are formal systems for constructing models that satisfy a given formula. They can also be used for proving theorems on the assumption that a formula is a theorem iff no model is found using the tableau system (which is the case here).

## What are Dynamic Tableaux?

In my dissertation I describe tableau systems for various dynamic modal logics, including public announcement logic. These tableau systems have rules that closely mirror the 'dynamic' semantics of dynamic modal logics.

## Usage instructions

To get started, perform the following steps.

1. Download the [Java Development Kit (JDK)](http://www.oracle.com/technetwork/java/javase/downloads/bag.html). Beware that installing the Java Runtime Environment (JRE) might not suffice, as it does not add the command line Java app to the system path on some platforms.
2. Download [Leiningen](http://leiningen.org). This download includes the Clojure environment.
3. Go to the project root folder (the one containing project.clj) in your terminal.
4. Run "lein test".

Among other things, this will test some well-known validities (Hilbert axioms, S5 axioms, PAL reduction axioms). These tests are located at the bottom of "test/dyntab/cascade-test.clj".

If you're interested in using my theorem prover as a library, you will probably want to look at [src/dyntab/cascade.clj](src/dyntab/cascade.clj) and [src/dyntab/syntax.clj](src/dyntab/syntax.clj) first.

There currently is no end user interface for the theorem prover.