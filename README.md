# Dynamic Tableaux for Public Announcement Logic

This Github project contains a Clojure implementation of the 'dynamic' tableau system for public announcement logic that I describe in my forthcoming PhD thesis. I briefly touch on some keywords below. For details and documentation, please see my PhD thesis.

# What's Public Announcement Logic?

Public announcement logic is a logic for reasoning about knowledge and public announcements. Public announcements are a form of learning where all agents simultaneously acquire common knowledge of a particular fact.

Introductory slides by Jan Plaza: http://faculty.plattsburgh.edu/jan.plaza/research/logic/public-slides.pdf

# What are Tablau Systems?

Tableau systems are formal systems for constructing models that satisfy a given formula. They can also be used for proving theorems on the assumption that a formula is a theorem iff no model can be found (which is the case here).

# What are Dynamic Tableaux?

In my dissertation I describe tableau systems for various dynamic modal logic, including public announcement logics. These tableau systems have rules that closely mirror the 'dynamic' semantics of dynamic modal logics.

# Status

The code works but there's no user-friendly interface yet. I plan to create a modest web-based front-end by January 2014, in time for my defense.

# Usage instructions

To get started, perform the following steps.

1. Download the [Java Development Kit (JDK)](http://www.oracle.com/technetwork/java/javase/downloads/bag.html). Beware that downloading the Java Runtime Environment (JRE) will not suffice, as the JRE does not contain the Java compiler.
2. Download [Leiningen](http://leiningen.org). This download includes the Clojure environment.
3. Go to the project root folder (the one containing project.clj) in your terminal.
4. Run "lein test".

Among other things, this will test some well-known validities (Hilbert axioms, S5 axioms, PAL reduction axioms). These tests are located at the bottom of "test/tableaux/cascade-test.clj".
