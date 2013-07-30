# Dynamic Web Tableaux for Public Announcement Logic

A web-based implementation of my 'dynamic' tableau system for public announcement logic. I explain some keywords below. I will expand this README in the future.

Beware: This is a work in progress. See below for more information.

# What's Public Announcement Logic?

Public announcement logic is a logic for reasoning about knowledge and public announcements. Public announcements are a form of learning where all agents simultaneously acquire common knowledge of a particular fact.

Introductory slides by Jan Plaza: http://faculty.plattsburgh.edu/jan.plaza/research/logic/public-slides.pdf

# What are Tablau Systems?

Tableau systems are formal systems for constructing models that satisfy a given formula. They can also be used for proving theorems on the assumption that a formula is a theorem iff no model can be found (which is the case here).

# What are Dynamic Tableaux?

In my dissertation (a work in progress) I describe tableau systems for various dynamic modal logic, including public announcement logics. These tableau systems have rules that closely mirror the 'dynamic' semantics of dynamic modal logics.

A short and very technical abstract by me: http://jdevuyst.appspot.com/abstracts/2013/jdevuyst-dynamic_tableaux_for_PAL-lics13-abstract.pdf

# Status

As-is the tableau system fully covers (multi-agent) S5 systems. Support for public announcement operators is not yet implemented. This will be added in August 2013.

A web-based front-end is targetted for September 2013.

# Usage instructions

To get started, perform the following steps.

1. Download the Java Development Kit (JDK) from java.com. Beware that downloading the Java Runtime Environment (JRE) will not suffice, as the JRE does not contain the Java compiler.
2. Download Leiningen. This download includes the Clojure environment.
3. Go to the project root folder (the one containing project.clj) in your terminal.
4. Run "lein repl".

Next, to run the core tests, enter "(use 'tableaux.core-test)". These tests some well-known validities. See "test/tableaux/core-test.clj" for more information.

To perform your own tests, first enter "(use 'tableaux.core)" and then type "(explain form)", where form is a formula using the syntax also found in "test/tableaux/core-test.clj".
