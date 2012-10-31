Surely
===

An algorithm to find the boolean satisfiability of a formula, Maybe.

(c) 2012 Gatlin Johnson <rokenrol@gmail.com>

0. SAT Solving, briefly
---

A SAT solver is a program that takes a [propositional formula][1] and tries to
find a truth value for each of its variables such that the whole proposition is
true. Failing that, it declares the proposition *unsatisfiable*. Either way,
you can be Sure of the result.

This problem, the *Boolean satisfiability problem*, is [NP-complete][2] and
thus equivalent to all other NP-complete problems. SAT solvers are useful,
then, because if a mapping exists from a problem to a SAT instance, a SAT
solver can be used to find a solution.

1. The interface
---

Surely exports a single function, `solve`, which accepts propositions in
*Conjunctive Normal Form*, as explained below. The output is of type `Maybe
[Int]`, which will either be `Just` a list of literals which are true, or
`Nothing`.

2. Conjunctive Normal Form
---
Most SAT solving algorithms take the input argument in Conjunctive Normal Form,
or *CNF*. Any proposition can be rewritten in CNF. A CNF proposition is a
*conjunction of disjunctions of literals.* Concretely,

    (p OR q OR -r) AND (-q OR s) AND (-q)

is in CNF, because a series of disjunctions are combined in a conjunction.

For notational simplicity, a proposition in CNF for our purposes is of type
`[[Int]]`. Variables are just unique integers, and truth is determined by the
polarity (minus sign or not). It is up to your application to perform the
conversion.

3. Goals
---

The goal of Surely is to provide an elegant, efficient, and extensible SAT
solving algorithm (in that order).

4. TODO
---

- Backjumping
- Conflict-driven learning
- I can probably choose an alternative to list with better memory guarantees

[1]: http://en.wikipedia.org/wiki/Propositional_calculus
[2]: http://en.wikipedia.org/wiki/NP-complete
