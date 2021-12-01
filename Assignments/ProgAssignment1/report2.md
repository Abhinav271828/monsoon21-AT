---
title: Automata Theory (CS1.302)
subtitle: |
          | Monsoon 2021, IIIT Hyderabad
          | Programming Assignment 1 Report (Part 2)
author: Abhinav S Menon
---

\def\vec#1#2{\begin{bmatrix} {#1} \\ {#2} \end{bmatrix}}

# Question 3
The code follows the set-based algorithm. The states are partitioned into two sets initially – one consisting of only accepting states, and the other consisting of all the remaining states. Then the states in each set are compared for distinguishability; two states are distinguishable if on some pair of input symbols, the transition leads to a states in two different sets. The partitions are divided further according to distinguishability, until no further divisions can be made; then the algorithm terminates.  
A partition is implemented as a vector of sets (treated as queue), from which a set is repeatedly popped, partitioned, and all new sets pushed back.

# Question 4
Here, the code first parses the regex and then creates an NFA. Parsing is done by a stack, which is finally converted to a vector of concatenated regexes.  
Based on the structure of the regex, it is then converted to an NFA. The regex is traversed and the NFA is built up recursively.
