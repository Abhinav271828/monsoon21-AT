---
title: Automata Theory (CS1.302)
subtitle: |
          | Monsoon 2021, IIIT Hyderabad
          | 14 October, Thursday (Tutorial 2)
authors: Taken by Alapan Chaudhari, Aditya Morolia
header-includes: \usepackage{mathtools}
---

# L-Systems
## Origins
Lindenmayer system or L-systems were created as a way to formalise patterns of bacteria growth. They are used as a recursive string-rewriting framework.  

They are a parallel rewriting system and a type of formal grammar. They describe how to form "valid" strings.  

## Structure
An axiom is a starting point in the system. A theorem is a string produced by the rules of the system. The rules are the rules of production or inference.  

Suppose we have the symbols M, I and U, and we describe our system as:  
Axiom: `MI`  
Rules:  
`xI -> xIU`  
`Mx -> Mxx`  
`xIIIy -> xUy`  
`xUUy -> xy`  
Problem: Is `MU` a theorem?  

Formally, an L-system is defined as a 3-tuple $G=(V,w,P)$, where

* $V$ is the alphabet set (variables + terminals).
* $w$ is the start/axiom/initiator is a string over $V$.
* $P$ is the set of production rules.

If $N$ is the set of nonterminals and $T$ the set of terminals, then $V = N \cup T$, and a production rule has the form $(V^*NV^*, V^*)$. The first element is called the predecessor and the second the predecessor.  

L-systems have a recursive nature that leads to self-similarity.

## Types
L-systems can be context-free or context-sensitive, and probabilistic or deterministic. In a probabilistic system, the probabilities of production rules with the same predecessor and context must add up to one.
