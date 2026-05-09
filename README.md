# Project 3: Regular Expression Engine

## Overview  
This project implements algorithms for working with NFAs, DFAs, and regular expressions in OCaml. It is divided into three parts: simulating NFAs, converting NFAs to DFAs, and converting regular expressions to NFAs.

---

## Part 1: NFAs  
Implemented functions to simulate NFAs:

- **move**: Computes next states from a set of states on a given symbol or epsilon  
- **e_closure**: Computes all states reachable through epsilon transitions  
- **accept**: Determines if an NFA accepts a given string  

---

## Part 2: NFA → DFA Conversion  
Implemented subset construction to convert an NFA into a DFA:

- **new_states**: Generates reachable DFA states  
- **new_trans**: Generates transitions  
- **new_finals**: Determines accepting states  
- **nfa_to_dfa**: Builds the DFA  

---

## Part 3: Regular Expressions  
Implemented conversion from regular expressions to NFAs:

- **regexp_to_nfa**: Converts a regex into an equivalent NFA  

Supported constructs:
- Empty string (ε)  
- Characters  
- Union (`|`)  
- Concatenation  
- Kleene star (`*`)  

---

## Testing  

Run tests using:

```bash
env OCAMLPATH=dep dune runtest -f
