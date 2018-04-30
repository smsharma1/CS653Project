The goal of this project is to build a SAT Solver that takes a CNF as input in Dimacs format and outputs SAT/UNSAT if the input is Satisfiable/Unsatisfiable. We implemented two popular algorithms in SAT solving namely DP (Davis-Putnam) and DPLL (Davis-Putnam-Logemann-Loveland) . The complexity of SAT solvers crucially depends on effective branching heuristics. In this project we have implemented DLIS (Dynamic Largest Individual Sum) heuristic for choosing literal to decide or resolve upon. We also present the trace of DPLL in the form of a tree by Exhaustive DPLL search. It can also be seen as a form of knowledge compilation and can be used for queries like model checking, model counting,etc and compilation into other succinct  forms.
