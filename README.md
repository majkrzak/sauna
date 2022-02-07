sauna – the ultimate sanuli solver
==================================

Automated solver for [_sanuli_](sanuli.fi), Finnish variation of [_wordle_](https://en.wikipedia.org/wiki/Wordle).


## Algorithm

The solver works in the min max way. In each step, word which will remove most of the possible solution is chosen.


## Preprocessing

Entry step takes approximately 20 minutes (on single core of Intel Core i7-6700K CPU @ 4.00GHz) and approximately 30 seconds for the second step. Due to the memoization of computing heavy tasks, this is only one time issue. Even though, to speed up the initialisation, two first calls were preprocessed and hardcoded.
