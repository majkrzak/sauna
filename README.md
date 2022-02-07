sauna – the ultimate sanuli solver
==================================

Automated solver for [_sanuli_](https://sanuli.fi), Finnish variation of [_wordle_](https://en.wikipedia.org/wiki/Wordle).


## Details


### Algorithm

After initial research and experimenting with different approaches, finally the way how the solver works settled in the min max like algorithm.

In each step, word which will reduce the set of possible solution the most is choosen.


### Preprocessing

Entry step takes approximately 20 minutes (on single core of Intel Core i7-6700K CPU @ 4.00GHz) and approximately 30 seconds for the second step. For subsequent calls, time is negligible. This overhead is only present during the initialization, because memoization technique was used for computational heavy tasks. Even though, to speed up the initialisation, two first calls were preprocessed and hardcoded.
