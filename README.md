hlisp
-----

hlisp stands for Haskell Lisp. Or Hasnain's Lisp. The choice is left up to the user.

Based (loosely) on Norvig's lis.py

Building
--------

    cabal sandbox init
    cabal install --only-dependencies
    cabal build
    ln -s dist/build/hl/hl .

It's always a good idea to test:

    ./hl tests.hl
