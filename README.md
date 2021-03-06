# radix
Command-line tool for emitting numbers in various bases.

-----

Homepage: [https://github.com/thomaseding/radix](https://github.com/thomaseding/radix)

Hackage: [https://hackage.haskell.org/package/radix](https://hackage.haskell.org/package/radix)



------


Install from git repository:
```
$ cabal configure
$ cabal build
$ cabal install
```

Install from Hackage.
```
$ cabal update
$ cabal install radix
```

These will install the `radix` command.

--------

Example usage:

```bash
> radix a7
(16) 167 ->  (2) 10100111
(16) 167 ->  (8) 247
(16) 167 -> (10) 167
```
```bash
> radix --pad 5 9
(10) 9 ->  (2) 01001
(10) 9 ->  (8) 00011
(10) 9 -> (16) 00009

(16) 9 ->  (2) 01001
(16) 9 ->  (8) 00011
(16) 9 -> (10) 00009
```
```bash
> radix 14
(8) 12 ->  (2) 1100
(8) 12 -> (10) 12
(8) 12 -> (16) C

(10) 14 ->  (2) 1110
(10) 14 ->  (8) 16
(10) 14 -> (16) E

(16) 20 ->  (2) 10100
(16) 20 ->  (8) 24
(16) 20 -> (10) 20
```
```bash
> radix 10
(2) 2 ->  (8) 2
(2) 2 -> (10) 2
(2) 2 -> (16) 2

(8) 8 ->  (2) 1000
(8) 8 -> (10) 8
(8) 8 -> (16) 8

(10) 10 ->  (2) 1010
(10) 10 ->  (8) 12
(10) 10 -> (16) A

(16) 16 ->  (2) 10000
(16) 16 ->  (8) 20
(16) 16 -> (10) 16
```
```bash
> radix 9 F
(10) 9 ->  (2) 1001
(10) 9 ->  (8) 11
(10) 9 -> (16) 9

(16) 9 ->  (2) 1001
(16) 9 ->  (8) 11
(16) 9 -> (10) 9

(16) 15 ->  (2) 1111
(16) 15 ->  (8) 17
(16) 15 -> (10) 15
```
