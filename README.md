# A Synchronous Approach to Quasi-Periodic Systems

This repository contains the source code of the examples presented
throughout my thesis.
This examples can be compiled and executed in version 1.2.3 of Zélus.
This version requires OCaml version 4.04.0.

There is one directory for each technical chapter of the thesis

2. A Short Introduction to Zélus
3. Quasi-Periodic Architectures
4. The Quasi-Synchronous Abstraction
5. Loosely Time-Triggered Architectures
6. Symbolic Simulation

## Build

To build all the example, install
[Zélus](http://zelus.di.ens.fr/download.html), then execute the
following commands in the root directory.

```
./configure
make
```

It is also possible to use the Zélus compiler without installing
it. In this case, you need to specify the path to the Zélus directory
before building the examples:

```
./configure ZELUSROOT=~/Downloads/zelus-1.2.2-nosundials-byte
make
```

Alternatively there is an individual Makefile in each directory but
you still need to generate the config file first.

## Related Publications

- Soundness of the Quasi-Synchronous Abstraction
  with Timothy Bourke and Marc Pouzet.
  *International Conference on Formal Methods in Computer-Aided Design (FMCAD), Oct. 2016*

- Loosely Time-Triggered Architectures: Improvements and Comparisons
  with Albert Benveniste and Timothy Bourke.
  *International Conference on Embedded Software (EMSOFT), Oct. 2015.*

- Loosely Time-Triggered Architectures: Improvements and Comparisons
  with Albert Benveniste and Timothy Bourke.
  *ACM Transaction on Embedded Computing Systems (TECS), Vol.15 N.4 August 2016.*

- A Unifying View of Loosely Time-Triggered Architectures
  with Albert Benveniste, Anne Bouillard and Paul Caspi
  *INRIA Research Report RR-8494, Mar. 2014*
