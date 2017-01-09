# Loosely Time-Triggered Architectures

To build the examples, type the following command:

```
make
```

This produces four executables:

- bp_ltta.byte: simulation of the back-pressure protocol
- tb_ltta.byte: simulation of the time-based protocol
- rb_ltta.byte: simulation of the round-based protocol
- gc_ltta.byte: simulation of the global-clock protocol

All the simulations run the following application on a
two-node quasi-periodic architecture.

```
let node m1(po2) = 0 -> po2 + 2
let node m2(po1) = 1 -> po1 + 2

let node app(c1, c2, dc1, dc2) = o1, o2 where
  rec present c1() -> do emit o1 = m1(po2) done
  and present c2() -> do emit o2 = m2(po1) done
  and po1 = link(c1, dc1, o1)
  and po2 = link(c2, dc2, o2)
```

File qpa.zls contains the Zélus model of the quasi-periodic architecture.
Other Zélus files (*_ltta.zls) contains the code of the LTTA controllers.