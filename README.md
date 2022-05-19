# Tezla

![main workflow](https://github.com/joaosreis/tezla/actions/workflows/main.yml/badge.svg)

An intermediate representation of Michelson smart contracts designed to ease
static analysis of smart contracts.

You can read more about it on
[this paper](https://drops.dagstuhl.de/opus/volltexte/2020/13417/pdf/OASIcs-FMBC-2020-4.pdf).

## Install instructions

### Using dune

```bash
git clone https://github.com/joaosreis/tezla.git
cd tezla
dune build @install
dune install
```

### Using opam

```bash
opam install https://github.com/joaosreis/tezla.git
```

---

Developed under the [FRESCO](https://release.di.ubi.pt/projects/fresco.html) project
(Formal Verification of Smart Contracts), generously funded by [Tezos
Foundation](https://tezos.foundation).
