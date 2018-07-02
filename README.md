# Mirage OCRA [![Build status](https://travis-ci.org/cfcs/mirage-ocra-demo.svg?branch=master)](https://travis-ci.org/cfcs/mirage-ocra-demo)

- This is a sample QubesOS unikernel using
  https://github.com/sg2342/ocaml-rfc6287 to
  perform authentication using the `OCRA-1:HOTP-SHA1-6:C-QN08-PSHA1`
  suite as implemented in [OTP.to cards](https://otp.to/)

- Since the low-level primitives are provided by [`nocrypto`](https://github.com/mirleft/ocaml-nocrypto) which is eternally stuck in a frustratingly fucked and/or unmaintained state, pin this revision which is the last occasion on which anybody cared about mirage-xen support: `opam pin add -n nocrypto -k git 'https://github.com/mirleft/ocaml-nocrypto.git#79d5db2488e338d161d7e170cd681a8120ce07d1'`

- This demo is probably a bit tricky to install since it uses the unpackaged
  [mirage-framebuffer](https://github.com/cfcs/mirage-framebuffer) library.
  See the [eye-of-mirage](https://github.com/cfcs/eye-of-mirage) README for
  installation instructions for that, and complain in the GitHub issues here
  if that gives you problems.

## Running it

- You probably want to run something like (after following the instructions above):
```shell
opam install cstruct mirage mirage-qubes mirage-key mirage-time-lwt rfc6287 mirage-framebuffer nocrypto
./yomake $(cat my.card.secret)
```

- You need to press enter since the state machine is line-based. There is no local echo.

## Demo
![mirage-ocra-test](https://user-images.githubusercontent.com/9653993/37250559-570080dc-2500-11e8-833e-24d0398c742e.png)
