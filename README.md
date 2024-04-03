# erlangZ21
An erlang library for interacting with the Roco/Fleishmann Z21 command station.

## Functionality
At the moment there is support for: driving, getting locomotive information, activating and deactivating locomotive functions and getting the serial number from the connected Z21 device. The library is utilized by [ercli](https://github.com/wfenglund/ercli).

## Build
In the terminal:
```bash
$ git clone https://github.com/wfenglund/erlangZ21
$ cd erlangZ21/
$ rebar3 compile
```
## Use
In the terminal:
```bash
$ cd erlangZ21/
$ rebar3 shell
```
In the rebar3/erlang shell:
```erlang
1> erlangZ21:get_serial_num(erlangZ21:udp_details()).
Serial number: XXXXXX
```
