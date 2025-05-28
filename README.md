# LeducForth

This project is a FORTH Compiler for the Raspberry Pi Pico 2 W (RP2350).

This is a simple FORTH compiler for the Raspberry Pi Pico 2 W or the Pimoroni Pico Plus 2 W.
It is strongly based on the Jones FORTH by Richard W.M. Jones, which can be found:

> http://annexia.org/forth

This core of this compiler is implemented in ARM assembly (Cortex M-33).

A second major influce is codescribe's updated version of Jones FORTH:

>    https://github.com/codescribe/jonesforth

## Getting Started

This project has few requirements:

### Hardware
You will need a [Raspberry Pi Pico Debug Probe](https://www.raspberrypi.com/documentation/microcontrollers/debug-probe.html) and a Raspberry Pi Pico 2 (W) or a Pimoroni Pico Plus 2 (W).

Connect the debug probe to the debug header on the Pico and connect the UART port on the probe to PINs 1-3 on the Pico.

### Software

I use [VS Code](https://code.visualstudio.com) and the [Raspberry Pi Pico](https://marketplace.visualstudio.com/items?itemName=raspberry-pi.raspberry-pi-pico) extension.


## Operation

Start a debugging session to flash the firmware. You are presented with a short message. You can enter basic Forth words now.

Send the text file `forth/core.f` to the Pico serial port for a more complete set of core Forth words.
