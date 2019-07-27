SimpleInst makes writing trivial inst{ruction,rumentation}s for RocketChip no rocket science anymore.

With SimpleInst you can turn simple C functions into instructions baked into the processor core as RoCC accelerator with multiple `funct`s. With some additional support from RocketChip (see the `rocc0-instrumentation` branch [here](https://github.com/atrosinenko/rocket-chip)) you can turn them (or any other similar enough RoCC accelerator) into per-instruction instrumenters.

**DISCLAIMER:** this repo is an early alpha, beware of severe bugs / vulnerabilities.

## Examples

This snippet adds two `funct`s (indexed 1 and 2). The first one counts `1`s in the binary representation of the first register operand (the second one is ignored). The second function computes some integer expression on both its arguments.

```cpp
#include <stdint.h>
uint64_t funct1(uint64_t x, uint64_t y)
{
  return __builtin_popcountl(x);
}

uint64_t funct2(uint64_t x, uint64_t y)
{
  return (x + y) * (x - y);
}
```

This code counts the `MUL` instruction invocations and overwrites the contents of `(uint64_t *)0x81005000` with it on every `MUL` occurrence through the L1D-cache. The `counter` variable does not require any memory accesses (even cached ones) because symbols from the `COMMON` section are mapped to registers (only scalars of no more than 8 bytes each are supported).

```cpp
#include <stdint.h>

uint64_t counter;

uint64_t instMUL()
{
  counter += 1;
  *((uint64_t *)0x81005000) = counter;
  return 0;
}
```

## Limitations

* It only supports functions with linear control flow (and probably static forward-only in the near future)
* Input snippets should be compiled into eBPF Little endian object files
* `COMMON` symbols are supported given they are scalars of no more than 8 bytes each
** `static` variables are not supported yet
* Memory accesses through pointer dereferences are supported
** memory can be **updated**: "internal" data dependencies (something is placed into an eBPF register, updated, then written somewhere) are respected
** no byte can be accessed inside the single instruction without natural "internally traceable" ordering (`*ptr1 += 1; *ptr2 = *ptr1 + 1; *ptr1 = *ptr2 * 2;` is considered undefined behavior)
* invalid memory accesses are handled only via watchdog yet :(

## Usage

* Write a C code snippet only containing `COMMON` variables (not `static`) and functions named `funct_N` (where N is a `funct` index) or `instXXX` (where `XXX` is some mnemonic [from here](https://github.com/atrosinenko/rocket-chip/blob/master/src/main/scala/rocket/Instructions.scala))
* Compile it like this: `clang -O3 -emit-llvm -c input.c -o - | llc -march=bpf -filetype=obj -o output.o`
* Fetch bytecode with `SimpleInstRoCC.fetchInstructions("path/to/output.o")`
* Hook `SimpleInstRoCC` into the CPU core like a regular RoCC accelerator
* Optionally pass the instrumentation handler mapping with `InstrumentationMapping` key (requires patched RocketChip)

## Links

This is an implementation of [QInst](https://github.com/atrosinenko/qinst) statically baked into a hardware.

This project looks quite similar to COPILOT (I cannot find much information on it and its license) but instructions are being expressed explicitly and probably COPILOT is much more powerful.
