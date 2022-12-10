# Day 10: Vim

To run this, open `input.txt` in vim.

Then run `source vm.vim`

The answer to part 1 will be in `part1.txt` and a rendering of the CRT for part 2 will be in `crt.txt`

While it executes, it will add comments to each line indicating the state of the world for each cycle of execution.  (I did this for debugging, and it was really handy.)

If it runs out of bounds, you can add a few extra `noop` instructions to the endto get to 240 cycles.

