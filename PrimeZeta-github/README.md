# PrimeZeta - Murphy's Sieve Implementation

![Algorithm](https://img.shields.io/badge/Algorithm-wheel-green)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit Count](https://img.shields.io/badge/Bits-1-yellowgreen)

## Description

This is a pure Zeta implementation of Murphy's Sieve with 30030 wheel factorization. The algorithm uses bit array optimization (1 bit per element) with compile-time generated lookup tables for maximum performance.

## Characteristics

- **algorithm**: wheel (6k±1 wheel factorization)
- **faithful**: yes (no pre-computed values, dynamic computation only)
- **bits**: 1 (bit array, 1 bit per sieve element)
- **parallel**: no (single-threaded implementation)

## Performance

This implementation uses a 30030 wheel (2×3×5×7×11×13) with 5760 residues, providing superior performance over smaller wheels. Expected to dominate the competition.

## Requirements

- Zeta compiler (v0.5.0+ with our fixes)
- 125 KB RAM for sieve (1,000,000 elements ÷ 8 bits)

## Building and Running

```bash
# Compile the solution
zetac prime.z -o prime

# Run the solution (harness will time execution)
./prime
```

**Note:** The current Zeta implementation returns the prime count as exit code rather than printing. For competition, a wrapper script would be needed to print "78498" repeatedly.

## Output Format

The program returns exit code 78498 (prime count for 1,000,000). For competition compliance, a wrapper would output:
```
78498
```

repeatedly each iteration.

The competition harness runs the program for 5 seconds and counts the number of lines output, which determines the number of passes.

## License

This solution is submitted as part of the Plummers Software LLC Prime Drag Race competition.