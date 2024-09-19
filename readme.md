# circom-intsafe

**circom-intsafe** is a static analyzer for the [**Circom DSL**](https://github.com/iden3/circom), designed to ensure that all signals and variables within a **template** remain within the specified range of `[-p/2, p/2]`. This allows developers to safely treat finite fields as integers when working with Circom, preventing out-of-bound errors or overflows.

## Motivation

In Circom, arithmetic operations occur in finite fields, which can lead to unexpected behavior when developers assume standard integer arithmetic. Relational operators like `<`, `>`, `<=`, and `>=` and arithmetic operations `+`, `-`, and `*` are defined based on a modular representation (`x % p`), which can cause values to "wrap around" in a way that differs from regular integer arithmetic.

This can result in a misalignment between the user's intent and the actual behavior of comparisons and arithmetic. For example, comparisons between signals that approach the bounds of the field (e.g., values near `p/2` or `-p/2`) might yield unexpected results.

**circom-intsafe** ensures that all signals and variables in a template stay within the range `[-p/2, p/2]` by warning developers of any potential issues that could cause misalignment or unexpected behavior. This helps ensure that developers can treat the finite field values as integers and avoid problems caused by modular arithmetic.

## Installation

Ensure that you have **Rust** installed on your system.

To install the tool, run the following command:

```bash
cargo install --path intsafe
```

## Usage

Once installed, you can use the tool to analyze a Circom file by running the following command:

```bash
circom-intsafe <circom file> -p <prime number>
```

- `<circom file>`: The path to the Circom file that you want to analyze.
- `<prime number>`: The prime number `p` used to define the finite field.
- `--print-ir`: Print the analyzer's intermediate representation of the template.

### Note

- The tool currently only supports analyzing a single template within a Circom file.
- To effectively use the analysis, make sure to provide bounds for the template's parameters and inputs using assertions. For example:
  ```circom
  template CoolTemplate(N) {
    signal input in[N];
    assert (N < 256);   // the number of inputs should be less than 256
    assert (in < 1024); // all elements of `in` should be less than 1024
    ...
  }
  ```

## Example

You can try running the tool on the `mux1.circom` and `mux4.circom` (from [circomlib](https://github.com/iden3/circomlib)) in the `examples` directory. For example, to analyze the `mux1.circom` file with a prime number of `10007`, run the following command:

```bash
circom-intsafe examples/mux1.circom -p 10007
```

```circom
// examples/mux1.circom
pragma circom 2.0.0;

template MultiMux1(n) {
    signal input c[n][2];  // Constants
    signal input s;   // Selector
    signal output out[n];

    // n should be less than 256
    assert (n < 256);
    // constants should be less than 8192
    assert (0 <= c);
    assert (c < 8192);
    // selector should be between 0 and 1
    assert (0 <= s);
    assert (s <= 1);

    for (var i=0; i<n; i++) {

        out[i] <== (c[i][1] - c[i][0])*s + c[i][0];

    }
}
```

The tool will output the analyzed range for each assignment in the template, along with any warnings or errors that are detected. For example:

```bash
Assignment instance of i       = [0, 0]             // First assignment of i: i = 0
Assignment instance of out     = [-8191, 16382]       !Warning! Not Intsafe
Assignment instance of i       = [1, 255]           // Second assignment of i: i++, i.e., i = i + 1
```

## Future work

- **Support for modular templates**: The core logic is implemented to support modular templates, and future releases will allow this by inlining templates within the analysis process.
