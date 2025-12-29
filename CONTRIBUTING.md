# Contributing to Veyra

Thank you for your interest in contributing to Veyra! 🚀

## How to Contribute

### Reporting Bugs

- Use GitHub Issues to report bugs
- Include steps to reproduce
- Include your OS and Veyra version

### Suggesting Features

- Open an issue with the "enhancement" label
- Describe the feature and its use case
- Explain why it would benefit users

### Code Contributions

1. Fork the repository
2. Create a feature branch: `git checkout -b feature/my-feature`
3. Make your changes
4. Run tests: `cargo test`
5. Commit: `git commit -m "Add my feature"`
6. Push: `git push origin feature/my-feature`
7. Open a Pull Request

### Code Style

- Use `rustfmt` for Rust formatting
- Follow existing code patterns
- Add comments for complex logic
- Include tests for new features

### Areas to Contribute

- **Stdlib Functions** - Add new built-in functions
- **Performance** - Optimize the interpreter or codegen
- **Documentation** - Improve docs and examples
- **Platform Support** - Help with macOS/Linux builds
- **Bug Fixes** - Fix issues from the tracker

## Development Setup

```bash
# Clone
git clone https://github.com/yourusername/veyra.git
cd veyra/veyra-native

# Install dependencies
# Windows: Install LLVM 17 from vovkos/llvm-package-windows
# Set LLVM_SYS_170_PREFIX environment variable

# Build
cargo build

# Test
cargo test

# Run examples
cargo run -- run examples/hello.veyra
```

## Code of Conduct

Be respectful, inclusive, and constructive. We're all here to make Veyra better!

## Questions?

Open an issue or reach out to the maintainers.

Thank you for contributing! 🎉
