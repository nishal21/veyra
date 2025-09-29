# Contributing to Veyra

We love your input! We want to make contributing to Veyra as easy and transparent as possible, whether it's:

- Reporting a bug
- Discussing the current state of the code
- Submitting a fix
- Proposing new features
- Becoming a maintainer

## Development Process

We use GitHub to host code, to track issues and feature requests, as well as accept pull requests.

1. Fork the repo and create your branch from `main`.
2. If you've added code that should be tested, add tests.
3. If you've changed APIs, update the documentation.
4. Ensure the test suite passes.
5. Make sure your code lints.
6. Issue that pull request!

## Local Development

### Setup

```bash
git clone https://github.com/nishal21/veyra.git
cd veyra
pip install -e .[dev]
```

### Testing

```bash
pytest tests/
```

### Code Style

We use Black for code formatting and flake8 for linting:

```bash
black src/
flake8 src/
```

## Any contributions you make will be under the MIT Software License

In short, when you submit code changes, your submissions are understood to be under the same [MIT License](http://choosealicense.com/licenses/mit/) that covers the project. Feel free to contact the maintainers if that's a concern.

## Report bugs using GitHub's [issue tracker](https://github.com/nishal21/veyra/issues)

We use GitHub issues to track public bugs. Report a bug by [opening a new issue](https://github.com/nishal21/veyra/issues/new); it's that easy!

## Write bug reports with detail, background, and sample code

**Great Bug Reports** tend to have:

- A quick summary and/or background
- Steps to reproduce
  - Be specific!
  - Give sample code if you can
- What you expected would happen
- What actually happens
- Notes (possibly including why you think this might be happening, or stuff you tried that didn't work)

## Use a Consistent Coding Style

- Use Black for Python code formatting
- Follow PEP 8 style guidelines
- Add type hints where possible
- Write descriptive commit messages

## License

By contributing, you agree that your contributions will be licensed under its MIT License.

## References

This document was adapted from the open-source contribution guidelines for [Facebook's Draft.js](https://github.com/facebook/draft-js/blob/master/CONTRIBUTING.md)