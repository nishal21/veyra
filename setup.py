#!/usr/bin/env python3

from setuptools import setup, find_packages

with open("README.md", "r", encoding="utf-8") as fh:
    long_description = fh.read()

setup(
    name="veyra",
    version="0.2.0",
    author="Nishal",
    author_email="nishalamv@gmail.com",
    description="A modern programming language for web development and rapid prototyping",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/nishal21/veyra",
    packages=find_packages(where="src"),
    package_dir={"": "src"},
    classifiers=[
        "Development Status :: 3 - Alpha",
        "Intended Audience :: Developers",
        "License :: OSI Approved :: MIT License",
        "Operating System :: OS Independent",
        "Programming Language :: Python :: 3",
        "Programming Language :: Python :: 3.8",
        "Programming Language :: Python :: 3.9",
        "Programming Language :: Python :: 3.10",
        "Programming Language :: Python :: 3.11",
        "Topic :: Software Development :: Interpreters",
        "Topic :: Internet :: WWW/HTTP :: Dynamic Content",
    ],
    python_requires=">=3.8",
    entry_points={
        "console_scripts": [
            "veyra=veyra.cli:main",
        ],
    },
    install_requires=[],
    extras_require={
        "dev": ["pytest", "black", "flake8", "mypy"],
        "test": ["pytest", "pytest-cov"],
    },
    include_package_data=True,
    zip_safe=False,
)