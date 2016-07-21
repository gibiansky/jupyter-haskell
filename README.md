# `jupyter`: Jupyter ↔ Haskell

The `jupyter` package provides a type-safe high-level interface for interacting with Jupyter kernels
and clients using the [Jupyter messaging
protocol](https://jupyter-client.readthedocs.io/en/latest/messaging.html), without having to deal
with the low-level details of network communication and message encoding and decoding.

## What is Jupyter?

The [Jupyter project](http://jupyter.readthedocs.io/en/latest/) is a set of tools and applications
for working with interactive coding environments. Jupyter provides the architecture and frontends,
and delegates the language-specific details to external programs called Jupyter *kernels*.
