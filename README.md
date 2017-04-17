# Harky

## Setup on Linux (native)

Install stack (if not already present):

    wget -qO- https://get.haskellstack.org/ | sh

Setup environment:

    cd harky
    stack setup

Run tests:

    stack test

Build executable:

    stack build

## Using Docker

Install stack (if not already present):

    wget -qO- https://get.haskellstack.org/ | sh

Setup environment:

    cd harky
    stack docker pull

Run tests:

    stack --docker test

Build executable:

    stack --docker build

## Compliance

Harky does not comply fully  with any previously published Markdown specification. However, it supports the following features:

* ATX and setext headlines (without suffixes)
* single-level lists
* blockquotes
* paragraphs
* emphasized and strongly emphasized inlines
* thematic breaks
* inline code
* simple links (inline href)
