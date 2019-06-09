# md2pdf

## Description

`md2pdf` is a simple tool convert markdown file to PDF file written by Haskell.

## Usage

Please install [HPDF](https://hackage.haskell.org/package/HPDF-1.4.10) package to run this program. Try `cabal install HPDF` or other ways suggested in the doc.

Then run `runghc Main.hs fileName.md` in the directory. You can find `fileName.pdf` in this folder soon.


The `README.pdf` in this project is an example converted from this readme file.

## Feature

Since `HPDF` is an incomplete PDF package, this md2pdf tool can only convert some basic markdown format very roughly:

- Headings (from Heading 1 (biggest) to Heading 6 (smallest));
- *Italic text*;
- **Emphasis text**;
- `Inline Code`;
- Code;
- Quote;
- Hyperlink;
- Unordered list;

## Contribution

>Author: Hangyi Wang


