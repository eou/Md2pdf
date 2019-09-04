# md2pdf

## Description

`md2pdf` is a simple tool convert markdown file to PDF file written by Haskell.

## Usage

Please install [HPDF](https://hackage.haskell.org/package/HPDF-1.4.10) and [filemanip](https://hackage.haskell.org/package/filemanip) to run this program. Try `cabal install HPDF` and `cabal install filemanip`.

Then You can run `runghc Main.hs ./Sample/Sample1.md ./Sample/Sample2.md` in the directory to convert several markdown files.

You can also run `runghc Main.hs ./Sample` in the directory to convert all markdown files to PDF files in `Sample` subdirectory.

All PDF files in this project are the outputs of this tool.

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

>Author: Harry

