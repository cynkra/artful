# RTF Parser Reflections

This is an unstructured collection of thoughts on RTF parser considerations.
It should be consolidated at a later date.

## RTF elements

These lists are not exhausitive, and include elements deemed relative to the problem of parsing BMS tables contained in RTF files.

#### Lists

Table structure:

- Row Definition Start: \trowd
- Row End: \row
- Cell End: \cell
- Cell Width: \cellx
- Paragraph in Table: \intbl
- Top Cell Border: \clbrdrt
- Bottom Cell Border: \clbrdrb
- Left Cell Border: \clbrdrl
- Right Cell Border: \clbrdrr

Merging and alignment:

- First Cell in Vertical Merge: \clvmgf
- Subsequent Merged Cell: \clvmrg 
- Vertical Align Top: \clvertalt
- Vertical Align Center: \clvertalc
- Vertical Align Bottom: \clvertalb

Whitespace, indentation, linebreaks:

- Left indent: \li (+ N number of spaces, e.g., \li192)
- Hanging indent: \pnhang
- Line break: \line

#### Notes

- There are no specific table header or footer elements.
- Pandoc drops information about whitepace, indentation and linebreaks.
- Pandoc drops information on cell aligment. In 99% of cases, cell alignment is
a non-issue as the tables are uniform in their dimensions except for the odd table which is split across multiple pages of varying dimensions.

## Parsing considerations

- Pandoc is a well estalbished tool which can take care of the heavy lifting.
This removes a maintenance burden.
- Whitespace, indentation, and linebreaks can be added to the pandoc converted file at minimum effort (currently regex based, but writing a pandoc extension might be a smart move)
- No header and footer rtf elements exist.
This means they have to be inferred at any rate.
Currently the heuristics used in artful surpass those in stateful, with a higher success rate.

