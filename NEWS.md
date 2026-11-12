# wfmR 0.0.1

First numbered version. It records every change since the initial commit
(30 July 2026), which set up the chart palettes, colour-vision diagnostics,
the ggplot2 theme, export helpers, and the Quarto HTML, PDF and revealjs
formats.

## Charts

* `wfm_colourblind()` switches every request for the primary chart palette,
  including the defaults of `wfm_palette()`, `scale_colour_wfm()` and
  `scale_fill_wfm()`, to the Wong (Colour Universal Design) palette. The
  setting lives in the `wfmR.colourblind` option and applies at draw time,
  so existing plot objects follow it. Named palettes, `wfm_colours()`
  lookups, constants and continuous scales are unchanged.
* `wfm_save_plot()` exports by path, and the new `wfm_save_all()` and
  `wfm_export_plot_data()` save every format of a chart and the data
  behind it. Chart fonts now match the Quarto formats' fonts.

## Quarto formats (`wfmr` and `wfmr-academic`, extension version 0.0.2)

* Headings: second-level PDF headings sit one point above the 11pt body
  text. Fourth-level headings are bold and fifth-level headings italic,
  both never numbered and on their own line with almost no space before
  the paragraph. Sixth-level headings run into their paragraph as
  "Heading.---Text" through the new `heading-six.lua` filter. The PDF
  previously had no fifth-level format, so documents using one failed to
  compile.
* PDF line spacing falls from 1.2 to 1.1, and HTML body text sets at a
  line height of 1.45.
* Figure captions set at footnote size in the PDF and 0.8rem in HTML;
  tables and table captions set a step below body text.
* Footnote text stays aligned, with a gap after each hanging marker.

## Other

* The README no longer has a Style Notes section.
* `logo/` holds Earthrise logo drafts and the script that draws them; it
  is excluded from the package build.
