# fit
The FIT file format is used by many sport and fitness devices made by companies
like Garmin. The _fit_ package provides an API for parsing these files for analysis
or conversion.

Currently this package is a pretty low-level effort, and you'll need to be familiar
with FIT to get much value from it. Specifically, the notion of the FIT "profile" is
ignored entirely (for now), so to make use of the decoded file you'll need to reference
the "Profile.xls" spreadsheet in the [FIT SDK](http://www.thisisant.com/resources/fit).

The `Fit` module exports a convenient API for examining a FIT file, as well as some
lenses for extracting information from the resulting data structures. It's intended
that the API in `Fit.Messages` (re-exported in `Fit`) should be sufficient and
convenient for most uses, but if you need access to the exact structure of the file
you can use the data structures in `Fit.Internal.FitFile` and parsers in
`Fit.Internal.Parse`.
