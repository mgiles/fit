# fit
The FIT protocol is used by many sport and fitness devices made by companies
like Garmin, for example running watches and bike computers. _fit_ provides an
API for parsing these files for analysis or conversion.

Currently this package is a pretty low-level effort, and you'll need to be familiar
with FIT to get much value from it. Specifically, the notion of the FIT "profile" is
ignored entirely, so to make use of the decoded file you'll need to reference the
"Profile.xls" spreadsheet in the [FIT SDK](http://www.thisisant.com/resources/fit).

The `Fit` module exports a convenient set of data types for examining FIT files, as
well as some lenses for extracting specific data. It's intended that the API in the
`Fit` module should be sufficient and convenient for most uses, but if you need access
to the exact structure of the file you can use the data types in `Fit.Internal.FitFile`
and parsers in `Fit.Internal.Parse`.

## Example

Given a FIT file named "file.fit", here's how you could extract all of the "speed" fields
from all of the "record" messages using the lenses in the Messages API. Looking at the FIT
Profile.xls file, you can see that the message number for "record" is `20`, and within a
record the field number for "speed" is `6`. In a GHCi session:

```haskell
Right fit <- readFileMessages "file.fit"
let speeds = fit ^.. message 20 . field 6 . int
```

Now `speeds :: [Int]` is a list of the speed recordings from your activity.
