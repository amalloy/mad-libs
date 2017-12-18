# mad-libs

Accepts a .flow grammar file on stdin, and writes to stdout random sentences
from that grammar, continuing forever until stdout is closed (like
`/usr/bin/yes`).

## .flow grammar rules

A mad-libs flow rule is a recursive decision graph with three types of elements:

- A terminal string
- A choice between two or more sub-grammars
- A sequence of two or more sub-grammars

A more formal (but still not really rigorous) grammar:

```
rule = choice | sequence | text
choice = '[' rule ('|' rule)* ']'
sequence = '(' rule (',' rule)* ')'
```

`text` is any number of characters which are not used to define choices or
sequences. If a `text` element must contain one of those special characters, it
may be scaped with a leading backslash. Leading and trailing spaces are ignored
in all rules.

## Sample usage

```
$ stack exec mad-libs < xkcd.flow  | head
Did you know that the latest sunset drifts out of sync with the moon because of a decree by the Pope in the 1500s ? Apparently that's why we have leap seconds .
Did you know that the blood moon drifts out of sync with the Zodiac because of a decree by the Pope in the 1500s ? Apparently it was even more extreme during the 1990s .
Did you know that the Summer Olympics might happen twice this year because of magnetic field reversal ? Apparently it's getting worse and no one knows why .
Did you know that daylight savings time drifts out of sync with the sun because of an arbitrary decision by FDR ? Apparently it's getting worse and no one knows why .
Did you know that leap year might not happen this year because of magnetic field reversal ? Apparently there's a proposal to fix it, but it might be unconstitutional .
Did you know that the earliest sunset drifts out of sync with the moon because of magnetic field reversal ? Apparently it's getting worse and no one knows why .
Did you know that daylight savings time might not happen this year because of magnetic field reversal ? Apparently it was even more extreme during the bronze age .
Did you know that Toyota Truck Month might not happen this year because of magnetic field reversal ? Apparently there's a proposal to fix it, but it is stalled in Congress .
Did you know that the latest sunrise might not happen this year because of precession of the moon ? Apparently that's why we have leap seconds .
Did you know that Toyota Truck Month drifts out of sync with the Gregorian Calendar because of a decree by the Pope in the 1500s ? Apparently it was even more extreme during the bronze age .
```
