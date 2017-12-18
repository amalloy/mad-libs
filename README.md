# mad-libs

Accepts a .flow grammar file on stdin, and writes to stdout a random sentence
from that grammar.

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
