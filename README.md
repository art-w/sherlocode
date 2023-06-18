**Find it online at [sherlocode.com](https://sherlocode.com) for OCaml projects!**
**And at [sherlocoq.sirref.org](https://sherlocoq.sirref.org) for Coq libraries!** (thanks to [patricoferris](https://github.com/patricoferris/sherlocode/tree/coq) <3)

This is a straightforward implementation of [Regular Expression Matching with a Trigram Index](https://swtch.com/~rsc/regexp/regexp4.html) by Russ Cox. For each of the characters, bigrams and trigrams, we store the set of line numbers where they appear. To answer a query, we first approximate the regex by enumerating the ngrams that are required/optional: the intersection/union of their indexed set yields good line candidates against which we can match the query.
