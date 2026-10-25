---
title: What it does not do
---

wiretype describes JSON, reads it and writes it. These it leaves to
something else:

- Keep members an object does not describe. A member an object does not
  describe is skipped, or, with `Object.error_unknown`, a problem.
- Indent its output. What it writes is minified.
- Say where a problem is by line and column; it says by path.
- Read a document as it arrives, or one larger than memory.
- Write a 64-bit integer as a string. An integer past 2^53 is one a
  JavaScript client cannot hold exactly; an id that may be larger
  travels as text, through
  [a kind of your own](/wiretype/guide/by-hand/#kinds-of-your-own).
- Query or edit a `Value.t` by path. `Value.find` is one member of an
  object.
