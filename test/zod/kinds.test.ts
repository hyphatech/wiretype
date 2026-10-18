// The ready-made kinds' rows, as the browser reads them: ../fixtures/kinds.json,
// the file test/test_wiretype.ml reads too, each row a string, whether the
// server takes it and whether zod does, through the checks the schema printer
// writes for each kind. `zod` is left out where it is the server's answer, and
// null where the browser's runtime decides it: whether `new URL` refuses a bad
// Punycode label depends on the IDNA the runtime was built with. The server
// never takes what the browser refuses; the two differ only where the browser
// is the looser: a URI, an instant past the years 0000 to 9999 once in UTC,
// and a duration longer than an OCaml Mtime.Span.t holds.
import { describe, expect, test } from "bun:test";
import * as z from "zod/mini";
import rows from "../fixtures/kinds.json";

type Row = { text: string; server: boolean; zod?: boolean | null };

const duration =
  /^P(?:\d+W|(?=\d|T\d)(?:\d+D)?(?:T(?=\d)(?:\d+H)?(?:\d+M)?(?:\d+(?:[.,]\d+)?S)?)?)$/;

const kinds: [keyof typeof rows, z.ZodMiniType][] = [
  ["instant", z.iso.datetime({ offset: true })],
  ["date", z.iso.date()],
  ["duration", z.iso.duration().check(z.regex(duration))],
  ["uuid", z.uuid()],
  ["uuid v7", z.uuid({ version: "v7" })],
  ["base64", z.base64()],
  ["base64url", z.base64url()],
  ["uri", z.url()],
  ["ipv4", z.ipv4()],
  ["ipv6", z.ipv6()],
];

// A value, a step, and whether the value is a multiple of it: the rows of
// test/test_wiretype.ml's multiple_rows, which the server decides alike.
const multiples: [value: number, step: number, multiple: boolean][] = [
  [0, 0.01, true],
  [-19.99, 0.01, true],
  [19.99, 0.01, true],
  [0.3, 0.1, true],
  [2.03, 0.07, true],
  [0.35, 0.1, false],
  [1e-7, 1e-8, true],
  [10, 2.5, true],
  [7, 2.5, false],
  [123456789.12, 0.01, true],
  [0.1, 0.3, false],
];

describe("multipleOf, as zod reads it", () => {
  for (const [value, step, multiple] of multiples) {
    test(`${value} a multiple of ${step}: ${multiple}`, () => {
      const read = z.number().check(z.multipleOf(step)).safeParse(value).success;
      expect(read).toBe(multiple);
    });
  }
});

// A whole number, a step, and whether the one is a multiple of the other:
// the rows of test/test_wiretype.ml's int_multiple_rows, through the exact
// check the schema prints for an int, since zod's multipleOf allows a
// rounding error that takes 3000000000000001 for a multiple of 3.
const intMultiples: [value: number, step: number, multiple: boolean][] = [
  [0, 3, true],
  [-9, 3, true],
  [10, 3, false],
  [3000000000000000, 3, true],
  [3000000000000001, 3, false],
  [9007199254740991, 7, false],
];

describe("an int's multiple, as the printed check reads it", () => {
  for (const [value, step, multiple] of intMultiples) {
    test(`${value} a multiple of ${step}: ${multiple}`, () => {
      const read = z
        .int()
        .check(z.refine((n) => n % step === 0))
        .safeParse(value).success;
      expect(read).toBe(multiple);
    });
  }
});

describe("the kinds, as zod reads them", () => {
  for (const [name, schema] of kinds) {
    for (const { text, server, zod = server } of rows[name] as Row[]) {
      test(`${name} ${JSON.stringify(text)}: server ${server}, zod ${zod}`, () => {
        const read = schema.safeParse(text).success;
        if (zod !== null) expect(read).toBe(zod);
        // The rule itself: nothing the server takes is refused here.
        if (server) expect(read).toBe(true);
      });
    }
  }
});
