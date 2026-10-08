// The ready-made kinds' rows, as the browser reads them: the same strings as
// test/test_wiretype.ml's, each with whether the server takes it and
// whether zod does, through the checks the schema printer writes for each
// kind. The server never takes what the browser refuses; the two differ only
// where the browser is the looser: a URI, an instant past the years 0000 to
// 9999 once in UTC, and a duration past a hundred thousand years.
import assert from "node:assert/strict";
import { describe, test } from "node:test";
import * as z from "zod/mini";

// A browser of null is the runtime's to decide: whether `new URL` refuses a
// bad Punycode label depends on the IDNA its Node was built with.
type Row = [text: string, server: boolean, browser: boolean | null];

const duration =
  /^P(?:\d+W|(?=\d|T\d)(?:\d+D)?(?:T(?=\d)(?:\d+H)?(?:\d+M)?(?:\d+(?:[.,]\d+)?S)?)?)$/;

const kinds: [string, z.ZodMiniType, Row[]][] = [
  [
    "instant",
    z.iso.datetime({ offset: true }),
    [
      ["2026-09-30T12:00:00Z", true, true],
      ["2026-09-30T12:00:00.5Z", true, true],
      ["2026-09-30T12:00:00.123456789+05:30", true, true],
      ["0000-01-01T00:00:00Z", true, true],
      ["2024-02-29T23:59:59-23:59", true, true],
      ["9999-12-31T23:59:59.999Z", true, true],
      ["9999-12-31T23:59:59-00:01", false, true],
      ["0000-01-01T00:00:00+00:01", false, true],
      ["2026-09-30t12:00:00Z", false, false],
      ["2026-09-30T12:00:00z", false, false],
      ["2026-09-30T12:00Z", false, false],
      ["2026-09-30T12:00:60Z", false, false],
      ["2026-02-29T00:00:00Z", false, false],
      ["2026-09-30T24:00:00Z", false, false],
      ["2026-09-30T12:00:00", false, false],
      ["2026-09-30T12:00:00+24:00", false, false],
      ["2026-09-30 12:00:00Z", false, false],
      ["2026-09-30T12:00:00.Z", false, false],
    ],
  ],
  [
    "date",
    z.iso.date(),
    [
      ["2026-09-30", true, true],
      ["2024-02-29", true, true],
      ["2000-02-29", true, true],
      ["0000-02-29", true, true],
      ["1900-02-29", false, false],
      ["2026-02-29", false, false],
      ["2026-13-01", false, false],
      ["2026-9-30", false, false],
      ["20260930", false, false],
    ],
  ],
  [
    "duration",
    z.iso.duration().check(z.regex(duration)),
    [
      ["PT1H30M", true, true],
      ["P1W", true, true],
      ["P1D", true, true],
      ["P1DT2H", true, true],
      ["PT0.5S", true, true],
      ["PT1,5S", true, true],
      ["PT0S", true, true],
      ["P1DT1H1M1.001S", true, true],
      ["P1Y", false, false],
      ["P1M", false, false],
      ["P1Y2M", false, false],
      ["PT", false, false],
      ["P", false, false],
      ["P1W1D", false, false],
      ["1D", false, false],
      ["PT1.5M", false, false],
      ["P1DT", false, false],
      ["P36525000D", true, true],
      ["P36525001D", false, true],
      ["P99999999999D", false, true],
      ["P1000000000000W", false, true],
      ["PT3000000000000000H", false, true],
      ["PT9999999999999999S", false, true],
    ],
  ],
  [
    "uuid",
    z.uuid(),
    [
      ["01890a5d-ac96-7a3b-9e5a-5f1c2a7b8c9d", true, true],
      ["01890A5D-AC96-7A3B-9E5A-5F1C2A7B8C9D", true, true],
      ["f47ac10b-58cc-4372-a567-0e02b2c3d479", true, true],
      ["00000000-0000-0000-0000-000000000000", true, true],
      ["ffffffff-ffff-ffff-ffff-ffffffffffff", true, true],
      ["FFFFFFFF-FFFF-FFFF-FFFF-FFFFFFFFFFFF", false, false],
      ["01890a5d-ac96-0a3b-9e5a-5f1c2a7b8c9d", false, false],
      ["01890a5d-ac96-7a3b-ce5a-5f1c2a7b8c9d", false, false],
      ["01890a5dac96-7a3b-9e5a-5f1c2a7b8c9d", false, false],
      ["01890a5d-ac96-7a3b-9e5a-5f1c2a7b8c9", false, false],
    ],
  ],
  [
    "uuid v7",
    z.uuid({ version: "v7" }),
    [
      ["01890a5d-ac96-7a3b-9e5a-5f1c2a7b8c9d", true, true],
      ["f47ac10b-58cc-4372-a567-0e02b2c3d479", false, false],
      ["00000000-0000-0000-0000-000000000000", false, false],
    ],
  ],
  [
    "base64",
    z.base64(),
    [
      ["", true, true],
      ["QQ==", true, true],
      ["QUI=", true, true],
      ["QUJD", true, true],
      ["QR==", true, true],
      ["QQ", false, false],
      ["QQ=", false, false],
      ["Q===", false, false],
      ["QU JD", false, false],
      ["QUJD=", false, false],
      ["-_8=", false, false],
    ],
  ],
  [
    "base64url",
    z.base64url(),
    [
      ["", true, true],
      ["QQ", true, true],
      ["QUI", true, true],
      ["QUJD", true, true],
      ["-_8", true, true],
      ["QQ==", false, false],
      ["Q", false, false],
      ["QU+D", false, false],
    ],
  ],
  [
    "uri",
    z.url(),
    [
      ["https://example.com/path?q=1#frag", true, true],
      ["mailto:a@b.c", true, true],
      ["urn:isbn:0451450523", true, true],
      ["http://[::1]:8080/", true, true],
      ["http://[::ffff:192.0.2.1]/", true, true],
      ["ftp://user:pw@host/x", true, true],
      ["https://example.com:65535", true, true],
      ["https://192.0.2.1/", true, true],
      ["foo:bar", true, true],
      ["example.com", false, false],
      ["https://exa mple.com", false, false],
      ["http://", false, false],
      ["https:", false, false],
      ["https://example.com:65536", false, false],
      ["http://[v1.fe]/", false, false],
      ["https://999.1.1.1/", false, false],
      ["https://example.com/a b", false, true],
      ["http:example.com", false, true],
      ["https://xn--nxasmq6b.com/", true, true],
      ["https://xn--mnchen-3ya.de/", true, true],
      ["https://xn--ls8h.la/", true, true],
      ["https://xn--zz.com/", false, null],
      ["https://xn--abc-.com/", false, null],
      ["https://xn--xn--a--gua.pt/", false, null],
      ["file:///etc/hosts", true, true],
      ["file://host/x", true, true],
      ["file://host:80/x", false, false],
      ["file://u@host/x", false, false],
      ["file://1.2.3.999/", false, false],
      ["foo://", true, true],
      ["foo://:80", false, false],
      ["foo://u@", false, false],
      ["foo://u@:1", false, false],
      ["http://[1.2.3.4::]/", false, false],
    ],
  ],
  [
    "ipv4",
    z.ipv4(),
    [
      ["192.0.2.1", true, true],
      ["0.0.0.0", true, true],
      ["255.255.255.255", true, true],
      ["256.0.0.1", false, false],
      ["01.2.3.4", false, false],
      ["1.2.3", false, false],
      ["1.2.3.4.5", false, false],
      [" 1.2.3.4", false, false],
    ],
  ],
  [
    "ipv6",
    z.ipv6(),
    [
      ["::", true, true],
      ["::1", true, true],
      ["2001:db8::1", true, true],
      ["1:2:3:4:5:6:7:8", true, true],
      ["1:2:3:4:5:6:7::", true, true],
      ["::2:3:4:5:6:7:8", true, true],
      ["FE80::1", true, true],
      ["1:2:3:4:5:6:7:8:9", false, false],
      [":::", false, false],
      ["1::2::3", false, false],
      ["::ffff:192.0.2.1", true, true],
      ["::ffff:192.0.2.256", false, false],
      ["fe80::1%eth0", false, false],
      ["12345::", false, false],
      ["1:2:3:4:5:6:7", false, false],
      ["1.2.3.4::", false, false],
      ["1.2.3.4::1", false, false],
      ["1:1.2.3.4::", false, false],
      ["1:2:3:4:5:6:1.2.3.4", true, true],
      ["1:2:3:4:5:6:7:1.2.3.4", false, false],
    ],
  ],
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
      assert.equal(read, multiple);
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
      assert.equal(read, multiple);
    });
  }
});

describe("the kinds, as zod reads them", () => {
  for (const [name, schema, rows] of kinds) {
    for (const [text, server, browser] of rows) {
      test(`${name} ${JSON.stringify(text)}: server ${server}, browser ${browser}`, () => {
        const read = schema.safeParse(text).success;
        if (browser !== null) assert.equal(read, browser);
        // The rule itself: nothing the server takes is refused here.
        if (server) assert.equal(read, true);
      });
    }
  }
});
