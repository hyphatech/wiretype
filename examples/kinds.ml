module J = Wiretype

let back name description text =
  match J.decode description text with
  | Error problems ->
      Printf.printf "%-9s %s: %s\n" name text
        (J.Problem.list_to_string problems)
  | Ok v -> (
      match J.encode description v with
      | Ok written -> Printf.printf "%-9s %s -> %s\n" name text written
      | Error u ->
          Printf.printf "%-9s %s: %s\n" name text (J.Unwritable.to_string u))

let () =
  back "instant" J.instant {|"2026-09-30T14:00:00.123456+02:00"|};
  back "date" J.date {|"2026-09-30"|};
  back "duration" J.duration {|"PT1H30M"|};
  back "uuid" (J.uuid ()) {|"0190F6E8-7E0B-7C55-8F1A-3C1D2B4A5E6F"|};
  back "base64" J.base64 {|"aGVsbG8="|};
  back "base64url" J.base64url {|"aGVsbG8"|};
  back "uri" J.uri {|"https://example.com/a?b=c"|};
  back "uri" J.uri {|"https://example.com:99999/"|};
  back "ipv4" J.ipv4 {|"192.168.0.1"|};
  back "ipv4" J.ipv4 {|"192.168.0.01"|};
  back "ipv6" J.ipv6 {|"FE80::1"|}
