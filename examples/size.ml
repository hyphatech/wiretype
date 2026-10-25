module J = Wiretype

type size = Small | Medium | Large

let size_json =
  J.enum ~kind:"size"
    (function Small -> "small" | Medium -> "medium" | Large -> "large")
    [ Small; Medium; Large ]
