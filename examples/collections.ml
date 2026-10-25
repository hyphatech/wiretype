module J = Wiretype

let tags = J.list ~max_items:10 J.string
let point = J.tuple2 J.number J.number
let scores = J.dict ~key:J.string ~value:J.int ()
let parent = J.nullable (J.uuid ())
