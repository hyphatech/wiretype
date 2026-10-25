module J = Wiretype

let quantity = J.int_bounded ~min:1 ~max:99 ()
let even = J.int_bounded ~multiple_of:2 ()
let price = J.number_bounded ~above:0. ~multiple_of:0.01 ()
let ratio = J.number_bounded ~min:0. ~below:1. ()
let name = J.string_bounded ~min_length:1 ~max_length:4 ()
let tags = J.list ~max_items:2 J.string
let scores = J.dict ~min_properties:1 ~key:J.string ~value:J.int ()
