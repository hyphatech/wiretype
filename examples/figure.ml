module J = Wiretype

type figure = Circle of float | Square of float

let circle =
  J.Object.Case.map "circle"
    (J.Object.map Fun.id
    |> J.Object.mem "radius" J.number ~enc:Fun.id
    |> J.Object.finish)
    ~dec:(fun r -> Circle r)

let square =
  J.Object.Case.map "square"
    (J.Object.map Fun.id
    |> J.Object.mem "side" J.number ~enc:Fun.id
    |> J.Object.finish)
    ~dec:(fun s -> Square s)

let figure_json =
  J.Object.map ~kind:"figure" Fun.id
  |> J.Object.case_mem "type" J.string ~enc:Fun.id
       ~enc_case:(function
         | Circle r -> J.Object.Case.value circle r
         | Square s -> J.Object.Case.value square s)
       [ J.Object.Case.make circle; J.Object.Case.make square ]
  |> J.Object.finish
