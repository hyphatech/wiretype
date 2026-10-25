module J = Wiretype

type comment = { text : string; replies : comment list }

let rec comment =
  lazy
    (J.Object.map ~kind:"comment" (fun text replies -> { text; replies })
    |> J.Object.mem "text" J.string ~enc:(fun c -> c.text)
    |> J.Object.mem "replies"
         (J.list (J.rec' comment))
         ~absent:[]
         ~enc:(fun c -> c.replies)
    |> J.Object.finish)

let comment_json = J.rec' comment
