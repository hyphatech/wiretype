type 'a page = { items : 'a list; next : string option } [@@deriving wiretype]

let products = page_json Product.product_json
