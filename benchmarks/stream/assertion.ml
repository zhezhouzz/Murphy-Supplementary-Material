let preds = [| "hd"; "mem"; "ord"; "<"; "==" |]

let op_pool = [| "theta_stream"; "theta_int" |]

let libs = [| "Stream" |]

let i_err = ([ -1 ], [ -3; -5 ])

let sampling_rounds = 6

let m = 4

let pre (acc : Stream.t) (s : Stream.t) (u : int) (v : int) =
  strict_sort acc && strict_sort_rev s && less_mem s acc

let post (acc : Stream.t) (s : Stream.t) (nu : Stream.t) = strict_sort nu
