let preds = [| "hd"; "mem"; "ord"; "<"; "==" |]

let op_pool = [| "theta_list"; "theta_int" |]

let libs = [| "Batchedq" |]

let i_err = ([ -7; -6 ], [ -5; 6 ])

let sampling_rounds = 6

let m = 4

let pre (f : Batchedq.t) (r : Batchedq.t) =
  uniq r
  && (not (empty f))
  && (not (empty r))
  && (not (size1 f))
  && not (size1 r)

let post (f : Batchedq.t) (r : Batchedq.t) (f' : Batchedq.t) (r' : Batchedq.t)
    (u : int) =
  iff (mem f' u || mem r' u || hd f u) (mem f u || mem r u)
  && uniq f'
  && implies (empty f') (empty r')
