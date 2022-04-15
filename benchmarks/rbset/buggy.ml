val balance : bool -> int -> Rbset.t -> Rbset.t -> Rbset.t

let rec balance (r : bool) (p : int) (tree1 : Rbset.t) (tree2 : Rbset.t) =
  match (r, p, tree1, tree2) with
  | _
    when ( false,
           (z : int),
           Rbset.node true
             (y : int)
             (Rbset.node true (x : int) (a : Rbset.t) (b : Rbset.t))
             (c : Rbset.t),
           (d : Rbset.t) ) ->
      Rbset.node true y (Rbset.node false x a b) (Rbset.node false z c d)
  | _
    when ( false,
           (z : int),
           Rbset.node true
             (x : int)
             (a : Rbset.t)
             (Rbset.node true (y : int) (b : Rbset.t) (c : Rbset.t)),
           (d : Rbset.t) ) ->
      Rbset.node true y (Rbset.node false x a b) (Rbset.node false z c d)
  | _
    when ( false,
           (x : int),
           (a : Rbset.t),
           Rbset.node true
             (z : int)
             (Rbset.node true (y : int) (b : Rbset.t) (c : Rbset.t))
             (d : Rbset.t) ) ->
      Rbset.node false z (Rbset.node true x a (Rbset.node true y b c)) d
  | _
    when ( false,
           (x : int),
           (a : Rbset.t),
           Rbset.node true
             (y : int)
             (b : Rbset.t)
             (Rbset.node true (z : int) (c : Rbset.t) (d : Rbset.t)) ) ->
      Rbset.node true y (Rbset.node false x a b) (Rbset.node false z c d)
  | _ when ((k : bool), (x : int), (a : Rbset.t), (b : Rbset.t)) ->
      Rbset.node k x a b
