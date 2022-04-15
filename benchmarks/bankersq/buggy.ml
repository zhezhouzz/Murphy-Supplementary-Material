val snoc :
  int ->
  Bankersq.t ->
  int ->
  Bankersq.t ->
  int ->
  int * Bankersq.t * int * Bankersq.t

let snoc (lenf : int) (f : Bankersq.t) (lenr : int) (r : Bankersq.t) (x : int) =
  let (lenr1 : int) = lenr + 1 in
  let (r1 : Bankersq.t) = Bankersq.liblazy (Bankersq.cons x r) in
  if lenr1 <= lenf then (lenf, Bankersq.liblazy (Bankersq.libforce f), lenr1, r1)
  else (lenf + lenr1, Bankersq.concat f r1, 0, Bankersq.liblazy Bankersq.nil)
