let preds = [| "hd"; "mem"; "ord"; "<"; "==" |]

let op_pool = [| "theta_list"; "theta_int" |]

let libs = [| "List" |]

let i_err = ([ 1; 2 ], [ 3; 4 ])

let sampling_rounds = 6

let m = 4

let pre (l1 : List.t) (l2 : List.t) = strict_sort l1 && strict_sort l2

let post (l1 : List.t) (l2 : List.t) (l3 : List.t) = strict_sort l3
