let lang1 l1 = 
  match l1 with
  | [] -> false
  | l1 -> List.for_all (fun x1 -> x1 = '1' || x1 = '0') l1;;

let lang2 l2 = 
  match l2 with
  | [] -> true
  | '0' :: tl -> List.for_all (fun x2 -> x2 = '1') tl
  | l2 -> List.for_all (fun x2 -> x2 = '1') l2;;

let lang3 l3 = 
  match l3 with
  | '0' :: tl ->
      (match List.rev tl with
       | '0' :: tl' -> List.for_all (fun x3 -> x3 = '0' || x3 = '1') (List.rev tl')
       | _ -> false)
  | _ -> false;;

let lang4 l4 = 
  ((List.fold_left (fun aux x4 -> if x4 = '1' then 1 + aux else aux) 0 l4) = 2)
  && List.for_all (fun x4 -> x4 = '1' || x4 = '0') l4;;

let rec lang5 l5 = 
  match l5 with
  | ['0'; '0'] | ['1'; '1'] -> true
  | '0' :: '0' :: tl -> lang5 tl
  | '1' :: '1' :: tl -> lang5 tl
  | _ -> false;;

  let recognizers = [lang1; lang2; lang3; lang4; lang5];;

  let belongsTo w = List.map (fun f -> f w) recognizers;;
  