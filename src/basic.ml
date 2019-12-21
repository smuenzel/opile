open! Core
open! Compiler_with_sexp


let rec clean_sexp : Sexp.t -> Sexp.t = function
  | List [ Atom x; _ ] when is_loc x -> List [ ]
  | List [ Atom "txt"; txt ] -> clean_sexp txt
  | List list ->
    List (List.map ~f:clean_sexp list)
    |> omit_nil
    |> filter_list
  | Atom _ as a -> a
and is_loc = String.is_suffix ~suffix:"loc"
and omit_nil : Sexp.t -> Sexp.t = function
  | List [ Atom _; List [ ] ] -> List [ ]
  | other -> other
and filter_list : Sexp.t -> Sexp.t = function
  | List x -> List (List.filter x ~f:(function | List [ ] -> false | _ -> true))
  | other -> other


let f2 str =
  let ttstr, _, _, _ =
    let env = Env.empty in
    Typemod.type_structure env str Location.none
  in
  ttstr
  |> [%sexp_of: Typedtree.structure]

let f str =
  let parsetree = Parse.implementation (Lexing.from_string str) in
  let cleaned_parsetree =
    parsetree
    |> [%sexp_of: Parsetree.structure]
    |> clean_sexp
  in
  let typedtree = 
    let ttstr, _, _, _ =
      let env = Env.empty in
      try
        Typemod.type_structure env parsetree Location.none
      with
      | Typetexp.Error (_,_,error) ->
        raise_s [%message "error typing" (error)]
    in
    ttstr
  in
  let cleaned_typedtree =
    typedtree
    |> [%sexp_of: Typedtree.structure]
    |> clean_sexp
  in
  print_s cleaned_parsetree;
  print_s cleaned_typedtree

let%expect_test "hello" =
  f {|
    let f () =
      let x, y =
        match z with
        | 0 -> 0, 1
        | 1 -> 1, 0
        | _ -> assert false
      in
      x + y
    |};
  [%expect {|
    (((pstr_desc
       (Pstr_value Nonrecursive
        (((pvb_expr
           ((pexp_desc
             (Pexp_fun Nolabel ((ppat_desc (Ppat_construct ((Lident "()")))))
              ((pexp_desc
                (Pexp_let Nonrecursive
                 (((pvb_expr
                    ((pexp_desc
                      (Pexp_match ((pexp_desc (Pexp_ident ((Lident z)))))
                       (((pc_lhs
                          ((ppat_desc (Ppat_constant (Pconst_integer 0)))))
                         (pc_rhs
                          ((pexp_desc
                            (Pexp_tuple
                             (((pexp_desc (Pexp_constant (Pconst_integer 0))))
                              ((pexp_desc (Pexp_constant (Pconst_integer 1))))))))))
                        ((pc_lhs
                          ((ppat_desc (Ppat_constant (Pconst_integer 1)))))
                         (pc_rhs
                          ((pexp_desc
                            (Pexp_tuple
                             (((pexp_desc (Pexp_constant (Pconst_integer 1))))
                              ((pexp_desc (Pexp_constant (Pconst_integer 0))))))))))
                        ((pc_lhs ((ppat_desc Ppat_any)))
                         (pc_rhs
                          ((pexp_desc
                            (Pexp_assert
                             ((pexp_desc (Pexp_construct ((Lident false)))))))))))))))))
                 ((pexp_desc
                   (Pexp_apply ((pexp_desc (Pexp_ident ((Lident +)))))
                    ((Nolabel ((pexp_desc (Pexp_ident ((Lident x))))))
                     (Nolabel ((pexp_desc (Pexp_ident ((Lident y))))))))))))))))))))))) |}]
