(* $Id: interp.ml,v 1.7 2019-01-29 17:26:15-08 - - $ *)

open Absyn

exception Unimplemented of string
let unimpl reason = raise (Unimplemented reason)

let want_dump = ref false

let interp_memref_var memref = 
	Hashtbl.find Tables.variable_table memref
 
let rec eval_expr (expr : Absyn.expr) : float = match expr with
    | Number number -> number
    | Memref memref -> match memref with 
		|Absyn.Arrayref (v,e) -> unimpl "no arrays yet sorry"(*interp_memref_arr memref  *)
		|Absyn.Variable varian -> eval_expr (interp_memref_var memref)
    | Unary (oper, expr) -> (Hashtbl.find Tables.unary_fn_table oper) (eval_expr expr) 
    | Binary (oper, expr1, expr2) -> (Hashtbl.find Tables.binary_fn_table oper) (eval_expr expr1) (eval_expr expr2)

let interp_print (print_list : Absyn.printable list) =
    let print_item item =
        (print_string " ";
         match item with
         | String string ->
           let regex = Str.regexp "\"\\(.*\\)\""
           in print_string (Str.replace_first regex "\\1" string)
         | Printexpr expr ->
           print_float (eval_expr expr))
    in (List.iter print_item print_list; print_newline ())

let interp_input (memref_list : Absyn.memref list) =
    let input_number memref =
        try  let number = Etc.read_number ()
             in (print_float number; print_newline ())
        with End_of_file -> 
             (print_string "End_of_file"; print_newline ())
    in List.iter input_number 

let interp_let (memref : Absyn.memref) = match memref with 
	|Absyn.Arrayref (var, expr) ->unimpl "no arrays yet"
	|Absyn.Variable var -> Hashtbl.add Tables.variable_table memref expr
	
	
	
	
	
	
	
let interp_stmt (stmt : Absyn.stmt) = match stmt with
    | Dim (ident, expr) -> unimpl "Dim (ident, expr)"
    | Let (memref, expr) -> interp_let memref expr
    | Goto label -> unimpl "Goto label"
    | If (expr, label) -> unimpl "If (expr, label)"
    | Print print_list -> interp_print print_list
    | Input memref_list -> interp_input memref_list

let rec interpret (program : Absyn.program) = match program with
    | [] -> ()
    | firstline::otherlines -> match firstline with
      | _, _, None -> interpret otherlines
      | _, _, Some stmt -> (interp_stmt stmt; interpret otherlines)

let interpret_program program =
    (Tables.init_label_table program; 
     if !want_dump then Tables.dump_label_table ();
     if !want_dump then Dumper.dump_program program;
     interpret program)

