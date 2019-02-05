(* $Id: interp.ml,v 1.7 2019-01-29 17:26:15-08 - - $ *)

open Absyn

exception Unimplemented of string
let unimpl reason = raise (Unimplemented reason)

let want_dump = ref false


let interp_memref_var var  = 
	Hashtbl.find Tables.variable_table var

let interp_memref_arr (v : Absyn.ident) e = 
	Array.get (Hashtbl.find Tables.array_table v) (int_of_float e)

let rec eval_expr (expr : Absyn.expr) : float = match expr with
    | Number number -> number
    | Memref memref -> (match memref with 
		|Absyn.Arrayref (v,e) -> interp_memref_arr v (eval_expr e)
		|Absyn.Variable varian -> interp_memref_var varian)
    | Unary (oper, expr) -> (Hashtbl.find Tables.unary_fn_table oper) (eval_expr expr) 
    | Binary (oper, expr1, expr2) -> try (Hashtbl.find Tables.binary_fn_table oper) (eval_expr expr1) (eval_expr expr2)  
                                     with Not_found ->   let relop_result = (Hashtbl.find Tables.relop_table oper) (eval_expr expr1) (eval_expr expr2) in
	                  if relop_result = true
					  then 1.
					  else 0.

			
let interp_print (print_list : Absyn.printable list) =
    let print_item item =
        (print_string " ";
         match item with
         | String string ->
           let regex = Str.regexp "\"\\(.*\\)\""
           in print_string (Str.replace_first regex "\\1" string)
         | Printexpr expr ->
           print_float (eval_expr expr))
    in (List.iter print_item print_list; print_newline ()); None


let interp_let (memref : Absyn.memref) (expr : Absyn.expr) =( match memref with 
	|Absyn.Arrayref (v,e) -> Array.set (Hashtbl.find Tables.array_table v)(int_of_float (eval_expr e)) (eval_expr expr)
	|Absyn.Variable var -> Hashtbl.add Tables.variable_table var (eval_expr expr)); None
	

let rec interp_input (memref_list : Absyn.memref list) =
   (* let input_number memref =*)
        try let number = Etc.read_number () in
			match memref_list with

			| first::other -> (match first with	
                                           |Absyn.Arrayref (v,e) -> Array.set (Hashtbl.find Tables.array_table v)(int_of_float (eval_expr e)) (number)
	                                   |Absyn.Variable var -> Hashtbl.add Tables.variable_table var number);
                                           interp_input other

			| [] ->None

        with End_of_file -> 
             (print_string "End_of_file"; print_newline ();None)
    (*in List.iter input_number; None *)

	
let interp_dim (ident : Absyn.ident) (expr : Absyn.expr ) = 
	Hashtbl.add Tables.array_table ident (Array.make (int_of_float (eval_expr expr)) 0.); None
	
let interp_goto label : Absyn.program option =
		Some (Hashtbl.find Tables.label_table label)
	
let interp_if (expr : Absyn.expr) label : Absyn.program option =
	 let relop_return = (eval_expr expr) in
											if relop_return = 1.
											then interp_goto label
											else None
	
let interp_stmt (stmt : Absyn.stmt) : (Absyn.program option) = match stmt with
    | Dim (ident, expr) ->  interp_dim ident expr
    | Let (memref, expr) -> interp_let memref expr
    | Goto label -> interp_goto label
    | If (expr, label) -> interp_if expr label
    | Print print_list -> interp_print print_list
    | Input memref_list -> interp_input memref_list

let rec interpret (program : Absyn.program) = match program with
    | [] -> ()
    | firstline::otherlines -> match firstline with
      | _, _, None -> interpret otherlines
      | _, _, Some stmt -> let stmt_return = (interp_stmt stmt) in 
		(match stmt_return with
		 |None  -> interpret otherlines
		 |Some line -> interpret line)
		

let interpret_program program =
    (Tables.init_label_table program; 
     if !want_dump then Tables.dump_label_table ();

     interpret program)

