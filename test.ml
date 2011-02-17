module B = struct
  open Bigarray
  let da1 = Array1.create float64 c_layout
  let ia1 = Array1.create int32 c_layout
end

open C_knitro
open Knitro_consts

let _ =
  let n = 2 in
  let m = 2 in
  let nnz_j = 4 in
  let nnz_h = 3 in
  
  let k = 

    let x_lo_bnds = B.da1 n in
    x_lo_bnds.{0} <- -.k_infbound;
    x_lo_bnds.{1} <- -.k_infbound;

    let x_up_bnds = B.da1 n in
    x_up_bnds.{0} <- 0.5;
    x_up_bnds.{1} <- k_infbound;

    let x_initial = B.da1 n in
    x_initial.{0} <- -.2.0;
    x_initial.{1} <-   1.0;

    let c_type = B.ia1 m in
    c_type.{0} <- Int32.of_int k_contype_quadratic;
    c_type.{1} <- Int32.of_int k_contype_quadratic;

    let c_lo_bnds = B.da1 m in
    c_lo_bnds.{0} <- 1.0;
    c_lo_bnds.{1} <- 0.0;

    let c_up_bnds = B.da1 m in
    c_up_bnds.{0} <- k_infbound;
    c_up_bnds.{1} <- k_infbound;

    let jac_index_cons = B.ia1 nnz_j in
    jac_index_cons.{0} <- 0l;
    jac_index_cons.{1} <- 0l;
    jac_index_cons.{2} <- 1l;
    jac_index_cons.{3} <- 1l;

    let jac_index_vars = B.ia1 nnz_j in
    jac_index_vars.{0} <- 0l;
    jac_index_vars.{1} <- 1l;
    jac_index_vars.{2} <- 0l;
    jac_index_vars.{3} <- 1l;

    let hess_index_rows = B.ia1 nnz_h in
    hess_index_rows.{0} <- 0l;
    hess_index_rows.{1} <- 0l;
    hess_index_rows.{2} <- 1l;

    let hess_index_cols = B.ia1 nnz_h in
    hess_index_cols.{0} <- 0l;
    hess_index_cols.{1} <- 1l;
    hess_index_cols.{2} <- 1l;

    let obj_type = k_objtype_general in
    let obj_goal = k_objgoal_minimize in

    let k = create () in

    set_param_int k k_param_hessopt k_hessopt_exact;
    set_param_int k k_param_outmode k_outmode_screen;
    set_param_int k k_param_outlev k_outlev_all;

    (* copy made of this data! *) 
    let status = init k {
      n = n ;
      obj_goal = obj_goal;
      obj_type = obj_type;
      x_lo_bnds = x_lo_bnds;
      x_up_bnds = x_up_bnds;
      m = m;
      c_type = c_type;
      c_lo_bnds = c_lo_bnds;
      c_up_bnds = c_up_bnds;
      nnz_j = nnz_j;
      jac_index_vars = jac_index_vars ;
      jac_index_cols = jac_index_cons;  
      nnz_h = nnz_h;
      hess_index_rows = hess_index_rows;
      hess_index_cols = hess_index_cols;
      x_initial = x_initial;
      lambda_initial_opt  = None
    }
    in
    ignore status;
    k
  in

  let x = B.da1 n in
  let lambda = B.da1 (m+n) in
  let c = B.da1 m in
  let obj_grad = B.da1 n in
  let jac = B.da1 nnz_j in
  let hess = B.da1 nnz_h in
  let hess_vector = B.da1 n in
  let obj = B.da1 1 in

  let p = {
    x = x;
    lambda = lambda;
    eval_status = 0;
    obj = obj;
    c = c;
    obj_grad = obj_grad;
    jac = jac;
    hess = hess;
    hess_vector = hess_vector;
  }
  in

  let eval_fc () =
    let t1 = x.{1} -. x.{0} *. x.{0} in
    let t2 = 1.0 -. x.{0} in
    obj.{0} <- 100.0 *. ( t1 *. t1 ) +. ( t2 *. t2 );
    c.{0} <- x.{0} *. x.{1};
    c.{1} <- x.{0} +. ( x.{1} *. x.{1} ); 
  in

  let eval_ga () =
    let t1 = x.{1} -. x.{0} *. x.{0} in
    obj_grad.{0} <- (-400.0 *. t1 *. x.{0}) -. (2.0 *. (1.0 -. x.{0}));
    obj_grad.{1} <- 200.0 *. t1;

    jac.{0} <- x.{1};
    jac.{1} <- x.{0};
    jac.{2} <- 1.0;
    jac.{3} <- 2.0 *. x.{1};
  in

  let eval_h () =
    hess.{0} <- (-400.0 *. x.{1}) +. (1200.0 *. x.{0} *. x.{0}) +. 2.0;
    hess.{1} <- (-400.0 *. x.{0}) +. lambda.{0};
    hess.{2} <- 200.0 +. (lambda.{1} *. 2.0);
  in

  let eval_hv () =
    (* ---- H[0,0]*v[0] + H[0,1]*v[1]. *)
    let t0 = ((-400.0 *. x.{1}) +. (1200.0 *. x.{0} *. x.{0}) +. 2.0) *. hess_vector.{0}
      +. ((-400.0 *. x.{0}) +. lambda.{0}) *. hess_vector.{1} in

    (* ---- H[1,0]*v[0] + H[1,1]*v[1]. *)
    let t1 = ((-400.0 *. x.{0}) +. lambda.{0}) *. hess_vector.{0} 
      +. (200.0 +. (lambda.{1} *. 2.0)) *. hess_vector.{1} in

    hess_vector.{0} <- t0;
    hess_vector.{1} <- t1;
  in


  let print_x () =
    Printf.printf "x=[";
    for i = 0 to n-1 do
      Printf.printf "%f " x.{i}
    done;
    print_endline "]"
  in

  try
    while true do 
      let status = solve k p in
      if status = k_rc_evalfc then
	eval_fc ()
      else if status = k_rc_evalga then
	eval_ga ()
      else if status = k_rc_evalh then 
	eval_h ()
      else if status = k_rc_evalhv then 
	eval_hv ()
      else if status = k_rc_newpoint then
	print_x ()
      else
	(* done *)
	raise Sys.Break

    done
  with Sys.Break ->
    flush stdout;
    print_x ()


(*
Copyright (c) 2011, barko 00336ea19fcb53de187740c490f764f4
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:
      
1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the
   distribution.

3. Neither the name of barko nor the names of contributors may be used
   to endorse or promote products derived from this software without
   specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)
