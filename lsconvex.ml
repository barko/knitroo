type pair = float * float 

(* using formulation described in:
   http://abel.ee.ucla.edu/cvxopt/examples/book/cvxfit.html

   minimize    (1/2) * || yhat - y ||_2^2
   subject to  yhat[j] >= yhat[i] + g[i] * (u[j] - u[i]), j, i = 0,...,p-1
   Variables  yhat (p), g (p).

   so, in terms of the Knitro vocabulary:

   x = [ yhat[0] ... y_hat[p-1] ; g[0] ... g[p-1] ]'

   gradient of f(x) (the objective):
   [ yhat[0  ] - y[0  ] ]
   [ yhat[1  ] - y[1  ] ] 
   [ ...                ]
   [ yhat[p-1] - y[p-1] ]
   [ 0                  ]
   [ 0                  ]
   [ ...                ]
   [ 0                  ]
 
   Hessian of f(x):
   H[i,j] = 1  for i = j , i < p
   H[i,j] = 0  otherwise

   rewriting the constraints in the form cL <= c(x) <= cU:
   first:
      yhat[i] + g[i]' * (u[j] - u[i]) <= yhat[j] , j, i = 0,...,p-1
   then substracting yhat[j]:
      -inf <= yhat[i] - yhat[j] + g[i] * (u[j] - u[i]) <= 0, , j, i = 0,...,p-1

   However, the constraints in which i = j are not informative, so we
   eliminate them:
      -inf <= yhat[i] - yhat[j] + g[i] * (u[j] - u[i]) <= 0, , j, i = 0,...,p-1, i <> j


   so, we have m = (p x (p-1)) linear constraints:

   each constraint k = 0 ... m-1 has a derivative with nonzero 
   elements at:

     r=i   -> +1, 
     r=j   -> -1,
     r=p+i -> (u[j] - u[i])
   
   with
     k = j * (p-1) + i     

*)

module B = struct
  open Bigarray
  let da1 = Array1.create float64 c_layout
  let ia1 = Array1.create int32 c_layout
end

open C_knitro
open Knitro_consts

let fit points =
  let u_list, y_list = List.split points in
  let u = Array.of_list u_list in
  let y = Array.of_list y_list in

  let p = Array.length u in

  (* number of variables *)
  let n = 2 * p in

  (* number of constraints *)
  let m = p * (p - 1) in

  (* number of non-zeros in the Hessian *)
  let nnz_h = p in

  (* number of nonzeros in the jacobian: each constraint derivative
     has three non zeros: *)
  let nnz_j = m * 3 in

  (* jacobian: since the constraints are linear, the jacobian is
     constant and therefore not recomputed in [eval_ga] *)
  let jac = B.da1 nnz_j in

  (* hessian: since the objective is quadratic, its second derivative
     is constant and therefore it is not recomputed in [eval_h] *)
  let hess = B.da1 nnz_h in

  let ktr = 

    (* variables *)
    let x_lo_bnds = B.da1 n in
    let x_up_bnds = B.da1 n in
    let x_initial = B.da1 n in

    begin
      for r = 0 to p-1 do
	(* yhat are unbounded *)
	x_lo_bnds.{r} <- -.k_infbound;
	x_up_bnds.{r} <-   k_infbound;
	x_initial.{r} <- y.(r);
      done;

      for r = p to n-1 do
	(* g are unbounded *)
	x_lo_bnds.{r} <- -.k_infbound;
	x_up_bnds.{r} <-   k_infbound;
	x_initial.{r} <- 0.0
      done;

    end;
    
    let hess_index_rows = B.ia1 nnz_h in
    let hess_index_cols = B.ia1 nnz_h in

    begin
      (* objective only depends on y_hat *)
      let z = ref 0 in
      for r = 0 to p-1 do
	let r = Int32.of_int r in
	hess_index_rows.{!z} <- r;
	hess_index_cols.{!z} <- r;
	hess.{!z} <- 1.0;
	incr z
      done;
      assert (!z = nnz_h)
    end;
    

    let c_type    = B.ia1 m in
    let c_lo_bnds = B.da1 m in
    let c_up_bnds = B.da1 m in

    let jac_index_vars = B.ia1 nnz_j in
    let jac_index_cons = B.ia1 nnz_j in

    begin
      let z = ref 0 in
      let k = ref 0 in

      for j = 0 to p-1 do
	for i = 0 to p-1 do
	  if i <> j then (

	    c_type.{!k} <- Int32.of_int k_contype_linear;
	    c_lo_bnds.{!k} <- -.k_infbound;
	    c_up_bnds.{!k} <- 0.0;

	    begin 
	      let k = Int32.of_int !k in

	      jac_index_cons.{!z} <- k;
	      jac_index_vars.{!z} <- Int32.of_int i;
	      jac.{!z} <- 1.0;

	      incr z;

	      jac_index_cons.{!z} <- k;
	      jac_index_vars.{!z} <- Int32.of_int j;
	      jac.{!z} <- -1.0;

	      incr z;

	      jac_index_cons.{!z} <- k;
	      jac_index_vars.{!z} <- Int32.of_int (p+i);
	      jac.{!z} <- u.(j) -. u.(i);

	      incr z;
	    end;

	    incr k;
	  )

	done
      done;

      assert (!z = nnz_j);
      assert (!k = m);
    end;

    let obj_type = k_objtype_quadratic in
    let obj_goal = k_objgoal_minimize in

    let ktr = create () in

    set_param_int ktr k_param_hessopt k_hessopt_exact;
    set_param_int ktr k_param_outmode k_outmode_screen;
    set_param_int ktr k_param_outlev k_outlev_all;

    (* copy made of this data! *) 
    let status = init ktr {
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
    ktr
  in

  let x = B.da1 n in
  let lambda = B.da1 (m+n) in
  let c = B.da1 m in
  let obj_grad = B.da1 n in
  let hess_vector = B.da1 n in (* not really used *)
  let obj = B.da1 1 in

  let pr = {
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

  (* evaluate the objective function f and the constraints c *)
  let eval_fc () =
    (* eval f *)
    begin 
      let norm = ref 0. in
      for r = 0 to p-1 do
	let diff = x.{r} -. y.(r) in
	norm := !norm +. diff *. diff 
      done;
      norm := 0.5 *. !norm;
      obj.{0} <- !norm;
    end;

    (* eval c *)
    begin 
      (* loops analogous to that used for setting sparsity of
	 jacobian, above: *)
      let k = ref 0 in
      for j = 0 to p-1 do
	for i = 0 to p-1 do
	  if j <> i then (
	    let yhat_i = x.{i} in
	    let yhat_j = x.{j} in
	    let g_i = x.{p+i} in
	    c.{!k} <- yhat_i -. yhat_j +. g_i *. ( u.(j) -. u.(i) );
	    incr k
	  )
	done
      done;
      assert (!k = m)
    end

  in

  begin 
    (* derivative with respect to g is zero *)
    for r = p to n-1 do
      obj_grad.{r} <- 0.;
    done
  end;
  
  (* evaluate the gradient of the objective; since the constraints are
     linear, the jacobian (gradient of the constraints) is constant,
     so the jacobian is not computed here but above. *)
  let eval_ga () =
    for r = 0 to p-1 do
      obj_grad.{r} <- x.{r} -. y.(r);
    done;
  in

  (* Since the objective is quadratic, the hessian is constant, so
     there's nothing to compute here: *)
  let eval_h () =
    ()
  in

  begin
    try
      while true do 
	let status = solve ktr pr in
	if status = k_rc_evalfc then
	  eval_fc ()
	else if status = k_rc_evalga then
	  eval_ga ()
	else if status = k_rc_evalh then 
	  eval_h ()
	else if status = k_rc_evalhv then 
	  failwith "hessian-vector multiplication not enabled"
	else if status = k_rc_newpoint then
	  ()
	else
	  (* done *)
	  raise Sys.Break

      done
    with Sys.Break ->
      ()
  end;

  let approx = ref [] in
  for r = p-1 downto 0 do
    approx := (x.{r}, x.{p+r}) :: !approx
  done; 

  !approx


