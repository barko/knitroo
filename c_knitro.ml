(* interface to the KNITRO nlp solver from \texttt{http://www.ziena.com} *)

type knitro

(* methods on the knitro type *)
external create : unit -> knitro = "knitro_new"
external set_param_float : knitro -> int -> float -> unit = "knitro_set_param_float"
external set_param_int : knitro -> int -> int -> unit = "knitro_set_param_int"
external get_param_float : knitro -> int -> float = "knitro_get_param_float"
external get_param_int : knitro -> int -> int = "knitro_get_param_int"

open Bigarray
type d_array_1 = (float, float64_elt, c_layout) Array1.t
type i_array_1 = (int32, int32_elt,   c_layout) Array1.t

type init = {
  n : int ;
  obj_goal : int ;
  obj_type : int ;
  x_lo_bnds : d_array_1;
  x_up_bnds : d_array_1;
  m : int ;
  c_type : i_array_1;
  c_lo_bnds : d_array_1;
  c_up_bnds : d_array_1;
  nnz_j : int ;
  jac_index_vars : i_array_1;
  jac_index_cols : i_array_1;
  nnz_h : int ;
  hess_index_rows : i_array_1;
  hess_index_cols : i_array_1;
  x_initial : d_array_1;
  lambda_initial_opt : d_array_1 option
}

external init : knitro -> init -> int = "knitro_init" 

(* a specification of a KNITRO problem *)
type problem = { 
  x : d_array_1;  
  lambda : d_array_1;  
  eval_status : int;
  obj : d_array_1;
  c : d_array_1;
  obj_grad : d_array_1;
  jac : d_array_1;
  hess : d_array_1;
  hess_vector : d_array_1
}

(* this assumes all dimensions are conformant -- it does not check
   that assumption *)
external solve : knitro -> problem -> int = "knitro_solve"


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

