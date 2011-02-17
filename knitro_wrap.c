/*
 * Changes:
 *      May   2010:  update to KNITRO 6.0
 *   13 March 2004:  added context_of_block
 *      Dec   2004:  changed to KNITRO 4.0 API -- much better 
 */

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/custom.h>
#include <caml/callback.h>
#include <caml/bigarray.h>
#include <caml/memory.h>
#include <knitro.h>
#include <math.h>
#include <memory.h>
#include <assert.h>

#include <stdio.h>

KTR_context* context_of_block( value block )
{
  KTR_context* context;
  void* block_data = Data_custom_val( block );
  assert( block_data != NULL );
  memcpy( &context, block_data, sizeof(KTR_context*) );

  return context;
}

static void knitro_finalize( value knitro )
{
  KTR_context* context = context_of_block( knitro );
  KTR_free( &context );
}

static struct custom_operations KnitroOperations = {
  "knitro",
  knitro_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

value knitro_new()
{
  value block = alloc_custom( &KnitroOperations, sizeof(KTR_context*), 0, 1 );
  assert( block != NULL );
  KTR_context* context = KTR_new();
  assert(context != NULL);
  void* block_data = Data_custom_val( block );
  assert(block_data !=  NULL);
  memcpy( block_data, &context, sizeof(KTR_context*) );
  return block;
}

value knitro_set_param_float( value knitro, value vKey, value vD )
{
  KTR_context* context = context_of_block( knitro );
  double D = Double_val(vD);
  int Key = Int_val(vKey);
  int err = KTR_set_double_param( context, Key, D );

  if (err) {
    char msg[512];
    sprintf( msg, "failed to set floating-point parameter whose id is %d with value %f", Key, D );
    failwith( msg );
  }

  return Val_unit;
}

value knitro_set_param_int( value knitro, value vKey, value vI )
{
  KTR_context* context = context_of_block( knitro );
  int I = Int_val(vI);
  int Key = Int_val(vKey);
  int err = KTR_set_int_param( context, Key, I );

  if (err) {
    char msg[512];
    sprintf( msg, "failed to set interger parameter whose id is %d with value %d", Key, I );
    failwith( msg );
  }

  return Val_unit;
}

value knitro_get_param_float( value knitro, value vKey )
{
  KTR_context* context = context_of_block( knitro );
  int Key = Int_val(vKey);
  double D;
  int err = KTR_get_double_param( context, Key, &D );
  
  if (err) {
    char msg[512];
    sprintf( msg, "failed to get floating point parameter whose id is %d", Key );
    failwith( msg );
  }

  return copy_double(D);
}

value knitro_get_param_int( value knitro, value vKey )
{
  KTR_context* context = context_of_block( knitro );
  int Key = Int_val(vKey);
  int I;
  int err = KTR_get_int_param( context, Key, &I );
  
  if (err) {
    char msg[512];
    sprintf( msg, "failed to get floating point parameter whose id is %d", Key );
    failwith( msg );
  }

  return Val_int(I);
}

int count_abnormal( double* array, int length )
{
  int count = 0;
  int i = 0;
  for ( i = 0 ; i < length ; i++ ) {
    double t = array[i];
    if ( isinf(t) || isnan(t) ) {
      count++;
    }
  }
  return count;
}
	 

value knitro_solve( value knitro, value problem )
{
  KTR_context* context = context_of_block( knitro );

  value v_x = Field( problem, 0 );  
  value v_lambda = Field( problem, 1 );  
  value v_eval_status = Field( problem, 2 );
  value v_obj = Field( problem, 3 );
  value v_c = Field( problem, 4 );
  value v_obj_grad = Field( problem, 5 );
  value v_jac = Field( problem, 6 );
  value v_hess = Field( problem, 7 );
  value v_hess_vector = Field( problem, 8 );
  // value v_user_param = Field( problem, 9 );
  
  // assert that each array has the correct type
  assert(Bigarray_val(v_x)->flags & BIGARRAY_FLOAT64 );
  assert(Bigarray_val(v_lambda)->flags & BIGARRAY_FLOAT64 );
  assert(Bigarray_val(v_obj)->flags & BIGARRAY_FLOAT64 );
  assert(Bigarray_val(v_c)->flags & BIGARRAY_FLOAT64 );
  assert(Bigarray_val(v_obj_grad)->flags & BIGARRAY_FLOAT64 );
  assert(Bigarray_val(v_jac)->flags & BIGARRAY_FLOAT64 );
  assert(Bigarray_val(v_hess)->flags & BIGARRAY_FLOAT64 );
  assert(Bigarray_val(v_hess_vector)->flags & BIGARRAY_FLOAT64 );

  // assert that each array has only one dimension 
  assert(Bigarray_val(v_x)->num_dims == 1 );
  assert(Bigarray_val(v_lambda)->num_dims == 1 );
  assert(Bigarray_val(v_obj)->num_dims == 1 );
  assert(Bigarray_val(v_c)->num_dims == 1 );
  assert(Bigarray_val(v_obj_grad)->num_dims == 1 );
  assert(Bigarray_val(v_jac)->num_dims == 1 );
  assert(Bigarray_val(v_hess_vector)->num_dims == 1 );

  int n = Bigarray_val(v_x)->dim[0];
  int m = Bigarray_val(v_c)->dim[0];
  int nnz_j = Bigarray_val(v_jac)->dim[0];
  int nnz_h = Bigarray_val(v_hess)->dim[0];

  // assert that all arrays are dimensioned consistently
  assert(Bigarray_val(v_x)->dim[0] == n);
  assert(Bigarray_val(v_lambda)->dim[0] == (m+n));
  assert(Bigarray_val(v_obj)->dim[0] == 1);
  assert(Bigarray_val(v_c)->dim[0] == m);
  assert(Bigarray_val(v_obj_grad)->dim[0] == n);
  assert(Bigarray_val(v_jac)->dim[0] == nnz_j);
  assert(Bigarray_val(v_hess)->dim[0] == nnz_h);
  assert(Bigarray_val(v_hess_vector)->dim[0] == n);

  // access data
  int eval_status = Int_val(v_eval_status);

  double* x = (double*)Data_bigarray_val(v_x);
  double* lambda = (double*)Data_bigarray_val(v_lambda);
  double* obj = (double*)Data_bigarray_val(v_obj);
  double* c = (double*)Data_bigarray_val(v_c);
  double* obj_grad = (double*)Data_bigarray_val(v_obj_grad);
  double* jac = (double*)Data_bigarray_val(v_jac);
  double* hess = (double*)Data_bigarray_val(v_hess);
  double* hess_vector = (double*)Data_bigarray_val(v_hess_vector);

  int status = 
    KTR_solve
    ( 
     context, 
     x,
     lambda,
     eval_status,
     obj,
     c,
     obj_grad,
     jac,
     hess,
     hess_vector,
     NULL );

  return Val_int(status);
}

value knitro_init( value knitro, value problem )
{
  
  KTR_context* context = context_of_block( knitro );

  value v_n = Field( problem, 0 );
  value v_obj_goal = Field( problem, 1 );
  value v_obj_type = Field( problem, 2 );
  value v_x_lo_bnds = Field( problem, 3 );
  value v_x_up_bnds = Field( problem, 4 );
  value v_m = Field( problem, 5 );
  value v_c_type = Field( problem, 6 );
  value v_c_lo_bnds = Field( problem, 7 );
  value v_c_up_bnds = Field( problem, 8 );
  value v_nnz_j = Field( problem, 9 );
  value v_jac_index_vars = Field( problem, 10 );
  value v_jac_index_cons = Field( problem, 11 );
  value v_nnz_h = Field( problem, 12 );
  value v_hess_index_rows = Field( problem, 13 );
  value v_hess_index_cols = Field( problem, 14 );
  value v_x_initial = Field( problem, 15 );
  value v_lambda_initial_opt = Field( problem, 16 );

  // assert that each array has the correct type
  assert(Bigarray_val(v_x_lo_bnds)->flags & BIGARRAY_FLOAT64 );
  assert(Bigarray_val(v_x_up_bnds)->flags & BIGARRAY_FLOAT64 );
  assert(Bigarray_val(v_c_type)->flags & BIGARRAY_INT32   );
  assert(Bigarray_val(v_c_lo_bnds)->flags & BIGARRAY_FLOAT64 );
  assert(Bigarray_val(v_c_up_bnds)->flags & BIGARRAY_FLOAT64 );
  assert(Bigarray_val(v_jac_index_vars)->flags & BIGARRAY_INT32 );
  assert(Bigarray_val(v_jac_index_cons)->flags & BIGARRAY_INT32 );
  assert(Bigarray_val(v_hess_index_rows)->flags & BIGARRAY_INT32 );
  assert(Bigarray_val(v_hess_index_cols)->flags & BIGARRAY_INT32 );
  assert(Bigarray_val(v_x_initial)->flags & BIGARRAY_FLOAT64 );

  // assert that each array has only one dimension 
  assert(Bigarray_val(v_x_lo_bnds)->num_dims == 1 );
  assert(Bigarray_val(v_x_up_bnds)->num_dims == 1 );
  assert(Bigarray_val(v_c_lo_bnds)->num_dims == 1 );
  assert(Bigarray_val(v_c_up_bnds)->num_dims == 1 );
  assert(Bigarray_val(v_jac_index_vars)->num_dims == 1 );
  assert(Bigarray_val(v_jac_index_cons)->num_dims == 1 );
  assert(Bigarray_val(v_hess_index_rows)->num_dims == 1 );
  assert(Bigarray_val(v_hess_index_cols)->num_dims == 1 );
  assert(Bigarray_val(v_x_initial)->num_dims == 1 );

  int n = Int_val(v_n);
  int obj_goal = Int_val(v_obj_goal);
  int m = Int_val(v_m);
  int obj_type = Int_val(v_obj_type);
  int nnz_j = Int_val(v_nnz_j);
  int nnz_h = Int_val(v_nnz_h);

  // assert that all arrays are dimensioned consistently
  assert(Bigarray_val(v_x_lo_bnds)->dim[0] == n );
  assert(Bigarray_val(v_x_up_bnds)->dim[0] == n );
  assert(Bigarray_val(v_c_type)->dim[0] == m );
  assert(Bigarray_val(v_c_lo_bnds)->dim[0] == m );
  assert(Bigarray_val(v_c_up_bnds)->dim[0] == m );
  assert(Bigarray_val(v_jac_index_vars)->dim[0] == nnz_j );
  assert(Bigarray_val(v_jac_index_cons)->dim[0] == nnz_j );
  assert(Bigarray_val(v_hess_index_rows)->dim[0] == nnz_h );
  assert(Bigarray_val(v_hess_index_cols)->dim[0] == nnz_h );
  assert(Bigarray_val(v_x_initial)->dim[0] == n );

  // access data
  double* x_lo_bnds = (double*)Data_bigarray_val(v_x_lo_bnds);
  double* x_up_bnds = (double*)Data_bigarray_val(v_x_up_bnds);
  int* c_type = (int*)Data_bigarray_val(v_c_type);
  double* c_lo_bnds = (double*)Data_bigarray_val(v_c_lo_bnds);
  double* c_up_bnds = (double*)Data_bigarray_val(v_c_up_bnds);
  int* jac_index_vars = (int*)Data_bigarray_val(v_jac_index_vars);
  int* jac_index_cons = (int*)Data_bigarray_val(v_jac_index_cons);
  int* hess_index_rows = (int*)Data_bigarray_val(v_hess_index_rows);
  int* hess_index_cols = (int*)Data_bigarray_val(v_hess_index_cols);
  double* x_initial = (double*)Data_bigarray_val(v_x_initial);

  double* lambda_initial = NULL;
  if ( Is_block( v_lambda_initial_opt ) ) {
    // Some x
    value v_lambda_initial = Field( v_lambda_initial_opt, 1 );
    assert(Bigarray_val(v_lambda_initial)->flags & BIGARRAY_FLOAT64 );
    assert(Bigarray_val(v_lambda_initial)->num_dims == 1 );
    assert(Bigarray_val(v_lambda_initial)->dim[0] == (m+n) );
    lambda_initial = (double*)Data_bigarray_val(v_lambda_initial);
  };

  int status = 
    KTR_init_problem
    (
     context,
     n,
     obj_goal,
     obj_type,
     x_lo_bnds,
     x_up_bnds,
     m,
     c_type,
     c_lo_bnds,
     c_up_bnds,
     nnz_j,
     jac_index_vars,
     jac_index_cons,
     nnz_h,
     hess_index_rows,
     hess_index_cols,
     x_initial,
     lambda_initial
     );
     
  return Val_int(status);
}


/*
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
*/
