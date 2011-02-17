open Sys
open Printf
open Str

;;

let chan = open_in Sys.argv.(1) in
let whitespace_re = regexp "[ #\t]+" in (* and hash *)
let ktr_re= regexp "^KTR_" in

let is_x conv s =
  try
    let _ = conv s in
      true
  with _ ->
    false
in

let is_int_or_float s =
  is_x int_of_string s || is_x float_of_string s
in

let rec process_lines () =
  try
    let line = input_line chan in
      if (String.length line) > 0 && line.[0] = '#' then begin
	let str_list = split whitespace_re line in
	  if (List.length str_list > 2) && (List.nth str_list 0) = "define" then begin
	    let c_name = List.nth str_list 1 in
	    let s_value = List.nth str_list 2 in
	    let ocaml_name = String.lowercase (List.nth (split ktr_re c_name) 0) in
	      if is_int_or_float s_value then
		printf "let k_%s = %s\n" ocaml_name s_value;
	  end;
      end;
      process_lines ()
  with End_of_file ->
    ()
in
  process_lines ()

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
