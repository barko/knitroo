open Printf
open Lsconvex

let _ =
  let input_file = Sys.argv.(1) in
  let points = ref [] in
  Mikmatch.Text.iter_lines_of_file (
    fun line ->
      match Pcre.split line with
	| [ xs ; ys ] ->
	    let x = float_of_string xs in
	    let y = float_of_string ys in
	    points := (x, y) :: !points

	| [] ->
	    () (* skip empty lines *)

	| _ ->
	    failwith (sprintf "bad line %S" line)
  ) input_file;
  
  let points = List.rev !points in

  let approx = fit points in

  let rec loop = function
    | (x, y) :: p, (y_hat, g) :: a ->
	printf "%f\t%f\t%f\t%f\n" x y y_hat g;
	loop (p, a)
    | [], [] ->
	()

    | _ ->
	failwith "inconsistent lengths"
  in

  loop (points, approx)

