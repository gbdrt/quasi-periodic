open Bndrel
open Symb
open Format

let print_clkexp ppf pr_clk = function
  | Clock x -> pr_clk ppf x
  | ClockDiff (x, y) -> fprintf ppf "@[<h>%a - %a@]" pr_clk x pr_clk y

let print_constraint pr_clk ppf = function
  | Upper (Clock i, r, b) -> fprintf ppf "@[<h>%a %a %d@]" pr_clk i pr_rel r b
  | Upper (ClockDiff (i, j), r, 0) ->
      fprintf ppf "@[<h>%a %a %a@]" pr_clk i pr_rel r pr_clk j
  | Upper (ClockDiff (i, j), r, b) ->
      fprintf ppf "@[<h>%a - %a %a %d@]" pr_clk i pr_clk j pr_rel r b

  | Lower (b, r, Clock i) -> fprintf ppf "@[<h>%d %a %a@]" b pr_rel r pr_clk i
  | Lower (0, r, ClockDiff (i, j)) ->
      fprintf ppf "@[<h>%a %a %a@]" pr_clk j pr_rel r pr_clk i
  | Lower (b, r, ClockDiff (i, j)) ->
      fprintf ppf "@[<h>%d %a %a - %a@]" b pr_rel r pr_clk i pr_clk j

  | Both (b1, LEQ, Clock i, LEQ, b2) when b1 = b2 ->
      fprintf ppf "@[<h>%a = %d@]" pr_clk i b1
  | Both (lb, lr, Clock i, ur, ub) ->
      fprintf ppf "@[<h>%d %a %a %a %d@]" lb pr_rel lr pr_clk i pr_rel ur ub
  | Both (b1, LEQ, ClockDiff (i, j), LEQ, b2) when b1 = b2 ->
      if b1 = 0 then fprintf ppf "@[<h>%a = %a@]" pr_clk i pr_clk j
      else fprintf ppf "@[<h>%a - %a = %d@]" pr_clk i pr_clk j b1
  | Both (lb, lr, ClockDiff (i, j), ur, ub) ->
      fprintf ppf "@[<h>%d %a %a - %a %a %d@]" lb pr_rel lr pr_clk i pr_clk j pr_rel ur ub

let print_constraints pr_clk ppf cs =
  let rec pr ppf = function
    | [] -> ()
    | [x] -> print_constraint pr_clk ppf x
    | x::xs -> fprintf ppf "%a@ && " (print_constraint pr_clk) x; pr ppf xs
  in
  fprintf ppf "@[<hv 0>";
  pr ppf cs;
  fprintf ppf "@]@."

 let of_string s =
  List.rev
    (Parser.relation_list Lexer.token (Lexing.from_string s))
