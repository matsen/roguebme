(* pplacer v0.2. Copyright (C) 2009  Frederick A Matsen.
 * This file is part of pplacer. pplacer is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. pplacer is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with pplacer. If not, see <http://www.gnu.org/licenses/>.
 *)

open MapsSets
open Fam_batteries


let of_newick_lexbuf lexbuf = 
  try
    Btree_parser.tree Btree_lexer.token lexbuf
  with 
  | Parsing.Parse_error -> failwith "couldn't parse tree!"

let of_newick_str s = 
  of_newick_lexbuf (Lexing.from_string s)

let list_of_newick_file fname =
  List.map of_newick_str (File_parsing.string_list_of_file fname)

let parse_tree_list_list string_list = 
  List.map
    (fun trees_line ->
      List.map
        of_newick_str
        (Str.split (Str.regexp "[ \t]+") trees_line))
    string_list

(* parse a trees file *)
let parse_trees_file fname =
  match File_parsing.string_list_of_file fname with
  | lower_tree_line::upper_trees_lines ->
      if not
        (Str.string_match
          (Str.regexp "# lower tree: \\(.*\\)") 
          lower_tree_line 
          0) then
        failwith "couldn't find lower tree line";
      let lower_tree = 
        of_newick_str (Str.matched_group 1 lower_tree_line) in
      (lower_tree,
      parse_tree_list_list upper_trees_lines)
  | [] -> invalid_arg (fname^": no trees!")


