(* given a tree T, we want to see if an S maximizing d_Q(T,S) across all S can
 * be taken to be a comb tree.
 * Thus for every S we ask if it has set or tied the record. If it has, and it
 * is a comb tree, then we record it.
 *
 * the output of this program is a collection of d[T,S]'s and the corresponding
 * distances, where S here is the record as described above.
 * *)

exception Not_comb

let min_n_tips = 4
and max_n_tips = 8

let () = 
  if not (!Sys.interactive) then
  begin
  for n_tips = min_n_tips to max_n_tips do
    Format.fprintf Format.std_formatter "@\n%d tips@\n" n_tips;
    let all = Array.of_list (Btree.make_all_unrooted n_tips) in
    let n = Array.length all
    and qsets = Array.map Quartet_btree.qset_of_t all 
    in
    for i=0 to n-1 do
      let max_qdist = ref 0
      and maximally_distant = ref (-1)
      in
      for j=0 to n-1 do
        if i <> j then begin
          let d = Quartet.qset_half_symdiff qsets.(i) qsets.(j) in
          if !max_qdist < d || (* store true record always *)
             (!max_qdist = d && Btree.is_comb all.(j))
             (* store tie if it's a comb *)
          then begin
            max_qdist := d;
            maximally_distant := j;
          end
        end
      done;
      Format.fprintf Format.std_formatter "d[%a,@,%a] = %d@\n"
        Btree.ppr_btree all.(i)
        Btree.ppr_btree all.(!maximally_distant)
        !max_qdist;
      if not (Btree.is_comb all.(!maximally_distant)) then begin
        Format.fprintf Format.std_formatter "@\nfound not comb for %a@\n"
          Btree.ppr_btree all.(i);
        for j=0 to n-1 do
          Format.fprintf Format.std_formatter "%d : %a@\n"
            (Quartet.qset_half_symdiff qsets.(i) qsets.(j))
            Btree.ppr_btree all.(j)
        done;
        raise Not_comb
      end
    done
  done
end

let x = Btree_io.of_newick_str
