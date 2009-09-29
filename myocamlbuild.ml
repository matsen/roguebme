open Ocamlbuild_plugin;; 
open Command;; 
dispatch begin function 
  | After_rules -> 
      ocaml_lib ~extern:true ~dir:"+glpk" "glpk"; 
      ocaml_lib ~extern:true ~dir:"+gsl"  "gsl"; 
  | _ -> () 
end;; 
