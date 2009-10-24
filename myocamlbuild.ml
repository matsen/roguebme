open Ocamlbuild_plugin;; 
open Command;; 
dispatch begin function 
  | After_rules -> 
      (* custom: incorporate libraries into bytecode *)
      flag ["link"; "ocaml"; "byte"] (A"-custom");
      ocaml_lib ~extern:true ~dir:"+glpk" "glpk"; 
      ocaml_lib ~extern:true ~dir:"+gsl"  "gsl"; 
  | _ -> () 
end;; 
