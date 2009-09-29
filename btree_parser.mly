%token CR EOF COMMA OPENP CLOSEP 
%token <int> INTEGER

%start tree
%type<Btree.btree> tree
%%

tree:
  | INTEGER
  { Btree.Leaf $1 }
  | OPENP tree COMMA tree CLOSEP
  { Btree.Node($2, $4) }

