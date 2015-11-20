Control.Print.printDepth := 10;
datatype 'a ntree = leaf of 'a | node of 'a ntree list;

fun map(f, [ ]) = [ ]
|   map(f, x::t) = f(x) :: map(f, t); 
 
fun subst (ntree, v1, v2) = let fun h(leaf(x)) =  if x=v1 then leaf(v2)
				|   h(node(l)) = node(map(h, l))
			    in
				h(ntree)
			    end;
	            
 (*subst(node([leaf("x"), node([leaf("y"), leaf("x"), leaf("z")])]),"x", "w") ;*)


fun reduce(f, b, [ ]) = b
|   reduce(f, b, x::t) = f(x, reduce(f, b, t));

 
fun toString(leaf(x)) = x
|	toString(node(l)) = let fun h(x,y) = toString(x) ^ " " ^ y
			    in reduce(h, "", l)
			    end;
						
(*print(toString(node([leaf("x"),node([leaf("y"),leaf("x"),leaf("z")])])));*)
	
	
