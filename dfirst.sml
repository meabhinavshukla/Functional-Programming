Control.Print.printDepth := 10;
datatype 'a tree = leaf | node of 'a * 'a tree * 'a tree;

fun insert(i, leaf) = node(i, leaf, leaf)
					| insert(i, node(v, left, right)) = if (i=v) then node(v, left, right)
														                  else if (i<v) then node(v, insert(i, left), right)
														                  else node(v, left, insert(i, right));

fun testcase() =
 let val t1 = node(100,leaf,leaf);
 val t2 = insert(50, t1);
 val t3 = insert(150, t2);
 val t4 = insert(200, t3);
 val t5 = insert(125, t4);
 val t6 = insert(175, t5);
 val t7 = insert(250, t6);
 val t8 = insert(25, t7);
 val root = insert(75, t8)
 in root
 end;


 
 (* Tail- recursive definition of inorder traversal on a BST. The idea is to flatten the tree in a list so that the list behaves as a stack.*)
fun dfirst2(tree) = 
						let fun df([], acc) = acc
						
							(* When both children are leaf nodes. *)
							|	df(node(v, leaf, leaf)::tail, acc) = df(tail , acc@[v])
							(* When the left child is a leaf and right child is a subtree*)
							|	df(node(v, leaf, t2)::tail , acc) = df([t2]@tail, acc@[v])
							(* When the right child is a leaf and left child is a subtree*)
							|	df(node(v, t1, leaf)::tail, acc) = df([t1]@[node(v, leaf, leaf)]@tail, acc )
							(* When both children are subtrees. *)
							|	df(node(v, t1, t2)::tail , acc) = df([t1]@[node(v, leaf, t2)]@tail , acc)
							
							
						in		
							df([tree] , [])
						end

fun test_dfirst() = dfirst2(testcase());
