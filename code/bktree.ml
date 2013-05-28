open WordDistance
open EditDistance

module type BKTree = 
	sig 
		type edge 
		type bktree 
		val create : unit -> bktree
    val getElements : 'a * bktree -> string * edge list
    val getRootWord : bktree -> string
    val getsubNodes : bktree -> edge list
    val getList : 'a * bktree -> edge list
    val getWord : 'a * bktree -> string
    val getDistance : 'a * 'b -> 'a
		val filterSubNodes : bktree -> int -> int -> bktree list
		val addWord: bktree -> string -> bktree
		val test: unit -> unit
 end;;

module BKTree(WD:WordDistance):BKTree=
	struct
		type edge = int * bktree
		and  bktree = Node of string * edge list 
		
		let editDistance = WD.wordDistance
		
		let create () = Node ("",[])
	
		let getElements edge = 
			let Node (word,list) = snd(edge) in
			(word,list)
			
		let getRootWord bktree =
			let Node (word,list) = bktree in
			word
		
		let getsubNodes bktree = 
			let Node (word,list) = bktree in
			list
			
		let getList edge = snd(getElements edge)
		let getWord edge = fst(getElements edge)
		let getDistance edge = fst(edge)
		let getNode edge = snd(edge)
		
(* TODO: Use List.map	*)
		let filterSubNodes bktree low high =
			let rec innerfilter list low high = 
				match list with
				| [] -> []
				| edge::tail -> if getDistance(edge)>=low && getDistance(edge)<=high then getNode(edge)::innerfilter tail low high else innerfilter tail low high in
			innerfilter (getsubNodes bktree) low high
			
					

	
		let rec addEdge list word distance =
				match list with
		| [] -> (distance,Node(word,[]))::[]
		| hd::tail -> 
			if fst(hd)=distance then
				(distance,Node ((getWord hd),addEdge (getList hd) word (editDistance (getWord hd) word)))::tail
			else
				hd::(addEdge tail word distance)
				
	let rec addWord bktree word =
	match bktree with
		| Node ("",[]) -> Node (word,[])
		| Node (str,list) -> Node (str,(addEdge list word (editDistance word str)))

	 let getListTest () = 
		assert(getList ((0,Node ("hello world",[]))) = []);
		()
	
	let getWordTest () =
		assert(getWord ((0,Node ("hello world",[]))) = "hello world"); 
	()
	
	let getDistanceTest () =
		assert(getDistance ((0,Node ("hello world",[]))) = 0);
		()
		
	let createTest () = 
		let bk = create () in
		let bk = addWord bk "air" in
		let bk = addWord bk "aid" in
		let bk = addWord bk "army" in
		let bk = addWord bk "adopt" in
		let bk = addWord bk "allusion"in
		let bk = addWord bk "ally" in
		let bk = addWord bk "alter" in
		let bk = addWord bk "amend" in
		let bk = addWord bk "assay"in
		assert(bk=Node ("air",
   [(1, Node ("aid", []));
    (3, Node ("army", [(2, Node ("ally", [])); (4, Node ("alter", []))]));
    (4, Node ("adopt", [(4, Node ("amend", [(4, Node ("assay", []))]))]));
    (6, Node ("allusion", []))]))
		
		
	
	let test () =
		getListTest ();
		getWordTest ();
		createTest ();
		()
	
	
		
end;;


module EditDTest = EditDistanceMod
module BKTreeTest = BKTree(EditDTest)

let _ = BKTreeTest.test ();;


