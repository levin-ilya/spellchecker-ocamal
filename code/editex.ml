open WordDistance


module EditexMod:WordDistance =
	struct
		let buildAlphaGroup () = 
		let alphaHash = Hashtbl.create 24 in
		Hashtbl.add alphaHash 'a' [0];
		Hashtbl.add alphaHash 'b' [1];
		Hashtbl.add alphaHash 'c' [2;9];
		Hashtbl.add alphaHash 'd' [3];
		Hashtbl.add alphaHash 'e' [0];
		Hashtbl.add alphaHash 'f' [7];
		Hashtbl.add alphaHash 'g' [6];
		Hashtbl.add alphaHash 'h' [-1];
		Hashtbl.add alphaHash 'i' [0]; 
		Hashtbl.add alphaHash 'j' [6]; 
		Hashtbl.add alphaHash 'k' [2];
		Hashtbl.add alphaHash 'l' [4];
		Hashtbl.add alphaHash 'm' [5];
		Hashtbl.add alphaHash 'n' [5]; 
		Hashtbl.add alphaHash 'o' [0];
		Hashtbl.add alphaHash 'p' [1;7];
		Hashtbl.add alphaHash 'q' [2]; 
		Hashtbl.add alphaHash 'r' [4]; 
		Hashtbl.add alphaHash 's' [8;9];
		Hashtbl.add alphaHash 't' [3]; 
		Hashtbl.add alphaHash 'u' [0]; 
		Hashtbl.add alphaHash 'v' [7]; 
		Hashtbl.add alphaHash 'w' [-1];
		Hashtbl.add alphaHash 'x' [8];
		Hashtbl.add alphaHash 'y' [0];
		Hashtbl.add alphaHash 'z' [8;9];
		alphaHash
		
  (* checks the hash table to see if their if c1 and c2 are in the same group	*)
	let isSameSoundGroup c1 c2 =
		let rec compareArrays c1 c2 results =
			match c1 with
				| [] -> results
				| hd::tail -> 
					if hd<0 then
						false
					else if List.mem hd c2 then
						true
					else 
						compareArrays tail c2 false in
				compareArrays c1 c2 false
					
	(* For Editex, the function r(a; b) *)
	(* returns 0 if a and b are identical, 1 if a and b are both occur*)
	(* in the same group, and 2 otherwise.*)
	let soundDistance char1 char2=
		let soundGroups = buildAlphaGroup () in
		match (char1,char2) with
			| (None,Some x) -> 1 (* Distance between the first string and an empty string *)
			| (None,None) -> failwith "not possible"
			| (Some x,None) -> failwith "not possible"
			| (Some char1,Some char2) ->
		if char1=char2 then
			0
		else if isSameSoundGroup (Hashtbl.find soundGroups char1) (Hashtbl.find soundGroups char2) then
			1
		else
			2
			
		let soundDistanceSilent char1 char2 = 
		match (char1,char2) with
			| (None,Some x) -> 1 (* Distance between the first string and an empty string *)
			| (None,None) -> failwith "not possible"
			| (Some _,None) -> failwith "not possible"
			| (Some char1,Some char2) -> 
		if char1 = 'h' || char1='w' & char1 != char2 then
			1
		else
			soundDistance (Some char1) (Some char2)
			
		
		let getChar string pos =
			if pos <=0 then
				None
			else
				Some string.[pos-1]
				
let rec inner_editex matrix stringX stringY posX posY =
	match (posX,posY) with
		| (0,0) ->  matrix.(0).(0) <- 0
		| (1,0) -> matrix.(posX).(posY) <- 1 (* Distance between the first string and an empty string *)
		| (0,1) -> matrix.(posX).(posY) <- 1 (* Distance between the first string and an empty string *)
		| (i,0) -> matrix.(posX).(posY) <- (matrix.(posX-1).(posY)) + (soundDistanceSilent (getChar stringX (i-1)) (getChar stringX (i)))
		| (0,j) -> matrix.(posX).(posY) <- (matrix.(posX).(posY-1)) + (soundDistanceSilent (getChar stringY (j-1)) (getChar stringY (j)))
		| (i,j) -> matrix.(i).(j) <- 
				 let leftnum = (matrix.(i-1).(j) + (soundDistanceSilent (getChar stringX (i-1)) (getChar stringX i))) in
         let topnum = (matrix.(i).(j-1) + (soundDistanceSilent (getChar stringY (j-1)) (getChar stringY j))) in
				 let diagonal = (matrix.(i-1).(j-1) + (soundDistance (getChar stringX i) (getChar stringY j))) in
				 min leftnum (min topnum diagonal)
				
	let editex stringX stringY =
		let rec matrixLoop matrix posX posY = 
			if posY > String.length stringY then
				matrixLoop matrix (posX+1) 0
			else if posX > String.length stringX then
				matrix
			else
				let _ = inner_editex matrix stringX stringY posX posY in
				matrixLoop matrix posX (posY+1) in 
		let matrix = Array.make_matrix (String.length stringX+1) (String.length stringY+1) 0 in
		let _ = matrixLoop matrix 0 0 in
		matrix.(String.length stringX).(String.length stringY)
		
		let wordDistance string1 string2 =
			editex string1 string2
			
		let soundDistanceTest () = 
			let _ = assert(soundDistance (Some 'a') (Some 'a') = 0) in
			let _ = assert(soundDistance (Some 'a') (Some 'e') = 1) in
			let _ = assert(soundDistance (Some 'b') (Some 'p')=1) in
			let _ = assert(soundDistance (Some 'c') (Some 'k')=1) in
			let _ = assert(soundDistance (Some 'c') (Some 's')=1) in
			let _ = assert(soundDistance (Some 'a') (Some 'z')=2) in
			let _ = assert(soundDistance (Some 'p') (Some 's')=2) in
			assert(soundDistance (Some 'c') (Some 'z')=1)
		
		let getCharTest () = 
			let _ = assert(getChar "test" (-1) = None) in 	
			let _ = assert(getChar "test" (0) = None) in
			assert(getChar "test" (1) = Some 't')
			
		let editexTest () =
			let _ = assert(editex "car" "car"=0) in
			let _ = assert(editex "car" "zar"=1) in
			assert(editex "car" "bar"=2)
		
		let test () = 
			soundDistanceTest ();
			getCharTest ();
			editexTest ();
end

module ED = EditexMod
let _ = ED.test ()


