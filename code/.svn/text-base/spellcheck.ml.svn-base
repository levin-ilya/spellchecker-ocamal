open WordDistance;;
open Bktree;;

module SpellChecker (WD:WordDistance) (BK:BKTree) =
	struct 
	let getsubNodes = BK.getsubNodes
	let editDistance = WD.wordDistance 

	
	let rec addToSearch searchQueue bktree low high =
		let newSearch = (BK.filterSubNodes bktree low high)@searchQueue	in
		newSearch																														
	
	let rec printList list =
		match list with
			| [] -> Printf.printf "---------\n" 
			| hd::tail -> Printf.printf "%s\n" ("T: "^(BK.getRootWord hd))

	let rec spellCheck toSearch misspelledWord threshold results= 
(*	let _ = printList toSearch in*)
	match toSearch with
		| [] -> results
		| hd::tail -> 
			let d = editDistance (BK.getRootWord hd) misspelledWord in
			let toSearch = addToSearch tail hd (d-threshold) (d+threshold) in
			if d<=threshold then 
				(* add it to results a nd keep searching *)
				spellCheck toSearch misspelledWord threshold ((BK.getRootWord hd)::results)
			else if (List.length toSearch > 0) then
			(* keep searching *)
			spellCheck toSearch misspelledWord threshold results
			else
				results
				
	let trimString string =
		String.sub string 0 ((String.length string)-1)
		
	let rec loadList wordList bkTree=
	match	wordList with
		| [] -> bkTree
		| hd::tail -> loadList tail (BK.addWord bkTree hd)
				
	let loadFile filename  =
		let lines = ref [] in
		let chan = open_in filename in
		try
		  while true; do
		    lines := (trimString (input_line chan)):: !lines
		  done; []
		with End_of_file ->
		  close_in chan;
			!lines
			
	let loadDictionary file =
		let wordList = loadFile file in
		let bktree = loadList wordList (BK.create ()) in
		bktree
	

	
	let spellCheckWord dictionary string threshold = 
		let results =  spellCheck [dictionary] (String.lowercase string) threshold [] in
		results
		
	let performanceTest dictionary file threshold =
		(*Turns "abandonned->abandoned" into a pair (abandonned,abandoned) *)
		let getPair line = 
			(String.sub line 0 (String.index line '-'),String.sub line ((String.index line '>')+1) ((String.length line)-(String.index line '>')-1)) in
		
		let testSpellCheck misspelledword correctword = 
			let list = spellCheckWord dictionary misspelledword threshold in
			List.exists (fun x-> x=correctword) list in 
			
		let rec loopList list count total=
			match list with
				| [] -> (count,total)
				| hd::tail -> 
					let pair = getPair hd in
					if (testSpellCheck (fst(pair)) (snd(pair))) then
						loopList tail (count+1) (total+1)
					else
						loopList tail (count) (total+1) in
						
			loopList (loadFile file) 0 0 

end;;