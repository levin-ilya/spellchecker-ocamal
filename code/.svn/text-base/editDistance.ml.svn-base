open WordDistance
	
module EditDistanceMod: WordDistance =
	struct

		let rec arrayNats num = 
			let rec innerArrayNats array num =
			if(num=0) then
				let _ = array.(num) <- num in
				array
			else
				let _ = array.(num) <- num in
				innerArrayNats array (num-1) in	
			innerArrayNats (Array.make (num+1) 0) (num)

		let getMin matrixArray row column cost =
			let diagonal = matrixArray.(row-1).(column-1) + cost in
			let left = matrixArray.(row).(column-1) + 1 in
			let top = matrixArray.(row-1).(column) + 1 in
			min top (min diagonal left)
		
		let rec rowLoop matrixArray row column stringX stringY = 
		if (row>String.length stringX) then
			matrixArray
		else if (row=0) then 
			let _ = matrixArray.(0) <-  arrayNats (String.length stringY) in 
			rowLoop matrixArray (row+1) 0 stringX stringY
		else if (column=0) then
		 let _ = matrixArray.(row).(0) <- row in
		 rowLoop matrixArray row (column+1) stringX stringY
		else if (column > (String.length stringY)) then
			rowLoop matrixArray (row+1) 0 stringX stringY
		else  
			let cost = if(stringX.[row-1]=stringY.[column-1]) then 0 else 1 in
			let _ =  matrixArray.(row).(column) <- getMin matrixArray row column cost in
			rowLoop matrixArray row (column+1) stringX stringY
			
	let wordDistance stringX stringY = 
	let matrixArray = Array.make_matrix ((String.length stringX)+1) ((String.length stringY)+1) 9 in
	let _ = rowLoop matrixArray 0 0 stringX stringY in
	matrixArray.((String.length stringX)).((String.length stringY))
	
	let minArrayTest () =
		let minArrayTest = Array.make_matrix 2 2 0 in
		assert(getMin minArrayTest 1 1 1= 1);
	  ()
		
	let diagonalMinTest () =
		let minArrayTest = Array.make_matrix 2 2 0 in
		let _ = minArrayTest.(0).(0) <- 1 in
		let _ = minArrayTest.(0).(1) <- 2 in
		let _ = minArrayTest.(1).(0) <- 3 in
		assert(getMin minArrayTest 1 1 1= 2)
		
	let leftMinTest () =
		let minArrayTest = Array.make_matrix 2 2 0 in
		let _ = minArrayTest.(0).(0) <- 1 in
		let _ = minArrayTest.(0).(1) <- 2 in
		let _ = minArrayTest.(1).(0) <- 0 in
		assert(getMin minArrayTest 1 1 1= 1)
		
	let topMinTest () = 
		let minArrayTest = Array.make_matrix 2 2 0 in
		let _ = minArrayTest.(0).(0) <- 1 in
		let _ = minArrayTest.(0).(1) <- 0 in
		let _ = minArrayTest.(1).(0) <- 3 in
		assert(getMin minArrayTest 1 1 1= 1)
		
	let editDistanceTest () = 
		let _ = assert(wordDistance "dog" "dog"=0) in
		let _ = assert(wordDistance "dog" "iog"=1) in
		let _ = assert(wordDistance "dog" "dig"=1) in
		let _ = assert(wordDistance "dog" "doi"=1) in
		let _ = assert(wordDistance "" ""=0) in
		let _ = assert(wordDistance "" "d"=1) in
		let _ = assert(wordDistance "dog" "abc"=3) in
		assert(wordDistance "dog" "harvard"=7)
		
	let test () =
		minArrayTest ();
		diagonalMinTest ();
		leftMinTest ();
		topMinTest ();
		editDistanceTest ();
end;;
	
module ED = EditDistanceMod
let _ = ED.test ()


	

			
			