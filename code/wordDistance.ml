module type WordDistance =
	sig
		val wordDistance : string -> string -> int
		val test: unit -> unit
	end;;