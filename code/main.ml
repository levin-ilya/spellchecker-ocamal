open EditDistance;;
open Bktree;;
open Spellcheck;;
open Editex;;

module EditDistance = EditDistanceMod
module EditDistanceSP = SpellChecker (EditDistance) (BKTree (EditDistance))

module Editex = EditexMod
module EditexSP = SpellChecker (Editex) (BKTree (Editex))


(*let bigTree = Printf.printf "Loading dictionary, please wait...\n";flush_all ();SP.loadDictionary "./bigList.txt";;*)
let globalthreshold = 1;;
let bigDictionary = "../data/bigList.txt";;
let smallDictionary = "../data/smallWordList.txt"
let misspelledFile = "../data/misspelledwords.txt";;

let getAndPrintResults spellchecker dictionary misspelledWord threshold =
	let rec printList list =
		match list with
			| [] -> ()
			| hd::tail -> Printf.printf "%s\n" hd; (printList tail) in
	let results = spellchecker dictionary misspelledWord threshold  in
	printList results

let rec main spellchecker dictionary threshold = 
	let _ = Printf.printf "Please type in a word you'd like to spell check (CTCL+C to quit):\n" in
	let input = read_line () in
	let _ = getAndPrintResults spellchecker dictionary input threshold in
	let _ = main spellchecker dictionary threshold in
	()
;;

let loadDictionary loadfunction file  =
	let dictionary = Printf.printf "Loading dictionary, please wait...\n";flush_all (); loadfunction file in
	dictionary;;

let performance peformancefunction dictionary fileName threshold = 
	let _ = Printf.printf "Running performance test...\n"; flush_all (); in
	let performanceResults = peformancefunction dictionary fileName threshold in
	let _ = Printf.printf "Total Spell Checks Passed: %d\n" (fst(performanceResults)) in
	let _ = Printf.printf "Total Spell Checks Failed: %d\n" (snd(performanceResults)-fst(performanceResults)) in
	let _ = Printf.printf "Total Words Tested: %d\n" (snd(performanceResults)) in
	let _ = Printf.printf "Percentage Accuracy: %0.5f\n" ((float_of_int (fst(performanceResults))) /. (float_of_int  (snd(performanceResults)))) in
	();;

let rec setup  () = Printf.printf "Which alogritm would you like to use?\n1) Edit Distance \n2) Editex Distance (Small Dictionary) \n3) Editex Distance (Large Dictionary ~10 min load time)  \n4) Edit Distance Performance (~7 minutes run time) \n5) Editex Performance (~3 hour run time) \n6) Quit\n";
	let r = read_line () in
	if r="1" then
		let sp = EditDistanceSP.spellCheckWord in
		let dictionary = loadDictionary EditDistanceSP.loadDictionary bigDictionary in
		main sp	dictionary globalthreshold
	else if r="2" then
		let sp = EditexSP.spellCheckWord in
		let dictionary = loadDictionary EditexSP.loadDictionary smallDictionary in
		main sp dictionary globalthreshold	
	else if r="3" then
		let sp = EditexSP.spellCheckWord in
		let dictionary = loadDictionary EditexSP.loadDictionary bigDictionary in
		main sp dictionary globalthreshold	
	else if r="4" then
		let dictionary = loadDictionary EditDistanceSP.loadDictionary bigDictionary in
		let perform = EditDistanceSP.performanceTest in 
		performance perform dictionary misspelledFile globalthreshold
	else if r="5" then
		let dictionary = loadDictionary EditexSP.loadDictionary bigDictionary in
		let perform = EditexSP.performanceTest in
		performance perform dictionary misspelledFile globalthreshold
	else if r="6" then
		exit 0
	else
		setup ();;

setup ()