//
// Various functions for practicing F#; homework #03.
//
// Jash Patel
// U. of Illinois, Chicago
// CS 341, Fall 2023
//

namespace homework

module hw03 =
   //
   // NOTE: all types, functions in the module must be indented.
   //

   //
   // subset S L
   // Checks if set S is a subset of list L.
   //
   let subset S L =
       List.forall (fun e -> List.exists ((=) e) L) S


   //
   // delete_tr e L
   // Deletes all occurrences of element e from list L using tail recursion.
   //
   let delete_tr e L =
       let rec aux e L acc =
           match L with
           | [] -> List.rev acc
           | hd :: tl -> if hd = e then aux e tl acc else aux e tl (hd :: acc)
       aux e L []


   //
   // delete_ho e L
   // Deletes all occurrences of element e from list L using higher-order functions.
   //
   let delete_ho e L =
       List.filter (fun x -> x <> e) L


   //
   // examAverages LT
   // Computes the average score for each student in the list of (netid, scores) tuples.
   //
   let examAverages LT =
       List.map (fun (netid, scores) -> (netid, List.average (List.map float scores))) LT


   //
   // pairwise L1 L2
   // Combines two lists pairwise into a list of tuples, assuming both lists are of the same length.
   //
   let rec pairwise L1 L2 =
       match L1, L2 with
       | [], [] -> []
       | hd1 :: tl1, hd2 :: tl2 -> (hd1, hd2) :: pairwise tl1 tl2
       | _, _ -> failwith "Lists are of different lengths"
