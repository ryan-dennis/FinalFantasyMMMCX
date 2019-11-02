open OUnit2 
open Party 

let ch = get_characters
let fighter = List.nth ch 0 
let thief = List.nth ch 1
let black_belt = List.nth ch 2
let red_mage = List.nth ch 3
let white_mage = List.nth ch 4
let black_mage = List.nth ch 5

let test = [
  "Testing get_characters">:: 
  (fun _ -> assert_equal [fighter;thief;black_belt;red_mage;white_mage;
                          black_mage] get_characters);
  "Testing add with 3 characters">:: 
  (fun _ -> assert_equal [fighter; red_mage; white_mage] (add ["fighter";"red mage";"white mage"] []))                        
]

let suite = "search test suite" >::: test

let _ = run_test_tt_main suite

