open OUnit2 
open Yojson.Basic.Util
open Party
open Gauntlet

let ch = get_characters
let fighter = List.nth ch 0 
let thief = List.nth ch 1
let black_belt = List.nth ch 2
let red_mage = List.nth ch 3
let white_mage = List.nth ch 4
let black_mage = List.nth ch 5

let party_tests = [
  "Testing get_characters">:: 
  (fun _ -> assert_equal [fighter;thief;black_belt;red_mage;white_mage;
                          black_mage] get_characters);
  "Testing add with 3 characters">:: 
  (fun _ -> assert_equal [fighter; red_mage; white_mage] (add ["fighter";"red mage";"white mage"] []))                        
]

let glt1 = "gauntlet1.json" |> Yojson.Basic.from_file |> from_json;;
let chaos_stats = boss_stats glt1 "Chaos"
let chaos_spells = ["ICE3";"LIT3";"FIR3";"ICE2"]
let chaos_skills = ["INFERNO";"SWIRL"]

let gauntlet_tests = [
  "start boss" >:: (fun _ -> assert_equal "Chaos" (start_boss glt1));
  "start dialogue" >::
  (fun _ -> assert_equal "It's Chaos!" (start_dialogue glt1));
  "final boss" >:: (fun _ -> assert_equal "Clarkson" (final glt1));
  "Chaos hp" >:: (fun _ -> assert_equal 1000 (chaos_stats.hp));
  "Chaos agility" >:: (fun _ -> assert_equal 50 (chaos_stats.agl));
  "Chaos defense" >:: (fun _ -> assert_equal 50 (chaos_stats.def));
  "Chaos strength" >:: (fun _ -> assert_equal 50 (chaos_stats.str));
  "Chaos hit rate" >:: (fun _ -> assert_equal 100 (chaos_stats.hit));
  "Chaos weaknesses" >:: (fun _ -> assert_equal [] (chaos_stats.weak));
  "Chaos resistances" >:: (fun _ -> assert_equal ["ICE"] (chaos_stats.resist));
  "Chaos spell chance" >::
  (fun _ -> assert_equal 64 (boss_spell_chance glt1 "Chaos"));
  "Chaos spells" >::
  (fun _ -> assert_equal chaos_spells (boss_spells glt1 "Chaos"));
  "Chaos skill chance" >::
  (fun _ -> assert_equal 64 (boss_skill_chance glt1 "Chaos"));
  "Chaos skills" >::
  (fun _ -> assert_equal chaos_skills (boss_skills glt1 "Chaos"));
  "Chaos next is Mutability" >::
  (fun _ -> assert_equal "Mutability" (next glt1 "Chaos"));
  "Chaos dialogue" >::
  (fun _ -> assert_equal "You've defeated Chaos!" (dialogue glt1 "Chaos"))
]

let suite = "search test suite" >::: List.flatten [
    party_tests;
    gauntlet_tests;
  ]

let _ = run_test_tt_main suite