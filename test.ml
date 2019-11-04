open OUnit2 
open Yojson.Basic.Util
open Party
open Gauntlet
open State

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
  (fun _ -> assert_equal [fighter; red_mage; white_mage]
      (add ["fighter";"red mage";"white mage"] []));  
  "Tetsing get_weapon of thief ">:: 
  (fun _ -> assert_equal "Dragon Sword" (get_weapon thief)); 
  "Testing get_spells of red mage ">:: 
  (fun _ -> assert_equal ["FIR2";"ICE";"LIT2"] (get_spells red_mage);)                         
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

let init = init_state glt1 [thief;red_mage;white_mage]
let r_set = set_health "red mage" 25 init
let b_set = set_health "Chaos" 1000 init
let t = get_turnorder init
let rem_red = remove_from_t "red mage" init
let r = get_turnorder rem_red
let rem_red_white = remove_from_t "white mage" rem_red
let rw = get_turnorder rem_red_white
let before_next = get_next_fighter rem_red_white
let twist = change_turns rem_red_white

let state_tests = [
  "Testing init set current boss correctly">:: 
  (fun _ -> assert_equal "Chaos" (get_current_boss init));
  "Testing init set next boss correctly">:: 
  (fun _ -> assert_equal "Mutability" (get_next_boss init));
  "Testing party is set correctly">:: 
  (fun _ -> assert_equal ["white mage";"red mage";"thief"] (get_party init));
  "Testing health was set correctly ">:: 
  (fun _ -> assert_equal 35 (get_health "red mage" init));
  "Testing set health works correctly">:: 
  (fun _ -> assert_equal 25 (get_health "red mage" r_set));
  "Testing get health for boss">:: 
  (fun _ -> assert_equal 2000 (get_health "Chaos" init)); 
  "Testing setting red mage health doesn't affect another character">::
  (fun _ -> assert_equal 2000 (get_health "Chaos" init));
  "Testing set on boss">:: 
  (fun _ -> assert_equal 1000 (get_health "Chaos" b_set));
  "Testing size of init's turnorder">:: 
  (fun _ -> assert_equal 4 (List.length t));
  "Testing remove from turnorder">::
  (fun _ -> assert_equal 3 (List.length r));
  "Testing remove from turnorder">:: 
  (fun _ -> assert_equal false (List.mem "red mage" r));
  "Testing remove from turnorder">::
  (fun _ -> assert_equal 2 (List.length rw));
  "Testing remove from turnorder">:: 
  (fun _ -> assert_equal false (List.mem "white mage" rw));

]

let suite = "search test suite" >::: List.flatten [
    party_tests;
    gauntlet_tests;
    state_tests;
  ]

let _ = run_test_tt_main suite