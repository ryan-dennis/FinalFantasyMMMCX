(** Test Plan:
    We implemented automatic OUnit testing by writing a test for every exposed
    function in each module. In situations where the function returned a value
    of an unexposed type, we tested those by having other functions that
    returned exposed types take those values in as input, then checked to
    ensure that they returned the expected values. Because we built a game
    where essentially every function is a helper function to be used to some
    degree by the Main module, we developed test cases using black box testing
    to ensure that our modules would work properly with one another, without
    focusing so much on how exactly they did so. This approach was used for
    the Party, Gauntlet, State, Status, and Command modules.

    This still left many functions untested, however; in particular, we were
    unable to test any functions from the Battle, Main, and Potions modules,
    as each had some random component that would've made it impossible to
    automatically test. To that end, since we have developed a game, we instead
    used arguably a form of black box testing by playing the game repeatedly.
    For example, if we wanted to test Potions, then we would try all possible
    Potions-related options in the game (ex. drinking each potion) to ensure
    the expected output. While this method was most useful for testing the
    aforementioned modules, since we did develop a game where each module was
    a crucial aspect of gameplay, we used it to test every module to some
    extent as well. Thus, this was our primary form of testing because of its
    flexibility.

    This testing approach demonstrates the correctness of the system because we
    know the system is correct when the game plays as expected. By playing the
    game to test, we therefore know for a fact whether the system is correct,
    because being able to play the game is the only indicator of correctness of
    our system.
*)

open OUnit2 
open Yojson.Basic.Util
open Party
open Gauntlet
open State
open Status
open Command

let ch = get_characters
let fighter = List.nth ch 0 
let thief = List.nth ch 1
let black_belt = List.nth ch 2
let red_mage = List.nth ch 3
let white_mage = List.nth ch 4
let black_mage = List.nth ch 5
let f = fun () -> find_character "mel" get_characters

let party_tests = [
  "Testing get_characters">:: 
  (fun _ -> assert_equal [fighter;thief;black_belt;red_mage;white_mage;
                          black_mage] get_characters);
  "Testing add with 3 characters">:: 
  (fun _ -> assert_equal [fighter; black_belt; white_mage]
      (add ["fighter";"black belt";"white mage"] []));  
  "Tetsing get_weapon of thief ">:: 
  (fun _ -> assert_equal "Dragon Sword" (get_weapon thief)); 
  "Testing get_spells of black belt ">:: 
  (fun _ -> assert_equal ["CURE";"RUSE";"MUTE";"CUR2";"FIR3";] (get_spells black_belt));
  "Testing find character works correctlt. Using black mage">::
  (fun _ -> assert_equal black_mage (find_character "black mage" get_characters));
  "Testing has spells works correctly using black belt. He does have spell">::
  (fun _ -> assert_equal true (has_spell black_belt "CURE"));
  "Testing has spells works correctly using black belt. He does not have spell">::
  (fun _ -> assert_equal false (has_spell black_belt "FIR2"));
  "Testing char_og_health returns the correct healt">::
  (fun _ -> assert_equal 676 (char_og_health white_mage));
  "Testing get_spells on black belt ">::
  (fun _ -> assert_equal ["CURE";"RUSE";"MUTE";"CUR2";"FIR3";] (get_spells black_belt));
  "Testing get name works for thief ">::
  (fun _ -> assert_equal "thief" (get_name thief));
  "Testing get stats works for fighter ">:: 
  (fun _ -> assert_equal 
      {str = 69; agl = 43; int = 23; vit = 993; mp = 70; hit_percent = 187; m_def = 60; fight_def = 60}
      (get_stats fighter));
  "Testing exception thrown">::
  (fun _ -> assert_raises (UnknownCharacter "mel") f );     
]

(** GAUNTLET TESTS *)
let glt1 = "testgauntlet.json" |> Yojson.Basic.from_file |> from_json;;
let chaos_sprite = ["testsprite"; "testsprite"]
let chaos_stats = boss_stats glt1 "Chaos"
let chaos_spells = ["ICE3";"LIT3";"CUR4";"FIR3";"ICE2";"NUKE"]
let chaos_skills = ["INFERNO";"SWIRL";"TORNADO"]

let gauntlet_tests = [
  "start boss" >:: (fun _ -> assert_equal "Chaos" (start_boss glt1));
  "start dialogue" >::
  (fun _ -> assert_equal "It's Chaos!" (start_dialogue glt1));
  "final boss" >:: (fun _ -> assert_equal "Professor Clarkson" (final glt1));
  "Chaos hp" >:: (fun _ -> assert_equal 1500 (chaos_stats.hp));
  "Chaos agility" >:: (fun _ -> assert_equal 60 (chaos_stats.agl));
  "Chaos defense" >:: (fun _ -> assert_equal 60 (chaos_stats.def));
  "Chaos magic defense" >:: (fun _ -> assert_equal 120 (chaos_stats.mdef));
  "Chaos strength" >:: (fun _ -> assert_equal 60 (chaos_stats.str));
  "Chaos hit rate" >:: (fun _ -> assert_equal 120 (chaos_stats.hit));
  "Chaos weaknesses" >:: (fun _ -> assert_equal ["Ice"] (chaos_stats.weak));
  "Chaos resistances" >:: (fun _ -> assert_equal ["Fire"] (chaos_stats.resist));
  "Chaos num of hits" >::
  (fun _ -> assert_equal 4 (boss_num_of_hits glt1 "Chaos"));
  "Chaos sprite" >::
  (fun _ -> assert_equal chaos_sprite (boss_sprite glt1 "Chaos"));
  "Chaos spell chance" >::
  (fun _ -> assert_equal 64 (boss_spell_chance glt1 "Chaos"));
  "Chaos ICE3 is icicle" >::
  (fun _ -> assert_equal "icicle" (boss_spell_name glt1 "Chaos" "ICE3"));
  "Chaos spells" >::
  (fun _ -> assert_equal chaos_spells (boss_spells glt1 "Chaos"));
  "Chaos skill chance" >::
  (fun _ -> assert_equal 64 (boss_skill_chance glt1 "Chaos"));
  "Chaos skills" >::
  (fun _ -> assert_equal chaos_skills (boss_skills glt1 "Chaos"));
  "Chaos INFERNO is fire" >::
  (fun _ -> assert_equal "fire" (boss_skill_name glt1 "Chaos" "INFERNO"));
  "Chaos next is Mutability" >::
  (fun _ -> assert_equal "Mutability" (next glt1 "Chaos"));
  "Chaos dialogue" >::
  (fun _ -> assert_equal "It's Mutability!" (dialogue glt1 "Chaos"))
]

(** STATE TESTS *)
let init = init_state glt1 [thief;black_belt;white_mage]
let r_set = set_health glt1 "black belt" init 25 
let b_set = set_health glt1 "Chaos" init 100
let t = get_turnorder init
let rem_red = remove_from_t "black belt" init
let r = get_turnorder rem_red
let remm = change_turns glt1 rem_red
let rem_red_white = remove_from_t "white mage" remm
let rw = get_turnorder rem_red_white
let before_next = get_next_fighter rem_red_white
let twist = change_turns glt1 rem_red_white
let blind = status_add "white mage" Blinded init 
let blind2 = status_add "white mage" Blinded blind
let blPo = status_add "white mage" Poisoned blind2
let remb = status_remove "white mage" Blinded blPo
let remp = status_remove "white mage" Poisoned blPo
let remnone = status_remove "white mage" Paralyzed blPo
let blpopa = status_add "white mage" Paralyzed blPo
let all_st = status_add "white mage" Silenced blpopa
let pure = pure_status "white mage" all_st
let all_st2 = set_health glt1 "white mage" all_st 30
let cure4 = cure4_status "white mage" all_st2 glt1
let sil_bb = status_add "thief" Silenced init
let par = status_add "thief" Paralyzed init
let use_he = used_heal "white mage" init
let use_pu = used_pure "white mage" use_he
let dead = set_health glt1 "black belt" init 0
let dead2 = set_health glt1 "white mage" dead 0
let dead3 = set_health glt1 "Chaos" dead2 0
let dead4 = set_health glt1 "thief" dead2 0
let mp = set_magic_points "thief" 5 init


let state_tests = [
  "Testing init set current boss correctly">:: 
  (fun _ -> assert_equal "Chaos" (get_current_boss init));
  "Testing init set next boss correctly">:: 
  (fun _ -> assert_equal "Mutability" (get_next_boss init));
  "Testing party is set correctly">:: 
  (fun _ -> assert_equal ["white mage";"black belt";"thief"] (get_party init));
  "Testing health was set correctly ">:: 
  (fun _ -> assert_equal 999 (get_health "black belt" init));
  "Testing set health works correctly">:: 
  (fun _ -> assert_equal 25 (get_health "black belt" r_set));
  "Testing get health for boss">:: 
  (fun _ -> assert_equal 1500 (get_health "Chaos" init)); 
  "Testing setting black belt health doesn't affect another character">::
  (fun _ -> assert_equal 1500 (get_health "Chaos" r_set));
  "Testing set on boss">:: 
  (fun _ -> assert_equal 100 (get_health "Chaos" b_set));
  "Testing size of init's turnorder">:: 
  (fun _ -> assert_equal 4 (List.length t));
  "Testing remove from turnorder. size">::
  (fun _ -> assert_equal 3 (List.length r));
  "Testing remove from turnorder, Character removed">:: 
  (fun _ -> assert_equal false (List.mem "black belt" r));
  "Testing remove from turnorder with two removed. Size">::
  (fun _ -> assert_equal 2 (List.length rw));
  "Testing remove from turnorder. White mage also removed">:: 
  (fun _ -> assert_equal false (List.mem "white mage" rw));
  "Testing remove from turnorder. black belt still removed">:: 
  (fun _ -> assert_equal false (List.mem "black belt" rw));
  "Testing remove from turnorder. Chaos not removed">:: 
  (fun _ -> assert_equal true (List.mem "Chaos" rw));
  "Testing status_add works.">:: 
  (fun _ -> assert_equal [Blinded] (get_status "white mage" blind));
  "Testing status_add on white mage does not affect black belt">:: 
  (fun _ -> assert_equal [] (get_status "black belt" blind));
  "Testing status_add does not duplicate">:: 
  (fun _ -> assert_equal [Blinded] (get_status "white mage" blind2));
  "Testing status_remove on Poisoned">:: 
  (fun _ -> assert_equal [Blinded] (get_status "white mage" remp));
  "Testing status_remove on Blinded">:: 
  (fun _ -> assert_equal [Poisoned] (get_status "white mage" remb));
  "testing status_remove when given a status affect not afflicted on character">:: 
  (fun _ -> assert_equal [Poisoned;Blinded] (get_status "white mage" remnone));
  "Testing is_Poisoned. Should be true">::
  (fun _ -> assert_equal true (is_poisoned "white mage" remnone));
  "Testing is_Poisoned. Should be false">::
  (fun _ -> assert_equal false (is_poisoned "black belt" remnone));
  "Testing is_Blinded. Should be true">::
  (fun _ -> assert_equal true (is_blinded "white mage" remnone));
  "Testing is_Blinded. Should be false">::
  (fun _ -> assert_equal false (is_blinded "black belt" remnone));
  "Testing is_Paralyzed. Should be false">::
  (fun _ -> assert_equal false (is_paralyzed "white mage" remnone));
  "Testing is_Paralyzed. Should be true">::
  (fun _ -> assert_equal true (is_paralyzed "white mage" all_st));
  "Testing is_silenced. Should be false">::
  (fun _ -> assert_equal false (is_silenced "white mage" remnone));
  "Testing is_silenced. Should be true">::
  (fun _ -> assert_equal true (is_silenced "white mage" all_st));
  "Testing pure_status works. All status should be gone">::
  (fun _ -> assert_equal [] (get_status "white mage" pure));
  "Testing cure4 removed status affects">::
  (fun _ -> assert_equal [] (get_status "white mage" cure4));
  "Testing cure4 revived character health">::
  (fun _ -> assert_equal 676 (get_health "white mage" cure4));
  "Testing is_valid_com for character Silenced using FIGHT">::
  (fun _ -> assert_equal true (is_valid_com "thief" sil_bb Command.Fight));
  "Testing is_valid_com for character Silenced using MAGIC">::
  (fun _ -> assert_equal false (is_valid_com "thief" sil_bb (Command.Magic "he")));
  "Testing is_valid_com for character Silenced using DRINK">::
  (fun _ -> assert_equal false (is_valid_com "thief" sil_bb (Command.Drink "llo")));
  "Testing is_valid_com for character Paralyzed using FIGHT">::
  (fun _ -> assert_equal false (is_valid_com "thief" par Command.Fight));
  "Testing is_valid_com for character Paralyzed using MAGIC">::
  (fun _ -> assert_equal false (is_valid_com "thief" par (Command.Magic "he")));
  "Testing is_valid_com for character Paralyzed using DRINK">::
  (fun _ -> assert_equal false (is_valid_com "thief" par (Command.Drink "llo")));
  "Testing is_valid_com for character with no effecrs using FIGHT">::
  (fun _ -> assert_equal true (is_valid_com "thief" init Command.Fight));
  "Testing is_valid_com for character with no effects using FIGHT">::
  (fun _ -> assert_equal true (is_valid_com "thief" init (Command.Magic "he")));
  "Testing is_valid_com for character with no effects using FIGHT">::
  (fun _ -> assert_equal true (is_valid_com "thief" init (Command.Drink "llo")));
  "Testing init potion heal is set correctly">::
  (fun _ -> assert_equal true (has_heal "white mage" init));
  "Testing init potion pure is set correctly">::
  (fun _ -> assert_equal true (has_pure "white mage" init));
  "Testing used_heal removed the potion heal. Should return false">::
  (fun _ -> assert_equal false (has_heal "white mage" use_he));
  "Testing used_pure removed the potion pure. Should return false">::
  (fun _ -> assert_equal false (has_pure "white mage" use_pu));
  "Testing removed the potion heal stays removed. Should return false">::
  (fun _ -> assert_equal false (has_heal "white mage" use_pu));
  "Testing is_dead returns true when character health is zero">::
  (fun _ -> assert_equal true (is_dead dead "black belt"));
  "Testing is_dead returns false when character health is above zero">::
  (fun _ -> assert_equal false (is_dead dead "white mage"));
  "Testing checl_alive with one player dead. Should be true">::
  (fun _ -> assert_equal true (check_alive dead));
  "Testing checl_alive with two players dead. Should be true">::
  (fun _ -> assert_equal true (check_alive dead2));
  "Testing checl_alive with two players dead and Boss. Should be true">::
  (fun _ -> assert_equal true (check_alive dead3));
  "Testing checl_alive with all three players dead. Should be false">::
  (fun _ -> assert_equal false (check_alive dead4));
  "testing set magic points sets correctly">::
  (fun _ -> assert_equal 5 (get_magic_points "thief" mp));
]

(** SPELLS TESTS *)
(** Functions that cannot be tested:
    - cast_spell, because it returns randomized values
    - get_skill, because it returns a value of the unexposed type skill
    - cast_boss_spell, because it returns randomized values
    - cast_boss_skill, because it returns randomized values
*)
let sp_st = remove_from_t "Chaos" init |> change_turns glt1

let nuke = Spells.get_spell "NUKE"
let cur4 = Spells.get_spell "CUR4"
let st_no_mp = set_magic_points "white mage" 0 sp_st

let spell_tests = [
  "friendly is invalid dmg target" >::
  (fun _ -> assert_equal false
      (Spells.is_valid_target sp_st nuke "white mage"));
  "friendly is valid friendly target" >::
  (fun _ -> assert_equal true
      (Spells.is_valid_target sp_st cur4 "white mage"));
  "not friendly is valid dmg target" >::
  (fun _ -> assert_equal true
      (Spells.is_valid_target sp_st nuke "Chaos"));
  "not friendly is invalid friendly target" >::
  (fun _ -> assert_equal false
      (Spells.is_valid_target sp_st cur4 "Chaos"));
  "white mage has enough mp" >::
  (fun _ -> assert_equal true
      (Spells.is_enough_mp cur4 white_mage sp_st));
  "white mage does not have enough mp" >::
  (fun _ -> assert_equal false
      (Spells.is_enough_mp cur4 white_mage st_no_mp));
]

(** COMMAND TESTS *)
let cmd_empty = try Some (parse "") with Empty -> None
let magic_mal = try Some (parse "cast") with Malformed -> None
let potion_mal = try Some (parse "drink") with Malformed -> None
let mal = try Some (parse "malformed") with Malformed -> None

let command_tests = [
  "parse empty" >:: (fun _ -> assert_equal None cmd_empty);
  "parse fight" >:: (fun _ -> assert_equal Fight (parse "fight"));
  "parse magic" >::
  (fun _ -> assert_equal (Magic "spell") (parse "cast spell"));
  "parse malformed magic" >::
  (fun _ -> assert_equal None magic_mal);
  "parse drink" >::
  (fun _ -> assert_equal (Drink "potion") (parse "drink potion"));
  "parse malformed drink" >::
  (fun _ -> assert_equal None potion_mal);
  "parse show" >:: (fun _ -> assert_equal Show (parse "magic"));
  "parse quit" >:: (fun _ -> assert_equal Quit (parse "quit"));
  "parse pass" >:: (fun _ -> assert_equal Pass (parse "pass"));
  "parse malformed" >:: (fun _ -> assert_equal None mal)
]

let suite = "search test suite" >::: List.flatten [
    party_tests;
    gauntlet_tests;
    state_tests;
    spell_tests;
    command_tests
  ]

let _ = run_test_tt_main suite