open OUnit2
module PTD = Prefix_tree_dict
open PTD

(* ===== UNIT ТЕСТЫ ===== *)

let test_empty _ =
  assert_bool "пустой словарь" (is_empty empty);
  assert_equal 0 (size empty);
  assert_equal [] (to_list empty)

let test_singleton _ =
  let d = singleton "hello" 42 in
  assert_bool "не пустой" (not (is_empty d));
  assert_equal 1 (size d);
  assert_equal (Some 42) (find_opt "hello" d);
  assert_equal None (find_opt "hell" d)

let test_insert_find _ =
  let d = empty 
    |> insert "apple" 1
    |> insert "app" 2
    |> insert "banana" 3
    |> insert "band" 4
  in
  assert_equal (Some 1) (find_opt "apple" d);
  assert_equal (Some 2) (find_opt "app" d);
  assert_equal (Some 3) (find_opt "banana" d);
  assert_equal (Some 4) (find_opt "band" d);
  assert_equal None (find_opt "ap" d);
  assert_equal 4 (size d)

let test_insert_overwrite _ =
  let d = empty |> insert "key" 1 |> insert "key" 2 in
  assert_equal (Some 2) (find_opt "key" d);
  assert_equal 1 (size d)

let test_mem _ =
  let d = empty |> insert "foo" 1 |> insert "bar" 2 in
  assert_bool "mem foo" (mem "foo" d);
  assert_bool "not mem baz" (not (mem "baz" d))

let test_remove _ =
  let d = empty 
    |> insert "apple" 1
    |> insert "app" 2
    |> insert "application" 3
  in
  let d1 = remove "app" d in
  assert_equal None (find_opt "app" d1);
  assert_equal (Some 1) (find_opt "apple" d1);
  assert_equal (Some 3) (find_opt "application" d1);
  assert_equal 2 (size d1)

let test_remove_all _ =
  let d = empty |> insert "a" 1 |> insert "b" 2 in
  let d' = d |> remove "a" |> remove "b" in
  assert_bool "пустой после удаления всех" (is_empty d')

let test_empty_key _ =
  let d = empty |> insert "" 42 |> insert "a" 1 in
  assert_equal (Some 42) (find_opt "" d);
  assert_equal (Some 1) (find_opt "a" d);
  assert_equal 2 (size d)

let test_list_conversion _ =
  let pairs = [("apple", 1); ("app", 2); ("banana", 3)] in
  let d = of_list pairs in
  let result = to_list d in
  assert_equal [("app", 2); ("apple", 1); ("banana", 3)] result

let test_map _ =
  let d = empty |> insert "a" 1 |> insert "b" 2 |> insert "c" 3 in
  let d' = map (fun x -> x * 2) d in
  assert_equal (Some 2) (find_opt "a" d');
  assert_equal (Some 4) (find_opt "b" d');
  assert_equal (Some 6) (find_opt "c" d')

let test_mapi _ =
  let d = empty |> insert "ab" 1 |> insert "cd" 2 in
  let d' = mapi (fun k v -> String.length k + v) d in
  assert_equal (Some 3) (find_opt "ab" d');
  assert_equal (Some 4) (find_opt "cd" d')

let test_filter _ =
  let d = empty 
    |> insert "apple" 1
    |> insert "apricot" 2
    |> insert "banana" 3
  in
  let d' = filter (fun k _ -> String.length k > 5) d in
  assert_equal None (find_opt "apple" d');
  assert_equal (Some 2) (find_opt "apricot" d');
  assert_equal 2 (size d')

let test_filter_values _ =
  let d = empty |> insert "a" 1 |> insert "b" 2 |> insert "c" 3 in
  let d' = filter_values (fun v -> v > 1) d in
  assert_equal None (find_opt "a" d');
  assert_equal (Some 2) (find_opt "b" d')

let test_fold_left _ =
  let d = empty |> insert "a" 1 |> insert "b" 2 |> insert "c" 3 in
  let sum = fold_left (fun acc _ v -> acc + v) 0 d in
  assert_equal 6 sum;
  let keys_list = fold_left (fun acc k _ -> k :: acc) [] d in
  assert_equal ["c"; "b"; "a"] keys_list

let test_fold_right _ =
  let d = empty |> insert "a" 1 |> insert "b" 2 |> insert "c" 3 in
  let sum = fold_right (fun _ v acc -> acc + v) d 0 in
  assert_equal 6 sum;
  let keys_list = fold_right (fun k _ acc -> k :: acc) d [] in
  assert_equal ["a"; "b"; "c"] keys_list

let test_merge _ =
  let d1 = empty |> insert "a" 1 |> insert "b" 2 in
  let d2 = empty |> insert "b" 20 |> insert "c" 3 in
  let d = merge d1 d2 in
  assert_equal (Some 1) (find_opt "a" d);
  assert_equal (Some 20) (find_opt "b" d);
  assert_equal (Some 3) (find_opt "c" d);
  assert_equal 3 (size d)

let test_merge_with _ =
  let d1 = empty |> insert "a" 1 |> insert "b" 2 in
  let d2 = empty |> insert "b" 20 |> insert "c" 3 in
  let d = merge_with (+) d1 d2 in
  assert_equal (Some 22) (find_opt "b" d)

let test_concat _ =
  let d1 = singleton "a" 1 in
  let d2 = singleton "b" 2 in
  let d3 = singleton "c" 3 in
  let d = concat [d1; d2; d3] in
  assert_equal 3 (size d)

(* Тест равенства при разном порядке вставки *)
let test_equal _ =
  let d1 = empty |> insert "a" 1 |> insert "b" 2 in
  let d2 = empty |> insert "b" 2 |> insert "a" 1 in
  let d3 = empty |> insert "a" 1 |> insert "b" 3 in
  let d4 = empty |> insert "a" 1 in
  assert_bool "одинаковые элементы в разном порядке" (equal (=) d1 d2);
  assert_bool "разные значения" (not (equal (=) d1 d3));
  assert_bool "разный размер" (not (equal (=) d1 d4))

(* Дополнительный тест: много элементов в разном порядке *)
let test_equal_many_elements _ =
  let pairs = [("apple", 1); ("app", 2); ("banana", 3); ("band", 4); ("cat", 5)] in
  let d1 = of_list pairs in
  let d2 = of_list (List.rev pairs) in
  let d3 = of_list [("band", 4); ("app", 2); ("cat", 5); ("banana", 3); ("apple", 1)] in
  assert_bool "равны при разном порядке 1" (equal (=) d1 d2);
  assert_bool "равны при разном порядке 2" (equal (=) d1 d3);
  assert_bool "равны при разном порядке 3" (equal (=) d2 d3)

let test_keys_values _ =
  let d = empty |> insert "b" 2 |> insert "a" 1 |> insert "c" 3 in
  assert_equal ["a"; "b"; "c"] (keys d);
  assert_equal [1; 2; 3] (values d)

let test_find_prefix _ =
  let d = empty 
    |> insert "apple" 1
    |> insert "application" 2
    |> insert "apply" 3
    |> insert "banana" 4
  in
  let result = find_prefix "app" d in
  assert_equal 3 (List.length result);
  assert_bool "содержит apple" (List.mem ("apple", 1) result)

let test_immutability _ =
  let d1 = empty |> insert "a" 1 in
  let d2 = insert "b" 2 d1 in
  let d3 = remove "a" d1 in
  assert_equal (Some 1) (find_opt "a" d1);
  assert_equal None (find_opt "b" d1);
  assert_equal 1 (size d1);
  assert_equal 2 (size d2);
  assert_equal 0 (size d3)

let unit_tests = "Unit тесты" >::: [
  "empty" >:: test_empty;
  "singleton" >:: test_singleton;
  "insert_find" >:: test_insert_find;
  "insert_overwrite" >:: test_insert_overwrite;
  "mem" >:: test_mem;
  "remove" >:: test_remove;
  "remove_all" >:: test_remove_all;
  "empty_key" >:: test_empty_key;
  "list_conversion" >:: test_list_conversion;
  "map" >:: test_map;
  "mapi" >:: test_mapi;
  "filter" >:: test_filter;
  "filter_values" >:: test_filter_values;
  "fold_left" >:: test_fold_left;
  "fold_right" >:: test_fold_right;
  "merge" >:: test_merge;
  "merge_with" >:: test_merge_with;
  "concat" >:: test_concat;
  "equal" >:: test_equal;
  "equal_many_elements" >:: test_equal_many_elements;
  "keys_values" >:: test_keys_values;
  "find_prefix" >:: test_find_prefix;
  "immutability" >:: test_immutability;
]

(* ===== PROPERTY-BASED ТЕСТЫ ===== *)

module Q = QCheck

(* Генератор ключей: строки из букв a-e длиной 0-5 *)
let key_gen = Q.Gen.(string_size ~gen:(char_range 'a' 'e') (int_range 0 5))
let key_arb = Q.make ~print:(fun s -> "\"" ^ s ^ "\"") key_gen

(* Генератор словаря *)
let pairs_gen = Q.Gen.list_size (Q.Gen.int_range 0 20) (Q.Gen.pair key_gen Q.Gen.int)
let dict_gen = Q.Gen.map PTD.of_list pairs_gen
let dict_arb = Q.make 
  ~print:(fun d -> PTD.to_list d |> List.map (fun (k,v) -> Printf.sprintf "(%s,%d)" k v) |> String.concat ";" |> Printf.sprintf "[%s]")
  dict_gen

(* Моноид: левый нейтральный элемент *)
let prop_monoid_left_identity =
  Q.Test.make ~name:"Моноид: merge empty d = d" ~count:100 dict_arb
    (fun d -> PTD.equal Int.equal (PTD.merge PTD.empty d) d)

(* Моноид: правый нейтральный элемент *)
let prop_monoid_right_identity =
  Q.Test.make ~name:"Моноид: merge d empty = d" ~count:100 dict_arb
    (fun d -> PTD.equal Int.equal (PTD.merge d PTD.empty) d)

(* Моноид: ассоциативность *)
let prop_monoid_associativity =
  Q.Test.make ~name:"Моноид: ассоциативность merge" ~count:100
    (Q.triple dict_arb dict_arb dict_arb)
    (fun (a, b, c) -> 
      PTD.equal Int.equal (PTD.merge (PTD.merge a b) c) (PTD.merge a (PTD.merge b c)))

(* Вставка затем поиск возвращает значение *)
let prop_insert_find =
  Q.Test.make ~name:"insert k v; find k = Some v" ~count:200
    (Q.pair key_arb Q.int)
    (fun (k, v) ->
      let d = PTD.insert k v PTD.empty in
      PTD.find_opt k d = Some v)

(* Удаление затем поиск возвращает None *)
let prop_remove_find =
  Q.Test.make ~name:"remove k; find k = None" ~count:200
    (Q.pair dict_arb key_arb)
    (fun (d, k) ->
      let d' = PTD.remove k d in
      PTD.find_opt k d' = None)

(* size = длина to_list *)
let prop_size_to_list =
  Q.Test.make ~name:"size = length (to_list)" ~count:100 dict_arb
    (fun d -> PTD.size d = List.length (PTD.to_list d))

(* of_list (to_list d) = d *)
let prop_of_list_to_list =
  Q.Test.make ~name:"of_list (to_list d) = d" ~count:100 dict_arb
    (fun d -> PTD.equal Int.equal (PTD.of_list (PTD.to_list d)) d)

(* map id = id *)
let prop_map_identity =
  Q.Test.make ~name:"map id = id" ~count:100 dict_arb
    (fun d -> PTD.equal Int.equal (PTD.map (fun x -> x) d) d)

(* map (f . g) = map f . map g *)
let prop_map_composition =
  Q.Test.make ~name:"map (f . g) = map f . map g" ~count:100 dict_arb
    (fun d ->
      let f x = x + 1 in
      let g x = x * 2 in
      PTD.equal Int.equal (PTD.map f (PTD.map g d)) (PTD.map (fun x -> f (g x)) d))

(* filter true = id *)
let prop_filter_true =
  Q.Test.make ~name:"filter (const true) = id" ~count:100 dict_arb
    (fun d -> PTD.equal Int.equal (PTD.filter (fun _ _ -> true) d) d)

(* filter false = empty *)
let prop_filter_false =
  Q.Test.make ~name:"filter (const false) = empty" ~count:100 dict_arb
    (fun d -> PTD.equal Int.equal (PTD.filter (fun _ _ -> false) d) PTD.empty)

(* fold_left корректно суммирует *)
let prop_fold_sum =
  Q.Test.make ~name:"fold_left sum = sum of values" ~count:100 dict_arb
    (fun d ->
      let sum1 = PTD.fold_left (fun acc _ v -> acc + v) 0 d in
      let sum2 = List.fold_left (fun acc (_, v) -> acc + v) 0 (PTD.to_list d) in
      sum1 = sum2)

(* Рефлексивность равенства *)
let prop_equal_reflexive =
  Q.Test.make ~name:"equal рефлексивно: d = d" ~count:100 dict_arb
    (fun d -> PTD.equal Int.equal d d)

(* Симметричность равенства *)
let prop_equal_symmetric =
  Q.Test.make ~name:"equal симметрично" ~count:100
    (Q.pair dict_arb dict_arb)
    (fun (d1, d2) -> PTD.equal Int.equal d1 d2 = PTD.equal Int.equal d2 d1)

(* Равенство не зависит от порядка вставки (для уникальных ключей) *)
let prop_equal_order_independent =
  Q.Test.make ~name:"equal не зависит от порядка вставки" ~count:100
    (Q.make pairs_gen)
    (fun pairs ->
      (* Убираем дубликаты, оставляя последнее значение для каждого ключа *)
      let dedup pairs =
        let tbl = Hashtbl.create 16 in
        List.iter (fun (k, v) -> Hashtbl.replace tbl k v) pairs;
        Hashtbl.fold (fun k v acc -> (k, v) :: acc) tbl []
      in
      let unique_pairs = dedup pairs in
      let d1 = PTD.of_list unique_pairs in
      let d2 = PTD.of_list (List.rev unique_pairs) in
      PTD.equal Int.equal d1 d2)

(* mem k = is_some (find_opt k) *)
let prop_mem_find =
  Q.Test.make ~name:"mem k = is_some (find_opt k)" ~count:200
    (Q.pair dict_arb key_arb)
    (fun (d, k) -> PTD.mem k d = Option.is_some (PTD.find_opt k d))

let qcheck_tests = List.map QCheck_ounit.to_ounit2_test [
  prop_monoid_left_identity;
  prop_monoid_right_identity;
  prop_monoid_associativity;
  prop_insert_find;
  prop_remove_find;
  prop_size_to_list;
  prop_of_list_to_list;
  prop_map_identity;
  prop_map_composition;
  prop_filter_true;
  prop_filter_false;
  prop_fold_sum;
  prop_equal_reflexive;
  prop_equal_symmetric;
  prop_equal_order_independent;
  prop_mem_find;
]

let property_tests = "Property-based тесты" >::: qcheck_tests

let () = 
  run_test_tt_main ("Prefix Tree Dict тесты" >::: [
    unit_tests;
    property_tests;
  ])
