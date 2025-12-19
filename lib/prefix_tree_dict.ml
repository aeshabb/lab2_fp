(* Prefix Tree Dict - полиморфный словарь на основе префиксного дерева (trie).
   Ключи - строки, значения - полиморфные.
   Структура неизменяемая и является моноидом. *)

module CharMap = Map.Make(Char)

(* Узел префиксного дерева *)
type 'a t = {
  value: 'a option;
  children: 'a t CharMap.t;
}

(* Пустой словарь - нейтральный элемент моноида *)
let empty = {
  value = None;
  children = CharMap.empty;
}

let is_empty t =
  Option.is_none t.value && CharMap.is_empty t.children

(* Создание словаря с одним элементом *)
let singleton key value =
  let rec aux chars =
    match chars with
    | [] -> { value = Some value; children = CharMap.empty }
    | c :: rest -> 
      { value = None; children = CharMap.singleton c (aux rest) }
  in
  aux (List.of_seq (String.to_seq key))

(* Вставка элемента. При существующем ключе - перезапись *)
let insert key value t =
  let rec aux chars node =
    match chars with
    | [] -> { node with value = Some value }
    | c :: rest ->
      let child = 
        match CharMap.find_opt c node.children with
        | Some child -> aux rest child
        | None -> aux rest empty
      in
      { node with children = CharMap.add c child node.children }
  in
  aux (List.of_seq (String.to_seq key)) t

(* Поиск значения по ключу *)
let find_opt key t =
  let rec aux chars node =
    match chars with
    | [] -> node.value
    | c :: rest ->
      match CharMap.find_opt c node.children with
      | Some child -> aux rest child
      | None -> None
  in
  aux (List.of_seq (String.to_seq key)) t

(* Проверка наличия ключа *)
let mem key t =
  Option.is_some (find_opt key t)

(* Удаление элемента. Очищает пустые ветви *)
let remove key t =
  let rec aux chars node =
    match chars with
    | [] -> 
      let new_node = { node with value = None } in
      if CharMap.is_empty new_node.children then None
      else Some new_node
    | c :: rest ->
      match CharMap.find_opt c node.children with
      | None -> Some node
      | Some child ->
        match aux rest child with
        | None -> 
          let new_children = CharMap.remove c node.children in
          if Option.is_none node.value && CharMap.is_empty new_children then None
          else Some { node with children = new_children }
        | Some new_child ->
          Some { node with children = CharMap.add c new_child node.children }
  in
  match aux (List.of_seq (String.to_seq key)) t with
  | None -> empty
  | Some t' -> t'

(* Преобразование в список пар (ключ, значение) в лексикографическом порядке *)
let to_list t =
  let rec aux prefix node acc =
    let acc = 
      match node.value with
      | Some v -> (prefix, v) :: acc
      | None -> acc
    in
    CharMap.fold (fun c child acc ->
      aux (prefix ^ String.make 1 c) child acc
    ) node.children acc
  in
  List.rev (aux "" t [])

(* Создание словаря из списка пар *)
let of_list pairs =
  List.fold_left (fun acc (k, v) -> insert k v acc) empty pairs

(* Количество элементов *)
let size t =
  let rec aux node =
    let count = if Option.is_some node.value then 1 else 0 in
    CharMap.fold (fun _ child acc -> acc + aux child) node.children count
  in
  aux t

(* Применение функции ко всем значениям *)
let map f t =
  let rec aux node =
    {
      value = Option.map f node.value;
      children = CharMap.map aux node.children;
    }
  in
  aux t

(* Применение функции с ключом ко всем парам *)
let mapi f t =
  let rec aux prefix node =
    {
      value = Option.map (f prefix) node.value;
      children = CharMap.mapi (fun c child -> 
        aux (prefix ^ String.make 1 c) child
      ) node.children;
    }
  in
  aux "" t

(* Фильтрация по предикату *)
let filter pred t =
  let rec aux prefix node =
    let new_value = 
      match node.value with
      | Some v when pred prefix v -> Some v
      | _ -> None
    in
    let new_children = 
      CharMap.fold (fun c child acc ->
        let new_prefix = prefix ^ String.make 1 c in
        let new_child = aux new_prefix child in
        if is_empty new_child then acc
        else CharMap.add c new_child acc
      ) node.children CharMap.empty
    in
    { value = new_value; children = new_children }
  in
  aux "" t

(* Фильтрация только по значению *)
let filter_values pred t =
  filter (fun _ v -> pred v) t

(* Левая свёртка в лексикографическом порядке ключей *)
let fold_left f init t =
  let rec aux prefix acc node =
    let acc = 
      match node.value with
      | Some v -> f acc prefix v
      | None -> acc
    in
    CharMap.fold (fun c child acc ->
      aux (prefix ^ String.make 1 c) acc child
    ) node.children acc
  in
  aux "" init t

(* Правая свёртка в обратном лексикографическом порядке *)
let fold_right f t init =
  let rec aux prefix node acc =
    let acc = 
      CharMap.to_rev_seq node.children 
      |> Seq.fold_left (fun acc (c, child) ->
        aux (prefix ^ String.make 1 c) child acc
      ) acc
    in
    match node.value with
    | Some v -> f prefix v acc
    | None -> acc
  in
  aux "" t init

(* Итерация по всем парам *)
let iter f t =
  fold_left (fun () k v -> f k v) () t

(* Объединение двух словарей - операция моноида.
   При конфликте ключей побеждает значение из второго словаря *)
let merge t1 t2 =
  let rec aux n1 n2 =
    let new_value = 
      match n2.value with
      | Some _ -> n2.value
      | None -> n1.value
    in
    let new_children =
      CharMap.merge (fun _ c1 c2 ->
        match c1, c2 with
        | None, None -> None
        | Some c, None -> Some c
        | None, Some c -> Some c
        | Some c1, Some c2 -> Some (aux c1 c2)
      ) n1.children n2.children
    in
    { value = new_value; children = new_children }
  in
  aux t1 t2

(* Объединение с пользовательской функцией разрешения конфликтов *)
let merge_with f t1 t2 =
  let rec aux n1 n2 =
    let new_value = 
      match n1.value, n2.value with
      | None, None -> None
      | Some v, None -> Some v
      | None, Some v -> Some v
      | Some v1, Some v2 -> Some (f v1 v2)
    in
    let new_children =
      CharMap.merge (fun _ c1 c2 ->
        match c1, c2 with
        | None, None -> None
        | Some c, None -> Some c
        | None, Some c -> Some c
        | Some c1, Some c2 -> Some (aux c1 c2)
      ) n1.children n2.children
    in
    { value = new_value; children = new_children }
  in
  aux t1 t2

(* Конкатенация списка словарей *)
let concat dicts =
  List.fold_left merge empty dicts

(* Эффективное сравнение без преобразования в списки.
   Сравнивает структуры напрямую рекурсивно *)
let equal value_eq t1 t2 =
  let rec aux n1 n2 =
    let values_eq = 
      match n1.value, n2.value with
      | None, None -> true
      | Some v1, Some v2 -> value_eq v1 v2
      | _ -> false
    in
    if not values_eq then false
    else
      let keys1 = CharMap.bindings n1.children |> List.map fst in
      let keys2 = CharMap.bindings n2.children |> List.map fst in
      if keys1 <> keys2 then false
      else
        CharMap.for_all (fun c child1 ->
          match CharMap.find_opt c n2.children with
          | None -> false
          | Some child2 -> aux child1 child2
        ) n1.children
  in
  aux t1 t2

(* Получение всех ключей *)
let keys t =
  fold_left (fun acc k _ -> k :: acc) [] t |> List.rev

(* Получение всех значений *)
let values t =
  fold_left (fun acc _ v -> v :: acc) [] t |> List.rev

(* Поиск всех пар с данным префиксом ключа *)
let find_prefix prefix t =
  let rec navigate chars node =
    match chars with
    | [] -> Some node
    | c :: rest ->
      match CharMap.find_opt c node.children with
      | Some child -> navigate rest child
      | None -> None
  in
  match navigate (List.of_seq (String.to_seq prefix)) t with
  | None -> []
  | Some subtree ->
    let rec collect current_prefix node acc =
      let acc = 
        match node.value with
        | Some v -> (prefix ^ current_prefix, v) :: acc
        | None -> acc
      in
      CharMap.fold (fun c child acc ->
        collect (current_prefix ^ String.make 1 c) child acc
      ) node.children acc
    in
    List.rev (collect "" subtree [])

(* Проверка наличия ключей с данным префиксом *)
let has_prefix prefix t =
  find_prefix prefix t <> []
