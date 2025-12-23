# Prefix Tree Dict (OCaml)
Беля Алексей Иванович 408256

Лабораторная работа по функциональному программированию (ИТМО): 
реализация неизменяемого полиморфного словаря `'key -> 'a` на основе префиксного дерева (trie),
обобщённого по типу ключа через функтор.

## Задача

- Реализовать структуру данных "prefix tree dict":
  - интерфейс уровня обычного словаря (ассоциативного массива),
  - внутренняя структура данных — префиксное дерево (trie) по ключам общего вида;
- Структура должна быть **неизменяемой** и **полиморфной** (`'a t`);
- Структура должна образовывать **моноид**:
  - нейтральный элемент `empty`,
  - ассоциативная операция объединения словарей `merge`;
- Реализовать базовые операции и функции высшего порядка:
  - `empty`, `is_empty`, `singleton`,
  - `insert`, `find_opt`, `mem`, `remove`,
  - `to_list`, `of_list`, `size`,
  - `map`, `mapi`, `filter`, `filter_values`,
  - `fold_left`, `fold_right`, `iter`;
- Реализовать операции над моноидом:
  - `merge`, `merge_with`, `concat`;
- Реализовать **эффективное сравнение** двух словарей `equal` без перевода в список и сортировки;
- Покрыть библиотеку **unit-тестами** и **property-based тестами** (QCheck), в том числе свойствами моноида.

## Структура проекта

- `dune-project` — описание dune-проекта и opam-пакета;
- `lib/`
  - `dune` — сборка библиотеки `prefix_tree_dict`;
  - `prefix_tree_dict.ml` — реализация структуры данных и всего публичного API;
- `test/`
  - `dune` — сборка тестового исполняемого файла;
  - `test_prefix_tree_dict.ml` — unit-тесты и property-based тесты.

## Основные идеи реализации

- Ключи в общем случае задаются модулем-аргументом функтора `Make`.
  Он должен удовлетворять сигнатуре `KEY` (см. `prefix_tree_dict.mli`):

  ```ocaml
  module type KEY = sig
    type t
    type elem

    val to_list : t -> elem list
    val of_list : elem list -> t
    val compare_elem : elem -> elem -> int
  end
  ```

  На основе модуля `Key : KEY` функтор `Make` строит модуль-префиксное дерево:

  ```ocaml
  module Make (Key : KEY) : sig
    type 'a t
    (* ...все операции словаря... *)
  end
  ```

  Внутренняя структура узла использует отображение по элементам ключа:

  ```ocaml
  module ElemMap = Map.Make (struct
    type t = Key.elem

    let compare = Key.compare_elem
  end)

  type 'a t = {
    value : 'a option;           (* значение по ключу, заканчивающемуся в этом узле *)
    children : 'a t ElemMap.t;   (* переходы по элементам ключа к поддеревьям *)
  }
  ```
- Операции `insert` / `find` / `find_opt` / `remove` рекурсивно спускаются по элементам ключа.
- Функции высшего порядка (`map`, `mapi`, `filter`, `fold_left`, `fold_right`) реализованы как обход всего дерева с аккуратным восстановлением структуры.
- Моноидная операция `merge` объединяет два дерева, проходя синхронно по структуре и разрешая конфликты по значениям.
- `equal value_eq t1 t2` рекурсивно сравнивает структуру двух деревьев и значения с помощью переданной функции сравнения `value_eq`.

## Тестирование

Тесты написаны с использованием OUnit2 и QCheck.

Покрываются:
- базовые операции словаря (`insert`, `find_opt`, `mem`, `remove`, `to_list`, `of_list`, `size`);
- функции высшего порядка (`map`, `mapi`, `filter`, `fold_left`, `fold_right`);
- операции моноида (`merge`, `merge_with`, `concat`);
- сравнение `equal`, в том числе при разном порядке вставки ключей;
- дополнительные утилиты (`keys`, `values`, `find_prefix`, `has_prefix`, `iter`).

Property-based тесты (QCheck) проверяют, в частности:
- законы моноида: левый/правый нейтральный элемент `empty` и ассоциативность `merge`;
- корректность `insert`/`remove`/`find_opt`;
- согласованность `size` и `to_list`;
- законы для `map` (identity, композиция);
- свойства `filter` и `equal`.

## Как собрать и запустить тесты

Требуется установленный OCaml и dune (см. версии в `dune-project`).

В корне проекта (`lab2/`):

```bash
# Сборка проекта
 dune build

# Запуск всех тестов
 dune test
```

Результатом запуска тестов должен быть отчёт с успешным прохождением 
всех unit- и property-based тестов.

## Пример использования

### Строковый словарь по умолчанию

Внутри `prefix_tree_dict.ml` определён модуль строковых ключей:

```ocaml
module StringKey : KEY with type t = string and type elem = char
```

и сразу же создан инстанс функтора:

```ocaml
include Make (StringKey)
```

Благодаря этому модуль `Prefix_tree_dict` по умолчанию ведёт себя как словарь `string -> 'a`,
совместимый с предыдущей версией задания.

Небольшой пример работы со строковым модулем (см. также тесты):

```ocaml
open Prefix_tree_dict

let d =
  empty
  |> insert "cat" 1
  |> insert "car" 2
  |> insert "dog" 3

let () =
  match find_opt "car" d with
  | Some v -> Printf.printf "car = %d\n" v
  | None -> Printf.printf "car not found\n";

  let only_c = find_prefix "ca" d in
  List.iter (fun (k, v) -> Printf.printf "%s -> %d\n" k v) only_c
```

Этот код создаёт словарь, находит значение по ключу `"car"` и выводит все пары с префиксом `"ca"`.

### Использование функтора с другими типами ключей

Благодаря функтору можно построить префиксный словарь по любому типу ключей,
который можно представить в виде списка элементов и сравнивать поэлементно. Например,
по спискам целых чисел:

```ocaml
open Prefix_tree_dict

module IntListKey : KEY with type t = int list and type elem = int = struct
  type t = int list
  type elem = int

  let to_list x = x
  let of_list x = x
  let compare_elem = Int.compare
end

module IntListDict = Make (IntListKey)

let d =
  IntListDict.empty
  |> IntListDict.insert [ 1; 2; 3 ] "abc"
  |> IntListDict.insert [ 1; 2; 4 ] "abd"

let () =
  match IntListDict.find_opt [ 1; 2; 3 ] d with
  | Some v -> Printf.printf "[1;2;3] -> %s\n" v
  | None -> Printf.printf "not found\n"
```

Таким образом, реализация префиксного дерева стала полиморфной не только по значению `'a`,
но и по типу ключа.

## Отчёт по лабораторной

Подробное теоретическое объяснение (описание trie, моноида, property-based тестов 
и разбор ключевых функций `insert`, `remove`, `merge`, `filter`, `map`, `equal`) 
приведено в отдельном отчёте (`Беля_ФП_2.pdf`).
