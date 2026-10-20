(* 式の型 *)
type exp =
| IntLit of int
| Plus of exp * exp
| Times of exp * exp
| Minus of exp * exp
| Div of exp * exp
| BoolLit of bool
| Eq of exp * exp
| Ne of exp * exp
| Lt of exp * exp
| Greater of exp * exp
| If of exp * exp * exp
| Var of string
| Let of string * exp * exp
| Fun of string * exp
| App of exp * exp
| LetRec of string * string * exp * exp

(* 値の型 *)
type value =
| IntVal of int
| BoolVal of bool 
| FunVal of string * exp * ((string * value) list)








(* inputは、まだ読み取っていない文字のリストを表す *)
type input = char list

(* stringで受け取ったソースコード列をinput型(charのリスト)に変換する *)
(* chars_of_string : string -> input *)
let chars_of_string s = 
  List.of_seq (String.to_seq s)

(* 条件condを満たすなら、次の1文字を読み取る *)
(* satisfy: (char -> bool) -> input -> (char * input) option *)
let satisfy cond input = 
  match input with
  | c :: cs when cond c -> Some (c, cs)
  | _ -> None

(* 指定した文字cと一致するかどうかを判定する *)
(* char_p: char -> input -> (char * input) option *)
let char_p c input =
  satisfy (fun x -> x = c) input

(* スペースを飛ばす *)
(* skip_spaces: input -> input *)
let rec skip_spaces input =
  match input with
  (* ' 'はスペース、'\t'はタブ、'\n'は改行、'\r'はキャリッジリターン(windowsなどの改行コードの一部)を表す *)
  | c :: cs when c = ' ' || c = '\t' || c = '\n' || c = '\r' ->
    skip_spaces cs (* 空白だったので、残りのリストcsに対して再帰的に処理を続ける *)
  | _ -> input (* 空白以外の文字が来たら、あるいはリストが空になったら、現在のリストをそのまま帰す *)

(* 数字の列が続く限り、束ねて整数に変換する *)
(* int_p; input -> (int * input) option *)
let int_p input =
  let is_digit c =
    c >= '0' && c <= '9' in
  
  (* 補助関数、数字が続く限りacc(アキュムレータ)に文字を溜め込む *)
  (* read_digits : input -> char list -> char list * input *)
  let rec read_digits chars acc =
    match chars with
    | c :: cs when is_digit c -> read_digits cs (c :: acc)
    | _ -> (List.rev acc, chars)
  in

  match read_digits input [] with
  | ([], _) -> None (*アキュムレータが空 = 最初から数字が1つもなかった場合 *)
  | (digits, rest) ->
    (* 文字のリストを文字列(string)に変換してから、整数(int)に変換する *)
    let str = String.of_seq (List.to_seq digits) in
    Some (int_of_string str, rest)

(* +や*といった指定された文字と、入力の先頭が一致するか確認し、一致したら、その文字列をトークンとして返す *)
(* string_p: string -> input -> (string * input) option *)
let string_p expected input =

  (* 補助関数、targetの文字のリストと、現在の入力charsが順番に一致するか確認する *)
  (* check_chars : input -> char list -> (string * input) option *)
  let rec check_chars chars target =
    match target with
    | [] -> Some (expected, chars) (* 探したい文字(target)をすべて読み終えたら、一致したとみなして成功 *)
    | t :: ts ->
      (match char_p t chars with
      | Some (_, rest) -> check_chars rest ts (* 1文字一致したので、残りを再帰的に確認する*)
      | None -> None (* 途中で一致しない文字があったら失敗*)) 
    in

  (* 探したい文字列kwを文字のリストに変換してから、check_charsに渡す *)
  check_chars input (chars_of_string expected)

  (* +、*、->などの記号を読む *)
  (* symbol_p: string -> input -> (string * input) option *)
  let symbol_p symbol input =
    string_p symbol input

  (* 識別子の続きを構成できる文字かを判定する *)
  (* is_ident_letter : char -> bool *)
  let is_ident_letter c =
    (c >= 'a' && c <= 'z')
    || (c >= 'A' && c <= 'Z')
    || (c >= '0' && c <= '9')
    || c = '_'

  (* if、then、trueなどの予約語を読む。識別子の途中には一致させない *)
  (* keyword_p: string -> input -> (string * input) option *)
  let keyword_p keyword input =
    match string_p keyword input with
    | None -> None
    | Some (_, c :: _) when is_ident_letter c -> None
    | Some (_, rest) -> Some (keyword, rest)

(* パースが成功したら、その結果を使って次のパースを行う *)
(* let* : (input -> ('a * input) option) ->
          ('a -> input -> ('b * input) option) ->
           input ->
          ('b * input) option        
*)
let ( let* ) p f =
  fun input ->
    match p input with
    | None -> None
    | Some (v, rest) -> f v rest

(* 常に成功する *)
(* return: 'a -> input -> ('a * input) option *)
let return v =
  fun input -> Some (v, input)

(* 成功したら空白をスキップ *)
(* token: (input -> ('a * input) option)
          input ->
          ('a * input) option
*)
let token p =
  let* v = p in
  fun input -> Some (v, skip_spaces input)

(* 左側のパーサを試し、失敗したら右側のパーサを試す *)
(* (<|>) :  ('a -> 'b option)
         -> ('a -> 'b option)
         -> 'a
         -> 'b option
*)
let ( <|> ) p1 p2 =
  fun input ->
    match p1 input with
    | Some _ as res -> res
    | None -> p2 input

(* 文字種の判定 *)
(* is_alpha: char -> bool *)
(* is_alnum: char -> bool *)
let is_alpha c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
let is_alnum c =
  is_alpha c || (c >= '0' && c <= '9') || c = '_'

(* 変数名のパースと予約語の判定 *)
(* ident_p: input -> (string * input) option *)
let ident_p input =
  (* 英文字が続く限りアキュムレータaccにため込む補助関数 *)
  let rec read_alnum chars acc =
    match chars with
    | c :: cs when is_alnum c -> read_alnum cs (c :: acc)
    | _ -> (List.rev acc, chars)
  in

  match input with
  | c :: cs when is_alpha c ->
    (* 1文字目がアルファベットだった場合のみ、残りの英数字を読み取る *)
    let (alunums, rest) = read_alnum cs [c] in
    let str = String.of_seq (List.to_seq alunums) in

    (* 予約語のリスト *)
    let reserved = ["let"; "rec"; "in"; "if"; "then"; "else"; "true"; "false"; "match "; "with"; "fun"; "->"] in

    if List.mem str reserved
    then None
    else Some (str, rest)
  | _ -> None






(* 出力の型の定義 *)
type 'a parser_result = ('a * input) option

(* 整数、真偽値、名前のパース *)
(* primary_expr: input -> (exp * input) option *)
let rec primary_expr input =
  ((let* n = token int_p in
    return (IntLit n))
    <|>
  ( let* _ = token (keyword_p "true") in
    return (BoolLit true))
    <|>
  ( let* _ = token (keyword_p "false") in
    return (BoolLit false))
    <|>
  ( let* x = token ident_p in
    return (Var x))
    <|>
    paren_expr)
input

(* 括弧のパース *)
(* paren_expr: input -> (exp * input) option *)
and paren_expr input =
  ( let* _ = token (symbol_p "(") in
    let* e = expr in
    let* _ = token (symbol_p ")") in
    return e) 
input

(* 関数適用のパース *)
(* app_expr : input -> (exp * input) option *)
and app_expr input =
  ( let* first = primary_expr in
  
  (* parse_rest : exp -> input -> (exp * input) option*)
  let rec parse_rest current = 
    ( let* argument = primary_expr in
      parse_rest (App(current, argument)))
      <|>
      return current
  in
  parse_rest first)
input

(* 無名関数のパース *)
(* fun_expr : input -> (exp * input) option *)
and fun_expr input =
  ( let* _ = token (keyword_p "fun") in
    let* x = token ident_p in
    let* _ = token (symbol_p "->") in
    let* body = expr in
    return (Fun (x, body)))
input

(* 名前付き関数のパース *)
(* named_fun_expr : input -> (exp * input) option *)
and named_fun_expr input =
  ( let* _ = token (keyword_p "let") in
    let* f = token ident_p in
    let* first_arg = token ident_p in
    
    (* =が来るまで、残りの仮引数を読み取る *)
    let rec parse_args acc =
      ( let* x = token ident_p in
      parse_args (x :: acc))
      <|>
      return (List.rev acc)
    in

    let* args = parse_args [first_arg] in
    let* _ = token (symbol_p "=") in
    let* body = expr in
    let* _ = token (keyword_p "in") in
    let* e2 = expr in

    (* ["x"; "y"; "z"]とbodyから
       Fun ("x", Fun ("y", Fun("z", body)))を作る *)
    let function_exp =
      List.fold_right
        (fun x current_body -> Fun (x, current_body))
        args
        body
    in
    return (Let (f, function_exp, e2)))
input

(* 再帰関数のパース *)
(* let_rec_expr : input -> (exp * input) option *)
and let_rec_expr input =
  ( let* _ = token (keyword_p "let") in
    let* _ = token (keyword_p "rec") in
    let* f = token ident_p in
    let* first_arg = token ident_p in
    
    (* =が来るまで、残りの仮引数を読み取る *)
    let rec parse_args acc =
      ( let* x = token ident_p in
      parse_args (x :: acc))
      <|>
      return (List.rev acc)
    in

    let* remaining_args = parse_args [] in
    let* _ = token (symbol_p "=") in
    let* body = expr in
    let* _ = token (keyword_p "in") in
    let* e2 = expr in

    let function_body =
      List.fold_right
        (fun x current_body -> Fun (x, current_body))
        remaining_args
        body
    in
    return (LetRec (f, first_arg, function_body, e2)))
input

(* 掛け算割り算のパース *)
(* times_div_expr: input -> (exp * input) option *)
and times_div_expr input =
  (let* e1 = app_expr in

   let rec parse_rest current_e =
     ((let* _ = token (symbol_p "*") in
       let* e2 = app_expr in
       let new_e = Times (current_e, e2) in
       parse_rest new_e)
      <|>
      (let* _ = token (symbol_p "/") in
       let* e2 = app_expr in
       let new_e = Div (current_e, e2) in
       parse_rest new_e)
      <|>
      return current_e)
   in

   parse_rest e1)
input

(* 足し算引き算のパース *)
(* plus_minus_expr: input -> (exp * input) option *)
and plus_minus_expr input =
  (let* e1 = times_div_expr in

   let rec parse_rest current_e =
     ((let* _ = token (symbol_p "+") in
       let* e2 = times_div_expr in
       let new_e = Plus (current_e, e2) in
       parse_rest new_e)
      <|>
      (let* _ = token (symbol_p "-") in
       let* e2 = times_div_expr in
       let new_e = Minus (current_e, e2) in
       parse_rest new_e)
      <|>
      return current_e)
   in

   parse_rest e1)
input

(* 比較演算のパース *)
(* comp_expr: input -> (exp * input) option *)
and comp_expr input =
  ((let* e1 = plus_minus_expr in
    let* _ = token (symbol_p "=") in
    let* e2 = plus_minus_expr in
    return (Eq (e1, e2)))
  <|>
  ( let* e1 = plus_minus_expr in
    let* _ = token (symbol_p "<>") in
    let* e2 = plus_minus_expr in
    return (Ne (e1, e2)))
  <|>
  ( let* e1 = plus_minus_expr in
    let* _ = token (symbol_p "<") in
    let* e2 = plus_minus_expr in
    return (Lt (e1, e2)))
  <|>
  ( let* e1 = plus_minus_expr in
    let* _ = token (symbol_p ">") in
    let* e2 = plus_minus_expr in
    return (Greater (e1, e2)))
  <|>
  plus_minus_expr)
input

(* ifのパース *)
(* if_expr: input -> (exp * input) option *)
and if_expr input =
  ( let* _ = token (keyword_p "if") in
    let* e1 = expr in
    let* _ = token (keyword_p "then") in
    let* e2 = expr in
    let* _ = token (keyword_p "else") in
    let* e3 = expr in
    return (If (e1, e2, e3)))
input

(* letのパース *)
(* let_expr : *)
and let_expr input =
  ( let* _ = token (keyword_p "let") in
    let* x = token ident_p in
    let* _ = token (symbol_p "=") in
    let* e1 = expr in
    let* _ = token (keyword_p "in") in
    let* e2 = expr in
    return (Let(x, e1, e2)))
input

and expr input =
  (let_rec_expr
  <|> named_fun_expr
  <|> let_expr
  <|> if_expr
  <|> fun_expr
  <|> comp_expr)
input

(* エントリポイント *)
(* parse: string -> exp *)
let parse s =
  let chars = chars_of_string s in
  (* 式全体のパースを開始する(一番優先度が低いexprから呼ぶ)*)
  match expr chars with
  | Some (ast, rest) ->
    (* パース成功。最後に残った空白を読み飛ばす *)
    let final_rest = skip_spaces rest in
    if final_rest = []
    then ast (* 残っていなければ、完成したastを返す *)
    else failwith "Parse Error: 式の後ろに余分な文字があります"
  | None -> failwith "Parse Error: 構文解析に失敗しました"






(* 環境の作成、更新、環境 *)
let emptyenv () = []
let ext env x v = (x, v) :: env
let rec lookup x env =
  match env with
  | [] -> failwith ("unbound variable:" ^ x)
  | (y, v)::tl -> if x = y 
                  then v
                  else lookup x tl

(* eval3: exp -> (string * value) list -> value *)
let rec eval3 e env = (* envを引数に追加 *)
  (* binopの形式は、interpreter2d.mlを使うことにした *)
  let binop f e1 e2 env = (* binopの中でもeval3を呼ぶのでenvを追加 *)
  match (eval3 e1 env, eval3 e2 env) with
  | (IntVal n1, IntVal n2) -> f n1 n2
  | _ -> failwith "integer value expected"
  in
  match e with
  | Var x -> lookup x env
  | IntLit n -> IntVal n
  | BoolLit n -> BoolVal n
  | Plus(e1, e2) -> binop (fun n1 n2 -> IntVal (n1 + n2)) e1 e2 env
  | Times(e1, e2) -> binop (fun n1 n2 -> IntVal (n1 * n2)) e1 e2 env
  | Minus(e1, e2) -> binop (fun n1 n2 -> IntVal (n1 - n2)) e1 e2 env
  | Div(e1, e2) -> binop (fun n1 n2 -> if n2 = 0 
                                       then failwith "zero division error"
                                       else IntVal (n1 / n2)) e1 e2 env
  | Eq(e1, e2) -> binop (fun n1 n2 -> BoolVal (n1 = n2)) e1 e2 env
  | Ne (e1, e2) -> binop (fun n1 n2 -> BoolVal (n1 <> n2)) e1 e2 env
  | Lt (e1, e2) -> binop (fun n1 n2 -> BoolVal (n1 < n2)) e1 e2 env
  | Greater(e1, e2) -> binop (fun n1 n2 -> BoolVal (n1 > n2)) e1 e2 env
  | If(e1, e2, e3) ->
    begin
      match (eval3 e1 env) with
      | BoolVal true -> eval3 e2 env
      | BoolVal false -> eval3 e3 env
      | _ -> failwith "wrong value"
    end
  | Let(x, e1, e2) ->
    let env1 = ext env x (eval3 e1 env)
    in eval3 e2 env1

let run input =
  let parsed = parse input in
  eval3 parsed (emptyenv ())