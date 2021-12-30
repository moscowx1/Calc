open System

type ParserLabel = string
type ParserError = string

type Position = {
  line : int
  column : int
}


type Parser<'T> = {
    func: (string -> Result<'T, ParserLabel * ParserError>)
    label: ParserLabel
}

let printResult result = 
    match result with
    | Ok (value, _) -> printfn "%A" value
    | Error (label, error) -> 
        printfn "Error parsing %s\n%s" label error

let satisfy predicate label = 
  let parseFn input = 
    if String.IsNullOrEmpty(input) then
      Error (label, "No more input")
    else 
      let first = input.[0]
      if predicate first then
        let remain = input.[1..]
        Ok (first, remain)
      else 
        let err = sprintf "Unexpected '%c'" first
        Error (label, err)

  {func = parseFn; label = label}

let pchar char = 
  let predicate ch = (ch = char)
  let label = sprintf "%c" char
  satisfy predicate label


let run (parser:Parser<_>) input =
    parser.func input

let bindP f p =
  let label = "unknown"
  let parserFn input =
    let res = run p input
    match res with
    | Error (label, err) -> Error (label, err) 
    | Ok (value,remain) ->
      let p2 = f value
      run p2 remain
  {func = parserFn; label = label}

let setLabel parser newLabel = 
  let parserFn input = 
    let res = parser.func input
    match res with
    | Ok s -> Ok s
    | Error (_, err) -> Error (newLabel, err)
  {func=parserFn; label=newLabel}

let getLabel parser = 
  parser.label

let ( <?> ) = setLabel

let ( >>= ) p f = bindP f p

let returnP x =
  let parserFn input =
    Ok (x,input)
  Parser innerFn

let mapP f =
  bindP (f >> returnP)

let ( <!> ) = mapP

let ( |>> ) x f = mapP f x

let applyP fP xP =
  fP >>= (fun f ->
  xP >>= (fun x ->
    returnP (f x) ))

let ( <*> ) = applyP

let lift2 f xP yP =
  returnP f <*> xP <*> yP

let andThen p1 p2 =
  let label = sprintf "%s then %s" (getLabel p1) (getLabel p2)
  p1 >>= (fun res ->
  p2 >>= (fun res' ->
    returnP (res,res') ))
  <?> label

let ( .>>. ) = andThen

let orElse p1 p2 =
  let label = sprintf "%s else %s" (getLabel p1) (getLabel p2)

  let parserFn input =
    let res = run p1 input

    match res with
    | Ok result -> result
    | Error err ->
      let res' = run p2 input

      res'

  {func = parserFn; label = label}

let ( <|> ) = orElse

let choice listOfParsers =
  List.reduce ( <|> ) listOfParsers

let anyOf listOfChars =
  listOfChars
  |> List.map pchar
  |> choice

let rec sequence parserList =
  let cons head tail = head::tail

  let consP = lift2 cons

  match parserList with
  | [] -> returnP []
  | head::tail -> consP head (sequence tail)

let rec parseZeroOrMore parser input =
  let res = run parser input
  match res with
  | Error _ -> ([],input)
  | Ok (value,remain) ->
    let (value',remain') = parseZeroOrMore parser remain
    let values = value::value'
    (values,remain')

let many parser =
  let innerFn input =
    Ok (parseZeroOrMore parser input)

  Parser innerFn

let many1 p =
  p      >>= (fun head ->
  many p >>= (fun tail ->
    returnP (head::tail) ))

let opt p =
  let some = p |>> Some
  let none = returnP None
  some <|> none

let (.>>) p1 p2 =
  p1 .>>. p2
  |> mapP fst

let (>>.) p1 p2 =
  p1 .>>. p2
  |> mapP snd

let between p1 p2 p3 =
  p1 >>. p2 .>> p3

let sepBy1 p sep =
  let sepThenP = sep >>. p
  p .>>. many sepThenP
  |>> fun (p,pList) -> p::pList

let sepBy p sep =
  sepBy1 p sep <|> returnP []