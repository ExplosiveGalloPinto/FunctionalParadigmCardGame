(*Functional Card Game*)
(*Preset Code -Dont Move-*)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank
datatype color = Red | Black
datatype move = Discard of card | Draw
exception IllegalMove

(* Escriba una función card_color, la cual toma una carta y retorna su color *)
fun card_color card = 
  case card of 
    (Clubs, _)   => Black 
  | (Spades, _)  => Black 
  | (Diamonds,_) => Red
  | (Hearts,_)   => Red
