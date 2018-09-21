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
	(Clubs, _) => Black
      | (Spades, _) => Black
      | (Diamonds,_) => Red
      | (Hearts,_) => Red
(*Escriba una función card_value, la cual toma una carta y retorna su valor*)
fun card_value card =
    case card of
	(_,Ace) => 11
      | (_, Num n) => n
      | (_,_) => 10

(*Escriba una función remove_card, la cuál toma una lista de cartas, una carta 
y una excepción. La funcion retorna la lista sin la carta*)
fun remove_card ([],selec_card, excep) = (*Así tira warning*)(*selec_card:card*)
    raise excep 
  | remove_card (card::cs, selec_card, excep : exn) =
    case card = selec_card of
	true => cs
      | false => card::remove_card(cs,selec_card, excep) 


(*Pruebas*)
val test1 = card_color (Clubs,Num 2);
val test2 = card_value (Clubs,Num 2);
val test3 = remove_card([(Hearts, Ace)],(Hearts,Ace),IllegalMove)
