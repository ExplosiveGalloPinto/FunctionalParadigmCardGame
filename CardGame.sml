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
				  
(*Escriba una función score, que toma una lista de cartas (en este caso serían las held-cards) y un int (en este caso sería el goal) y determina el score*)
fun score (hand : card list, goal : int)=
    let val sum = sum_cards(hand)
	val preliminar_score =
	    if sum>goal
	    then 3*(sum-goal)
	    else goal-sum
    in
	if all_same_color(hand)
	then preliminar_score div 2
	else prelim_score
    end

(*Creo que por aca va el officiate Nose, me duele el pitochu*)	
fun officiate(card_list : card list, move_list: move list, goal: int)=
    let fun helperFun (current_card_list: card list, currrent_hand: card list, currrent_move_list : move list)=
	    case current_move_list of
		[] => score(current_hand, goal)
	      | Draw::ms => (
		  case current_card_list of
		      []=> score(current_hand, goal)
		    | c::cs  => if sum_cards(c::current_hand)>goal
				then score(c::current_hand, goal)
				else helper(cs, c::current_hand, ms))
	      | (Discard c)::ms => helper(curent_card_list, remove_card(current_hand, c, IllegalMove),ms)
    in
	helper(card_list, [], move_list)
    end
				  
(*Pruebas*)
val test1 = card_color (Clubs,Num 2);
val test2 = card_value (Clubs,Num 2);
val test3 = remove_card([(Hearts, Ace)],(Hearts,Ace),IllegalMove)
