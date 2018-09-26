(*Functional Card Game*)
(*Preset Code -Dont Move-*)
(*We can't:
- Use the # character to index items
- Write down any explicit types
- Do not use isSome, valOf, null, hd, tl*)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank
datatype color = Red | Black
datatype move = Discard of card | Draw
				      
exception IllegalMove
	      
fun draw_card (card_list, held_cards) =
    case card_list of
	[] => raise IllegalMove
      | card::cs => (cs,(card::held_cards))
    handle IllegalMove => (card_list, held_cards)
	

(* Escriba una funcion card_color, la cual toma una carta y retorna su color *)
fun card_color card = 
    case card of
	(Clubs, _) => Black
      | (Spades, _) => Black
      | (Diamonds,_) => Red
      | (Hearts,_) => Red
(*Escriba una funcion card_value, la cual toma una carta y retorna su valor*)
fun card_value card =
    case card of
	(_,Ace) => 11
      | (_, Num n) => n
      | (_,_) => 10

(*Escriba una funcion remove_card, la cual toma una lista de cartas, una carta 
y una excepcion. La funcion retorna la lista sin la carta*)
fun remove_card (xs,selec_card, excep) =
    case xs of
	[] => raise excep
      | card::cs => case card = selec_card of
			true => cs
		      | false => card::discard_card(cs,selec_card)
    handle excep => xs
			  
(*Escriba una funcion all_same_color, la cual toma una lista de cartas y retorna true 
si todas las cartas que estan dentro de la lista, son del mismo color. 
Nota: usen pattern maching anidado como lo vimos en clase. *)
fun all_same_color xs =
    let
	fun recursiveCheck (ls, color) = 
	    case ls of
		[] => true
	      | hd::tl => case (card_color hd) = color of
			      true => recursiveCheck(tl,color)	     
			    | false => false
    in
	case xs of
	    [] => false
	  | hd::tl => recursiveCheck(xs, card_color hd)
    end 
						      


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
val test3 = remove_card([(Hearts, Num 3)],(Hearts,Ace),IllegalMove);
val test4 = all_same_color [(Hearts, Ace), (Hearts, Ace)];
(*
val test5 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)];
val test6 = score ([(Hearts, Num 2),(Clubs, Num 4)],10);
val test7 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15);
val test8 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],[Draw,Draw,Draw,Draw,Draw],42); *)
