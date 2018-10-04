(*Functional Card Game B*)
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
      | card::cs => if card = selec_card
		    then cs
		    else card::remove_card(cs,selec_card, excep)
			  
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

(*Escriba una función sum_cards, la cual toma una lista de cartas y retorna la suma de sus valores. (Nota: para este problema es requerido que la solución utilice la técnica tail recursion)*)
fun sum_cards (hand) =
    let
	fun adding_values (list, acc) =
	    case list of
		[] => acc
	      | card::cs => adding_values(cs, (card_value card) + acc)
    in
	adding_values(hand, 0)
    end
				  
(*Escriba una funcion score, que toma una lista de cartas (en este caso serian las held-cards) y un int (en este caso seria el goal) y determina el score*)
fun score (hand, goal)=
    let
	val sum = sum_cards(hand)
	val preliminar_score =
	    if sum>goal
	    then 3*(sum-goal)
	    else goal-sum
    in
	case hand of
	    [] => preliminar_score div 2
	   |_ => if all_same_color hand
		 then preliminar_score div 2
		 else preliminar_score
    end

(* Escriba una funcion officiate. Esta funcion toma la lista de cartas, una lista de movimientos, y un int (que seria el goal); y retorna el score obtenido al final del juego*)	
fun officiate (card_list, move_list, goal) =
    let fun helperFun (current_card_list, current_hand, current_move_list)=
	    case current_move_list of
		[] => score(current_hand, goal)
	      | Draw::ms => (
		  case current_card_list of
		      []=> score(current_hand, goal)
		    | c::cs  => if sum_cards(c::current_hand)>goal
				then score(c::current_hand, goal)
				else helperFun(cs, c::current_hand, ms))
	      | (Discard c)::ms => helperFun(current_card_list, remove_card(current_hand, c, IllegalMove),ms)
    in
	helperFun(card_list, [], move_list)
    end





(*Parte B del proyecto*)

fun score_challenge (hand, goal)=
    let
	fun Ace_counter(hand)=
	    case hand of
		[] => 0
	       |card::rest_of_hand  =>
		if card_value card = 11
		then 1+Ace_counter rest_of_hand
		else Ace_counter rest_of_hand
				 
	fun Ace_tester(hand, ace_qty, score)=
	    case hand of 
		[] => score 
	      | card::rest_of_hand =>
		if card_value card = 11
		then
		    if ace_qty > 0
		    then Ace_tester (rest_of_hand,ace_qty-1,score+1)
		    else Ace_tester (rest_of_hand,ace_qty-1,score+11)
		else Ace_tester (rest_of_hand,ace_qty,score+card_value card)
				
 	fun lowest_score(best_score, hand, ace_qty, ace_as_one )=
	    let
		fun score (hand, ace_qty)=
		    let
			val sum = Ace_tester (hand, ace_qty, 0)
			val preliminar_score =
			    if sum>goal
			    then 3*(sum-goal)
			    else goal-sum
		    in
			case hand of
			    [] => preliminar_score div 2
			 |_ => if all_same_color hand
			       then preliminar_score div 2
			       else preliminar_score
		    end
	    in
		if ace_qty = 0
		then best_score
		else
		    if score (hand, ace_qty)<best_score 
		    then lowest_score(score (hand, ace_qty),hand,ace_qty-1,ace_as_one+1)
		    else lowest_score(best_score,hand,ace_qty-1,ace_as_one)
	    end
    in
	lowest_score(goal, hand, Ace_counter hand, 0)
    end
	
fun officiate_challenge (card_list, move_list, goal) =
    let fun helperFun (current_card_list, current_hand, current_move_list)=
	    case current_move_list of
		[] => score_challenge(current_hand, goal)
	      | Draw::ms => (
		  case current_card_list of
		      []=> score_challenge(current_hand, goal)
		    | c::cs  => helperFun(cs, c::current_hand, ms))
	      | (Discard c)::ms => helperFun(current_card_list, remove_card(current_hand, c, IllegalMove),ms)
    in
	helperFun(card_list, [], move_list)
    end

(*Testing careful_player*)
fun careful_player(card_list,goal) = 
    let 
        fun discard_seletion(held_card,check_list,card) =
            case check_list of
             [] => (false, card) 
             |x::xs => if score(card::remove_card(held_card, x, IllegalMove), goal) = 0
                       then (true,x)
                       else discard_seletion(held_card,xs,card)
   
        fun move_selection(held_list,card_list,move_list) =
            if score(held_list,goal) = 0
            then move_list
            else  
                 case card_list of
                      [] => move_list
                      |x::xs => case held_list of 
                                     [] => move_selection(x::held_list,xs,move_list@[Draw])
                                    |y::ys => case  discard_seletion(held_list,held_list,x) of
                                                    (true,x1) => move_list@[(Discard x1),Draw]
                                                  | (false,x2) => if ((goal - sum_cards(held_list) > 10) orelse
                                                             (sum_cards(x::held_list) <= goal))
                                                         then 
                                                               move_selection(x::held_list,xs,move_list@[Draw])
                                                         else
                                                               move_selection(x::held_list,xs,move_list@[Discard y])
    in                                                             
        move_selection([],card_list,[])
    end    
				  
val test2 = officiate_challenge ([(Spades, Ace),(Clubs, Ace)],[Draw, Draw], 15);
val test3 = careful_player([(Clubs,Ace),(Hearts,Num(5)),(Spades,Ace),(Clubs,Jack)],22);
