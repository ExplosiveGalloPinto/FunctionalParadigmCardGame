use "project2-PartB.sml";

val number = {
Spades=[
(Spades,Num(10)),
(Spades,Num(2)),
(Spades,Num(3)),
(Spades,Num(4)),
(Spades,Num(5)),
(Spades,Num(6)),
(Spades,Num(7)),
(Spades,Num(8)),
(Spades,Num(9))],
Clubs=[
(Clubs,Num(10)),
(Clubs,Num(2)),
(Clubs,Num(3)),
(Clubs,Num(4)),
(Clubs,Num(5)),
(Clubs,Num(6)),
(Clubs,Num(7)),
(Clubs,Num(8)),
(Clubs,Num(9))],
Hearts=[
(Hearts,Num(10)),
(Hearts,Num(2)),
(Hearts,Num(3)),
(Hearts,Num(4)),
(Hearts,Num(5)),
(Hearts,Num(6)),
(Hearts,Num(7)),
(Hearts,Num(8)),
(Hearts,Num(9))],
Diamonds=[
(Diamonds,Num(10)),
(Diamonds,Num(2)),
(Diamonds,Num(3)),
(Diamonds,Num(4)),
(Diamonds,Num(5)),
(Diamonds,Num(6)),
(Diamonds,Num(7)),
(Diamonds,Num(8)),
(Diamonds,Num(9))]
};

val honors ={
Spades=[
(Spades,Queen),
(Spades,Ace),
(Spades,Jack),
(Spades,King)],

Diamonds=[
(Diamonds,Queen),
(Diamonds,Ace),
(Diamonds,Jack),
(Diamonds,King)],

Hearts=[
(Hearts,Queen),
(Hearts,Ace),
(Hearts,Jack),
(Hearts,King)],

Clubs=[
(Clubs,Queen),
(Clubs,Ace),
(Clubs,Jack),
(Clubs,King)]
};


val deck = [
(Spades,Num(10)),
(Spades,Num(2)),
(Spades,Num(3)),
(Spades,Num(4)),
(Spades,Num(5)),
(Spades,Num(6)),
(Spades,Num(7)),
(Spades,Num(8)),
(Spades,Num(9)),
(Clubs,Num(10)),
(Clubs,Num(2)),
(Clubs,Num(3)),
(Clubs,Num(4)),
(Clubs,Num(5)),
(Clubs,Num(6)),
(Clubs,Num(7)),
(Clubs,Num(8)),
(Clubs,Num(9)),
(Hearts,Num(10)),
(Hearts,Num(2)),
(Hearts,Num(3)),
(Hearts,Num(4)),
(Hearts,Num(5)),
(Hearts,Num(6)),
(Hearts,Num(7)),
(Hearts,Num(8)),
(Hearts,Num(9)),
(Diamonds,Num(10)),
(Diamonds,Num(2)),
(Diamonds,Num(3)),
(Diamonds,Num(4)),
(Diamonds,Num(5)),
(Diamonds,Num(6)),
(Diamonds,Num(7)),
(Diamonds,Num(8)),
(Diamonds,Num(9)),
(Spades,Queen),
(Spades,Ace),
(Spades,Jack),
(Spades,King),
(Diamonds,Queen),
(Diamonds,Ace),
(Diamonds,Jack),
(Diamonds,King),
(Hearts,Queen),
(Hearts,Ace),
(Hearts,Jack),
(Hearts,King),
(Clubs,Queen),
(Clubs,Ace),
(Clubs,Jack),
(Clubs,King)
];

val combination = [(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace),(Clubs,Jack),(Spades,Num(8))];

val mvs = [Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw];

val test1 = if officiate_challenge(#Spades number, mvs, 60) = 3 then 1 else 0;
val test2 = if officiate_challenge(#Clubs honors, mvs, 42) = 0 then 1 else 0;
val test3 = if officiate_challenge(#Hearts honors, mvs, 43) = 1 then 1 else 0;
val test4 = if officiate_challenge((Clubs,Queen)::(#Hearts honors), mvs, 60) = 9 then 1 else 0;
val test5 = if officiate_challenge((#Clubs number)@[(Hearts,King)], mvs, 6) = 6 then 1 else 0;
val test6 = if officiate_challenge([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)], mvs, 42) = 3 then 1 else 0;

val test7 = if score_challenge(combination,10) = 18 then 1 else 0;
val test8 = if score_challenge(deck,10) = 990 then 1 else 0;
val test9 = if score_challenge(combination,50) = 3 then 1 else 0;
val test10 = if score_challenge(deck,50) = 870 then 1 else 0;
val test11 = if score_challenge(deck,300) = 120 then 1 else 0;
val test12 = if score_challenge(deck,350) = 0 then 1 else 0;
		
val test13 = if careful_player([(Clubs,Ace),(Hearts,Num(5)),(Spades,Ace),(Clubs,Jack)],22) = [Draw,Draw,Discard (Hearts,Num 5),Draw] then 1 else 0;
val test14 = if careful_player(#Spades number,60) = [Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw] then 1 else 0;
val test15 = if careful_player(#Clubs number,50) = [Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Discard (Clubs,Num 4),Draw] then 1 else 0;
val test16 = if careful_player(#Clubs honors,42) = [Draw,Draw,Draw,Draw] then 1 else 0;
val test17 = if careful_player((#Spades number)@[(Diamonds,King)],64) = [Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw,Draw] then 1 else 0;
val test18 = if careful_player([],10) = [] then 1 else 0;

val obtained = test1 + test2 + test3 + test4 + test5 + test6 + test7 + test8 + test9 + test10 + test11 + test12 + test13 + test14 + test15 + test16 + test17 + test18;
val final_grade = (obtained * 100) div 18;
