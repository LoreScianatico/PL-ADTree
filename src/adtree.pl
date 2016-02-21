%Reads the chosen dataset
consult_ds(X):- (X=1 -> consult(tennis); X=2 -> consult(golf); consult(xor)).

%prints list elements
print([]):-!.
print([H|T]):-writeln(H),print(T).

%retrieve last element of a list
last([X],X):-!.
last([_,X],X):-!.
last([_|L],X):-last(L,X).

last1([0,true,t],[0,true,t]):-!.
last1(L,X):-last(L,X).

%defining wplus, wminus, wsum
wplus(C,Tot,T):-findall(X,(sample_1([_,E,Class,X]),evaluate(C,E,T),Class=p),L),sum(L,Tot).
wminus(C,Tot,T):-findall(X,(sample_1([_,E,Class,X]),evaluate(C,E,T),Class=n),L),sum(L,Tot).
wsum(C,Tot,T):-wplus(C,Subt1,T), wminus(C,Subt2,T), Tot is Subt1+Subt2.

%sums list elements
sum(L,X):-sum(L,0,X).
sum([],X,X):-!.
sum([H|T],X1,X):- X2 is X1+H, sum(T,X2,X).

%replace last element in list
replace([L,_],Temp,X,L1):-append(Temp,[L],Temp1),append(Temp1,[X],L1),!.
replace([H|T],Temp,X,L1):-append(Temp,[H],Temp1),!,replace(T,Temp1,X,L1).
replace([H|T],X,L1):-replace([H|T],[],X,L1).

%list length
length([],0).
length([_|Coda],N):-length(Coda,Ncoda),N is Ncoda + 1.

%cerca la position di un elemento in una list
position(A,N):-attribute(X),position(X,A,N,1).
position([X],X,1,1) :-!.
position([X|_],X,P,P1) :- P is P1,!.
position([_|T],X,P,A) :- P1 is A+1, position(T,X,P,P1).

%take i-th element from list
take(Num,L,Elem):- take(Num,L,1,Elem).
take(Num,[T|_],Cont,T):- Cont=Num,!.
take(Num,[_|C],Cont,Elem):- Cont<Num,J is Cont+1,take(Num,C,J,Elem).

%compare element in samples
compare(A,N,B,L):-B=t,take(N,L,Elem), atom(Elem), Elem=A,!.
compare(A,N,B,L):-B=f,take(N,L,Elem), atom(Elem), Elem\=A,!.
compare(A,N,B,L):-B=t,take(N,L,Elem), number(Elem), Elem<A,!.
compare(A,N,B,L):-B=f,take(N,L,Elem), number(Elem), Elem>=A,!.
compare(_,B):-B=t,!.
compare(_,B):-B=f,fail.

%evaluation
evaluate(C,E,T):-T=p, evaluate(C,E).
evaluate(C,E,T):-T=n, not(evaluate(C,E)).
evaluate([],_):-!.
evaluate([H1|Tail],E):- length(H1,3),two_elements(H1,A,B),compare(A,B),evaluate(Tail,E).
evaluate([H1|Tail],E):- length(H1,4),three_elements(H1,A,N,B),compare(A,N,B,E),evaluate(Tail,E).

%n-elements chops lists
two_elements([_,A,B],A,B).
three_elements([_,A,N,B],A,N,B).

%adding weigths to samples
%1 at first iteration, then adjusted
append_weigths:-bagof(X,sample(X),S),append_ones(S).
append_ones([]):-!.
append_ones([H|Tail]):-append(H,[1],H1),asserta(sample_1(H1)),append_ones(Tail).

%tree init
%creates root node
create_root:-
	wplus([[0,true,t]],A1,p),
	wminus([[0,true,t]],A2,p),
	A is 1/2 * log(A1/A2),
	R_new=[true,t,A],
	assertz(root([0,true,t],A)),
	rules(R),append(R,[R_new],R1),retract(rules(R)),assertz(rules(R1)).

%pre-adapt
%weigths are pre-adapted
preadapt:-bagof(X,sample_1(X),S),new_weights(S).
new_weights([]):-!.
new_weights([H|Tail]):-extract(H,Class,W_old),
		real(Class,Y),
		root(_,A),
		W_new is W_old*exp(-1*A*Y),
		replace(H,W_new,H1),asserta(sample_1(H1)),retract(sample_1(H)),!,
		new_weights(Tail).

%extract weight and other data from samples
extract([_,_,A,B],A,B).
extract([_,E,_,_],E).

%real assign a number to object
real(p,1).
real(n,-1).

%hipotheys generation
%supports nominal and numeric
generate_condition:-Cond=[],asserta(cond(Cond)),attribute(S),generate_attributes(S).
generate_attributes([]):-!.
generate_attributes([H|Tail]):-generate_dominium(H),generate_attributes(Tail).
generate_dominium(A):-dominio(A,S), (S=continuous -> thresholds(A); conditions(A,S)).
conditions(_,[]):-!.
conditions(A,[H|Tail]):-assembly_rule(A,H,C),cond(Cond_old),retract(cond(Cond_old)),append(Cond_old,[C],Cond),asserta(cond(Cond)),conditions(A,Tail).

%quicksort
sort(list,sorted_list):-sort_ld(list,sorted_list-[]),!.
sort_ld([], L-L):-!.
sort_ld([H|T], L1-Y2):- partition(H,T,Minus,Major),
			sort_ld(Minus, L1-[H|L2]),
			sort_ld(Major,L2-Y2).
partition(_,[],[],[]):-!.
partition([Center,Val],[[First,Val2]|Others],[[First,Val2]|Others_min],Others_maj):- First<Center, partition([Center,Val],Others,Others_min,Others_maj).
partition([Center,Val],[[First,Val2]|Others],Others_min,[[First,Val2]|Others_maj]):- First>=Center, partition([Center,Val],Others,Others_min,Others_maj).

%find numerical thresholds
thresholds(A):- position(A,N),set(N,Values),sort(Values,Sorted_values),find_thresholds(A,Sorted_values).
set(N,Values):-findall([X,C],sample_1([_,X,C,_]),S),values(S,N,Values).
values(S,N,Values):-values(S,N,[],Values).
values([],_,Values,Values):-!.
values([[X,C]|T],N,Temp,Values):-position(X,A,N,1),El=[A,C],append(Temp,[El],Temp1),values(T,N,Temp1,Values).
find_thresholds(_,[_|[]]):-!.
find_thresholds(A,[[N1,C1],[N2,C2]|T]):-(C1=C2 -> find_thresholds(A,[[N2,C2]|T]); threshold(A,N1,N2)).
threshold(A,N1,N2):-mean(N1,N2,Soglia), assembly_rule(A,Soglia,C),cond(Cond_old),retract(cond(Cond_old)),append(Cond_old,[C],Cond),asserta(cond(Cond)).

%mean values
mean(X,Y,Z):-Z is (X+Y)/2.

%assembly a rule
assembly_rule(A,V,Rule):-position(A,N),Rule=[-1,V,N,t].

%pre-conditions
generate_preconditions:-P=[[0,true,t]],asserta(precond(P)).

%Zeta function to minimize
%negate build negative rule
negate([N,_,_],B):-B=[N,true,f],!.
negate([N,A,V,_],B):-B=[N,A,V,f],!.

%concatenation
concatenate(P,C,A):-flatten(P,P),A=[P,C],!.
concatenate(P,C,A):-append(P,[C],A).

%funzione zeta
zeta(C1,C2,Val):-negate(C2,NC2),
	concatenate(C1,C2,A),
	wplus(A,First,p),
	wminus(A,Second,p),
	concatenate(C1,NC2,B),
	wplus(B,Third,p),
	wminus(B,Fourth,p),
	wsum(C1,Fifth,n),
	Val is 2*(sqrt(First*Second) + sqrt(Third*Fourth)) + Fifth.

%list minimum
min([H|T],Min):-min(T,H,Min).
min([],Min,Min).
min([H|T],Min,Min1):-H<Min, min(T,H,Min1),!.
min([H|T],Min,Min1):-H>=Min, min(T,Min,Min1).

%rule set init (void)
create_rules:-rules=[],assertz(rules(rules)).

%minimize function
minimize(P1,C1):-precond(P),minimize(P,[],[],[],P1,C1).
minimize([],P1,C1,_,P1,C1):-!.
minimize([H|T],Temp1,Temp2,Temp3,P1,C1):-cond(C), minimum(H,C,[],[],[],P2,C2,Val),append1([Val],[Temp3],list),min(list,Minimum),
		(Minimum=Val -> minimize(T,P2,C2,Val,P1,C1); minimize(T,Temp1,Temp2,Temp3,P1,C1)).
minimum(_,[],P1,C1,Ris,P1,C1,Ris):-!.
minimum(P,[H|T],TP1,TC1,Temp,P1,C1,Ris):-zeta(P,H,Val),
				%write(Val), write('   -   '), writeln(Temp),
				append1([Val],[Temp], A), min(A,Minimum),
				(Minimum=Val -> minimum(P,T,P,H,Val,P1,C1,Ris); minimum(P,T,TP1,TC1,Temp,P1,C1,Ris)).

append1(A,[[]],M):-append(A,[],M),!.
append1(A,B,M):-append(A,B,M).

%add a rule
add_rule(Num,P,C_num,A,B):-minimize(P,C),
	concatenate(P,C,First),
	wplus(First,X1,p),
	wminus(First,X2,p),
	A is 1/2 * log((X1+1)/(X2+1)),
	negate(C,NC),
	concatenate(P,NC,Second),
	wplus(Second,Y1,p),
	wminus(Second,Y2,p),
	B is 1/2 * log((Y1+1)/(Y2+1)),
	concatenate(P,C,Rule),
	R_new=[Rule,A,B],
	last1(P,X),
	num_cond(C,Num,C_num),
	assertz(son(X,C_num,A,B)),
	rules(R),append(R,[R_new],R1),retract(rules(R)),assertz(rules(R1)).

num_cond([_,V,N,T],Num,C_num):-C_num=[Num,V,N,T].

%add preconditions
add_preconditions(P,C):-precond(P1),
		concatenate(P,C,A),
		append(P1,[A],P2),
		negate(C,NC),
		concatenate(P,NC,A1),
		append(P2,[A1],P_new),
		retract(precond(P1)),asserta(precond(P_new)).

%update weigths
update_all_weigths(P,C,A,B):-bagof(X,sample_1(X),S),update_weigths(P,C,A,B,S).
update_weigths(_,_,_,_,[]):-!.
update_weigths(P,C,A,B,[H|Tail]):-extract(H,Class,W_old),
		real(Class,Y),
		concatenate(P,C,Rule),
		extract(H,E),
		(evaluate(Rule,E) -> W_new is W_old*exp(-1*A*Y); W_new is W_old*exp(-1*B*Y)),
		replace(H,W_new,H1),asserta(sample_1(H1)),retract(sample_1(H)),!,
		update_weigths(P,C,A,B,Tail).

%first step for learning
start_learning:-create_rules,append_weigths,create_root,preadapt,generate_preconditions,generate_condition.

%boost process
boost(X):-add_rule(X,P,C,A,B), add_preconditions(P,C),update_all_weigths(P,C,A,B).

%iteration
iterate(Stop):-iterate(1,Stop).
iterate(X,Stop):-X>Stop,!,writeln('Completed..').
iterate(X,Stop):-X=<Stop,write('Iteration step n.:  '),writeln(X),boost(X) ,X1 is X+1, iterate(X1,Stop).

%start learning
learn(X):-start_learning, iterate(X).

%classification of new sample
classify(X,V):-root(root,A),visit([root],X,S1),append(S1,[A],S),sum(S,T), V is sign(T).

%tree visiting
visit(Starting_node,X,S):-visit(Starting_node,X,[],S),!.
visit([H|T],X,Temp,S):-findall([C,A,B],son(H,C,A,B),Cp),analize(X,Cp,Co,Stemp),
			append1(Temp,Stemp,Temp1),append1(T,Co,Tall),
			visit(Tall,X,Temp1,S).
visit([],_,S,S):-!.
analize(X,Cond,Co,Score):-analize(X,Cond,[],Co,[],Score).
analize(X,[[C,A,B]|T],TCo,Co,Temp,Score):-(evaluate([C],X) -> append1(Temp,[A],Temp1),append1(TCo,[C],TCo1);
					append1(Temp,[B],Temp1),negate(C,NC),append1(TCo,[NC],TCo1)),
					analize(X,T,TCo1,Co,Temp1,Score).
analize(_,[],Co,Co,Score,Score):-!.

%user interaction
start:- writeln('Alternating Decision Tree'),
	write('Choose a dataset, 1 for tennis data, 2 for golf data, \n3 for xor data: '), read(X), consult_ds(X),
	write('Choose a number for iteration steps in learning: '), read(Y), learn(Y),
	query.

output(V):-(V=1 -> writeln('Positive.'); writeln('Negative.')), again.

again:-write('Query again? (y/n): '),read(R), (R=y -> query; tree).
query:-write('Write an instance to query tree: '), read(Z), classify(Z,V), output(V).

tree:-write('Show tree nodes? (y/n): '), read(X), (X=y -> show_tree; exit).

show_tree:-root(One,Two), bagof([X,Y,A,B],son(X,Y,A,B),S1), append([[One,Two]],S1,S), print(S), again.
exit:-writeln('Terminating.').
