%consultazione del dataset scelto.
consulta(X):- (X=1 -> consult(tennis); X=2 -> consult(golf); consult(xor)).

%stampa gli elementi di una lista
stampa([]):-!.
stampa([H|T]):-writeln(H),stampa(T).

%ultimo elemento di una lista
last([X],X):-!.
last([_,X],X):-!.
last([_|L],X):-last(L,X). 

last1([0,true,t],[0,true,t]):-!.
last1(L,X):-last(L,X).

%definizione di wplus, wminus, wsum 
wplus(C,Tot,T):-findall(X,(esempio1([_,E,Class,X]),valuta(C,E,T),Class=p),L),sum(L,Tot). 
wminus(C,Tot,T):-findall(X,(esempio1([_,E,Class,X]),valuta(C,E,T),Class=n),L),sum(L,Tot). 
wsum(C,Tot,T):-wplus(C,Subt1,T), wminus(C,Subt2,T), Tot is Subt1+Subt2.

%sum funzione sommatoria di elementi di una lista
sum(L,X):-sum(L,0,X).
sum([],X,X):-!.
sum([H|T],X1,X):- X2 is X1+H, sum(T,X2,X).

%funzione di sostituzione ultimo elemento in lista
sostituisci([L,_],Temp,X,L1):-append(Temp,[L],Temp1),append(Temp1,[X],L1),!.
sostituisci([H|T],Temp,X,L1):-append(Temp,[H],Temp1),!,sostituisci(T,Temp1,X,L1).
sostituisci([H|T],X,L1):-sostituisci([H|T],[],X,L1).

%lunghezza lista
lunghezza([],0).
lunghezza([_|Coda],N):-lunghezza(Coda,Ncoda),N is Ncoda + 1.

%cerca la posizione di un elemento in una lista
posizione(A,N):-attributo(X),posizione(X,A,N,1).
posizione([X],X,1,1) :-!.
posizione([X|_],X,P,P1) :- P is P1,!.
posizione([_|T],X,P,A) :- P1 is A+1, posizione(T,X,P,P1).

%accesso all'i-esimo elemento della lista
accesso(Num,L,Elem):- accesso(Num,L,1,Elem).
accesso(Num,[T|_],Cont,T):- Cont=Num,!.
accesso(Num,[_|C],Cont,Elem):- Cont<Num,J is Cont+1,accesso(Num,C,J,Elem).

%confronto di elemento arbitrari degli esempi
confronta(A,N,B,L):-B=t,accesso(N,L,Elem), atom(Elem), Elem=A,!.
confronta(A,N,B,L):-B=f,accesso(N,L,Elem), atom(Elem), Elem\=A,!.
confronta(A,N,B,L):-B=t,accesso(N,L,Elem), number(Elem), Elem<A,!.
confronta(A,N,B,L):-B=f,accesso(N,L,Elem), number(Elem), Elem>=A,!.
confronta(_,B):-B=t,!.
confronta(_,B):-B=f,fail.

%valutazione
valuta(C,E,T):-T=p, valuta(C,E).
valuta(C,E,T):-T=n, not(valuta(C,E)).
valuta([],_):-!.
valuta([H1|Tail],E):- lunghezza(H1,3),due_elem(H1,A,B),confronta(A,B),valuta(Tail,E).
valuta([H1|Tail],E):- lunghezza(H1,4),tre_elem(H1,A,N,B),confronta(A,N,B,E),valuta(Tail,E).

%n-elementi scompone le liste
due_elem([_,A,B],A,B).
tre_elem([_,A,N,B],A,N,B).

%aggiunta dei pesi agli esempi
%all'inizio tutti uguali ad uno, cambieranno durante l'elaborazione
appendi_pesi:-bagof(X,esempio(X),S),appendi1(S).
appendi1([]):-!.
appendi1([H|Tail]):-append(H,[1],H1),asserta(esempio1(H1)),appendi1(Tail).

%inizializzazione dell'albero
%creazione del nodo root, che ha condizione sempre vera e assegna sempre un valore
crea_radice:-
	wplus([[0,true,t]],A1,p), 
	wminus([[0,true,t]],A2,p), 
	A is 1/2 * log(A1/A2), %crea il root, che inserisce il valore A alla lista di scores
	R_new=[true,t,A],
	assertz(radice([0,true,t],A)),
	regole(R),append(R,[R_new],R1),retract(regole(R)),assertz(regole(R1)).

%pre-adattamento
%i pesi vengono pre-adattati prima di procedere con l'algoritmo
preadatta:-bagof(X,esempio1(X),S),nuovi_pesi(S).
nuovi_pesi([]):-!.
nuovi_pesi([H|Tail]):-estrai(H,Class,W_old),
		reale(Class,Y),
		radice(_,A),
		W_new is W_old*exp(-1*A*Y),
		sostituisci(H,W_new,H1),asserta(esempio1(H1)),retract(esempio1(H)),!,
		nuovi_pesi(Tail).

%estrai restituisce peso e classificazione di ogni esempio o dati di ogni esempio
estrai([_,_,A,B],A,B).
estrai([_,E,_,_],E).

%reale assegna un valore numerico alla classificazione
reale(p,1).
reale(n,-1).

%generazione delle ipotesi
%supporta nominale e numerico
genera_cond:-Cond=[],asserta(cond(Cond)),attributo(S),genera_attributi(S).
genera_attributi([]):-!.
genera_attributi([H|Tail]):-genera_dominio(H),genera_attributi(Tail).
genera_dominio(A):-dominio(A,S), (S=continuous -> soglie(A); condizioni(A,S)).
condizioni(_,[]):-!.
condizioni(A,[H|Tail]):-assembla_regola(A,H,C),cond(Cond_old),retract(cond(Cond_old)),append(Cond_old,[C],Cond),asserta(cond(Cond)),condizioni(A,Tail).

%quicksort
ordina(Lista,Lista_ord):-ordina_ld(Lista,Lista_ord-[]),!.
ordina_ld([], L-L):-!.
ordina_ld([H|T], L1-Y2):- partiziona(H,T,Minori,Maggiori),
			ordina_ld(Minori, L1-[H|L2]),
			ordina_ld(Maggiori,L2-Y2).
partiziona(_,[],[],[]):-!.
partiziona([Perno,Val],[[Primo,Val2]|Resto],[[Primo,Val2]|Resto_min],Resto_mag):- Primo<Perno, partiziona([Perno,Val],Resto,Resto_min,Resto_mag).
partiziona([Perno,Val],[[Primo,Val2]|Resto],Resto_min,[[Primo,Val2]|Resto_mag]):- Primo>=Perno, partiziona([Perno,Val],Resto,Resto_min,Resto_mag).

%cerca le soglie numeriche
soglie(A):- posizione(A,N),insieme(N,Valori),ordina(Valori,Valori_ord),cerca_soglie(A,Valori_ord).
insieme(N,Valori):-findall([X,C],esempio1([_,X,C,_]),S),valori(S,N,Valori).
valori(S,N,Valori):-valori(S,N,[],Valori).
valori([],_,Valori,Valori):-!.
valori([[X,C]|T],N,Temp,Valori):-posizione(X,A,N,1),El=[A,C],append(Temp,[El],Temp1),valori(T,N,Temp1,Valori).
cerca_soglie(_,[_|[]]):-!.
cerca_soglie(A,[[N1,C1],[N2,C2]|T]):-(C1=C2 -> cerca_soglie(A,[[N2,C2]|T]); soglia(A,N1,N2)).
soglia(A,N1,N2):-media(N1,N2,Soglia), assembla_regola(A,Soglia,C),cond(Cond_old),retract(cond(Cond_old)),append(Cond_old,[C],Cond),asserta(cond(Cond)).

%media di due valori
media(X,Y,Z):-Z is (X+Y)/2.

%crea la regola da testare per l'albero
assembla_regola(A,V,Regola):-posizione(A,N),Regola=[-1,V,N,t].

%insieme delle precondizioni
genera_pre:-P=[[0,true,t]],asserta(precond(P)).

%funzione zeta da minizzare per lo splitting
%dettagli di implementazione in documentazione
%negato costruisce la condizione al negativo
negato([N,_,_],B):-B=[N,true,f],!.
negato([N,A,V,_],B):-B=[N,A,V,f],!.

%concatenazione di precondizione-condizione
concatena(P,C,A):-flatten(P,P),A=[P,C],!.
concatena(P,C,A):-append(P,[C],A).

%funzione zeta
zeta(C1,C2,Val):-negato(C2,NC2),
	concatena(C1,C2,A),
	wplus(A,Primo,p),
	wminus(A,Secondo,p),
	concatena(C1,NC2,B),
	wplus(B,Terzo,p),
	wminus(B,Quarto,p),
	wsum(C1,Quinto,n),
	Val is 2*(sqrt(Primo*Secondo) + sqrt(Terzo*Quarto)) + Quinto.

%minimo di una lista
min([H|T],Min):-min(T,H,Min).
min([],Min,Min).
min([H|T],Min,Min1):-H<Min, min(T,H,Min1),!.
min([H|T],Min,Min1):-H>=Min, min(T,Min,Min1).

%inizializzazione del set di regole (vuoto)
crea_regole:-Regole=[],assertz(regole(Regole)).

%minimo della funzione 
minimizza(P1,C1):-precond(P),minimizza(P,[],[],[],P1,C1).
minimizza([],P1,C1,_,P1,C1):-!.
minimizza([H|T],Temp1,Temp2,Temp3,P1,C1):-cond(C), minimo(H,C,[],[],[],P2,C2,Val),append1([Val],[Temp3],Lista),min(Lista,Minimo),
		(Minimo=Val -> minimizza(T,P2,C2,Val,P1,C1); minimizza(T,Temp1,Temp2,Temp3,P1,C1)).
minimo(_,[],P1,C1,Ris,P1,C1,Ris):-!.
minimo(P,[H|T],TP1,TC1,Temp,P1,C1,Ris):-zeta(P,H,Val), 
				%write(Val), write('   -   '), writeln(Temp),
				append1([Val],[Temp], A), min(A,Minimo), 
				(Minimo=Val -> minimo(P,T,P,H,Val,P1,C1,Ris); minimo(P,T,TP1,TC1,Temp,P1,C1,Ris)).

append1(A,[[]],M):-append(A,[],M),!.
append1(A,B,M):-append(A,B,M).

%aggiunta della regola
aggiungi_regola(Num,P,C_num,A,B):-minimizza(P,C),
	concatena(P,C,Primo),
	wplus(Primo,X1,p),
	wminus(Primo,X2,p),
	A is 1/2 * log((X1+1)/(X2+1)),
	negato(C,NC),
	concatena(P,NC,Secondo),
	wplus(Secondo,Y1,p),
	wminus(Secondo,Y2,p),
	B is 1/2 * log((Y1+1)/(Y2+1)),
	concatena(P,C,Regola),
	R_new=[Regola,A,B],
	last1(P,X),
	num_cond(C,Num,C_num),
	assertz(figlio(X,C_num,A,B)),
	regole(R),append(R,[R_new],R1),retract(regole(R)),assertz(regole(R1)).

num_cond([_,V,N,T],Num,C_num):-C_num=[Num,V,N,T].

%aggiornamento delle precondizioni
aggiunta_pre(P,C):-precond(P1),
		concatena(P,C,A),
		append(P1,[A],P2),
		negato(C,NC),
		concatena(P,NC,A1),
		append(P2,[A1],P_new),
		retract(precond(P1)),asserta(precond(P_new)).

%aggiornamento dei pesi degli esempi 
aggiornamento_pesi(P,C,A,B):-bagof(X,esempio1(X),S),aggiorna_pesi(P,C,A,B,S).
aggiorna_pesi(_,_,_,_,[]):-!.
aggiorna_pesi(P,C,A,B,[H|Tail]):-estrai(H,Class,W_old),
		reale(Class,Y),
		concatena(P,C,Regola),
		estrai(H,E),
		(valuta(Regola,E) -> W_new is W_old*exp(-1*A*Y); W_new is W_old*exp(-1*B*Y)),
		sostituisci(H,W_new,H1),asserta(esempio1(H1)),retract(esempio1(H)),!,
		aggiorna_pesi(P,C,A,B,Tail).

%prime fasi dell'apprendimento
inizio_apprendimento:-crea_regole,appendi_pesi,crea_radice,preadatta,genera_pre,genera_cond.

%processo da iterare per aggiungere le regole
boost(X):-aggiungi_regola(X,P,C,A,B), aggiunta_pre(P,C),aggiornamento_pesi(P,C,A,B).

%iterazione
iterazione(Stop):-iterazione(1,Stop).
iterazione(X,Stop):-X>Stop,!,writeln('Completato.').
iterazione(X,Stop):-X=<Stop,write('Iterazione n.:  '),writeln(X),boost(X) ,X1 is X+1, iterazione(X1,Stop).

%avvio dell'apprendimento, da inserire il numero di iterazioni
apprendi(X):-inizio_apprendimento, iterazione(X).

%classificazione del nuovo esempio
classifica(X,V):-radice(Radice,A),percorri([Radice],X,S1),append(S1,[A],S),sum(S,T), V is sign(T).

%percorso dell'albero, da iterare
percorri(Partenza,X,S):-percorri(Partenza,X,[],S),!.
percorri([H|T],X,Temp,S):-findall([C,A,B],figlio(H,C,A,B),Cp),analizza(X,Cp,Co,Stemp),
			append1(Temp,Stemp,Temp1),append1(T,Co,Tall),
			percorri(Tall,X,Temp1,S).
percorri([],_,S,S):-!.
analizza(X,Cond,Co,Score):-analizza(X,Cond,[],Co,[],Score).
analizza(X,[[C,A,B]|T],TCo,Co,Temp,Score):-(valuta([C],X) -> append1(Temp,[A],Temp1),append1(TCo,[C],TCo1); 
					append1(Temp,[B],Temp1),negato(C,NC),append1(TCo,[NC],TCo1)),
					analizza(X,T,TCo1,Co,Temp1,Score).
analizza(_,[],Co,Co,Score,Score):-!.

%interazione con l'utente
avvio:- writeln('Alternating Decision Tree'),
	write('Scegliere il dataset da utilizzare, 1 per il tennis, 2 per il golf, \n3 per lo xor: '), read(X), consulta(X),
	write('Scegliere il numero di iterazioni per il processo di apprendimento: '), read(Y), apprendi(Y),
	interroga.

output(V):-(V=1 -> writeln('Positivo.'); writeln('Negativo.')), ancora.

ancora:-write('Interrogare ancora? (s/n): '),read(R), (R=s -> interroga; albero).
interroga:-write('Per interrogare l\'albero scrivere un\'istanza: '), read(Z), classifica(Z,V), output(V).

albero:-write('Visualizzare i nodi dell\'albero? (s/n): '), read(X), (X=s -> visualizza; esci).

visualizza:-radice(Uno,Due), bagof([X,Y,A,B],figlio(X,Y,A,B),S1), append([[Uno,Due]],S1,S), stampa(S), ancora.
esci:-writeln('Terminato.').
