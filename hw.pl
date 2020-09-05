:- module(hw, [cost/2, sort_units/2, buyable/4]).
:- [kb].



%Helpers for cost

isim(active_unit(X,_),X).
star(active_unit(_,Y),Y).
%%%%%%


%Cost predicate takes ([active_unit(Name, Star)],Cost) arguments and calculates total cost of active_unit list.
%Cost of one active_unit is equal to 3**(Star-1) x Cost of that unit.  

cost([],0).
cost([X | H],Cost) :- isim(X,Name),star(X,Yildiz),unit(Name,Maliyet,_),cost(H,Rcost),Cost is Rcost + (3**(Yildiz-1))*Maliyet.  




%%%%%%%% Helpers for Sort_units

findcost(X,Cost):- unit(X,Cost,_).


mysort([],Acc,Acc).
mysort([H|T],Acc,Sorted):-insert(H,Acc,NAcc),mysort(T,NAcc,Sorted).



insert(X,[Y|Accrest],[Y|Sortrest]):-findcost(X,Costx),findcost(Y,Costy),Costx=<Costy,insert(X,Accrest,Sortrest).
insert(X,[Y|Accrest],[X,Y|Accrest]):-findcost(X,Costx),findcost(Y,Costy),Costx>Costy.
insert(X,[],[X]).   

%%%%%%%%%%%



%Sort_units predicate sorts the list of units based on their costs in a descending order.

sort_units(List,Result) :- mysort(List,[],Result).





%%%%%%Helpers for buyable predicate.
totalcost([],0).
totalcost([X | H], Cost) :- unit(X,Maliyet,_),totalcost(H,Rcost),Cost is Rcost + Maliyet. 


combinations([A],_,[A]).
combinations([_],N,[]) :-N > 0.

combinations([_|T],N,T2) :-combinations(T,N,T2).
combinations([H|T],N,[H|T2]) :-N1 is N+1,combinations(T,N1,T2).
%%%%%%%%



%%%% First argument is a list of units that available to buy, second argument is total money, third argument is buyable units,
%%%% and fourth arguments is remaining money after buying.
%%%% Buyable predicate returns all possible unit list and remaining money combinations. 

buyable(List,Money,Restlist,RestMoney) :- combinations(List,0,Restlist),totalcost(Restlist,Cost),Cost=<Money,RestMoney is Money - Cost. 



