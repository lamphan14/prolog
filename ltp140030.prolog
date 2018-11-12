oddMultOf3(N) :- (
                 %check whether the given Number is an integer or not
                 not(integer(N))
                 ->
                 % If N is not an integer, print an error message
                 write('ERROR:The given parameter is not an integer')
                 ;%else part
                 %If N is an integer, check N is divisible by 3 and N is odd or not
                 0 is N mod 3 , !, 1 is N mod 2
                 ).
                 
% Define the base condition for list_prod.
list_prod([], 0).
%Define the function list_prod.
list_prod([H|T], Number) :- 
% Call the function list_prod_1 to compute the product.
list_prod_1(T, H, Number).
% Define the base condition for list_prod_1.
list_prod_1([], R, R).
% Define the function list_prod_1.
list_prod_1([H|T], H1, R) :-   
 % Call the function list_prod_1 recursively.
 list_prod_1(T, H, R1),
 % Compute the product.
    R is R1 * H1.
    
palindrome(List) :- 
	reverse(List,List).
	reverse(L1,L2) :- rev(L1,[],L2).
	rev([],L,L).
	rev([X|L],L2,L3) :- rev(L,[X|L2],L3).
	
%Define a predicate secondMin/2
% with the signature secondMin(List, Min2)
secondMin(List, Min2) :-
    (   ground(List)
    -> (   is_list(List),
            maplist(number, List)
        -> (   sort(List, [_,Second|_])
            %if the list has fewer than two unique
            %elements, then your predicate should
            %display the following,
            %“ERROR: List has fewer than two unique elements.”
            -> Min2 = Second
            ;   throw('ERROR:List has fewer than two unique elements.')
            )
        ;   throw('ERROR: "The element" is not a number.')
        )
    ;   throw(error(instantiation_error, _))
    ).
    %segregate/3
    segregate([],[],[]).
segregate([H|T],[H|X],Odd):- 
	0 is H mod 2,!,segregate(T,X,Odd).
segregate([H|T],Even,[H|Y]):- 
	segregate(T,Even,Y).
	
%bookends/3
bookends(Prefix,Suffix,AList):- prefix(Prefix,AList), suffix(Suffix,AList).
prefix([],_):- !.
prefix([HP|TP],[HL|TL]):- HP == HL,prefix(TP,TL).
suffix(L,L):- !.
suffix(S,[_|T1]):- suffix(S,T1),!.

%subslice/2
subslice([],_):- !.
subslice(Sub,[H|T]):- prefix(Sub,[H|T]),!;subslice(Sub,T).

%shift/3
shift(L1, N, L2) :-
    N < 0, !,
    N1 is -N,
    shift(L2, N1, L1).
shift(L1, N, L2) :- 
    append(Lx, Ly, L1), % L1 is Lx || Ly
    append(Ly, Lx, L2), % L2 is Ly || Lx
    length(Lx, N).      % The length of Lx is N
 %luhn algorithm
 %Function to calculate sum of 2 digit number.
sum2(Num,Result):-
    N1 is mod(Num,10),
    N2 is div(Num,10),
    Result is N1+N2,!.
%Function to calculate sum of a 2 digit number.
sum(Num,Result):-
    N1 is mod(Num,10),
    N2 is div(Num,10),
    %Calculate second digit from right * 2.
    N3 is N2*2,
    %If second number from right * 2 is more than 9,
    % call the function sum2.
    N3>9 -> sum2(N3,X),Result is X+N1,!;
    %Otherwise, add the digits.
    N1 is mod(Num,10),
    N2 is div(Num,10),
    N3 is N2*2,
    Result is N1+N3,!.
%Define the base condition.
digitsum(0,Sum,Result):-
    Result is Sum,!.
%Define the function digits().
digitsum(N,Sum,Result):-
    %Extract 2 last digits from the number.
    Rem is mod(N,100),
    Ques is div(N,100),
    %Calculate the sum of 2 digits.
    sum(Rem,S2),
    NewSum is Sum+S2,
    digitsum(Ques,NewSum,Result),!.
%Define the function luhn().
luhn(Num):-
    %Call the function to add the digits.
    digitsum(Num,0,Result),
    %Calculate the value of sum mod 10.
    Rem is mod(Result,10),
    %Check if the mod value is 0 or not.
    Rem = 0.
    
    %Genealogy
parents(X,Y):-
parent(X,Y).

mother(X,Y):-
parents(X,Y),
female(X).

father(X,Y):-
parents(X,Y),
male(X).

child(X,Y):-
parents(X,Y).

grandparent(X,Y):-
parents(Z,Y),
parents(X,Z).

grandfather(X,Y):-
male(X),
parents(Z,Y),
parents(X,Z).

grandmother(X,Y):-
parents(Z,Y),
parents(X,Z),
female(X).

grandchild(X,Y):-
parents(Y,Z),
parents(Z,X).

grandson(X,Y):-
parents(Y,Z),
parents(Z,X),
male(X).

granddaughter(X,Y):-
parents(Y,Z),
parents(Z,X),
female(X).

sibling(X,Y):-
parents(Z,X),
parents(Z,Y),
not(X=Y).