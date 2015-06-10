:- module(utility,[member1/2, drop/3, index_of/3, count_of/3]).

/** <module> Utility Predicates
* This module has some useful list based predicates
* @author Sam Neaves
*/

%% member1(+Item:atom, +List:list) is semidet.  % 1
%
% 1 : This is true when Item is in List.
%

%% member1(-Item:var,+List:list) is nondet.     % 2
%
% 2 : Item is the first item in the List or fails for an empty List
%

%% member1(+Item:atom, -List:list) is det.     % 3
%
% 3 : Item is the head of a List, then a var.
%
member1(I,[I|_]) :- !.
member1(I,[_|RS]) :- member1(I,RS).

%% drop(+N:int,+L:list,-NewL:list) is det.
%
% Means list NewL is L with its first N elements removed.
%
drop(0,L,L).
drop(N,[_H|T],R) :-
	N>0, M is N-1,
	drop(M,T,R).

%% index_of(+List:list, +Element:atom, -Index:int) is nondet.
%
% Finds the (Oth) Index for Element in List, will find the next Index
% on back tracking.
%
%
index_of([Element|_], Element, 0).
index_of([_|Tail], Element, Index):-
  index_of(Tail, Element, Index1),
  Index is Index1+1.




%% count_of(+List:list, +Element:atom, -Count:int) is det          %1
%
%       1: Count of Element in List.
%

%%	count_of(+List:list, +Element:atom, +Count:int) is semidet %2
%
%	2: Is it true that List has Count copies of Element?
%

%% count_of(+List:list, -Element:atom, -Count:int) is det          %3
%
%	3: How many of first Element in List?
%
count_of(List, _,Count):-
	List =[],
	Count is 0.

count_of(List,Element,Count):-
	List =[H|T],
	count_of(T,Element,Count0),
	Element = H,
	Count is 1 + Count0,!.

count_of(List,Element,Count):-
	List =[_|T],
	count_of(T,Element,Count),!.












