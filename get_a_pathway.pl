

pathway_for_tree_liker_with_reactions(File,Pathway):-
	open(File,append,Stream,[]),
	forall(
	    batch(_X,Sample),
	    (
	    sample_diagnosis(Sample,Class,_),
	    downcase_atom(Class,Low_Class),
	    downcase_atom(Sample,Low_Sample),
	    format(Stream,"~w microarray(~w),",[Low_Class,Low_Sample]),
	    forall(
		(
		all_reactions_in_pathway(Pathway, Reactions),
		member(Reaction_First,Reactions),
	        reaction_follows_type_slow(Reaction_First, Reaction_Second, Type),
	        downcase_atom(Type,_LowType),
	        member(Reaction_Second,Reactions),
		downcase_atom(Reaction_First,Low_Reaction_First),
		downcase_atom(Reaction_Second,Low_Reaction_Second)
	        %reactome_name(Reaction_First, Display_NameR1),
	        %reactome_name(Reaction_Second, Display_NameR2)
	        ),
		%can also output reaction type here if we want to Low_Type
		format(Stream,"connected(~w,~w,~w),",[Low_Sample,Low_Reaction_First,Low_Reaction_Second])
	        ),
	    forall(
		(
		all_reactions_in_pathway(Pathway, Reactions2),
		member(Reaction_2,Reactions2)
	        ),
		(
		tuple(Sample,Reaction_2,State),
		downcase_atom(Reaction_2,Low_Reaction_2),
		%downcase_atom(State,Low_State),
		    ((State=1->StateTF=true);(StateTF=false)	),
		format(Stream,"state(~w,~w),",[Low_Reaction_2,StateTF]))
	    ),
		format(Stream,"~n~n",[])

	    )
	 ),
	 close(Stream).




tree_liker_with_reactions_v2(File,Pathway):-
	open(File,append,Stream,[]),
	all_links_in_pathway(Pathway,Links),
	forall(
	    batch(_X,Sample),
	    (
	    sample_diagnosis(Sample,Class,_),
	    downcase_atom(Class,Low_Class),
	    format(Stream,"~w,",[Low_Class]),
	    forall(
		(
		member(Link,Links),
		Link = link(ReactionA,ReactionB,LinkType),
		downcase_atom(ReactionA,Low_ReactionA),
		downcase_atom(ReactionB,Low_ReactionB),
		downcase_atom(LinkType,LowLinkType),
		Low_Link = link(Low_ReactionA,Low_ReactionB,LowLinkType)
	        ),
		format(Stream, "~w,",[Low_Link])
	    ),

	    forall(
		(
		all_reactions_in_pathway(Pathway, Reactions2),
		member(Reaction_2,Reactions2)
	        ),
		(
		tuple(Sample,Reaction_2,State),
		downcase_atom(Reaction_2,Low_Reaction_2),
		%downcase_atom(State,Low_State),
		    ((State=1->StateTF=reaction_on(Low_Reaction_2));(StateTF=reaction_off(Low_Reaction_2))	         ),
		format(Stream,"~w,",[StateTF]))
	    ),
		format(Stream,"~n~n",[])

	    )
	 ),
	 close(Stream).



tree_liker_with_reactions_v3(File,Pathway):-
	open(File,append,Stream,[]),
	all_links_in_pathway(Pathway,Links),
	forall(
	    batch(_X,Sample),
	    (
	    sample_diagnosis(Sample,Class,_),
	    downcase_atom(Class,Low_Class),
	    downcase_atom(Sample,Low_Sample),
	    format(Stream,"~w, sample(~w),",[Low_Class, Low_Sample]),
	    forall(
		(
		member(Link,Links),
		Link = link(ReactionA,ReactionB,LinkType),
		downcase_atom(ReactionA,Low_ReactionA),
		downcase_atom(ReactionB,Low_ReactionB),
		downcase_atom(LinkType,LowLinkType),
		Low_Link = link(Low_ReactionA,Low_ReactionB,LowLinkType)
	        ),
		format(Stream, "~w,",[Low_Link])
	    ),

	    forall(
		(
		all_reactions_in_pathway(Pathway, Reactions2),
		member(Reaction_2,Reactions2)
	        ),
		(
		tuple(Sample,Reaction_2,State),
		downcase_atom(Reaction_2,Low_Reaction_2),
		%downcase_atom(State,Low_State),
		    ((State=1->StateTF=reaction_on(Low_Sample,Low_Reaction_2));(StateTF=reaction_off(Low_Sample,Low_Reaction_2))	         ),
		format(Stream,"~w,",[StateTF]))
	    ),
		format(Stream,"~n~n",[])

	    )
	 ),
	 close(Stream).

tree_liker_with_reactions_v4(File,Pathway):-
	open(File,append,Stream,[]),
	all_links_in_pathway(Pathway,Links),
	forall(
	    batch(_X,Sample),
	    (
	    sample_diagnosis(Sample,Class,_),
	    downcase_atom(Class,Low_Class),
	    downcase_atom(Sample,Low_Sample),
	    format(Stream,"~w, sample(~w),",[Low_Class, Low_Sample]),
	    forall(
		(
		member(Link,Links),
		Link = link(ReactionA,ReactionB,LinkType),
		tuple(Sample,ReactionA,1),
		tuple(Sample,ReactionB,1),
		downcase_atom(ReactionA,Low_ReactionA),
		downcase_atom(ReactionB,Low_ReactionB),
		downcase_atom(LinkType,LowLinkType),
		Low_Link = link_on(Low_ReactionA,Low_ReactionB,LowLinkType)
	        ),
		format(Stream, "~w,",[Low_Link])
	    ),
		format(Stream,"~n~n",[])
	    )
	 ),
	 close(Stream).

tree_liker_with_reactions_v5(File,Pathway):-
	open(File,append,Stream,[]),
	all_links_in_pathway(Pathway,Links),
	forall(
	    batch(_X,Sample),
	    (
	    sample_diagnosis(Sample,Class,_),
	    downcase_atom(Class,Low_Class),
	    downcase_atom(Sample,Low_Sample),
	    format(Stream,"~w, sample(~w), ",[Low_Class, Low_Sample]),
	    forall(
		(
		member(Link,Links),
		Link = link(ReactionA,ReactionB,LinkType),
		tuple(Sample,ReactionA,1),
		tuple(Sample,ReactionB,1),
		downcase_atom(ReactionA,Low_ReactionA),
		downcase_atom(ReactionB,Low_ReactionB),
		downcase_atom(LinkType,LowLinkType),
		Low_Link = link_on(Low_ReactionA,Low_ReactionB,LowLinkType)
	        ),
		format(Stream, "reaction(~w,~w), reaction(~w,~w), ~w,",[Low_Sample,Low_ReactionA,Low_Sample, Low_ReactionB,Low_Link])
	    ),
		format(Stream,"~n~n",[])
	    )
	 ),
	 close(Stream).


tree_liker_with_reactions_v6(File,Pathway):-
	open(File,append,Stream,[]),
	all_links_in_pathway(Pathway,Links),
	forall(
	    batch(_X,Sample),
	    (
	    sample_diagnosis(Sample,Class,_),
	    downcase_atom(Class,Low_Class),
	    downcase_atom(Sample,Low_Sample),
	    format(Stream,"~w, sample(~w), ",[Low_Class, Low_Sample]),
	    forall(
		(
		member(Link,Links),
		Link = link(ReactionA,ReactionB,LinkType),
		tuple(Sample,ReactionA,AOnOrOff),
		tuple(Sample,ReactionB,BOnOrOff),
		downcase_atom(ReactionA,Low_ReactionA),
		downcase_atom(ReactionB,Low_ReactionB),
		downcase_atom(LinkType,LowLinkType),
		Low_Link = link(Low_ReactionA,Low_ReactionB,LowLinkType)
	        ),
		format(Stream,"reaction(~w,~w), reaction(~w,~w), ~w,",[Low_ReactionA,AOnOrOff,Low_ReactionB,BOnOrOff,Low_Link])
	    ),
		format(Stream,"~n~n",[])
	    )
	 ),
	 close(Stream).

tree_liker_with_reactions_v7(File,Pathway):-
	open(File,append,Stream,[]),
	all_links_in_pathway(Pathway,Links),
	forall(
	    batch(_X,Sample),
	    (
	    sample_diagnosis(Sample,Class,_),
	    downcase_atom(Class,Low_Class),
	    downcase_atom(Sample,Low_Sample),
	    format(Stream,"~w, sample(~w), ",[Low_Class, Low_Sample]),
	    forall(
		(
		member(Link,Links),
		Link = link(ReactionA,ReactionB,LinkType),
		tuple(Sample,ReactionA,AOnOrOff),
		tuple(Sample,ReactionB,BOnOrOff),
		downcase_atom(ReactionA,Low_ReactionA),
		downcase_atom(ReactionB,Low_ReactionB),
		downcase_atom(LinkType,LowLinkType),
		Low_Link = link(Low_ReactionA,Low_ReactionB,LowLinkType)
	        ),
		format(Stream,"reaction(~w,~w), reaction(~w,~w), ~w,",[Low_ReactionA,AOnOrOff,Low_ReactionB,BOnOrOff,Low_Link])
	    ),
		format(Stream,"~n~n",[])
	    )
	 ),
	 close(Stream).



all_pathways_tree_liker:-
	working_directory(_X,'C:/Users/Sam/Documents/Files/Version3/Pathways_for_tree_liker'),
	forall(
	    (
	       component_type_slow(Component,'Pathway'),
	       atom_concat(Component,'.txt',File)
	     ),
	    tree_liker_with_reactions_v7(File,Component)
	),
	working_directory(_Y,'C:/Users/Sam/Documents/Files/Version3').


%%DCG Tree Liker%%%
person_likes(Who, Whats) -->
    atom(Who), atom(': '),
    likes_(Whats).

likes_([])         --> [newline].
likes_([X])        --> atom(X), [newline].
likes_([X,Y|Rest]) --> atom(X), atom(', '), likes_([Y|Rest]).

atom(A) --> [atom(A)].

output(newline) :- nl.
output(atom(A)) :- format("~w", [A]).

%%%%%%%%














