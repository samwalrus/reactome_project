:-module(graphs_module,[pathway_for_text_file_names/2,
		       pathway_reaction_ids_names/3,
		       agg_reactions/2,
		       reaction_in_pathway/2,
		       count_on_reactions/2,
		       count_on_reactions_ac/2,
		       count_on_reactions_scc/2]).

pathway_for_text_file_names(File,Pathway):-
	 open(File,append,Stream,[]),
	 forall(
	 (
	 all_reactions_in_pathway(Pathway, Reactions),
	 member(Reaction_First,Reactions),
	 %reaction_follows_type_slow(Reaction_First, Reaction_Second, Type),
	 link_type(Reaction_First, Reaction_Second, Type), %for preprocessed files and speed
	 downcase_atom(Type,LowType),
	 member(Reaction_Second,Reactions),
	 reactome_name(Reaction_First, Display_NameR1),
	 reactome_name(Reaction_Second, Display_NameR2)
	 ),
	 format(Stream,'~w#~w#~w#~w#~w~n',[Reaction_First,Display_NameR1,Reaction_Second,Display_NameR2,LowType])),
	 close(Stream).


pathway_reaction_ids_names(File,Pathway,Sample):-
	 open(File,append,Stream,[]),
	  all_reactions_in_pathway(Pathway, Reactions),
	 forall(
		 member(Reaction,Reactions),
	         (
	         tuple(Sample,Reaction,State),
		 reactome_name(Reaction, Display_Name),
		 id_stable_reactions_fast(Reaction,Stable),
                 format(Stream,'~w#~w#~w#~w~n',[Reaction,Display_Name,Stable,State])
	         )
	 ),
	 close(Stream).

agg_reactions(File,Pathway):-
        open(File,append,Stream,[]),
	forall(
		 reaction_in_pathway(Reaction,Pathway),
	         (
		 count_on_reactions(Reaction,CountOn),
		 Avg is CountOn / 70,
		 reactome_name(Reaction, Display_Name),
		 id_stable_reactions_fast(Reaction,Stable),
                 format(Stream,'~w#~w#~w#~w~n',[Reaction,Display_Name,Stable,Avg])
	         )
	 ),
	close(Stream).


agg_reactions_ac(File,Pathway):-
        open(File,append,Stream,[]),
	forall(
		 reaction_in_pathway(Reaction,Pathway),
	         (
		 count_on_reactions_ac(Reaction,CountOn),
		 Avg is CountOn / 70,
		 reactome_name(Reaction, Display_Name),
		 id_stable_reactions_fast(Reaction,Stable),
                 format(Stream,'~w#~w#~w#~w~n',[Reaction,Display_Name,Stable,Avg])
	         )
	 ),
	close(Stream).


agg_reactions_scc(File,Pathway):-
        open(File,append,Stream,[]),
	forall(
		 reaction_in_pathway(Reaction,Pathway),
	         (
		 count_on_reactions_scc(Reaction,CountOn),
		 Avg is CountOn / 70,
		 reactome_name(Reaction, Display_Name),
		 id_stable_reactions_fast(Reaction,Stable),
                 format(Stream,'~w#~w#~w#~w~n',[Reaction,Display_Name,Stable,Avg])
	         )
	 ),
	close(Stream).

reaction_in_pathway(Reaction,Pathway):-
	all_reactions_in_pathway(Pathway, Reactions),
	member(Reaction,Reactions).

count_on_reactions(Reaction,CountOn):-
	findall(Sample,tuple(Sample,Reaction,1),SamplesOn), length(SamplesOn,CountOn).


count_on_reactions_ac(Reaction,CountOn):-
	findall(Sample,tuple_ac(Sample,Reaction,1),SamplesOn), length(SamplesOn,CountOn).

count_on_reactions_scc(Reaction,CountOn):-
	findall(Sample,tuple_scc(Sample,Reaction,1),SamplesOn), length(SamplesOn,CountOn).






%rdf(X,'http://www.biopax.org/release/biopax-level3.owl#xref','http://www.reactome.org/biopax/47/48887#UnificationXref31').
%
%
%
rdf_reaction(R):-
	rdf(R,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.biopax.org/release/biopax-level3.owl#BiochemicalReaction').


reaction_cross(R,X):-
	rdf_reaction(R),
	rdf(R,'http://www.biopax.org/release/biopax-level3.owl#xref',X).


%'http://www.biopax.org/release/biopax-level3.owl#xref')

x_ref_react_id(X,REACT_ID):-
	rdf(X,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.biopax.org/release/biopax-level3.owl#UnificationXref'),
	rdf(X,'http://www.biopax.org/release/biopax-level3.owl#db',literal(type('http://www.w3.org/2001/XMLSchema#string', 'Reactome'))),
	rdf(X,'http://www.biopax.org/release/biopax-level3.owl#id', literal(type('http://www.w3.org/2001/XMLSchema#string', REACT_ID))).


%rdf(X,'http://www.biopax.org/release/biopax-level3.owl#id',.


reaction_reactome_id(Reactome_Id):-
	rdf_reaction(Reactome_String),
	reactome_string_id(Reactome_String,Reactome_Id).

id_stable_reactions_fast(Reactome_Id,Stable):-
       atom_concat('http://www.reactome.org/biopax/47/48887#', Reactome_Id, Reactome_String_Name),
       reaction_cross(Reactome_String_Name,X),
       x_ref_react_id(X,Stable).
