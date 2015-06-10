:- module(reactome_utility,[
	      reactome_string_id/2,
	      reactome_name/2,
	      reactome_name_fast/2,
	      protein_reactome_id_to_uniprot_id/2,
	      reactome_string_type/2,
	      component_type/2,
	      component_type_slow/2,
	      type_c/2,
	      all_reactions/1,
	      input_to_reaction_c/2,
	      inputs_to_reaction_list/2,
	      output_to_reaction_c/2,
	      outputs_to_reaction_list/2,
	      component_of_complex/2,
	      component_of_protein_set/2,
	      complex_c/2,
	      protein_set_c/2,
	      child_component/2,
	      descendant_component/2,
	      descendant_complex_or_set/2,
	      child_type_protein/2,
	      all_children_proteins/1,
	      cellular_location/2,
	      descendants_at_max_depth/2,
	      depth_of_entity/2,
	      depth_of_descendant/3,
	      max_depth_of_descendant/2,
	      reaction_follows/3,
	      reaction_follows_type/3,
	      reaction_follows_type_slow/3,
	      reaction_follows_entity_slow/4,
	      reaction_controlled_by/4,
	      reaction_controlled_by_old/4,
	      pathway_component/2,
	      pathway_component_is_reaction/2,
	      pathway_descendant_reaction/2,
	      pathway_components_one_level/2,
	      pathway_component_is_pathway/2,
	      pathway_has_no_subpathways/1,
	      all_reactions_in_pathway/2,
	      reaction_in_pathway/3,
	      pathway_for_js_fiddle/1,
	      pathway_for_text_file/2,
	      pathway_for_warmr_or_aleph/2,
	      link_in_pathway/2

	  ]).

/** <module> Reactome Utility Predicates
* This module has some useful predicates for interacting with the
* owl:rdf ontology file for Reactome
* @author Sam Neaves
*/



%%    reactome_string_id(+R:reactome_string, -Id:reactome_id) is semidet
%
%     Reactome_String is a string of the form:
%	'http://www.reactome.org/biopax/47/48887#Protein1'
%
reactome_string_id(Reactome_String,Reactome_Id):-
	name(Reactome_String,Char_Codes),
	drop(40,Char_Codes,Char_Codes_Short),
	name(Reactome_Id,Char_Codes_Short).


%%    reactome_name(?Rid:reactome_id,?Name:atom) is semidet
%
%     Use with single quotes works in both directions.
%
%
reactome_name(Reactome_Id, Display_Name):-
	rdf(Reactome_String_Name,'http://www.biopax.org/release/biopax-level3.owl#displayName', literal(type('http://www.w3.org/2001/XMLSchema#string',Display_Name ))),
	reactome_string_id(Reactome_String_Name,Reactome_Id).



%%    reactome_name_fast(+Rid:reactome_id,-Name:atom) is semidet
%
%     Use with single quotes only works in direction of id to name.
%
reactome_name_fast(Reactome_Id,Display_Name):-
	atom_concat('http://www.reactome.org/biopax/47/48887#', Reactome_Id, Reactome_String_Name),
	rdf(Reactome_String_Name,'http://www.biopax.org/release/biopax-level3.owl#displayName', literal(type('http://www.w3.org/2001/XMLSchema#string',Display_Name ))).

%%    protein_reactome_id_to_uniprot_id(?Reactome_Id:protein_id,?UniprotId:uniprot_id)
%%    is semidet
%
% Use with single quotes
% protein_reactome_Id_to_Uniprot_Id2('Protein7',UniprotId) works in both
% directions.
%
protein_reactome_id_to_uniprot_id(Reactome_Id, Uniprot_Id):-
	rdf(Reactome_String_Name,'http://www.biopax.org/release/biopax-level3.owl#entityReference', Protein_Ref),
	rdf(Protein_Ref,'http://www.biopax.org/release/biopax-level3.owl#xref', Unification_Ref),
	rdf(Unification_Ref,'http://www.biopax.org/release/biopax-level3.owl#db',literal(type('http://www.w3.org/2001/XMLSchema#string', 'UniProt'))),
	rdf(Unification_Ref,'http://www.biopax.org/release/biopax-level3.owl#id',literal(type('http://www.w3.org/2001/XMLSchema#string', Uniprot_Id))),
	reactome_string_id(Reactome_String_Name,Reactome_Id).


%% reactome_string_type(+Reactome_string: reactome_string,-ReactomeType:reactome_type) is semidet
%
% Reactome_String is a string (atom) of the form:
% http://www.biopax.org/release/biopax-level3.owl#BiochemicalReaction
%
reactome_string_type(Reactome_String,Reactome_Type):-
	name(Reactome_String,Char_Codes),
	drop(48,Char_Codes,Char_Codes_Short),
	name(Reactome_Type,Char_Codes_Short).

%%	component_type(+Component: reactome_id, -Type:Reactome_type)is semidet
%
%
% This is works in one direction only and returns protein sets as
% proteins.
%
% @see component_type_slow/2 and type_c/2
%
component_type(Component, Type):-
	atom_concat('http://www.reactome.org/biopax/47/48887#', Component, Reactome_String_Name),
	rdf(Reactome_String_Name,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',Reactome_Type_String),
	reactome_string_type(Reactome_Type_String, Type).

%%	component_type_slow(?Component:reactome_id,?Type:reactome_type)is semidet
%
%	This works in both directions but can be slow. It returns
%	protein sets as proteins.
%
%	@see component_type/2 and type_c/2
%
component_type_slow(Component,Type):-
	rdf(Reactome_String_Name,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type',Reactome_Type_String),
	atom_concat('http://www.reactome.org/biopax/47/48887#', Component, Reactome_String_Name),
	reactome_string_type(Reactome_Type_String, Type).

%%	type_c(+Component: reactome_id, -Type:Reactome_type) is semidet
%
%
% This three place predicate definition is simple checking of proteins,
% protein_sets and complexes, They work in one direction. The order of
% these three predicates is important for speed. The _c means
% calculated. Rdf files that have been preprocced will have a type/2
% predicate to improve speed. Unlike component_type/2 and
% component_type_slow/2 these return the correct type for protein sets
%
% @tbd add defs for small molecules, dna, rna and physical entities
% maybe at the momnent the parent_child and descedant relations depend
% on this not being the case
%
type_c(Component,SimpleType):-
	component_type(Component, Type),
	Type = 'Protein',
	rdf(Reactome_String_Name,'http://www.biopax.org/release/biopax-level3.owl#comment',
	    literal(type('http://www.w3.org/2001/XMLSchema#string', 'Converted from EntitySet in Reactome'))
	   ),
	reactome_string_id(Reactome_String_Name, Component),
	SimpleType = protein_set.

type_c(Component,SimpleType):-
	rdf(Reactome_String_Name, 'http://www.biopax.org/release/biopax-level3.owl#entityReference',_),%%% we need this not to pick up protein sets..
	reactome_string_id(Reactome_String_Name, Component),
	component_type(Component, Type),
	Type = 'Protein',
	SimpleType = protein.



type_c(Component,SimpleType):-
	component_type(Component, Type),
	Type = 'Complex',
	SimpleType = complex.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%Membership related predicates%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%	all_reactions(-Reactions:list) is det
%
%	finds all the Reactions as a list in the ontology
%
all_reactions(Reactions):-
	 findall(R,component_type_slow(R, 'BiochemicalReaction'),Reactions).

%%	input_to_reaction_c(+Reaction_Id: reaction_id,-Input:reactome_id) is nondet
%
%  _c is for calculated, some ontologies should be preprocessed for
%  speed. On back tracking finds all inputs to a given reaction. Fails
%  if reaction has no input or if reaction does not exist.
%
% @tbd What to do with 'Physical entities'? Should this be changed to
% only return, proteins, complexs and protein_sets?

input_to_reaction_c(Reaction_Id, Reaction_Input_Id):-
	rdf(Reaction_String, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.biopax.org/release/biopax-level3.owl#BiochemicalReaction'),
	reactome_string_id(Reaction_String,Reaction_Id),
	rdf(Reaction_String,'http://www.biopax.org/release/biopax-level3.owl#conversionDirection',literal(type('http://www.w3.org/2001/XMLSchema#string', 'LEFT-TO-RIGHT'))),
	rdf(Reaction_String,'http://www.biopax.org/release/biopax-level3.owl#left', Reaction_Input_String),
	reactome_string_id(Reaction_Input_String,Reaction_Input_Id).
	%(type(Reaction_Input_Id, protein);type(Reaction_Input_Id, protein_set);type(Reaction_Input_Id,complex)). %only return, proteins, complexs, protein_set? SLOW


%%	inputs_to_reaction_list(+Reaction_Id:reaction_id, -Inputs:list)is semidet
%
% uses input_to_reaction_c/2 rather than a preprocessed
% input_to_reaction/2 so is not very fast
%

inputs_to_reaction_list(Reaction_Id, Inputs):-
	bagof(Input,input_to_reaction_c(Reaction_Id, Input), Inputs).

%%	output_to_reaction_c(+Reaction_Id:reaction_id,-Output_Id:reactome_id)is nondet
%
%	_c is for calculated, some ontologies should be preprocessed for
%	speed. On back tracking finds all outputs for a given reaction.
%	Fails if reaction has no output or if reaction does not exist
%	@tbd What to do with 'Physical entities'? Should this be changed to
%       only return, proteins, complexs and protein_sets?
%
output_to_reaction_c(Reaction_Id, Reaction_Output_Id):-
	rdf(Reaction_String, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.biopax.org/release/biopax-level3.owl#BiochemicalReaction'),
	reactome_string_id(Reaction_String,Reaction_Id),
	rdf(Reaction_String,'http://www.biopax.org/release/biopax-level3.owl#conversionDirection',literal(type('http://www.w3.org/2001/XMLSchema#string', 'LEFT-TO-RIGHT'))),
	rdf(Reaction_String,'http://www.biopax.org/release/biopax-level3.owl#right', Reaction_Output_String),
	reactome_string_id(Reaction_Output_String,Reaction_Output_Id).
	%(type(Reaction_Output_Id, protein);type(Reaction_Output_Id, protein_set);type(Reaction_Output_Id,complex)).%only return, proteins, complexs, protein_set? SLOW

%%	outputs_to_reaction_list(+Reaction_Id:reaction_id,-Outputs:list)is semidet
%
%	uses output_to_reaction_c so is not very fast
%

outputs_to_reaction_list(Reaction_Id, Outputs):-
	bagof(Output,output_to_reaction_c(Reaction_Id, Output), Outputs).


%%	component_of_complex(+Complex_Id:complex_id,-Component_Id:reactome_id) is nondet
%
%       Back tracks to find all components of a complex
%
component_of_complex(Complex_Id, Component_Id):-
	rdf(Complex_Reactome_String,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.biopax.org/release/biopax-level3.owl#Complex'),
	reactome_string_id(Complex_Reactome_String, Complex_Id),
	rdf(Complex_Reactome_String,'http://www.biopax.org/release/biopax-level3.owl#component', Component_Reactome_String),
	reactome_string_id(Component_Reactome_String, Component_Id).
	%(type(Component_Id, protein);type(Component_Id, protein_set);type(Component_Id,complex)).%only return, proteins, complexs, protein_set slow?

%%	complex_c(+Complex_Id:complex_id,-List_of_Children_Of_Complex:list)is semidet
%
%	_c is for calculated the ontology would normally be
%	preproccessed to give a complex/3
%
%
complex_c(Complex_Id,List_Of_Children_Of_Complex):-
	component_type(Complex_Id,'Complex'), %Might slow it down but allows it to generate- prob to slow
	bagof(Component_Id, component_of_complex(Complex_Id, Component_Id),List_Of_Children_Of_Complex).


%%	component_of_protein_set(+Protein_Set_Id:protein_set_id,-Protein_Id:protein_id) is nondet
%
%	Is Protein_Set_Id a protein set? If so find members of that set
%	on backtracking.
%
component_of_protein_set(Protein_Set_Id,SetMemberProtein):-
	rdf(
	    Reactome_String_Name,'http://www.biopax.org/release/biopax-level3.owl#comment',
	    literal(type('http://www.w3.org/2001/XMLSchema#string', 'Converted from EntitySet in Reactome'))
	    ),
	reactome_string_id(Reactome_String_Name, Protein_Set_Id),
	rdf(Reactome_String_Name, 'http://www.biopax.org/release/biopax-level3.owl#memberPhysicalEntity', SetMemberProteinString),
	reactome_string_id(SetMemberProteinString,SetMemberProtein).
	%(type(SetMemberProtein, protein);type(SetMemberProtein, protein_set);type(SetMemberProtein,complex)).%only return, proteins, complexs, protein_set slow?

%%	protein_set_c(+Protein_Set_Id:protein_set_id,-List_Of_Children_Of_Protein_Set:list) is semidet
%
%	_c means calcualted. The ontology would normally be preprocessed
%	to provide protein_set/3
%
protein_set_c(Protein_Set_Id, List_Of_Children_Of_Protein_Set):-
	component_type(Protein_Set_Id,'Protein'),
	bagof(Component_Id, component_of_protein_set(Protein_Set_Id, Component_Id),List_Of_Children_Of_Protein_Set).




%%	child_component(+Parent:reactome_id,-Child:reactome_id)is nondet
%
%	Uses complex/2 and protein_set/2 which would be asserted on
%	preprocessing, otherwise you would need complex_c/2 and
%	protein_set_c/2 uses child and parent as a metaphor for
%	membership of a protein set or componet of a protein complex
%
child_component(Parent, Child):-
	%AND equivlant is for a complex
	complex(Parent,Children),
	member(Child,Children).

child_component(Parent, Child):-
	%OR equivlant is for a set.
	protein_set(Parent,Children),
	member(Child,Children).

%% descendant_component(+Parent:reactome_id, -Child:reactome_id)is nondet
%
%	On backtracking finds all descendants of a parent component.
%	Uses parent child and descendant as a metaphor for the relations
%	of set membership of a protein set or component of a protein
%	complex. Uses type/2 for speed which should be preprocessed
%	otherwise see type_c/2
%
descendant_component(Parent,Child):-
	child_component(Parent, Child).

descendant_component(Parent,Descendant):-
	child_component(Parent,Inter),
	descendant_component(Inter,Descendant).

%%	descendant_complex_or_set(+Parent: reactome_id, -Descendant:reactome_id) is semidet
%
%	Finds on backtracking all descendents of a component that will
%	also have descendants as they are themselves complexes or
%	protein sets.
%
descendant_complex_or_set(Parent,Descendant):- %descendant is a protein complex
	descendant_component(Parent,Descendant),
	type(Descendant,complex).

descendant_complex_or_set(Parent,Descendant):- %descendant is a protein set
	descendant_component(Parent,Descendant),
	type(Descendant,protein_set).

%%	 child_type_protein(+Parent:reactome_id, +Child:protein_id) is semidet
%
%	Is it true that Child is a simple protein not a protien complex
%	or set and is a Child of Parent. Uses type/2 which would be
%	preprocessed otherwise use type_c/2
%
child_type_protein(Parent,Child):-
	child_component(Parent,Child),
	type(Child,protein).

%%	all_children_proteins(+Parent:reactome_id) is semidet
%
%	Is it true that a component such as a complex or protein set has
%	'children' all of which are proteins. i.e none of its children
%	are also protein sets or complexes. type/2 would be preprocessed
%	otherwise see type_c/2
%
%	@see type_c/2
%
all_children_proteins(Parent):-
	type(Parent,Type),
	Type\=protein, %dont know why I need this line.
	foreach(child_component(Parent,Child),type(Child,protein)).



%% cellular_location(+Reactome_Id:reactome_id, -Location:reactome_location) is nondet
%
%   It may be better to use the go_terms then this as I am not sure
%   about hierarchy of locations.
%
%   @tbd
%
cellular_location(Reactome_Id_Entity, Location):-
	atom_concat('http://www.reactome.org/biopax/47/48887#',Reactome_Id_Entity, Reactome_String_Name),
	rdf(Reactome_String_Name,'http://www.biopax.org/release/biopax-level3.owl#cellularLocation',VocabString),
	rdf(VocabString, 'http://www.biopax.org/release/biopax-level3.owl#term',literal(type('http://www.w3.org/2001/XMLSchema#string', Location))).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%Depth Related Predicates%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%	depth_of_entity(+Entity: reactome_id, -Depth:int) is det
%
%	If an entity does not have a parent, then its depth is 0 other
%	it is its parents depth +1
%

%if an entity does not have a parent, then its depth is 0
depth_of_entity(Entity,Depth):-
	\+ child_component(_Parent,Entity),
	Depth is 0.

%Otherwise an entity's depth is its parents depth +1
depth_of_entity(Entity,Depth):-
	child_component(Parent,Entity),
	depth_of_entity(Parent,ParentDepth),
	Depth is ParentDepth +1.

%%	depth_of_descendant(+Entity:reactome_id,-Descendant:reactome_id,-Depth:int)is nondet
%
%	For an Entity find each decesdent and its correpsonding depth
%

% finds the descendants of an entity and there depths
depth_of_descendant(Entity, Descendant, Depth):-
	descendant_complex_or_set(Entity,Descendant),
	depth_of_entity(Descendant,Depth).


%%	max_depth_of_descendant(+Entity:reactome_id, -Max:int) is det
%
%	For an Entity find the Max deth of any of descendants
%
max_depth_of_descendant(Entity, Max):-
		aggregate(max(Depth),Descendant,depth_of_descendant(Entity, Descendant, Depth), Max).


%%	descendants_at_max_depth(+Entity: reactome_id,-Descendant:reactome_id) is nondet
%
%	On back tracking find each Descendant for Entity which has the
%	maxdepth
%

descendants_at_max_depth(Entity,Descendant):-
	max_depth_of_descendant(Entity, Max),
	depth_of_descendant(Entity, Descendant, Max).




%%% Pathway Extraction %%%%

%% reaction_follows(?Reaction_First:reaction_id, ?Reaction_Second:reaction_id,?Linking_Entity: reactome_id) is nondet
%
%	Reaction_Second follows Reaction_First if Linking_Entity is an
%	input to Reaction_Second and the output of Reaction_First OR if
%	a the output Linking_Enity of Reaction_First is a controlling
%	Enity for Reaction_Second
%
%	@see reaction_controlled_by/4 and reaction_follows_type/3 for
%	the version with type rather than entity. Uses the preprocessed
%	versions. For calculated versions reaction_follows_slow/3
%
%


%A reaction follows another reaction of the input to one reaction is the output of another
reaction_follows(Reaction_First, Reaction_Second,Linking_entity):-
	outputs_to_reaction(Reaction_First, Linking_entity),
	inputs_to_reaction(Reaction_Second, Linking_entity).
%Or if the output to a reaction controls another reaction
reaction_follows(Reaction_First, Reaction_Second, Linking_Entity):-
	outputs_to_reaction(Reaction_First, Linking_Entity),
	reaction_controlled_by(Reaction_Second,_Control_Id,Linking_Entity, _Type).


%%	reaction_follows_type(?Reaction_First:reactome_id,?Reaction_Second:reactome_id,?Type: type_of_link) is nondet
%
%	@see reaction_follows/3 for the version with linking entity
%	rather than type. Uses preprocessed outputs_to_reaction/2 and
%	input_to_reaction/2 see reaction_follows_type_slow/3 for
%	calculated version
%
reaction_follows_type(Reaction_First, Reaction_Second, Type):-
	outputs_to_reaction(Reaction_First, Linking_Entity),
	reaction_controlled_by(Reaction_Second,_Control_Id,Linking_Entity, Type).

reaction_follows_type(Reaction_First, Reaction_Second,'follows'):-
	outputs_to_reaction(Reaction_First, Linking_entity),
	inputs_to_reaction(Reaction_Second, Linking_entity).

%%	reaction_follows_type_slow(?Reaction_First:reactome_id,?Reaction_Second:reactome_id,?Type:type_of_link) is nondet
%
%
reaction_follows_type_slow(Reaction_First, Reaction_Second, Type):-
	output_to_reaction_c(Reaction_First, Linking_Entity),
	reaction_controlled_by_old(Reaction_Second,_Control_Id,Linking_Entity, Type).

reaction_follows_type_slow(Reaction_First, Reaction_Second,'follows'):-
	output_to_reaction_c(Reaction_First, Linking_entity),
	input_to_reaction_c(Reaction_Second, Linking_entity).

%%	reaction_follows_entity_slow(?Reaction_First: reactome_id,?Reaction_Second: reaction_id, ?Linking_Entity: reactome_id,?Type: link_type) is nondet
%
%	Uses the calculated version output_to_reaction_c/2 and
%	input_to_reaction/2 see reaction_follows/3 and
%	reaction_follows_type/2 for preprocessed versions
%
%
reaction_follows_entity_slow(Reaction_First, Reaction_Second, Linking_Entity,Type):-
	output_to_reaction_c(Reaction_First, Linking_Entity),
	reaction_controlled_by_old(Reaction_Second,_Control_Id,Linking_Entity, Type).

reaction_follows_entity_slow(Reaction_First, Reaction_Second,Linking_entity,'follows'):-
	output_to_reaction_c(Reaction_First, Linking_entity),
	input_to_reaction_c(Reaction_Second, Linking_entity).

%%	reaction_controlled_by(+Reaction:reaction_id,+Control_Id:control_id,-Controller_Id:reactome_id,-Type:type) is nondet
%
%	A Reaction is is Control_Id Type is inhibition or activation
%
reaction_controlled_by(Reaction,Control_Id,Controller_Id, Type):-
	atom_concat('http://www.reactome.org/biopax/47/48887#',Reaction,Reaction_String),
	rdf(Control_Id_String,'http://www.biopax.org/release/biopax-level3.owl#controlled',Reaction_String),
	rdf(Control_Id_String, 'http://www.biopax.org/release/biopax-level3.owl#controlType', literal(type(_,Type))),
	rdf(Control_Id_String, 'http://www.biopax.org/release/biopax-level3.owl#controller',Controller_String),
	reactome_string_id(Control_Id_String,Control_Id),
	reactome_string_id(Controller_String,Controller_Id).

%%	reaction_controlled_by_old(+Reaction:reaction_id,+Control_Id:control_id,-Controller_Id:reactome_id,-Type:type)
%	is nondet
%
%	A Reaction is is Control_Id Type is inhibition or activation
%

reaction_controlled_by_old(Reaction,Control_Id,Controller_Id, Type):-
	rdf(Control_Id_String,'http://www.biopax.org/release/biopax-level3.owl#controlled',Reaction_String),
	rdf(Control_Id_String, 'http://www.biopax.org/release/biopax-level3.owl#controller',Controller_String),
	rdf(Control_Id_String, 'http://www.biopax.org/release/biopax-level3.owl#controlType', literal(type(_,Type))),
	atom_concat('http://www.reactome.org/biopax/47/48887#',Reaction,Reaction_String),
	reactome_string_id(Control_Id_String,Control_Id),
	reactome_string_id(Controller_String,Controller_Id).

%%	pathway_component(+Pathway:pathway_id, -Component: reactome_id)is nondet
%
%	Component of Pathway. Does not look into subpathways.
%
pathway_component(Pathway,Component):-
	rdf(Pathway, 'http://www.biopax.org/release/biopax-level3.owl#pathwayComponent', Component).

%% pathway_component_is_reaction(+Pathway, ?Component) is nondet
%
%	Only finds reaction components and does NOT look into
%	subpathways
%
%	@see pathway_component/2
%
pathway_component_is_reaction(Pathway, Component):-
	pathway_component(Pathway,Component),
	rdf(Component, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.biopax.org/release/biopax-level3.owl#BiochemicalReaction'). %type component is reaction

%%	pathway_descendant_reaction(+Pathway:pathway_id,-Component:reaction_id)is nondet
%
%	Finds all the reaction Component s of a Pathway on backtracking
%	including components of subpathways.
%
%	@see pathway_component_is_reaction/2
%
pathway_descendant_reaction(Pathway, Component):-
	pathway_component_is_reaction(Pathway, Component).

pathway_descendant_reaction(Pathway,Component):-
	pathway_component_is_pathway(Pathway,Component_Pathway),
	pathway_descendant_reaction(Component_Pathway,Component).


%%	pathway_components_one_level(+Pathway,-Components:list) is det
%
%	Finds all Components of Pathway at one level. Does not look into
%	subpathways
%
pathway_components_one_level(Pathway,Components):-
	findall(Component, pathway_component(Pathway,Component),Components).

%%	pathway_component_is_pathway(+Pathway: pathway_id,Component:pathway_id) is nondet
%
%       Finds subpathways of a pathway
%
pathway_component_is_pathway(Pathway,Component):-
	pathway_component(Pathway,Component),
	rdf(Component, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'http://www.biopax.org/release/biopax-level3.owl#Pathway'). %type componet is pathway.

%%	pathway_has_no_subpathways(+Pathway) is nondet
%
%	Is it true that Pathway has no subpathways?
%
%
pathway_has_no_subpathways(Pathway):-
	pathway_components_one_level(Pathway,Components),
	forall(member(Component,Components), pathway_component_is_reaction(Pathway, Component)).

%% all_reactions_in_pathway(+Pathway:pathway_id, -Reactions:list) is nondet
%
%	Finds all reactions in a pathway including looking at
%	subpathways.
%
all_reactions_in_pathway(Pathway, Set_Of_Reactions):-
	atom_concat('http://www.reactome.org/biopax/47/48887#',Pathway,PathwayString),
	findall(Reactome_Id,(pathway_descendant_reaction(PathwayString, Component),reactome_string_id(Component,Reactome_Id)),Reactions),
	list_to_set(Reactions,Set_Of_Reactions).

%%	reaction_in_pathway(+Pathway:pathway_id, -Set_Of_Reactions:list,-Reaction:reaction_id) is nondet
%
%	Gives each Reaction for a Pathway in numeric order
%
reaction_in_pathway(Pathway, Set_Of_Reactions,Reaction):-
	all_reactions_in_pathway(Pathway, Set_Of_Reactions),
	%rdf(Rs,'http://www.w3.org/1999/02/22-rdf-syntax-ns#type','http://www.biopax.org/release/biopax-level3.owl#BiochemicalReaction'),
	%reactome_string_id(Rs, Reaction),
	member(Reaction, Set_Of_Reactions). %this is done to to find the reactions in order..


%%	pathway_for_js_fiddle(+Pathway:pathway_id) is det
%
%	This outputs to the screen a JSON file which can be copied
%	pasted into http://jsfiddle.net/walruses/p6N5x/
%
pathway_for_js_fiddle(Pathway):-
	 forall(
	 (
	 all_reactions_in_pathway(Pathway, Reactions),
	 member(Reaction_First,Reactions),
	 reaction_follows_type_slow(Reaction_First, Reaction_Second, Type),
	 downcase_atom(Type,LowType),
	 member(Reaction_Second,Reactions),
	 reactome_name(Reaction_First, Display_NameR1),
	 reactome_name(Reaction_Second, Display_NameR2)
	 ),
	 format('{source: "~w",target:"~w",type:"~w"},',[Display_NameR1,Display_NameR2,LowType])).

%%	pathway_for_text_file(+File:filename,+Pathway:pathway_id) is det
%
%	Outputs a text File that can be imported into cytoscape or other
%	network viewers. Outputs reaction_ids, can be edited for
%	display_names
%
%	@tbd safe file handling?
%
pathway_for_text_file(File,Pathway):-
	 open(File,append,Stream,[]),
	 forall(
	 (
	 all_reactions_in_pathway(Pathway, Reactions),
	 member(Reaction_First,Reactions),
	 reaction_follows_type_slow(Reaction_First, Reaction_Second, Type),
	 downcase_atom(Type,LowType),
	 member(Reaction_Second,Reactions)
	 %reactome_name(Reaction_First, Display_NameR1),
	 %reactome_name(Reaction_Second, Display_NameR2)
	 ),
	 format(Stream,'"~w"#"~w"#"~w"~n',[Reaction_First,Reaction_Second,LowType])),
	 close(Stream).

%%	pathway_for_warmr_or_aleph(+File:filename,+Pathway:pathway_id)is det
%
%	Outputs a File for Pathway in a format useful as input in Aleph
%	or ACE/Warmr/Tilde
%
%       @tbd safe file handling?
%
pathway_for_warmr_or_aleph(File,Pathway):-
	open(File,append,Stream,[]),
	 forall(
	 (
	 all_reactions_in_pathway(Pathway, Reactions),
	 member(Reaction_First,Reactions),
	 reaction_follows_type_slow(Reaction_First, Reaction_Second, Type),
	 downcase_atom(Type,LowType),
	 member(Reaction_Second,Reactions)
	 %reactome_name(Reaction_First, Display_NameR1),
	 %reactome_name(Reaction_Second, Display_NameR2)
	 ),
	 format(Stream,"link('~w','~w','~w').~n",[Reaction_First,Reaction_Second,LowType])),
	 close(Stream).


%%	link_in_pathway(+Pathway:reactome_id, -Link:link(R1,R2,LinkType)
%	is det
%
%	From the preprocessed file of all links find a link in this
%	pathway by selecting reactions. Link has entity not type at the
%	moment
%
%	@tbd types or entities ? import link/3
%
%
link_in_pathway(Pathway,Link):-
	all_reactions_in_pathway(Pathway, Reactions),
	member(Reaction1,Reactions),
	member(Reaction2,Reactions),
	link(Reaction1,Reaction2,LinkType),
	Link = link(Reaction1,Reaction2,LinkType).


all_links_in_pathway(Pathway,Links):-
	findall(Link,link_in_pathway(Pathway,Link),Links).


whole_network_warmr_entity(File,Time1,Time2):-
	statistics(runtime,_),
	statistics(runtime,[_,Time1]),
	open(File,append,Stream,[]),
	 forall(
	 (
	 all_reactions(Reactions),
	 member(Reaction_First,Reactions),
	 reaction_follows_entity_slow(Reaction_First, Reaction_Second, Entity,_Type),
	 member(Reaction_Second,Reactions),
	 reactome_name(Entity, Display_Name_E),
	 downcase_atom(Display_Name_E,Low_Name_Entity)

	 %reactome_name(Reaction_Second, Display_NameR2)
	 ),
	 format(Stream,'link("~w","~w","~w").~n',[Reaction_First,Reaction_Second,Low_Name_Entity])),
	 close(Stream),
	 statistics(runtime,[_,Time2]).

whole_network_warmr(File,Time1,Time2):-
	statistics(runtime,_),
	statistics(runtime,[_,Time1]),
	open(File,append,Stream,[]),
	 forall(
	 (
	 all_reactions(Reactions),
	 member(Reaction_First,Reactions),
	 reaction_follows_type_slow(Reaction_First, Reaction_Second, Type),
	 downcase_atom(Type,LowType),
	 member(Reaction_Second,Reactions)
	 %reactome_name(Reaction_First, Display_NameR1),
	 %reactome_name(Reaction_Second, Display_NameR2)
	 ),
	 format(Stream,'link("~w","~w","~w").~n',[Reaction_First,Reaction_Second,LowType])),
	 close(Stream),
	 statistics(runtime,[_,Time2]).

whole_network_txt_file(File,Time1,Time2):-
	statistics(runtime,_),
	statistics(runtime,[_,Time1]),
	open(File,append,Stream,[]),
	 forall(
	 (
	 all_reactions(Reactions),
	 member(Reaction_First,Reactions),
	 reaction_follows_type_slow(Reaction_First, Reaction_Second, Type),
	 downcase_atom(Type,LowType),
	 member(Reaction_Second,Reactions)
	 %reactome_name(Reaction_First, Display_NameR1),
	 %reactome_name(Reaction_Second, Display_NameR2)
	 ),
	 format(Stream,'"~w"#"~w"#"~w"~n',[Reaction_First,Reaction_Second,LowType])),
	 close(Stream),
	 statistics(runtime,[_,Time2]).

