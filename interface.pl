:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).
:- use_module(reactome_utility_module).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/js_write)).

:- http_handler('/', basic, []).
:- http_handler('/get_graph', get_graph, []).
http:location(files, '/f', []).
:- http_handler(files(.), http_reply_from_files('assets', []), [prefix]).

pathway_options(Id,option([value=Id],Name)):-
	reactome_name_fast(Id, Name).

sample_options(Sample,option([value=Sample],Sample)).


server(Port) :-
        http_server(http_dispatch, [port(Port)]).

node(id1,sam,on).
node(id2,louise,on).
node(id3,wally,off).
node(id4,old_boy,off).
node(id5,ann_whoo,on).
node(id6, humphrey,unknown).
node(id7,hypotenuse,on).
node(id8,moon,on).

link_(id1,id2,5).
link_(id2,id3,5).
link_(id3,id4,5).
link_(id3,id6,5).
link_(id6,id4,5).
link_(id6,id8,5).


my_get_json(Json,Nodes,Links):-
    Json = json([ nodes=Nodes,
		  links=Links
            ]).
my_node_to_json(Node,Json):-
	Node = node(Id,Name,State),
	Json =json([
                  display_name=Name,
                  name=Id,
                  group=State]).

my_link_to_json(Link,Json):-
	Link = link_(Id1,Id2,Value),
	Json =json([
                  source=Id1,
                  target=Id2,
                  value=Value]).



all_nodes(Nodes):-
	findall(Json,
		(   node(X,Y,Z),
		    Node = node(X,Y,Z),
		    my_node_to_json(Node,Json)
		),
		Nodes).

all_links(Links):-
	findall(Json,
		(   link_(X,Y,Z),
		    Link = link_(X,Y,Z),
		    my_link_to_json(Link,Json)
		),
		Links).



get_graph(Request):-
	     all_links(Links),
	     all_nodes(Nodes),
	     my_get_json(_JsonGraph,Nodes,Links),

	  %catch because http_parameters throws if a param is invalid
	    catch(
	         http_parameters(Request,
				[
				     %optional for testing
				  pathway(Pathway, [optional(true)]),
				 sample(Sample, [optional(true)])
				 ]),
	         _E,
	         fail),
	    !,
	    Json_Param = json([pathway=Pathway, sample=Sample]),
	reply_json(Json_Param,[json_object(term)]).

style -->
	html(style([ '.node {stroke: #fff; stroke-width: 1.5px;}\n',
		     '.link { stroke: #999; stroke-opacity: .6; }\n'

		   ])).


page_content(_Request) -->
        %script,
	html(
	   [div([],[
	    div([id=myshow],["{{formdata}}"]),
            div([ng-show="message"],["{{ message }}"]),
	    form(
		[ng-submit="processForm()"],
		[
		label([for=pathways],'Pathways'),
		\pathway_input,
		label([for=samples],'Samples'),
		\sample_input,
		input([type=submit, value='view'])
		]
	    )]
	    )

	   ]
	).


pathway_input -->
	{
            findall(Pathway_Id, component_type_slow(Pathway_Id,'Pathway'),Reactome_Ids),
            maplist(pathway_options, Reactome_Ids, Options)
        },
	html([
	    select([size=20,id=pathways, ng-model="formdata.pathway"],Options)

	]).


sample_input -->
	{
            samples(Samples),
            maplist(sample_options, Samples, Options)
        },
	html([
	    select([size=20,id=samples, ng-model="formdata.sample"],Options)

	]).



basic(_Request) :-
	reply_html_page(
	   [title('My Application'),
	    script([type='text/javascript',src='http://d3js.org/d3.v3.min.js'],[]),
	    script([type='text/javascript',src='http://ajax.googleapis.com/ajax/libs/angularjs/1.3.14/angular.js'],[]),
            script([type='text/javascript',src='//ajax.googleapis.com/ajax/libs/jquery/2.0.3/jquery.min.js'],[])],

	    [\basic(_Request2)]).


basic(_Request2) -->
	script,
        style,
	html([div([ng-app="myApp", ng-controller="customersCtrl"],
		[
		 'd3-graph'([]),
                 div([],["{{response}}"]),

		  \page_content(_Request)

		]

		)]).




script -->
          js_script({|javascript(_Var)||

		var app = angular.module('myApp', []);

		var myData =    {
					id: 4,
					name: "Kim",
					status: "Best Friend"
				}

		app.directive('d3Graph', function(){
			function link(scope,element, attr){
			   element.text("hello ducks");
                           var data = scope.response;
			   console.log(data);
			}
			return {
			   link: link,
			   restrict: 'E'
			   //scope:{ data: '='}
			}
		});
		app.controller('customersCtrl', function($scope, $http) {
			$scope.formdata ={};

			$scope.processForm = function(){

				//alert("hello");
				var request = $http({
				method: "post",
				url: "http://localhost:3075/get_graph",
				//transformRequest: transformRequestAsFormPost,
				data: $.param($scope.formdata),
				headers: { 'Content-Type': 'application/x-www-form-urlencoded' }

				});

				// Store the data-dump of the FORM scope.
				request.success(
					function( response ) {
						console.log(response);
						$scope.response = response;

					}
				);
			};

		});


	|}).
