:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/websocket)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_http_plugin)). 


:- http_handler(root(.), http_reply_file('js/comm.html', []), []).
% :- http_handler(root('bands.json'), http_reply_file('js/bands.json', []), []).
:- http_handler(root(hello_world), say_hi, []).
:- http_handler(root(ws),
                http_upgrade_to_websocket(echo, []),
                [spawn([])]).

connected(X, Y) :-
  rdf(X,'http://dbpedia.org/ontology/associatedBand',Y);
  rdf(Y,'http://dbpedia.org/ontology/associatedBand',X).


send_links(_,_,[]).
send_links(WebSocket, Current, [H|T]) :-
  string_concat(Current, ",", S1),
  string_concat(S1, H, S2),
  ws_send(WebSocket, text(S2)),
  send_links(WebSocket, Current, T).

load_band(A, S) :-
  rdf_load(A, [format(xml)]),
  findall(B, (rdf(A,'http://dbpedia.org/ontology/associatedBand',B)), L1),
  findall(C, (rdf(C,'http://dbpedia.org/ontology/associatedBand',A)), L2),
  append(L1, L2, S).

load_bands(WebSocket, _, _, _, 100) :-
  ws_send(WebSocket, text("Maximum band load size exceeded")).
load_bands(WebSocket, _, [Current|_], Current, _) :-
  ws_send(WebSocket, text("Target band found.")),
  rdf_load(Current, [format(xml)]).
load_bands(WebSocket, Visited, [Current|Open], Target, N) :-
  ws_send(WebSocket, text(Current)),
  (connected(Target, _) -> 
    format("Found solution after ~w hops.\n", [N]),
    rdf_load(Target, [format(xml)]);
    (member(Current,Visited) -> 
      % current band is already loaded
      load_bands(WebSocket, Visited, Open, Target, N);
      % load current band 
      N1 is N + 1,
      load_band(Current, AddOpen),
      send_links(WebSocket, Current, AddOpen),
      append(Open, AddOpen, UpdatedOpen),
      load_bands(WebSocket, [Current|Visited], UpdatedOpen, Target, N1))).

band_paths(WebSocket, Band1, Band2) :-
  ws_send(WebSocket, text("Start")),
  atom_concat('http://dbpedia.org/resource/', Band1, Q1),
  atom_concat('http://dbpedia.org/resource/', Band2, Q2),
  ws_send(WebSocket, text(Q1)),
  ws_send(WebSocket, text(Q2)),
  ws_send(WebSocket, text("Before load_bands")),
  load_bands(WebSocket, [], [Q1], Q2, 0).


echo(WebSocket) :-
    ws_receive(WebSocket, Message),
    (   Message.opcode == close
    ->  true
    ;   band_paths(WebSocket, Message.data, 'Black_Lungs'),
        echo(WebSocket)
    ).

server(Port) :-
        http_server(http_dispatch, [port(Port)]).

say_hi(_Request) :-
        reply_html_page(title('Hello World'),
                        [ h1('Hello World'),
                          p(['This example demonstrates generating HTML ',
                             'messages from Prolog'
                            ])
                        ]).