
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_http_plugin)). 

load_band(A, S) :-
  rdf_load(A, [format(xml)]),
  findall(B, (rdf(A,'http://dbpedia.org/ontology/associatedBand',B)), L1),
  findall(C, (rdf(C,'http://dbpedia.org/ontology/associatedBand',A)), L2),
  append(L1, L2, S).

load_bands(_, _, _, 5000) :-
  writeln("Maximum band load size exceeded").
load_bands(_, [Current|_], Current, _) :-
  writeln("Target band found."),
  rdf_load(Current, [format(xml)]).
load_bands(Visited, [Current|Open], Target, N) :-
  (connected(Target, _) -> 
    format("Solution found after ~w hops.\n", [N]),
    rdf_load(Target, [format(xml)]);
    (member(Current,Visited) -> 
      % current band is already loaded
      load_bands(Visited, Open, Target, N);
      % load current band 
      N1 is N + 1,
      load_band(Current, AddOpen),
      append(Open, AddOpen, UpdatedOpen),
      load_bands([Current|Visited], UpdatedOpen, Target, N1))).

band_paths(Band1, Band2) :-
  atom_concat('http://dbpedia.org/resource/', Band1, Q1),
  atom_concat('http://dbpedia.org/resource/', Band2, Q2),
  load_bands([], [Q1], Q2, 0).

% TODO: Find paths
% https://www.cpp.edu/~jrfisher/www/prolog_tutorial/2_15.html
connected(X, Y) :-
  rdf(X,'http://dbpedia.org/ontology/associatedBand',Y);
  rdf(Y,'http://dbpedia.org/ontology/associatedBand',X).

path(A,B,Path) :-
       travel(A,B,[A],Q), 
       reverse(Q,Path).

travel(A,B,P,[B|P]) :- 
       connected(A,B).
travel(A,B,Visited,Path) :-
       connected(A,C),           
       C \== B,
       \+member(C,Visited),
       travel(C,B,[C|Visited],Path). 


% I/O

% http://stackoverflow.com/a/8289523 (modified)
% export like: findall([A,B], (rdf(A,'http://dbpedia.org/ontology/associatedBand',B)), L1), write_pairs("band.pairs", L1).
loop_through_list(File, List) :-
    member(Element, List),
    [First, Second] = Element,
    write(File, First),
    write(File, ' '),
    write(File, Second),
    write(File, '\n'),
    fail.

write_pairs(Filename,List) :-
    open(Filename, write, File, [encoding(utf8)]),
    \+ loop_through_list(File, List),
    close(File).

% stdout
print_pairs([]).
print_pairs([H|T]) :-
  [F,S] = H,
  format("~w ~w\n", [F,S]),
print_pairs(T).