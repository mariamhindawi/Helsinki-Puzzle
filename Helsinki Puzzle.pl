grid_build(N,M):-
               helper(N,N,[],M).
helper(_,0,A,A).
helper(N,C,A,M):-
		C>0,
		length(T,N),
		C1 is C-1,
		helper(N,C1,[T|A],M).



 
grid_gen(N,M):-
		grid_build(N, M),
                trans(M,M3),
       		acceptable_permutation(M,M3),
		num_gen(1, N, List),
      		generateMatrix(M, List).
	 
 generateMatrix([], _).
 generateMatrix([H|T], List):-
				generateRow(H, List),
				generateMatrix(T, List).

generateRow([], _).
generateRow([H|T], List):-
			member(H, List),
			generateRow(T, List).



num_gen(F,F,[F]).
num_gen(F,Y,[F|T]):-
      			  F=<Y,
			  F1 is F+1,
	                  num_gen(F1,Y,T).


check_num_grid(G):-
        	 length(G,N),
        	 flatten(G,List),
         	 max(List,Maximum),
      		 Maximum=<N,
		 num_gen(1,Maximum,L),
        	 subset(L,List).

max([T],T).
max([K,H|T],L2):-
		K>=H,
		max([K|T],L2).
max([K,H|T],L2):-
		K<H,
		max([H|T],L2).



acceptable_distribution(G):-
                          length(G,N),
                          helper2(G,N).
helper2(_,0).
helper2(G,N):-
		row(G,N,Res),
		coloumn(G,N,Res1),
		\+ Res == Res1,
		N>0,
		N1 is N-1,
		helper2(G,N1).
		row([H|_],1,H).

row([_|T],S,X):-
		S>1,
		S1 is S-1,
		row(T,S1,X).

coloumn([],_,[]).
coloumn([H|T],S,[H1|T1]):-
			row(H,S,H1),
			coloumn(T,S,T1).


row_col_match(M):-
	 
		 acceptable_distribution(M),
		 trans(M,M1),
         	 helper33(M,M1).


helper33(_,[]).
helper33(M,[H|T]):-
                 member(H,M),
                 helper33(M,T).
 

acceptable_permutation([],[]).
acceptable_permutation(L,Result):-
	                        permutation(L,Result),
				helper55(L,Result).

helper55([],[]).
helper55([H|T],[H1|T1]):-
	
			\+ H==H1,
			helper55(T,T1).



trans([],[]).
trans(M,M2):-
	    length(M,N),
	    helper1(M,N,[],M1),
	    reverse(M1,M2).
helper1(_,0,Ac,Ac).
helper1(M,N,Acc,M1):-
	           coloumn(M,N,Res),
 	           append(Acc,[Res],L),
	           N>0,
	           N1 is N - 1,
	           helper1(M,N1,L,M1).

 
distinct_rows(M):-
	length(M,N),
	getrow(M,N,N).
 
getrow(_,0,_).
getrow(M,N,C):-
	      row(M,N,R),
	      C>0,
	      C1 is C-1,
	      comparerow(M,R,C1),
	      N>0,
	      N1 is N-1,
	      getrow(M,N1,C1).
              comparerow(_,_,0).

comparerow([H|T],R,C1):-
	              \+ R==H,
	              C1>0,
	              C2 is C1-1,
	              comparerow(T,R,C2).



distinct_columns(M):-
	         length(M,N),
	         getcoloumn(M,N,N).

getcoloumn(_,0,_).
getcoloumn(M,N,C):-
		coloumn(M,N,R),
	        C>0,
		C1 is C-1,
		comparecoloumn(M,R,C1),
		N>0,
		N1 is N-1,
		getcoloumn(M,N1,C1).
        	comparecoloumn(_,_,0).

comparecoloumn(M,R,C1):-
			coloumn(M,C1,R1),
			\+ R==R1,
			C1>0,
			C2 is C1 -1,
			comparecoloumn(M,R,C2).


 
helsinki(N,M):-
	      grid_gen(N,M),
              check_num_grid(M),
	      acceptable_distribution(M),
	      distinct_rows(M),
	      distinct_columns(M),
              row_col_match(M).



	
 

 











