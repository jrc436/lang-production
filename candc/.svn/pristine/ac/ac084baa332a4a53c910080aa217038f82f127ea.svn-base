
:- module(resolveDRT,[resolveDRS/4,goldAntecedent/2]).

:- use_module(boxer(bindingViolation),[noBindingViolationDrs/1]).
:- use_module(boxer(freeVarCheck),[boundVarCheckContext/2,drsCondition/2]).
:- use_module(library(lists),[member/2,append/3,select/3]).
:- use_module(semlib(options),[option/2]).
:- use_module(semlib(errors),[warning/2,gold/2]).
:- use_module(boxer(categories),[att/3]).
:- use_module(knowledge(antecedent),[score/4]).


/* ========================================================================
   Dynamic Predicate
======================================================================== */

:- dynamic antecedent/2.


/* ========================================================================
   Managing Gold Standard Antecedents
======================================================================== */

goldAntecedent(Index,Att):-
   att(Att,antecedent,Antecedent), 
   number(Antecedent), !,
%  write(antecedent(Index,Antecedent)),nl,
   assert(antecedent(Index,Antecedent)).

goldAntecedent(_,_).


/* ========================================================================
   resolveDRS(+PDRS,     % Projective Discourse Representation Structure
              +C1-C2,    % Difference List of Contexts
              +P1-P2,    % Difference List of Presuppositions
              +T1-T2).   % Tags (token information)
======================================================================== */

resolveDRS(B,C1-C2,P1-P2,Tags):- 
   option('--resolve',true), !, 
   copy_term(Tags,L-[]), 
   setof(X,T^member(X:T,L),IDs),
   resolvePDRS(B,C1-C2,P1-P2,IDs).

resolveDRS(_,C-C,P-P,_).


/* ========================================================================
   resolvePDRS(+PDRS,
              +C1-C2, % Context is a difference list of pointed DRSs
              +P1-P2, % Presuppsitions
              +T1-T2) % Tags
======================================================================== */

resolvePDRS(sdrs([],_),C-C,P-P,_):- !.

resolvePDRS(sdrs([lab(_,B)|L],C),C1-C3,P1-P3,IDs):- !,
   resolvePDRS(B,C1-C2,P1-P2,IDs),
   resolvePDRS(sdrs(L,C),C2-C3,P2-P3,IDs).

resolvePDRS(sdrs([sub(B1,B2)|L],C),C1-C3,P1-P4,IDs):- !,
   resolvePDRS(B1,C1-C2,P1-P2,IDs),
   resolvePDRS(B2,C2-_,P2-P3,IDs),
   resolvePDRS(sdrs(L,C),C2-C3,P3-P4,IDs).

resolvePDRS(merge(B1,B2),C1-C3,P1-P3,IDs):- !,
   resolvePDRS(B1,C1-C2,P1-P2,IDs),
   resolvePDRS(B2,C2-C3,P2-P3,IDs).

resolvePDRS(lab(_,B),Context,P,IDs):- !,
   resolvePDRS(B,Context,P,IDs).

resolvePDRS(K:B,C1-C2,P1-P3,IDs):-
   anaphoric(K:B,ADRS,C1,P1), !,                                  %%% if there is a free pointer
   project([K:B|C1],ADRS,P1,P1-P2,[K:B|C1],[],IDs),               %%% then resolve it
   resolvePDRS(K:B,C1-C2,P2-P3,IDs).

resolvePDRS(K:drs(D,C),C1-[K:drs(D,C)|C1],P,IDs):- !,
   resolveConds(C,[K:drs(D,C)|C1],P,IDs).

resolvePDRS(U,C-C,P-P,_):- 
   warning('unknown DRS in resolvePDRS/4: ~p',[U]).


/* ========================================================================
   Resolve Conditions
======================================================================== */

resolveConds([],_,P-P,_):- !.

resolveConds([_:C|L],Context,P,IDs):- !, 
   resolveConds([C|L],Context,P,IDs).

resolveConds([not(B)|C],Context,P1-P3,IDs):- !,
   resolvePDRS(B,Context-_,P1-P2,IDs),
   resolveConds(C,Context,P2-P3,IDs).

resolveConds([nec(B)|C],Context,P,IDs):- !,
   resolveConds([not(B)|C],Context,P,IDs).

resolveConds([pos(B)|C],Context,P,IDs):- !,
   resolveConds([not(B)|C],Context,P,IDs).

resolveConds([prop(_,B)|C],Context,P,IDs):- !,
   resolveConds([not(B)|C],Context,P,IDs).

resolveConds([imp(B1,B2)|C],C1,P1-P4,IDs):- !,
   resolvePDRS(B1,C1-C2,P1-P2,IDs),
   resolvePDRS(B2,C2-_,P2-P3,IDs),
   resolveConds(C,C1,P3-P4,IDs).

resolveConds([duplex(_,B1,_,B2)|C],Context,P,IDs):- !,
   resolveConds([imp(B1,B2)|C],Context,P,IDs).

resolveConds([or(B1,B2)|C],C1,P1-P4,IDs):- !,
   resolvePDRS(B1,C1-_,P1-P2,IDs),
   resolvePDRS(B2,C1-_,P2-P3,IDs),
   resolveConds(C,C1,P3-P4,IDs).

resolveConds([_|C],Context,P,IDs):- !,
   resolveConds(C,Context,P,IDs).


/* ========================================================================
   Identify Anaphoric Material (free pointers)

   K1 = K2:X K3:Y
        K2:dog(X,Y)
        K3:male(Y)
        K1:walks(X)
======================================================================== */

anaphoric(P:drs(PDom,PCon),F:drs(FDom,FCon),Context,Presups):-
   member(F:_:_,PDom), \+ P==F,       % pick a free pointer (of DRS domain) 
   \+ (member(K:_,Context), K==F),    % should not be in context (that would mean it's resolved already)
   \+ (member(K:_,Presups), K==F),    % should not be in presuppositions (would mean it's resolved already)
   anaphoricSet(PDom,F,FDom),
   anaphoricSet(PCon,F,FCon),
   noFreeVars(FCon,P,PDom), !.


/* ========================================================================
   Check for bound variable
======================================================================== */

boundVar(X,P1,Dom):-
   member(P2:_:Y,Dom), 
   X==Y, !, \+ P1==P2.
  
boundVar(_,_,_).


/* ========================================================================
   Check if there are no free variables
======================================================================== */

noFreeVars([],_,_).

noFreeVars([F:_:rel(X,Y,_,_)|L],P,Dom):- !,
   (boundVar(X,P,Dom);boundVar(X,F,Dom)),
   (boundVar(Y,P,Dom);boundVar(Y,F,Dom)),
   noFreeVars(L,P,Dom).

noFreeVars([_|L],P,Dom):- !,
  noFreeVars(L,P,Dom).


/* ========================================================================
   Compute Anaphoric Material
======================================================================== */

anaphoricSet([],_,[]).
anaphoricSet([P:E|L1],F,[P:E|L2]):- P==F, !, anaphoricSet(L1,F,L2).
anaphoricSet([_|L1],F,L2):- anaphoricSet(L1,F,L2).


/* ========================================================================
   Projection -- try to bind, else accommodate

   project(+List of Context DRSs (Possible antecedents),
           +Anaphoric DRS,
           +List of presuppositions seen so far (could act as antecedents),
           +Pair of Ingoing and Output List of Presuppositions
           +List of DRSs (local DRS + context DRS, to check for binding violations)
           -Accumulator of solution/4,
           -List of IDs to compute proximity)
======================================================================== */

% No further context DRSs, no presupposed DRSs, but earlier binding
% solutions; so pick most probable solution
%
project([],B,[],P1-P2,Bs,Solutions,_):-                        % Tried all possibilities
   sort([solution(0.94,_,_,free)|Solutions],Sorted),           % Sort on score
   ( 
     B = _:drs(_,[_:Pro:pred(_,female,n,2)]),
     \+ member(solution(_,_,_,bow),Solutions),
     option('--semantics',pdrs), option('--roles',verbnet),
     option('--modal',true), option('--tense',true),
     option('--theory',sdrt), !,
     warning('no gold antecedent for pronoun ~p',[Pro])
   ; true ),
   best(Sorted,Bs,B,P1-P2), !.                                 % Add global accommodation as possibility (free)

% No further context DRSs, try a presupposed DRS as antecedent
%
project([],K2:B2,[K1:drs([K0:_:X|D],C)|P],P1-P2,Bs,Solutions,IDs):-
   K1==K0,                                 % Antecedent DRS from context
   match(K1,C,X,B2,IDs,Bs,Y,Score,Ant), !,    % Match antecedent with anaphoric DRS
   project([],K2:B2,[K1:drs(D,C)|P],P1-P2,Bs,[solution(Score,K1:X,K2:Y,Ant)|Solutions],IDs).

% No further context DRSs, try accommodation in presupposition
%
project([],K2:B2,[K1:drs([],_)|P],P1-P2,Bs,Solutions,IDs):- !,
   project([],K2:B2,P,P1-P2,Bs,[solution(0.91,K1:_,K2:_,global)|Solutions],IDs).

% Try next presupposed DRS
%
project([],K,[_|P],P1-P2,Bs,Solutions,IDs):- !,
   project([],K,P,P1-P2,Bs,Solutions,IDs).

% Match antecedent with anaphoric DRS
% Look in same DRS for other antecedent
% 
project([K1:drs([K0:_:X|D],C)|Context],K2:B2,P,P1-P2,Bs,Solutions,IDs):-      
   K1==K0,
   match(K1,C,X,B2,IDs,Bs,Y,Score,Source), !,
   project([K1:drs(D,C)|Context],K2:B2,P,P1-P2,Bs,[solution(Score,K1:X,K2:Y,Source)|Solutions],IDs).

% Try next discourse referent (in case matching failed)
%
project([K1:drs([_|D],C)|Context],A,P,P1-P2,Bs,Solutions,IDs):- !,
   project([K1:drs(D,C)|Context],A,P,P1-P2,Bs,Solutions,IDs).

% Tried all discourse referents, accommodate (non-global)
% and go on with next context DRS
%
project([K1:drs([],_)|Context],K2:B2,P,P1-P2,Bs,Solutions,IDs):- !,
   length(Context,Levels), Prob is 0.05/(Levels + 1), Score is 1-Prob,
   project(Context,K2:B2,P,P1-P2,Bs,[solution(Score,K1:_,K2:_,local)|Solutions],IDs).

% Try next context DRS (all other cases)
%
project([_|Context],A,P,P1-P2,Bs,Solutions,IDs):- !,  % first argument can be an SDRS?
   project(Context,A,P,P1-P2,Bs,Solutions,IDs).


/* ========================================================================
   Best (sorted on score, the lower the better!)
======================================================================== */   

best([Solution|_],Bs,ADRS,P-[ADRS|P]):-         % DRS with free pointer
   Solution = solution(_Score,_,_,free),        % hence add to list of presuppositions
   append(Bs,[ADRS|P],Context),
   boundVarCheckContext(Context,ADRS), !.

best([Solution|_],Bs,ADRS,P-P):- 
   Solution = solution(_Score,X,Y,Reason),
   member(Reason,[local,global]),
   append(Bs,P,Context),
   \+ \+ (X=Y, boundVarCheckContext(Context,ADRS)), !, 
   X=Y.

best([Solution|_],Bs,ADRS,P1-P2):- 
   Solution = solution(_Score,X,Y,Reason),
   \+ member(Reason,[local,global,free]),
   append(Bs,P1,Context),
   \+ \+ (X=Y,                                  % if unifying X with Y does not
          boundVarCheckContext(Context,ADRS),   % yield any free variables
          noBindingViolationDrs(Bs)), !,        % or binding violations
   X=Y,                                         % then do so
   updatePresups(P1,ADRS,P2).

best([_|L],Bs,ADRS,P):- best(L,Bs,ADRS,P).


/* ========================================================================
   Update Presuppositions
======================================================================== */   

updatePresups([],_,[]).
updatePresups([K:drs(D1,C1)|L],P:drs(D2,C2),[K:drs(D3,C3)|L]):- P==K, !, append(D1,D2,D3), append(C1,C2,C3).
updatePresups([B|L1],P,[B|L2]):- updatePresups(L1,P,L2).


/* ========================================================================
   Check if there is gold standard data available
======================================================================== */   

goldAntecedentIndex(Conds,AnaInd,AntInd):- 
   antecedent(AnaInd,AntInd),               % there is a gold antecedent
   member( _:AnaInd:_,Conds), !.            % for the current anaphoric expression


/* ========================================================================
   Match antecedent with presupposition

   match(+Label of Antecedent DRS,
         +Conditions of Antecedent DRS,
         +Referent of Antecedent DRS,
         +Unlabeled Anaphoric DRS,
         +List of Token IDs,
         +List of Context DRSs
         -Referent of Anaphoric DRS,
         -Matching Score,
         -Matching Type)

======================================================================== */   

% There is a gold-standard antecedent available; take this as antecedent
%
match(K1,C1,X,drs([_:_:Y|_],C2),IDs,Bs,Y,0,bow):-
   goldAntecedentIndex(C2,I2,AntInd),
   member(K2:I1:Ant,C1), K1==K2,     % The antecedent is part of the 
   member(AntInd,I1),                % DRS under consideration
   drsCondition(Z,Ant), Z==X, !,     % and has the right antecedent referent
   refConditions(X,Bs,[]-XConds),
   refConditions(Y,Bs,[]-YConds),
   proximity(I1,I2,IDs,Prox),
   gold('ana_ant(~q,~q,~p). % Antecedent: ~p; Anaphor: ~p.',[YConds,XConds,Prox,I1,I2]).

% Experimental version of pronoun resolution (vector space model)
%
match(K1,C1,X1,drs([_:_:Y1|_],C2),IDs,Bs,Y2,NewScore,new):-
   option('--x',true),                     
   member( _:I2:Ana,C2),                   % get anaphor condition 
   \+ I2=[],                               % with position info
   drsCondition(Y2,Ana), Y1==Y2,           % and proper DRS condition
   member(K2:I1:Ant,C1), K1==K2, \+ I1=[], % get antecedent condition
   drsCondition(X2,Ant), X1==X2,           % make sure it really is an antecedent condition
   refConditions(X1,Bs,[]-Conds), 
   proximity(I1,I2,IDs,Prox), Prox > 0,
   score(C2,Conds,Prox,Score),
   noConflicts(Y1,C2,X1,C1), !,
   NewScore is 1-Score.

% Old rule-based algorithm
%
match(K1,C1,X,drs(_,C2),_IDs,_Bs,Y,NewScore,P):-
   member( _:_:Ana,C2),
   member(K2:_:Ant,C1),          K1==K2, 
   matching(Y^Ana,Z^Ant,Score,P), Z==X,
   noConflicts(Y,C2,X,C1), !,
   NewScore is 1-Score.              % inverse score for sorting purposes


/* ========================================================================
   Calculate Proximity
======================================================================== */   

proximity([X],[Y],IDs,P):- number(X), number(Y), X<Y, !, from(IDs,X,Y,P).
proximity(_  ,_  ,_  ,0).

from([],_,_,0).
from([X|L],X,Y,D):- !, to(L,Y,0,D).
from([_|L],X,Y,D):- from(L,X,Y,D).

to([X|_],X,D1,D2):- !, D2 is D1 + 1.
to([_|L],X,D1,D2):- D is D1 + 1, to(L,X,D,D2).


/* ========================================================================
   Get conditions for a specific discourse referent
======================================================================== */   

refConditions(X,[K:drs(D,C1)|L],L1-L2):-
   select(_:_:C,C1,C2), 
   member(C,[pred(Z,_,_,_),named(Z,_,_,_),role(_,Z,_,1),role(Z,_,_,-1)]), Z==X, !,
   refConditions(X,[K:drs(D,C2)|L],[C|L1]-L2).

refConditions(X,[K:drs(D,C1)|L],L1-L3):-
   select(_:_:eq(Z,Y),C1,C2), Z==X, !,
   refConditions(X,[K:drs(D,C2)|L],L1-L2),
   refConditions(Y,[K:drs(D,C2)|L],L2-L3).

refConditions(X,[_|L],L1-L2):- !, refConditions(X,L,L1-L2).
    
refConditions(_,[],L-L):- \+ L = [].


/* ========================================================================
   Check for Conflicts
======================================================================== */   

noConflicts(X,AnaConds,Y,AntConds):-                    
    \+ \+ ( X=Y,                                           % resolving must
            \+ ( member(_:_:not(_:drs(_,C0)),AntConds),    % not result in X=X
                 member(_:_:eq(A,B),C0),                   % in a negated DRS
                 A==X, B==X ),
            \+ ( member(_:_:pred(A,male,_,_),AnaConds),    % not result in
                 member(_:_:pred(B,female,_,_),AntConds),  % hermaphrodites
                 A==X, B==X ),                             
            \+ ( member(_:_:pred(A,female,_,_),AnaConds),
                 member(_:_:pred(B,male,_,_),AntConds),
                 A==X, B==X ) ).


/* ========================================================================
   Matching (anaphor, antecedent)
======================================================================== */   

% time
matching(Y^pred(Y,now,a,1),Z^pred(Z,now,a,1),0.99,a:now).

% he
matching(Y^pred(Y,male,n,2),Z^named(Z,S,per,_),0.9,per:S).
matching(Y^pred(Y,male,n,2),Z^named(Z,S,_,_),0.1,per:S).
matching(Y^pred(Y,male,n,2),Z^pred(Z,male,n,2),0.99,n:male).
matching(Y^pred(Y,male,n,2),Z^pred(Z,S,n,_),0.5,n:S):-  option('--x',false).
matching(Y^pred(Y,male,n,2),Z^card(Z,_,_),0.1,card):- option('--x',false).

% she
matching(Y^pred(Y,female,n,2),Z^named(Z,S,per,_),0.9,per:S).
matching(Y^pred(Y,female,n,2),Z^named(Z,S,_,_),0.1,per:S).
matching(Y^pred(Y,female,n,2),Z^pred(Z,female,n,2),0.99,n:female).
matching(Y^pred(Y,female,n,2),Z^pred(Z,S,n,_),0.5,n:S):-  option('--x',false).
matching(Y^pred(Y,female,n,2),Z^card(Z,_,_),0.1,card):- option('--x',false).

% it
matching(Y^pred(Y,neuter,a,_),Z^named(Z,S,per,_),0.1,per:S).
matching(Y^pred(Y,neuter,a,_),Z^named(Z,S,_,_),0.8,per:S).
matching(Y^pred(Y,neuter,a,_),Z^pred(Z,neuter,a,_),0.99,a:neuter).
matching(Y^pred(Y,neuter,a,_),Z^pred(Z,S,n,_),0.5,n:S).

% they, them, theirs, this, that, those, these
matching(Y^pred(Y,thing,n,12),Z^pred(Z,S,n,_),0.5,n:S):-  option('--x',false).
matching(Y^pred(Y,thing,n,12),Z^named(Z,S,_,_),0.1,per:S):- option('--x',false).

% I, me, mine, you, yours, we, us, ours, myself, yourself, ourselves
matching(Y^pred(Y,person,n,1),Z^pred(Z,S,n,_),0.1,n:S):-    option('--x',false).
matching(Y^pred(Y,person,n,1),Z^named(Z,S,per,_),0.8,per:S):- option('--x',false).
matching(Y^pred(Y,person,n,1),Z^named(Z,S,_,_),0.5,per:S):-   option('--x',false).

% the
matching(Y^pred(Y,S,n,_),Z^pred(Z,S,n,_),0.9,n:S).

% names
matching(Y^named(Y,S,T,_),Z^named(Z,S,T,_),0.9,per:S).
matching(Y^named(Y,S,_,_),Z^named(Z,S,_,_),0.7,per:S).

% timex
matching(Y^timex(Y,date(_:D1,_:D2,_:D3,_:D4)),Z^timex(Z,date(_:D1,_:D2,_:D3,_:D4)),0.9,timex).
