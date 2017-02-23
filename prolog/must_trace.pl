/* Part of LogicMOO Base Logicmoo Debug Tools
% ===================================================================
% File '$FILENAME.pl'
% Purpose: An Implementation in SWI-Prolog of certain debugging tools
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: '$FILENAME.pl' 1.0.0
% Revision: $Revision: 1.1 $
% Revised At:  $Date: 2002/07/11 21:57:28 $
% Licience: LGPL
% ===================================================================
*/

:- module(must_trace,
   [
      must/1, % Goal must succeed at least once once
      must_once/1, % Goal must succeed at most once
      must_det/1, % Goal must succeed determistically
      sanity/1,  % like assertion but adds trace control
      nop/1, % syntactic comment
      scce_orig/3
    ]).

:- meta_predicate
        must(0),
        must_once(0),
        must_det(0),
        nop(*),
        sanity(0),
        scce_orig(0,0,0).

:- set_module(class(library)).

:- reexport(library('first')).
:- reexport(library('ucatch')).
:- reexport(library('dmsg')).
:- reexport(library('rtrace')).
:- reexport(library('bugger')).
:- reexport(library('dumpst')).
:- reexport(library('frames')).

:- use_module(library(debug)).

% TODO Make a speed,safety,debug Triangle instead of these flags
:- create_prolog_flag(runtime_must,debug,[]).


%! must(:Goal) is nondet.
%
% Goal must succeed at least once once
%
% Wrap must/1 over parts of your code you do not trust
% If your code fails.. it will rewind to your entry block (at the scope of this declaration) and invoke rtrace/1 .
% If there are 50 steps to your code, it will save you from pushing `creep` 50 times.  
% Instead it turns off the leash to allow you to trace with your eyeballs instead of your fingers.
%
%% must( :Goal) is semidet.
%
% Must Be Successfull.
%

% must(Goal):- \+ flag_call(runtime_debug == true) ,flag_call(unsafe_speedups == true) ,!,call(Goal).
% must(Call):- !, (repeat, (catchv(Call,E,(dmsg(E:Call),fail)) *-> true ; (ignore(rtrace(Call)),leash(+all),repeat,wdmsg(failed(Call)),trace,Call))).

must(Goal):- skipWrapper,!, (Goal *-> true;throw(failed_must(Goal))).
must(Goal):- current_prolog_flag(runtime_must,How),!,
          (How == speed -> call(Goal);
           How == debug -> on_f_rtrace(Goal);
           How == keep_going -> ignore(on_f_rtrace(Goal));
           on_f_rtrace(Goal)).
must(Goal):-  get_must(Goal,MGoal),!,call(MGoal).
must(Goal):- Goal*->true;prolog_debug:assertion_failed(fail, must(Goal)).

%! sanity(:Goal) is det.
%
% Optional Sanity Checking.
%
% like assertion/1 but adds trace control
%

sanity(_):- current_prolog_flag(runtime_safety,0),!.

sanity(Goal):- \+ tracing,
   \+ current_prolog_flag(runtime_safety,3),
   \+ current_prolog_flag(runtime_debug,0),
   (current_prolog_flag(runtime_speed,S),S>1),
   !,
   (1 is random(10)-> must(Goal) ; true).
sanity(Goal):- quietly(Goal),!.
sanity(_):- dumpST,fail.
sanity(Goal):- tlbugger:show_must_go_on,!,dmsg(show_failure(sanity,Goal)).
sanity(Goal):- setup_call_cleanup(wdmsg(begin_FAIL_in(Goal)),rtrace(Goal),wdmsg(end_FAIL_in(Goal))),!,dtrace(assertion(Goal)).

%! must_once(:Goal) is det.
%
% Goal must succeed at most once
%
must_once(Goal):- must(Goal),!.


%! must_det(:Goal) is det.
%
% Goal must succeed determistically
%

% must_det(Goal):- current_prolog_flag(runtime_safety,0),!,must_once(Goal).
must_det(Goal):- \+ current_prolog_flag(runtime_safety,3),!,must_once(Goal).
must_det(Goal):- must_once(Goal),!.
/*
must_det(Goal):- must_once((Goal,deterministic(YN))),(YN==true->true;dmsg(warn(nondet_exit(Goal)))),!.
must_det(Goal):- must_once((Goal,deterministic(YN))),(YN==true->true;throw(nondet_exit(Goal))).
*/

%! nop( :Goal) is det.
%
%  Comments out code without losing syntax
%
nop(_).


/*
scce_orig(Setup,Goal,Cleanup):-
   \+ \+ '$sig_atomic'(Setup), 
   catch( 
     ((Goal, deterministic(DET)),
       '$sig_atomic'(Cleanup),
         (DET == true -> !
          ; (true;('$sig_atomic'(Setup),fail)))), 
      E, 
      ('$sig_atomic'(Cleanup),throw(E))). 
*/

:- abolish(system:scce_orig,3).
system:scce_orig(Setup,Goal,Cleanup):-
   \+ \+ '$sig_atomic'(Setup), 
   catch( 
     ((Goal, deterministic(DET)),
       '$sig_atomic'(Cleanup),
         (DET == true -> !
          ; (true;('$sig_atomic'(Setup),fail)))), 
      E, 
      ('$sig_atomic'(Cleanup),throw(E))). 




