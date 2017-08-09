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
:- module(rtrace,
   [
      rtrace/1,  % Non-interactive tracing
      rtrace_break/1,  % Interactive tracing
      quietly/1,  % Non-det notrace/1
      restore_trace/1, % After call restor tracer
      rtrace/0, % Start non-intractive tracing
      srtrace/0, % Start non-intractive tracing at System level
      nortrace/0, % Stop non-intractive tracing
      push_tracer/0,pop_tracer/0,reset_tracer/0, % Reset Tracer to "normal"
      on_x_debug/1, % Non-intractive tracing when exception occurs 
      on_f_rtrace/1, % Non-intractive tracing when failure occurs 
      maybe_leash/1, % Set leash only when it makes sense
      maybe_leash/0,
      non_user_console/0,
      ftrace/1, % rtrace showing only failures
      push_guitracer/0,pop_guitracer/0
   ]).

:- set_module(class(library)).
:- module_transparent(nortrace/0).

:- thread_local(t_l:rtracing/0).
:- thread_local(t_l:tracer_reset/1).
:-thread_local(t_l:wasguitracer/1).
:-thread_local(t_l:wastracer/1).

:- meta_predicate
   rtrace(0),
   restore_trace(0),
   on_x_debug(0),
   on_f_rtrace(0),  
   
   rtrace_break(0),
   quietly(0),
   ftrace(0).

%! on_f_rtrace( :Goal) is det.
%
% If :Goal fails trace it 
%


% on_f_rtrace(Goal):-  Goal *-> true; ((nortrace,notrace,debugCallWhy(failed(on_f_rtrace(Goal)),Goal)),fail).

on_f_rtrace(Goal):-  Goal *-> true; (rtrace(Goal),debugCallWhy(on_f_rtrace(Goal),Goal)).



%! on_x_debug( :Goal) is det.
%
% If there If Is an exception in :Goal then rtrace.
%
on_x_debug(Goal):- 
 notrace(((tracing;t_l:rtracing),maybe_leash(+exception))) 
  -> Goal
   ;
   (catchv(Goal,E,(ignore(debugCallWhy(on_x_debug(E,Goal),Goal)),throw(E)))).
   

:- meta_predicate(maybe_hide(:)).
maybe_hide(M:P):- (current_prolog_flag(runtime_debug,N), N>1) -> true ; '$hide'(M:P). 

%! maybe_leash( +Flags) is det.
%
% Only leashes interactive consoles
%
maybe_leash(Some):- maybe_leash->leash(Some);true.
:- maybe_hide(maybe_leash/1).

maybe_leash:- \+ non_user_console, \+ current_prolog_flag(runtime_must,keep_going).
non_user_console:- \+ stream_property(current_input, tty(true)),!.
non_user_console:- \+ stream_property(current_input,close_on_abort(false)).

%! get_trace_reset( ?Reset) is det.
%
% Get Tracer.
%
get_trace_reset((notrace,set_prolog_flag(debug,WasDebug),CC3,'$visible'(_, OldV),'$leash'(_, OldL),RestoreTrace)):- 
     (notrace(tracing) -> (notrace,RestoreTrace = trace) ; RestoreTrace = notrace),
     '$leash'(OldL, OldL),'$visible'(OldV, OldV),
     (current_prolog_flag(debug,true)->WasDebug=true;WasDebug=false),     
     (current_prolog_flag(gui_tracer, GWas)->CC3=set_prolog_flag(gui_tracer, GWas);CC3=true),!,
     RestoreTrace.

:- maybe_hide(get_trace_reset/1).
:- maybe_hide(get_trace_reset/1).



%! push_guitracer is nondet.
%
% Save Guitracer.
%
push_guitracer:-  notrace(ignore(((current_prolog_flag(gui_tracer, GWas);GWas=false),asserta(t_l:wasguitracer(GWas))))).
:- maybe_hide(push_guitracer/0).


%! pop_guitracer is nondet.
%
% Restore Guitracer.
%
pop_guitracer:- notrace(ignore(((retract(t_l:wasguitracer(GWas)),set_prolog_flag(gui_tracer, GWas))))).
:- maybe_hide(pop_guitracer/0).


%! push_tracer is det.
%
% Push Tracer.
%
push_tracer:- get_trace_reset(Reset)->asserta(t_l:tracer_reset(Reset)).
:- maybe_hide(push_tracer/0).

%! pop_tracer is det.
%
% Pop Tracer.
%
pop_tracer:- notrace((retract(t_l:tracer_reset(Reset))->Reset;true)).
:- maybe_hide(pop_tracer/0).

%! reset_tracer is det.
%
% Reset Tracer.
%
reset_tracer:- notrace(ignore((t_l:tracer_reset(Reset)->Reset;true))).
:- maybe_hide(reset_tracer/0).


:- multifile user:prolog_exception_hook/4.
:- dynamic user:prolog_exception_hook/4.
:- module_transparent user:prolog_exception_hook/4.
% Make sure interactive debugging is turned back on
user:prolog_exception_hook(error(_, _),_, _, _) :- 
   notrace((  reset_tracer ->
     maybe_leash ->
     t_l:rtracing ->
     leash(+all),
     fail)).

%! quietly( :Goal) is nondet.
%
% Unlike notrace/1, it allows nondet tracing 
%
% But also may be break when excpetions are raised during Goal.
%
quietly(Goal):- !, Goal.
quietly(Goal):- tracing -> scce_orig(notrace,Goal,trace); Goal.
:- maybe_hide(quietly/1).


%! rtrace is det.
%
% Start RTracer.
%

rtrace:- notrace((start_rtrace,debug)),trace.

:- maybe_hide(rtrace/0).

start_rtrace:- 
      leash(-all),
      assert(t_l:rtracing),
      set_prolog_flag(access_level,system),
      push_guitracer,
      set_prolog_flag(gui_tracer,false),
      visible(+all),
      visible(+exception),
      maybe_leash(+exception).

:- maybe_hide(start_rtrace/0).

%! srtrace is det.
%
% Start RTracer.
%
srtrace:- notrace, set_prolog_flag(access_level,system), rtrace.

:- maybe_hide(srtrace/0).



%! nortrace is det.
%
% Stop Tracer.
%
stop_rtrace:- 
  notrace,
  maybe_leash(+all),
  visible(+all),
  maybe_leash(+exception),
  retractall(t_l:rtracing),
  !.
:- maybe_hide(stop_rtrace/0).

nortrace:- stop_rtrace,ignore(pop_tracer).

:- maybe_hide(nortrace/0).


:- thread_local('$leash_visible'/2).

%! restore_trace( :Goal) is det.
%
% restore  Trace.
%
%! restore_trace( :Goal) is nondet.
%
% restore  Trace.
%
restore_trace(Goal):- 
  setup_call_cleanup(
   push_leash_visible,
   scce_orig(push_tracer,Goal,pop_tracer),
   restore_leash_visible).

restore_trace0(Goal):- 
  '$leash'(OldL, OldL),'$visible'(OldV, OldV),
   scce_orig(restore_leash_visible,
   ((Goal*-> (push_leash_visible, '$leash'(_, OldL),'$visible'(_, OldV)) ; fail)),
   ('$leash'(_, OldL),'$visible'(_, OldV))).

:- maybe_hide(system:'$leash'/2).
:- maybe_hide(system:'$visible'/2).

push_leash_visible:- notrace((('$leash'(OldL0, OldL0),'$visible'(OldV0, OldV0), asserta('$leash_visible'(OldL0,OldV0))))).
restore_leash_visible:- notrace((('$leash_visible'(OldL1,OldV1)->('$leash'(_, OldL1),'$visible'(_, OldV1));true))).

% restore_trace(Goal):- setup_call_cleanup(get_trace_reset(Reset),Goal,notrace(Reset)).
:- maybe_hide(restore_trace/0).



%! rtrace( :Goal) is nondet.
%
% Trace a goal non-interactively until the first exception on
%  total failure
%
% ?- rtrace(member(X,[1,2,3])).
%    Call: (9) [lists] lists:member(_7172, [1, 2, 3])    
%    Unify: (9) [lists] lists:member(_7172, [1, 2, 3])   
%    Call: (10) [lists] lists:member_([2, 3], _7172, 1)  
%    Unify: (10) [lists] lists:member_([2, 3], 1, 1)     
%    Exit: (10) [lists] lists:member_([2, 3], 1, 1)      
%    Exit: (9) [lists] lists:member(1, [1, 2, 3])        
% X = 1 ;                                                
%    Redo: (10) [lists] lists:member_([2, 3], _7172, 1)  
%    Unify: (10) [lists] lists:member_([2, 3], _7172, 1) 
%    Call: (11) [lists] lists:member_([3], _7172, 2)     
%    Unify: (11) [lists] lists:member_([3], 2, 2)        
%    Exit: (11) [lists] lists:member_([3], 2, 2)         
%    Exit: (10) [lists] lists:member_([2, 3], 2, 1)      
%    Exit: (9) [lists] lists:member(2, [1, 2, 3])        
% X = 2 ;                                                
%    Redo: (11) [lists] lists:member_([3], _7172, 2)     
%    Unify: (11) [lists] lists:member_([3], _7172, 2)    
%    Call: (12) [lists] lists:member_([], _7172, 3)      
%    Unify: (12) [lists] lists:member_([], 3, 3)         
%    Exit: (12) [lists] lists:member_([], 3, 3)          
%    Exit: (11) [lists] lists:member_([3], 3, 2)         
%    Exit: (10) [lists] lists:member_([2, 3], 3, 1)      
%    Exit: (9) [lists] lists:member(3, [1, 2, 3])        
% X = 3.                                                 
%                                                        
%  ?- rtrace(fail).                                      
%    Call: (9) [system] fail                             
%    Fail: (9) [system] fail                             
% ^  Redo: (8) [rtrace] rtrace:rtrace(user:fail)
% false.

/*
  ?- rtrace((member(X,[writeln(1),throw(good),writen(failed)]),X)).
   Call: (10) [lists] lists:member(_13424, [writeln(1), throw(good), writen(failed)])
   Unify: (10) [lists] lists:member(_13424, [writeln(1), throw(good), writen(failed)])
   Call: (11) [lists] lists:member_([throw(good), writen(failed)], _13424, writeln(1))
   Unify: (11) [lists] lists:member_([throw(good), writen(failed)], writeln(1), writeln(1))
   Exit: (11) [lists] lists:member_([throw(good), writen(failed)], writeln(1), writeln(1))
   Exit: (10) [lists] lists:member(writeln(1), [writeln(1), throw(good), writen(failed)])
   Call: (10) [system] writeln(1)
1
   Exit: (10) [system] writeln(1)
X = writeln(1) ;
   Redo: (11) [lists] lists:member_([throw(good), writen(failed)], _13424, writeln(1))
   Unify: (11) [lists] lists:member_([throw(good), writen(failed)], _13424, writeln(1))
   Call: (12) [lists] lists:member_([writen(failed)], _13424, throw(good))
   Unify: (12) [lists] lists:member_([writen(failed)], throw(good), throw(good))
   Exit: (12) [lists] lists:member_([writen(failed)], throw(good), throw(good))
   Exit: (11) [lists] lists:member_([throw(good), writen(failed)], throw(good), writeln(1))
   Exit: (10) [lists] lists:member(throw(good), [writeln(1), throw(good), writen(failed)])
   Call: (10) [system] throw(good)
ERROR: Unhandled exception: good
*/

set_leash_vis(OldL,OldV):- '$leash'(_, OldL),'$visible'(_, OldV),!.
:- maybe_hide(set_leash_vis/2).

next_rtrace:- (nortrace;(rtrace,trace,notrace(fail))).
:- maybe_hide(next_rtrace/0).

rtrace(Goal):- notrace(tracing)-> rtrace0((trace,Goal)) ; setup_call_cleanup(true,rtrace0((trace,Goal)),stop_rtrace).

rtrace0(Goal):-
  push_tracer,!,rtrace,trace,
  ((Goal,notrace,deterministic(YN),true)*->
    (YN == true -> pop_tracer ; next_rtrace);
    ((notrace,pop_tracer,!,fail))).

:- maybe_hide(rtrace/1).
:- maybe_hide(rtrace0/1).
:- '$set_predicate_attribute'(rtrace/1, hide_childs, true).
:- '$set_predicate_attribute'(rtrace0/1, hide_childs, false).


%! rtrace_break( :Goal) is nondet.
%
% Trace a goal non-interactively and break on first exception 
% or on total failure
%
rtrace_break(Goal):- \+ maybe_leash, !, rtrace(Goal).
rtrace_break(Goal):- stop_rtrace,trace,debugCallWhy(rtrace_break(Goal),Goal).
%:- maybe_hide(rtrace_break/1).
:- '$set_predicate_attribute'(rtrace_break/1, hide_childs, false).



:- unlock_predicate(system:notrace/1).
%:- if_may_hide('$hide'(quietly/1)).
%:- if_may_hide('$hide'(system:notrace/1,  hide_childs, 1)).
%:- if_may_hide('$hide'(system:notrace/1)).
:- maybe_hide(system:tracing/0).
:- maybe_hide(system:notrace/0).
:- maybe_hide(system:trace/0).
:- lock_predicate(system:notrace/1).

%! ftrace( :Goal) is nondet.
%
% Functor Trace.
%
ftrace(Goal):- restore_trace((
   visible(-all),visible(+unify),
   visible(+fail),visible(+exception),
   maybe_leash(-all),maybe_leash(+exception),trace,Goal)).



:- ignore((source_location(S,_),prolog_load_context(module,M),module_property(M,class(library)),
 forall(source_file(M:H,S),
 ignore((functor(H,F,A),
  ignore(((\+ atom_concat('$',_,F),(export(F/A) , current_predicate(system:F/A)->true; system:import(M:F/A))))),
  ignore(((\+ predicate_property(M:H,transparent), module_transparent(M:F/A), \+ atom_concat('__aux',_,F),debug(modules,'~N:- module_transparent((~q)/~q).~n',[F,A]))))))))).

:- use_module(library(logicmoo_util_common)).
:- fixup_exports.
:- maybe_hide('$toplevel':save_debug).
:- maybe_hide('$toplevel':toplevel_call/1).
:- maybe_hide('$toplevel':residue_vars(_,_)).
:- maybe_hide('$toplevel':save_debug).
:- maybe_hide('$toplevel':no_lco).

end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.

% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/logicmoo_util_rtrace.pl
:- module(rtrace,
   [
      nortrace/0,
      pop_tracer/0,
      push_tracer/0,
      push_tracer_and_notrace/0,
      pop_guitracer/0,
      rtrace/0,
      stop_rtrace/0,
      start_rtrace/0,
      push_guitracer/0,
      reset_tracer/0,
      rtrace/1,  % dtrace why choice points are left over
      ftrace/1, % tells why a call didn't succeed once
      restore_trace/1,
      on_x_debug/1,
      hotrace/1,
      maybe_leash/1      
    ]).

:- meta_predicate
	catchv(0,-,0),
        restore_trace(0),
        on_x_debug(0),
        % % ftrace(0),        gftrace(0),ggtrace(0),grtrace(0),rtrace(0),hotrace(0).

:- module_transparent
      hotrace/1,
      nortrace/0,
      pop_tracer/0,
      push_tracer/0,
      push_tracer_and_notrace/0,
      reset_tracer/0,
      pop_guitracer/0,
      rtrace/0,      
      push_guitracer/0.

% 



%! maybe_leash( +Flags) is det.
%
% Only leashs the main thread
%
%maybe_leash(-Some):- thread_self_main->leash(-Some);true.
%maybe_leash(+Some):- thread_self_main->leash(+Some);true.
maybe_leash(Some):- thread_self_main->leash(Some);true.

:- meta_predicate hotrace(0).

hotrace(Goal):-!,call(Goal).
hotrace(Goal):-
   ((tracing,notrace )-> Tracing = trace ;   Tracing = true),
   '$leash'(OldL, OldL),'$visible'(OldV, OldV),
   (Undo =   notrace(((notrace,'$leash'(_, OldL),'$visible'(_, OldV), Tracing)))),
   (RTRACE = notrace((visible(-all),visible(+exception),maybe_leash(-all),maybe_leash(+exception)))),!,
   scce_orig(RTRACE,(notrace,Goal),Undo).


% :- trace(hotrace/1, -all).       
% hotrace(Goal):- get_hotrace(Goal,Y),Y.
%:- mpred_trace_less(hotrace/1).
%:- maybe_hide(hotrace/1).
%:- maybe_hide(hotrace/1).


:- thread_local(tl_rtrace:rtracing/0).


% =========================================================================

 

%! rtrace is nondet.
%
% R Trace.
%
rtrace:- notrace,push_guitracer,set_prolog_flag(gui_tracer,false),start_rtrace,trace. % push_guitracer,noguitracer

start_rtrace:- asserta(tl_rtrace:rtracing),visible(+all),visible(+exception),maybe_leash(-all),maybe_leash(+exception).
:- maybe_hide(start_rtrace/0).



%! nortrace is nondet.
%
% Nor Trace.
%
nortrace:- notrace,stop_rtrace.

stop_rtrace:- ignore(retract(tl_rtrace:rtracing)),
 visible(+all),visible(+exception),maybe_leash(+all),maybe_leash(+exception).
:- maybe_hide(stop_rtrace/0).

push_tracer_and_notrace:- notrace,push_tracer,notrace.



%! rtrace( :Goal) is nondet.
%
% R Trace.
%
% rtrace(Goal):- hotrace(tl_rtrace:rtracing),!, Goal.

% rtrace(Goal):- wdmsg(rtrace(Goal)),!, restore_trace(scce_orig(rtrace,(trace,Goal),nortrace)).

% rtrace(Goal):- notrace(tl_rtrace:rtracing),!,call(Goal).
rtrace(Goal):- !,scce_orig(rtrace,(trace,Goal),stop_rtrace).
/*
rtrace(Goal):- !,scce_orig(notrace(start_rtrace),call((notrace(rtrace),Goal)),notrace(stop_rtrace)).
rtrace(Goal):- tracing,!,setup_call_cleanup(start_rtrace,call(Goal),notrace(stop_rtrace)).
rtrace(Goal):- \+ tracing,start_rtrace,!,setup_call_cleanup(trace,call(Goal),(notrace,stop_rtrace)).
rtrace(Goal):- 
  ((tracing,notrace )-> Tracing = trace ;   Tracing = true),
   '$leash'(OldL, OldL),'$visible'(OldV, OldV),
   wdmsg(rtrace(Goal)),
   (Undo =   (((notrace,ignore(retract(tl_rtrace:rtracing)),'$leash'(_, OldL),'$visible'(_, OldV), Tracing)))),
   (RTRACE = ((notrace,asserta(tl_rtrace:rtracing),visible(+all),maybe_leash(-all),maybe_leash(+exception),trace))),!,
   scce_orig(RTRACE,(trace,Goal),Undo).
*/
/*
:- maybe_hide(system:call_cleanup/2).
:- maybe_hide(system:call_cleanup/2).
:- maybe_hide(system:setup_call_cleanup/3).
:- maybe_hide(system:setup_call_cleanup/3).
:- maybe_hide(system:setup_call_catcher_cleanup/4).
:- maybe_hide(system:setup_call_catcher_cleanup/4).
*/
:- maybe_hide(hotrace/1).
:- maybe_hide(notrace/1).
:- maybe_hide(rtrace/0).
:- maybe_hide(nortrace/0).
:- maybe_hide(pop_tracer/0).
:- maybe_hide(tl_rtrace:rtracing/0).
:- maybe_hide(system:tracing/0).
:- maybe_hide(system:notrace/0).
:- maybe_hide(system:trace/0).
 :- meta_predicate  ftrace(0).





% :- mpred_trace_less(rtrace/0).
% :- mpred_trace_less(nortrace/0).
% :- mpred_trace_less(nortrace/0).
% :- mpred_trace_less(rtrace/0).

:- unlock_predicate(system:notrace/1).
% :- mpred_trace_less(system:notrace/1).
%:- maybe_hide(hotrace/1).
%:- maybe_hide(hotrace/1).
% :- if_may_hide('$hide'(hotrace/1)).
% :- if_may_hide('$hide'(system:notrace/1,  hide_childs, 1)).
% :- if_may_hide('$hide'(system:notrace/1)).
:- maybe_hide(notrace/1).
:- lock_predicate(system:notrace/1).


:- maybe_hide(system:trace/0).
:- maybe_hide(system:notrace/0).
:- maybe_hide(system:tracing/0).

%:- ( listing(hotrace/1),redefine_system_predicate(system:notrace(_)), mpred_trace_none(hotrace(0)) ).
% :- if_may_hide('$hide'(hotrace/1)).
% :- if_may_hide('$hide'(hotrace/1,  hide_childs, 1)).



%! on_x_debug( :GoalC) is nondet.
%
% If there If Is A an exception in  :Goal Class then r Trace.
%
on_x_debug(C):- !,
 notrace(((skipWrapper;tracing;(tl_rtrace:rtracing)),maybe_leash(+exception))) -> C;
   catchv(C,E,
     (wdmsg(on_x_debug(E)),catchv(rtrace(with_skip_bugger(C)),E,wdmsg(E)),dtrace(C))).
on_x_debug(Goal):- with_each(0,on_x_debug,Goal).



/*

rtrace(Goal):- notrace((tracing,'$leash'(OldL, OldL),'$visible'(OldV, OldV))),start_rtrace,!,
   (scce_orig(trace,Goal,stop_rtrace)*-> set_leash_vis(OldL,OldV) ; notrace((set_leash_vis(OldL,OldV),!,fail))).
rtrace(Goal):- 
  setup_call_cleanup(
  ('$leash'(OldL, OldL),'$visible'(OldV, OldV),start_rtrace),
   scce_orig(start_rtrace,Goal,stop_rtrace),
   (notrace,set_leash_vis(OldL,OldV))).

rtrace(Goal):- notrace((tracing,'$leash'(OldL, OldL),'$visible'(OldV, OldV))),start_rtrace,!,
   (Goal*-> set_leash_vis(OldL,OldV) ; notrace((set_leash_vis(OldL,OldV),!,fail))).

rtrace(Goal):- '$leash'(OldL, OldL),'$visible'(OldV, OldV),start_rtrace,!,
   (Goal*-> set_leash_vis(OldL,OldV) ; notrace((set_leash_vis(OldL,OldV),!,fail))).

rtrace(Goal):- notrace(tracing),!, restore_trace(scce_orig(start_rtrace,(Goal*->notrace;(stop_rtrace,!,fail)),notrace(stop_rtrace))).
rtrace(Goal):- !, restore_trace(scce_orig(start_rtrace,(Goal*->notrace;(notrace,!,nortrace,fail)),notrace(stop_rtrace))).

rtrace(Goal):-
  push_tracer,!,rtrace,trace,
  ((Goal,notrace,deterministic(YN),)*->
    (YN == true -> pop_tracer ; next_rtrace);
    ((notrace,pop_tracer,!,fail))).
*/

