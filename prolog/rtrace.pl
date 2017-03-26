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
   rtrace(0),
   rtrace_break(0),
   quietly(0),
   ftrace(0).

%! on_f_rtrace( :Goal) is det.
%
% If :Goal fails trace it 
%
on_f_rtrace(Goal):- Goal*->true;rtrace_break(Goal).


%! on_x_debug( :Goal) is det.
%
% If there If Is an exception in :Goal then rtrace.
%
on_x_debug(Goal):- 
 notrace(((tracing;t_l:rtracing),maybe_leash(+exception))) 
  -> Goal
   ;
   catchv(Goal,E,(rtrace_break(Goal),throw(E))).


%! maybe_leash( +Flags) is det.
%
% Only leashes interactive consoles
%
maybe_leash(Some):- maybe_leash->leash(Some);true.

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

:- '$set_predicate_attribute'(get_trace_reset(_), trace, 0).
:- '$set_predicate_attribute'(get_trace_reset(_), hide_childs, 1).



%! push_guitracer is nondet.
%
% Save Guitracer.
%
push_guitracer:-  notrace(ignore(((current_prolog_flag(gui_tracer, GWas);GWas=false),asserta(t_l:wasguitracer(GWas))))).
:- '$hide'(push_guitracer/0).
:- '$set_predicate_attribute'(push_guitracer(), hide_childs, 1).


%! pop_guitracer is nondet.
%
% Restore Guitracer.
%
pop_guitracer:- notrace(ignore(((retract(t_l:wasguitracer(GWas)),set_prolog_flag(gui_tracer, GWas))))).
:- '$hide'(pop_guitracer/0).
:- '$set_predicate_attribute'(pop_guitracer(), hide_childs, 1).


%! push_tracer is det.
%
% Push Tracer.
%
push_tracer:- get_trace_reset(Reset)->asserta(t_l:tracer_reset(Reset)).
:- '$set_predicate_attribute'(push_tracer, trace, 0).
:- '$set_predicate_attribute'(push_tracer, hide_childs, 1).


%! pop_tracer is det.
%
% Pop Tracer.
%
pop_tracer:- notrace((retract(t_l:tracer_reset(Reset))->Reset;true)).
:- '$set_predicate_attribute'(pop_tracer, trace, 0).
:- '$set_predicate_attribute'(pop_tracer, hide_childs, 1).


%! reset_tracer is det.
%
% Reset Tracer.
%
reset_tracer:- notrace(ignore((t_l:tracer_reset(Reset)->Reset;true))).
:- '$set_predicate_attribute'(reset_tracer, trace, 0).
:- '$set_predicate_attribute'(reset_tracer, hide_childs, 1).


% Make sure interactive debugging is turned back on
user:prolog_exception_hook(error(_, _),_, _, _) :- 
     reset_tracer ->
     maybe_leash ->
     t_l:rtracing ->
     leash(+all),
     fail.

%! quietly( :Goal) is nondet.
%
% Unlike notrace/1, it allows nondet tracing 
%
% But also may be break when excpetions are raised during Goal.
%
quietly(Goal):- (notrace(tracing;t_l:rtracing),notrace)->each_call_cleanup(notrace,Goal,trace);Goal.
:- '$set_predicate_attribute'(quietly(_), hide_childs, 1).
:- '$set_predicate_attribute'(quietly(_), trace, 0).



%! rtrace is det.
%
% Start RTracer.
%
rtrace:- (notrace(t_l:rtracing) -> (visible(+all),trace) ; ((notrace,assert(t_l:rtracing),push_tracer,      
      push_guitracer,set_prolog_flag(gui_tracer,false),visible(+all),visible(+exception),
      maybe_leash(-all),maybe_leash(+exception),debug,trace))).

:- '$set_predicate_attribute'(rtrace, trace, 0).
:- '$set_predicate_attribute'(rtrace, hide_childs, 1).

%! rtrace is det.
%
% Start RTracer.
%
srtrace:- notrace, set_prolog_flag(access_level,system), rtrace.

:- '$set_predicate_attribute'(srtrace, trace, 0).
:- '$set_predicate_attribute'(srtrace, hide_childs, 1).



%! nortrace is det.
%
% Stop Tracer.
%
nortrace:- notrace,pop_guitracer,maybe_leash(-all),maybe_leash(+all),visible(+all),visible(+exception),maybe_leash(+exception),
  (retract(t_l:rtracing)->pop_tracer;true),!.

stop_rtrace:- ignore(retract(tl_rtrace:rtracing)),visible(+all),visible(+exception),maybe_leash(+all),maybe_leash(+exception).

:- '$set_predicate_attribute'(nortrace, trace, 0).
:- '$set_predicate_attribute'(nortrace, hide_childs, 1).



:- thread_local('$leash_visible'/2).

%! restore_trace( :Goal) is det.
%
% restore  Trace.
%
%! restore_trace( :Goal) is nondet.
%
% restore  Trace.
%
restore_trace(Goal):- !,
  each_call_cleanup(push_tracer,Goal,pop_tracer).
restore_trace(Goal):- 
  '$leash'(OldL, OldL),'$visible'(OldV, OldV),
   scce_orig(restore_leash_visible,
   ((Goal*-> (push_leash_visible, '$leash'(_, OldL),'$visible'(_, OldV)) ; fail)),
   ('$leash'(_, OldL),'$visible'(_, OldV))).

:- '$hide'(system:'$leash'/2).
:- '$hide'(system:'$visible'/2).

push_leash_visible:- '$leash'(OldL0, OldL0),'$visible'(OldV0, OldV0), asserta('$leash_visible'(OldL0,OldV0)).
restore_leash_visible:- once(rtrace('$leash_visible'(OldL1,OldV1))->('$leash'(_, OldL1),'$visible'(_, OldV1));true).

% restore_trace(Goal):- setup_call_cleanup(get_trace_reset(Reset),Goal,notrace(Reset)).
:- '$set_predicate_attribute'(restore_trace, trace, 0).
:- '$set_predicate_attribute'(restore_trace, hide_childs, 1).



%! rtrace( :Goal) is nondet.
%
% Trace a goal non-interactively until the first exception on
%  total failure
%
rtrace(Goal):- 
  push_tracer,!,rtrace,trace,
  ((Goal,notrace,deterministic(YN))*->
    (YN == true -> pop_tracer ; next_rtrace);
    ((notrace,pop_tracer,!,fail))).

:- '$set_predicate_attribute'(rtrace(_), trace, 0).
:- '$set_predicate_attribute'(rtrace(_), hide_childs, 0).


%! rtrace_break( :Goal) is nondet.
%
% Trace a goal non-interactively and break on first exception 
% or on total failure
%
rtrace_break(Goal):- \+ maybe_leash, !, rtrace(Goal).
rtrace_break(Goal):- stop_rtrace,trace,debugCallWhy(rtrace_break(Goal),Goal).
:- '$set_predicate_attribute'(rtrace_break(_), trace, 0).
:- '$set_predicate_attribute'(rtrace_break(_), hide_childs, 0).


next_rtrace:- (nortrace;(rtrace,trace,notrace(fail))).
:- '$set_predicate_attribute'(next_rtrace, hide_childs, 1).
:- '$set_predicate_attribute'(next_rtrace, trace, 0).



:- unlock_predicate(system:notrace/1).
%:- if_may_hide('$set_predicate_attribute'(quietly(_), trace, 0)).
%:- if_may_hide('$set_predicate_attribute'(system:notrace(_), hide_childs, 1)).
%:- if_may_hide('$set_predicate_attribute'(system:notrace(_), trace, 0)).
:- '$set_predicate_attribute'(system:tracing, trace, 0).
:- '$set_predicate_attribute'(system:notrace, trace, 0).
:- '$set_predicate_attribute'(system:trace, trace, 0).
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

:- fixup_exports.
:- '$hide'('$toplevel':save_debug).
:- '$hide'('$toplevel':residue_vars(_,_)).
:- '$hide'('$toplevel':save_debug).
:- '$hide'('$toplevel':no_lco).

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
        ftrace(0),        
        gftrace(0),
        ggtrace(0),
        grtrace(0),
	rtrace(0),
	hotrace(0).

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
   each_call_cleanup(RTRACE,(notrace,Goal),Undo).


% :- trace(hotrace/1, -all).       
% hotrace(Goal):- get_hotrace(Goal,Y),Y.
%:- mpred_trace_less(hotrace/1).
%:- '$set_predicate_attribute'(hotrace(_), hide_childs, 1).
%:- '$set_predicate_attribute'(hotrace(_), trace, 0).


:- thread_local(tl_rtrace:rtracing/0).


% =========================================================================

 

%! rtrace is nondet.
%
% R Trace.
%
rtrace:- notrace,push_guitracer,set_prolog_flag(gui_tracer,false),start_rtrace,trace. % push_guitracer,noguitracer

start_rtrace:- asserta(tl_rtrace:rtracing),visible(+all),visible(+exception),maybe_leash(-all),maybe_leash(+exception).



%! nortrace is nondet.
%
% Nor Trace.
%
nortrace:- notrace,stop_rtrace.

stop_rtrace:- ignore(retract(tl_rtrace:rtracing)),visible(+all),visible(+exception),maybe_leash(+all),maybe_leash(+exception).

push_tracer_and_notrace:- notrace,push_tracer,notrace.



%! rtrace( :Goal) is nondet.
%
% R Trace.
%
% rtrace(Goal):- hotrace(tl_rtrace:rtracing),!, Goal.

% rtrace(Goal):- wdmsg(rtrace(Goal)),!, restore_trace(each_call_cleanup(rtrace,(trace,Goal),nortrace)).

% rtrace(Goal):- notrace(tl_rtrace:rtracing),!,call(Goal).
rtrace(Goal):- !,setup_call_cleanup(start_rtrace,call((rtrace,Goal)),notrace(stop_rtrace)).
rtrace(Goal):- tracing,!,setup_call_cleanup(start_rtrace,call(Goal),stop_rtrace).
rtrace(Goal):- \+ tracing,start_rtrace,!,setup_call_cleanup(trace,call(Goal),(notrace,stop_rtrace)).
rtrace(Goal):- 
  ((tracing,notrace )-> Tracing = trace ;   Tracing = true),
   '$leash'(OldL, OldL),'$visible'(OldV, OldV),
   wdmsg(rtrace(Goal)),
   (Undo =   (((notrace,ignore(retract(tl_rtrace:rtracing)),'$leash'(_, OldL),'$visible'(_, OldV), Tracing)))),
   (RTRACE = ((notrace,asserta(tl_rtrace:rtracing),visible(+all),maybe_leash(-all),maybe_leash(+exception),trace))),!,
   each_call_cleanup(RTRACE,(trace,Goal),Undo).
/*
:- '$set_predicate_attribute'(system:call_cleanup(_,_), trace, 0).
:- '$set_predicate_attribute'(system:call_cleanup(_,_), hide_childs, 1).
:- '$set_predicate_attribute'(system:setup_call_cleanup(_,_,_), trace, 0).
:- '$set_predicate_attribute'(system:setup_call_cleanup(_,_,_), hide_childs, 1).
:- '$set_predicate_attribute'(system:setup_call_catcher_cleanup(_,_,_,_), trace, 0).
:- '$set_predicate_attribute'(system:setup_call_catcher_cleanup(_,_,_,_), hide_childs, 1).
*/
:- '$set_predicate_attribute'(hotrace(_), trace, 0).
:- '$set_predicate_attribute'(hotrace(_), hide_childs, 1).
:- '$set_predicate_attribute'(notrace(_), trace, 0).
:- '$set_predicate_attribute'(notrace(_), hide_childs, 1).
:- '$set_predicate_attribute'(rtrace(_), trace, 0).
:- '$set_predicate_attribute'(rtrace(_), hide_childs, 0).
:- '$set_predicate_attribute'(rtrace, trace, 0).
:- '$set_predicate_attribute'(rtrace, hide_childs, 1).
:- '$set_predicate_attribute'(nortrace, trace, 0).
:- '$set_predicate_attribute'(nortrace, hide_childs, 1).
:- '$set_predicate_attribute'(pop_tracer, trace, 0).
:- '$set_predicate_attribute'(pop_tracer, hide_childs, 1).
:- '$set_predicate_attribute'(tl_rtrace:rtracing, trace, 0).
:- '$set_predicate_attribute'(system:tracing, trace, 0).
:- '$set_predicate_attribute'(system:notrace, trace, 0).
:- '$set_predicate_attribute'(system:trace, trace, 0).
 :- meta_predicate  ftrace(0).





% :- mpred_trace_less(rtrace/0).
% :- mpred_trace_less(nortrace/0).
% :- mpred_trace_less(nortrace/0).
% :- mpred_trace_less(rtrace/0).

:- unlock_predicate(system:notrace/1).
% :- mpred_trace_less(system:notrace/1).
%:- '$set_predicate_attribute'(hotrace(_), trace, 0).
%:- '$set_predicate_attribute'(hotrace(_), hide_childs, 1).
% :- if_may_hide('$set_predicate_attribute'(hotrace(_), trace, 0)).
% :- if_may_hide('$set_predicate_attribute'(system:notrace(_), hide_childs, 1)).
% :- if_may_hide('$set_predicate_attribute'(system:notrace(_), trace, 0)).
:- '$hide'(notrace/1).
:- lock_predicate(system:notrace/1).


:- '$hide'(system:trace/0).
:- '$hide'(system:notrace/0).
:- '$hide'(system:tracing/0).

%:- ( listing(hotrace/1),redefine_system_predicate(system:notrace(_)), mpred_trace_none(hotrace(0)) ).
% :- if_may_hide('$set_predicate_attribute'(hotrace(_), trace, 0)).
% :- if_may_hide('$set_predicate_attribute'(hotrace(_), hide_childs, 1)).



%! on_x_debug( :GoalC) is nondet.
%
% If there If Is A an exception in  :Goal Class then r Trace.
%
on_x_debug(C):- !,
 notrace(((skipWrapper;tracing;(tl_rtrace:rtracing)),maybe_leash(+exception))) -> C;
   catchv(C,E,
     (wdmsg(on_x_debug(E)),catchv(rtrace(with_skip_bugger(C)),E,wdmsg(E)),dtrace(C))).
on_x_debug(Goal):- with_each(0,on_x_debug,Goal).

