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
      restore_guitracer/0,
      rtrace/0,
      stop_rtrace/0,
      start_rtrace/0,
      save_guitracer/0,
      reset_tracer/0,
      rtrace/1,  % dtrace why choice points are left over
      ftrace/1, % tells why a call didn't succeed once
      restore_trace/1,
      on_x_rtrace/1,
      hotrace/1,
      maybe_leash/1      
    ]).

:- meta_predicate
	catch_safe(0,-,0),
        restore_trace(0),
        on_x_rtrace(0),
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
      restore_guitracer/0,
      rtrace/0,      
      save_guitracer/0.

% :- include('logicmoo_util_header.pi').

%! catch_safe( :GoalGoal, ?E, :GoalRecovery) is nondet.
%
% Catchv.
%
catch_safe(Goal,E,Recovery):- nonvar(E) -> catch(Goal,E,Recovery); % normal mode (the user knows what they want)
                         catch(Goal,E,(bubbled_ex_check(E),Recovery)). % prevents promiscous mode

% catch_safe(Goal,E,Recovery):- catch(Goal,E,(bubbled_ex_check(E),Recovery)). % prevents promiscous mode


%! bubbled_ex( ?VALUE1) is nondet.
%
% Bubbled Ex.
%
bubbled_ex(block(_,_)).
bubbled_ex('$aborted').
bubbled_ex('time_limit_exceeded').
bubbled_ex('$time_limit_exceeded').



%! bubbled_ex_check( ?E) is nondet.
%
% Bubbled Ex Check.
%
bubbled_ex_check(E):- ( \+ bubbled_ex(E)),!.
bubbled_ex_check(E):-throw(E).




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

 
:-thread_local(t_l:wasguitracer/1).
:-thread_local(t_l:wastracer/1).



%! save_guitracer is nondet.
%
% Save Guitracer.
%
save_guitracer:- ignore(((current_prolog_flag(gui_tracer, GWas);GWas=false),asserta(t_l:wasguitracer(GWas)))).



%! restore_guitracer is nondet.
%
% Restore Guitracer.
%
restore_guitracer:- ignore((retract(t_l:wasguitracer(GWas)),set_prolog_flag(gui_tracer, GWas))).




%! rtrace is nondet.
%
% R Trace.
%
rtrace:- notrace,save_guitracer,set_prolog_flag(gui_tracer,false),start_rtrace,trace. % save_guitracer,noguitracer

start_rtrace:- asserta(tl_rtrace:rtracing),visible(+all),visible(+exception),maybe_leash(-all),maybe_leash(+exception).



%! nortrace is nondet.
%
% Nor Trace.
%
nortrace:- notrace,stop_rtrace.

stop_rtrace:- ignore(retract(tl_rtrace:rtracing)),visible(+all),visible(+exception),maybe_leash(+all),maybe_leash(+exception).

push_tracer_and_notrace:- notrace((push_tracer,notrace)).
   


%! push_tracer is nondet.
%
% Push Tracer.
%
push_tracer:- hotrace((get_tracer(Reset),asserta(t_l:wastracer(Reset)))),!.



%! pop_tracer is nondet.
%
% Pop Tracer.
%
pop_tracer:- notrace((retract(t_l:wastracer(Reset)),!,Reset)),!.



%! reset_tracer is nondet.
%
% Reset Tracer.
%
reset_tracer:- notrace((ignore(((t_l:wastracer(Reset),Reset))))).
 



%! get_tracer( ?Reset) is nondet.
%
% Get Tracer.
%
get_tracer(Reset):- 
    Reset =  ((set_prolog_flag(debug,WasDebug),CC,CC2,'$leash'(_, OldL),'$visible'(_, OldV))),
    '$leash'(OldL, OldL),'$visible'(OldV, OldV),
     (current_prolog_flag(debug,true)->WasDebug=true;WasDebug=false),
     (tl_rtrace:rtracing->CC2=rtrace;CC2= nortrace),
     (tracing -> CC = trace ; CC = notrace),!.

    

 :- meta_predicate  restore_trace(0).



%! restore_trace( :GoalGoal) is nondet.
%
% restore  Trace.
%
restore_trace(Goal):- 
  each_call_cleanup(push_tracer,Goal,pop_tracer).


 :- meta_predicate  rtrace(0).




%! rtrace( :GoalGoal) is nondet.
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


:- export(on_x_rtrace/1).



%! on_x_rtrace( :GoalC) is nondet.
%
% If there If Is A an exception in  :Goal Class then r Trace.
%
on_x_rtrace(C):- 
 notrace(((skipWrapper;tracing;(tl_rtrace:rtracing)),maybe_leash(+exception))) -> C;
   catch_safe(C,E,
     (wdmsg(on_x_rtrace(E)),catch_safe(rtrace(with_skip_bugger(C)),E,wdmsg(E)),dtrace(C))).

