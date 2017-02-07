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

% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/logicmoo_util_rtrace.pl
:- module(must_trace,
   [
      must/1, % Goal must succeed at least once once
      must_once/1, % Goal must succeed at most once
      must_det/1, % Goal must succeed determistically
      sanity/1,  % like assertion but adds trace control
      nop/1, % syntactic comment
      rtrace/1,  % Non-intractive tracing
      no_trace/1,  % Non-det notrace/1
      restore_trace/1, % After call restor tracer
      rtrace/0, % Start non-intractive tracing
      nortrace/0, % Stop non-intractive tracing
      reset_tracer/0, % Reset Tracer to "normal"
      on_x_rtrace/1, % Non-intractive tracing when exception occurs 
      maybe_leash/1, % Set leash only when it makes sense
      wdmsg/1 % debug messages
    ]).

:- meta_predicate
        must(0),
        must_once(0),
        must_det(0),
        nop(*),
        sanity(0),
        rtrace(0),
        unless(0,0,0),
	catch_safe(0,?,0),
        restore_trace(0),
        on_x_rtrace(0),
        on_f_rtrace(0),
        on_x_fail(0),
        if_may_hide(0), 
	rtrace(0),
        rtrace_break(0),
	no_trace(0).

if_may_hide(Goal):- call(Goal).

% TODO Make a speed,safety,debug Triangle instead of these flags
:- create_prolog_flag(must_type,debug,[]).

% Unless Goal succeeds OnFail else OnException
unless(Goal,OnFail,OnException):- 
  catch((Goal*->true;OnFail),error(E,C),(wdmsg(error(E,C)),OnException)).

%! must(:Goal) is nondet.
%
% Goal must succeed at least once once
%
% Wrap must/1 over parts of your code you do not trust
% If your code fails.. it will rewind to your entry block (at the scope of this declaration) and invoke rtrace/1 .
% If there are 50 steps to your code, it will save you from pushing `creep` 50 times.  
% Instead it turns off the leash to allow you to trace with your eyeballs instead of your fingers.
%
must(Goal):- current_prolog_flag(must_type,How),!,
          (How == speed -> call(Goal);
           How == debug -> on_f_rtrace(Goal);
           How == keep_going -> ignore(on_f_rtrace(Goal))).
must(Goal):- Goal*->true;assertion_failed(fail, must(Goal)).


%! sanity(:Goal) is det.
%
% like assertion but adds trace control
%
sanity(Goal):- current_prolog_flag(must_type,How),!,
          (How == speed -> true;
           How == debug -> \+ \+ ignore(on_f_rtrace(Goal))).
           How == keep_going -> \+ \+ ignore(on_x_fail(on_f_rtrace(Goal))).
sanity(Goal):- assertion(Goal).

%! must_once(:Goal) is det.
%
% Goal must succeed at most once
%
must_once(Goal):- must(Goal),!.


%! must_det(:Goal) is det.
%
% Goal must succeed determistically
%
must_det(Goal):- must_once((Goal,sanity(detetermistic(true)))).


%! nop( :Goal) is det.
%
%  Comments out code without losing syntax
%
nop(_).


%! wdmsg( +Term) is det.
%
%  `wdmsg(E):-format(user_error,'~N% ~q.',[E]).`
%
wdmsg(E):-format(user_error,'~N% ~q.',[E]).


%! catch_safe( :Goal, ?E, :GoalRecovery) is nondet.
%
%  Like catch/3 but rethrows block/2 and $abort/0.
%
catch_safe(Goal,E,Recovery):- 
   nonvar(E) 
   -> catch(Goal,E,Recovery); % normal mode (the user knows what they want)
   catch(Goal,E,(rethrow_bubbled(E),Recovery)). % prevents promiscous mode

%! bubbled_ex( ?Ex) is det.
%
% Bubbled Exception.
%
bubbled_ex('$aborted').
bubbled_ex('time_limit_exceeded').
bubbled_ex(block(_,_)).


%! rethrow_bubbled( ?E) is det.
%
% Bubbled Exception Check.
%
rethrow_bubbled(E):- ( \+ bubbled_ex(E)),!.
rethrow_bubbled(E):-throw(E).


%! on_f_rtrace( :Goal) is det.
%
% If :Goal fails trace it 
%
on_f_rtrace(Goal):- unless(Goal,rtrace_break(Goal),rtrace_break(Goal)).

%! on_x_fail( :Goal) is det.
%
% If there If Is an exception in :Goal just fail
%
on_x_fail(Goal):- unless(Goal,fail,fail).

%! on_x_rtrace( :Goal) is det.
%
% If there If Is an exception in :Goal then rtrace.
%
on_x_rtrace(Goal):- 
 notrace(((tracing;t_l:rtracing),maybe_leash(+exception))) 
  -> Goal
   ;
   catch_safe(Goal,E,(rtrace_break(Goal),throw(E))).


%! maybe_leash( +Flags) is det.
%
% Only leashes interactive consoles
%
maybe_leash(Some):- maybe_leash->leash(Some);true.
maybe_leash:- stream_property(current_input, tty(true)),stream_property(current_input,close_on_abort(false)).


%! get_trace_reset( ?Reset) is det.
%
% Get Tracer.
%
get_trace_reset((notrace,set_prolog_flag(debug,WasDebug),CC3,CC2,'$visible'(_, OldV),'$leash'(_, OldL),notrace(CC0))):- 
     '$leash'(OldL, OldL),'$visible'(OldV, OldV),
     (current_prolog_flag(debug,true)->WasDebug=true;WasDebug=false),
     (t_l:rtracing->CC2=rtrace;CC2= nortrace),
     (tracing -> CC0 = trace ; CC0 = notrace),
     (current_prolog_flag(gui_tracer, GWas)->CC3=set_prolog_flag(gui_tracer, GWas);CC3=true),!.

:- thread_local(t_l:rtracing/0).
:- '$set_predicate_attribute'(t_l:rtracing, trace, 0).
:- thread_local(t_l:tracer_reset/1).
:- '$set_predicate_attribute'(get_trace_reset(_), trace, 0).


%! push_tracer is det.
%
% Push Tracer.
%
push_tracer:- notrace((get_trace_reset(Reset),asserta(t_l:tracer_reset(Reset)))),!.
:- '$set_predicate_attribute'(push_tracer, trace, 0).
:- '$set_predicate_attribute'(push_tracer, hide_childs, 1).


%! pop_tracer is det.
%
% Pop Tracer.
%
pop_tracer:- notrace(retract(t_l:tracer_reset(Reset))->Reset;true).
:- '$set_predicate_attribute'(pop_tracer, trace, 0).
:- '$set_predicate_attribute'(pop_tracer, hide_childs, 1).


%! reset_tracer is det.
%
% Reset Tracer.
%
reset_tracer:- notrace(ignore(t_l:tracer_reset(Reset)->Reset;true)).
:- '$set_predicate_attribute'(reset_tracer, trace, 0).
:- '$set_predicate_attribute'(reset_tracer, hide_childs, 1).


% Make sure interactive debugging is turned back on
user:prolog_exception_hook(error(_, _),_, _, _) :- 
     reset_tracer ->
     maybe_leash ->
     t_l:rtracing ->
     leash(+all),
     fail.

%! no_trace( :Goal) is det.
%
% Unlike notrace/1, it allows nondet tracing 
%
% But also may be break when excpetions are raised during Goal.
%
no_trace(Goal):- setup_call_cleanup(push_tracer,(notrace,Goal),pop_tracer).
/*
no_trace(Goal):- \+ tracing,!,call(Goal).
no_trace(Goal):-
   ((tracing,notrace )-> Tracing = trace ;   Tracing = true),
   '$leash'(OldL, OldL),'$visible'(OldV, OldV),
   (Undo =   notrace(((notrace,'$leash'(_, OldL),'$visible'(_, OldV), Tracing)))),
   (RTRACE = notrace((visible(-all),visible(+exception),maybe_leash(-all),maybe_leash(+exception)))),!,
   setup_call_cleanup_each(RTRACE,(notrace,Goal),Undo).
*/
:- '$set_predicate_attribute'(no_trace(_), hide_childs, 1).
:- '$set_predicate_attribute'(no_trace(_), trace, 0).



%! rtrace is det.
%
% Start RTracer.
%
rtrace:- notrace, (t_l:rtracing -> true ; assert(t_l:rtracing)),
      set_prolog_flag(gui_tracer,false),visible(+all),visible(+exception),
      maybe_leash(-all),maybe_leash(+exception),debug,trace.

:- '$set_predicate_attribute'(rtrace, trace, 0).
:- '$set_predicate_attribute'(rtrace, hide_childs, 1).



%! nortrace is det.
%
% Stop Tracer.
%
nortrace:- notrace,maybe_leash(-all),visible(+all),visible(+exception),maybe_leash(+exception),(retract(t_l:rtracing)->true;true),maybe_leash(+all).
:- '$set_predicate_attribute'(nortrace, trace, 0).
:- '$set_predicate_attribute'(nortrace, hide_childs, 1).



%! restore_trace( :Goal) is det.
%
% restore  Trace.
%
restore_trace(Goal):- 
  setup_call_cleanup(('$leash'(OldL, OldL),'$visible'(OldV, OldV)),
   Goal,
    notrace(('$leash'(_, OldL),'$visible'(_, OldV)))).

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
rtrace_break(Goal):- \+ maybe_leash, !, break(Goal).
rtrace_break(Goal):- rtrace(Goal)*->break;(break,fail).
:- '$set_predicate_attribute'(rtrace_break(_), trace, 0).
:- '$set_predicate_attribute'(rtrace_break(_), hide_childs, 0).


next_rtrace:- (nortrace;(rtrace,trace,notrace(fail))).
:- '$set_predicate_attribute'(next_rtrace, hide_childs, 1).
:- '$set_predicate_attribute'(next_rtrace, trace, 0).


setup_call_cleanup_percall(RTRACE,Goal,Undo):-
   setup_call_cleanup(RTRACE,Goal,Undo),(Undo;(RTRACE,fail)).


:- unlock_predicate(system:notrace/1).
:- if_may_hide('$set_predicate_attribute'(no_trace(_), trace, 0)).
:- if_may_hide('$set_predicate_attribute'(system:notrace(_), hide_childs, 1)).
:- if_may_hide('$set_predicate_attribute'(system:notrace(_), trace, 0)).
:- '$set_predicate_attribute'(system:tracing, trace, 0).
:- '$set_predicate_attribute'(system:notrace, trace, 0).
:- '$set_predicate_attribute'(system:trace, trace, 0).
:- lock_predicate(system:notrace/1).

