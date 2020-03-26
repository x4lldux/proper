-module(parallel_crash_tests).

-include_lib("proper/include/proper.hrl").

-include_lib("eunit/include/eunit.hrl").


%%------------------------------------------------------------------------------
%% Helper macros
%%------------------------------------------------------------------------------

%% NOTE: Never add long_result to Opts for these macros.

state_is_clean() ->
    get() =:= [].

assertEqualsOneOf(_X, none) ->
    ok;
assertEqualsOneOf(X, List) ->
    ?assert(lists:any(fun(Y) -> Y =:= X end, List)).

-define(_passes(Test),
	?_passes(Test, [])).

-define(_passes(Test, Opts),
	?_assertRun(true, Test, Opts, true)).

-define(_errorsOut(ExpReason, Test),
	?_errorsOut(ExpReason, Test, [])).

-define(_errorsOut(ExpReason, Test, Opts),
	?_assertRun({error,ExpReason}, Test, Opts, true)).

-define(_assertRun(ExpResult, Test, Opts, AlsoLongResult),
	?_test(begin
	    ?assertMatch(ExpResult, proper:quickcheck(Test,Opts)),
	    proper:clean_garbage(),
	    ?assert(state_is_clean()),
	    case AlsoLongResult of
		true ->
		    ?assertMatch(ExpResult,
				 proper:quickcheck(Test,[long_result|Opts])),
		    proper:clean_garbage(),
		    ?assert(state_is_clean());
		false ->
		    ok
	    end
	end)).

-define(_assertCheck(ExpShortResult, CExm, Test),
	?_assertCheck(ExpShortResult, CExm, Test, [])).

-define(_assertCheck(ExpShortResult, CExm, Test, Opts),
	?_test(?assertCheck(ExpShortResult, CExm, Test, Opts))).

-define(assertCheck(ExpShortResult, CExm, Test, Opts),
	begin
	    ?assertMatch(ExpShortResult, proper:check(Test,CExm,Opts)),
	    ?assert(state_is_clean())
	end).

-define(_fails(Test),
	?_fails(Test, [])).

-define(_fails(Test, Opts),
	?_failsWith(_, Test, Opts)).

-define(_failsWith(ExpCExm, Test),
	?_failsWith(ExpCExm, Test, [])).

-define(_failsWith(ExpCExm, Test, Opts),
	?_assertFailRun(ExpCExm, none, Test, Opts)).

-define(_failsWithOneOf(AllCExms, Test),
	?_failsWithOneOf(AllCExms, Test, [])).

-define(_failsWithOneOf(AllCExms, Test, Opts),
	?_assertFailRun(_, AllCExms, Test, Opts)).

-define(SHRINK_TEST_OPTS, [{start_size,10},{max_shrinks,10000}]).

-define(_shrinksTo(ExpShrunk, Type),
	?_assertFailRun([ExpShrunk], none, ?FORALL(_X,Type,false),
			?SHRINK_TEST_OPTS)).

-define(_shrinksToOneOf(AllShrunk, Type),
	?_assertFailRun(_, [[X] || X <- AllShrunk], ?FORALL(_X,Type,false),
			?SHRINK_TEST_OPTS)).

-define(_nativeShrinksTo(ExpShrunk, TypeStr),
	?_assertFailRun([ExpShrunk], none,
			?FORALL(_X,assert_can_translate(?MODULE,TypeStr),false),
			?SHRINK_TEST_OPTS)).

-define(_nativeShrinksToOneOf(AllShrunk, TypeStr),
	?_assertFailRun(_, [[X] || X <- AllShrunk],
			?FORALL(_X,assert_can_translate(?MODULE,TypeStr),false),
			?SHRINK_TEST_OPTS)).

-define(_assertFailRun(ExpCExm, AllCExms, Test, Opts),
	?_test(begin
		   ShortResult = proper:quickcheck(Test, Opts),
		   CExm1 = get_cexm(),
		   ?checkCExm(CExm1, ExpCExm, AllCExms, Test, Opts),
		   ?assertEqual(false, ShortResult),
		   LongResult = proper:quickcheck(Test, [long_result|Opts]),
		   CExm2 = get_cexm(),
		   ?checkCExm(CExm2, ExpCExm, AllCExms, Test, Opts),
		   ?checkCExm(LongResult, ExpCExm, AllCExms, Test, Opts)
	       end)).


simple_test_() ->
    [
     ?_passes(symb_statem:prop_simple())
    ].


parallel_simple_test_() ->
    [
     %% {timeout, 20, ?_passes(symb_statem:prop_parallel_simple())}
     ?_passes(symb_statem:prop_parallel_simple())
    ].
