-module(erlDigreco).

-export([main/0, get_all_lines/4, get_data/0, compare/3, distance/3, execute/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The code implements a multi-core digit recogoniser, it takes
%% as input 2 files, one training data and other is the testing
%% data. The code is executed in parallel using 4 threads.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_all_lines(Fd, Accum, Parent, Type) ->
    case file:read_line(Fd) of
        eof  ->
            %file:close(Fd),
            Parent ! { Type , Accum };
        {ok, Line} ->
            [Num, Tmp] = string:tokens(Line, "\t"),
            Tmp2 = string:strip(Tmp, both, $\n),
            Tmp3 = string:strip( string:strip(Tmp2, both, $[ ), both, $] ),
            Data = string:tokens(Tmp3, ", " ),
            get_all_lines(Fd, Accum ++ [ [Num | Data] ], Parent, Type )
    end.

get_data() ->
    receive
        {Type, Accum} -> ok
    end,
    {Type, Accum}.

distance( TestL, TrainL, Num) ->
    %io:format("~p~n", [TestL]),
    Dist = lists:zipwith(
        fun(X,Y) ->
                Num1 = list_to_integer(X),
                Num2 = list_to_integer(Y),
                abs( Num1*Num1 - Num2*Num2 )
        end, TestL, TrainL),
    TotDist = lists:foldl(fun(X, Sum) -> X + Sum end, 0, Dist),
    [Num, TotDist].

compare(TestL, TrainData, Parent) ->
    AllDist = [ distance(tl(TestL), Data, Num) || [ Num | Data ] <- TrainData ],
    %io:format("~p~n", [AllDist]),
    Cmp = fun(Lst1, Lst2) ->
            Num1 = tl(Lst1),
            Num2 = tl(Lst2),
            if
                Num1 < Num2 -> true;
                true -> false
            end
          end,
    SortDist = lists:sublist(lists:sort(Cmp, AllDist ), 10),
    Guess    = [ Num || [Num, _ ] <- SortDist ],
    Counter  = fun(Num, List) ->
                length([ N || N <- List, N=:=Num ])
               end,
    Freq     = [ [Num, Counter(Num, Guess)] || Num <- Guess ],
    [Ans, _] = lists:nth( 10, lists:sort(Cmp, Freq)),
    Parent ! { hd(TestL), Ans}.
    %io:format("~p~n", [time()]).

execute(Train, Test, Process) ->
    if
        length(Test) =:= 0 ->
            ok;
        Process < 4 ->
            spawn( erlDigreco, compare, [ hd(Test), Train, self() ] ),
            execute(Train, tl(Test), Process+1 );
        true ->
            get_data(),
            execute(Train, Test, Process-1)
    end.

main() ->
    {ok, FdTrain} = file:open("train.txt", [read]),
    {ok, FdTest}  = file:open("test.txt", [read]),

    io:format("~p~n", [time()]),
    spawn( erlDigreco, get_all_lines, [FdTrain, [], self(), train ]),
    spawn( erlDigreco, get_all_lines, [FdTest,  [], self(), test  ]),

    {Type1, Data1} = get_data(),
    { _ , Data2}   = get_data(),
    
    file:close(FdTest),
    file:close(FdTrain),

    %io:format("~p", [Data1]),
    case Type1 of
        test ->
            execute(Data1, Data2, 0);
        train  ->
            execute(Data2, Data1, 0)
    end,

    io:format("~p~n", [time()]),
    ok.
