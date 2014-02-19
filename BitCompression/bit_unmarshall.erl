-module(bit_unmarshall).
-compile(export_all).

openfile() ->
    {ok, Inp} = file:open("recieved.txt", [read, raw, binary]),
    read_file(Inp),
    file:close(Inp),
    ok.

read_file(Inp) ->
    {ok, Fd} = file:open("byte-Output", [append, raw, binary]),
    case file:read(Inp, 2) of
        {ok, Data} ->
            %io:format("~p~n", [Data] ),
            <<Num_courses:8, Name_Len:8>> = Data,
            {ok, Name} = file:read(Inp, Name_Len),
            %io:format("~p~n", [ erlang:binary_to_atom(Name, utf8) ] ),
            file:write( Fd,  [ io_lib:fwrite("~s", [erlang:binary_to_list(Name) ]) ] ),
            unMarshallData(Fd, Inp, Num_courses),
            file:write(Fd, ["\n"]),
            read_file(Inp);
        eof ->
            ok
    end,
    file:close(Fd),
    ok.

unMarshallData(OutFD, Inp, Num_courses) ->
    Iter = lists:seq(1,Num_courses),
    Get_courseName =
    fun(Fd) ->
            {ok, <<CourseLen/integer>>} = file:read(Inp, 1),
            {ok, BitName} = file:read(Fd, CourseLen),
            erlang:binary_to_list(BitName)
    end,
    CourseName = [ Get_courseName(Inp) || _ <- Iter ],
    {ok, ReadIn} = file:read(Inp, Num_courses - erlang:trunc(Num_courses/8)),

    Unpadded = << <<T:7>> || <<T:7>> <= ReadIn >>,
    %io:format("~p~n", [Unpadded]),
    Decompress = << <<X:7>> || <<X:7>> <= Unpadded >>,
    %io:format("Here : ~p~n", [Decompress]),

    Marks = extract(Decompress, []),
    %io:format("~w~n", [Marks]),
    Data = lists:zip(CourseName, Marks),
    %io:format("~p~n", [Data]),
    [ file:write(OutFD, [ io_lib:fwrite(":~s,~p", [Course, Num])] ) || {Course, Num} <- Data ],
    ok.

extract(Data, Marks) ->
    case Data of
        << M:7/integer , Rest/bits >> ->
            %io:format("~p~n",[M]),
            extract(Rest, Marks ++ [M] );
        <<>> -> Marks
    end.
