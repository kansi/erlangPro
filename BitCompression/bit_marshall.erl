-module(bit_marshall).
-compile(export_all).

openfile() ->
    {ok, Inp} = file:open("input", [read]),
    {ok, Out} = file:open("byte-IR", [write, raw, binary]),
    read_file(Inp, Out),
    file:close(Inp),
    file:close(Out),
    ok.

read_file(Inp, Out) ->
    case file:read_line(Inp) of
        {ok, Data} ->
            marshallData(Data, Out),
            io:format("Yes~n"),
            read_file(Inp, Out);
        eof ->
            ok
    end,
    ok.

marshallData(Data, Fd) ->
    %io:format("~p~n",[Data]),
    Student     = string:tokens( string:strip(Data, right, $\n), ":" ),
    Num_courses = erlang:length(Student)-1,
    Name_Len    = erlang:length(hd(Student)),
    Num_courses_Len_name = <<Num_courses:8, Name_Len:8>>,
    Courses = [ { hd(string:tokens(X, ",")), erlang:length(hd(string:tokens(X, ",")))  } || X <- tl(Student) ],
    Tmp     = [ string:to_integer(hd(tl(string:tokens(X, ",")))) || X <- tl(Student) ],
    Marks   = << <<M:7>> || {M, _} <- Tmp >>,
    PaddingSize = (8 - (bit_size(Marks) rem 8)) rem 8,
    PaddedCompressed = <<Marks:(bit_size(Marks))/bitstring, 0:PaddingSize>>,

    file:write(Fd, [Num_courses_Len_name] ),
    file:write(Fd, hd(Student)),
    [ file:write(Fd, [<<Len:8>>, Name]) || {Name, Len} <- Courses ],
    file:write(Fd, [PaddedCompressed] ),
    ok.
