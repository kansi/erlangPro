-module(xmlParser).
-compile(export_all).

-include_lib("/usr/lib64/erlang/lib/xmerl-1.3.2/include/xmerl.hrl").

main() ->
    {Tree, _} = xmerl_scan:file("xml-IR"),
    [ extract(Student) || Student <- Tree#xmlElement.content ],
    ok.

extract(Student) ->
    #xmlElement{content=[#xmlText{value=Name}]} = hd(Student#xmlElement.content),
    CourseMarks = fun(X) ->
                          [ Y || #xmlAttribute{value=Y} <- X#xmlElement.attributes ]
                  end,
    CourseList = [ CourseMarks(Data) || Data <- tl(Student#xmlElement.content)],
    io:format("~p~n~p~n", [Name, CourseList]),
    ok.
