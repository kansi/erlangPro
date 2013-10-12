%% Edited: 27/06/2013
%% Author: Vanshdeep Singh
%% Description: Below code implements various database query
%% functions, which can be used to add and get data from the database
%% This database shoud rum on the same node where yaws is running.

-module(db_asset).
-include_lib("stdlib/include/qlc.hrl").
-export([start/2, stop/1]).
-export([install/1, add_user/5, add_asset/5, issue_asset/3, get_users/0, get_assets/0, get_query/0, spawn_db/0, get_issued_assets/0]).

%% include the table definations for the database
-include("table_definations.hrl").

%% this function creates the schema and tables.
%% this is a one time run function to set up the db.
install(Node) ->
  mnesia:create_table( users,
                      [{attributes, record_info(fields, users)},
                       {type, set},
                       {disc_copies, Node}]),

  mnesia:create_table(assets,
                      [{attributes, record_info(fields, assets)},
                       {disc_copies, Node},
                       {type, set}]),

  mnesia:create_table(issued_assets,
                      [{attributes, record_info(fields, issued_assets)},
                       {disc_copies, Node},
                       {type, bag}]).

%%the start waits for the tables to load
start(normal, []) ->
  mnesia:wait_for_tables([users, assets, issued_assets], 3000),
  db_asset_sup:start_link().

stop(_) -> ok.

%% This function spawns the get_query function of the database.
%% Note: database should be spawned in the same shell
%% where yaws is running
spawn_db() ->
  Pid=spawn(?MODULE, get_query, []),
  register(assetDB,Pid),
  Pid.

%% This function waits to recieve a query, and upon revieving
%% a query it executes it using appropriate funcitons
get_query() ->
  receive

    {From, insert_user, Data} ->
      #users{user_id=UserId, user_name=Username, contact=Contact, email=Email, user_type=User_type} = Data,
      %Status = add_user(UserId, Username, Contact, Email, User_type), 
      try add_user(UserId, Username, Contact, Email, User_type) of
        _ -> From ! "User added successfully"
      catch
        Error -> From ! "Error, unable to add user due to the following error : " ++ Error
      end; 

    {From, insert_asset, Data} ->
      #assets{asset_id=Id, asset_name=Name, description=Description, location=Location, state=State} = Data,
      try add_asset(Id, Name, Description, Location, State) of
        _ -> From ! "Asset added successfully"
      catch
        Error -> From ! "Error, unable to add Asset due to the following error : " ++ Error
      end; 

    {From, issue_asset, Data} ->
      #issued_assets{asset_id=AssetId, user_id=UserId, issue_date=IssueDate} = Data,
      try issue_asset(AssetId, UserId, IssueDate) of
        _ -> From ! "Success"
      catch
        _ -> From ! "Error"
      end; 

    {From, get_assets} ->
      All_assets = get_assets(),
      From ! All_assets;
 
    {From, get_issued_assets} ->
      Issued_assets = get_issued_assets(),
      From ! Issued_assets;

    {From, get_users} ->
      All_users = get_users(),
      From ! All_users;

    _ -> io:format("Nothing entered~n")
  end,

  get_query().

%% this function adds users to the database
add_user(UserId, Username, Contact, Email, User_type) ->
  Write = fun() ->
      mnesia:write(#users{user_id=UserId,
                          user_name=Username,
                          contact = Contact,
                          email = Email,
                          user_type = User_type})
  end,
  mnesia:activity(transaction, Write).

%% this function adds assets to the database
add_asset(AssetId, Asset_name, Description, Location, State) ->
  Write = fun() ->
      mnesia:write(#assets{asset_id=AssetId,
                           asset_name=Asset_name,
                           description = Description,
                           location = Location,
                           state = State})
  end,
  mnesia:activity(transaction, Write).

%% This funtion stores all the id's of assets and users, to whom
%% the asset has been issued
issue_asset(AssetId, UserId, Issue_date) ->
  Issue = fun() ->
      mnesia:write(#issued_assets{asset_id=AssetId,
                                  user_id=UserId,
                                  issue_date=Issue_date})
  end,
  mnesia:activity(transaction, Issue).

%% This functions gets all the users from the database
get_users() ->

  Query=qlc:q([X || X<-mnesia:table(users)]),
  Get_users = fun() -> qlc:e(Query) end,
  {atomic, Rows}=mnesia:transaction(Get_users),
  Sorted_rows=lists:keysort(2, Rows),
  All_users = lists:map
              (
      fun(User) ->
          #users{user_id=UserId, user_name=Username, contact=Contact, email=Email, user_type=_}=User,
          [{"user_id", UserId}, {"name", Username}, {"email", Email}, {"contact", Contact}]
          %        io:format("Id: ~p, Name: ~p, Contact: ~p~n, Email: ~p~n",[UserId, Username, Contact, Email])
      end,
      Sorted_rows
      ),
  All_users.

%% This functions gets all the assets from the database
get_assets() ->
  Query=qlc:q([X || X<-mnesia:table(assets)]),
  Get_assets = fun() -> qlc:e(Query) end,
  {atomic, Rows}=mnesia:transaction(Get_assets),
  Sorted_rows=lists:keysort(2, Rows),
  All_assets = lists:map
               (
      fun(Asset) ->
          #assets{asset_id=Id, asset_name=Name, description=Des, location=Loc, state=State}=Asset,
          [{"asset_id", Id}, {"name", Name}, {"description", Des}, {"location", Loc}, {"state", State } ]
      end,
      Sorted_rows
      ),
  All_assets.

%% This funciton gets all the assets that have been issued from the database
get_issued_assets() ->
  Query=qlc:q([X || X<-mnesia:table(issued_assets)]),
  Get_issued_assets = fun() -> qlc:e(Query) end,
  {atomic, Rows}=mnesia:transaction(Get_issued_assets),
  Sorted_rows=lists:keysort(2, Rows),
  Issued_assets = lists:map
               (
      fun(Asset) ->
          #issued_assets{asset_id=A_Id, user_id=U_Id, issue_date=Issue_Date, return_date=Return_date}=Asset,
          [{"asset_id", A_Id}, {"user_id",U_Id}, {"issue_date", Issue_Date}, {"return_date", Return_date} ]
      end,
      Sorted_rows
      ),
  Issued_assets.
