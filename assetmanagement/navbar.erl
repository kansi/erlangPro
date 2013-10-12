%% Edited: 27/06/2013
%% Author: Vanshdeep Singh
%% Description: This code generated the navigation bar
%% for the assetmanagement portal

-module(navbar).
-compile(export_all).

navbar() ->
  Link_decor = {style, "TEXT-DECORATION: NONE"},
  
  Navlinks=
   [
      {a, [{href, "#"}, Link_decor], "Home |" },
      {a, [{href, "add_asset.yaws"}, Link_decor], " Add Assets |" },
      {a, [{href, "add_user.yaws"}, Link_decor], " Add Users |" },
      {a, [{href, "issue_assets.yaws"}, Link_decor], " Issue Assets |" },
      {a, [{href, "show_issued_assets.yaws"}, Link_decor], " Issued Assets " }
   ],
  Navlinks.
