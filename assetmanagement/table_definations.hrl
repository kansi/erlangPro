%% Edited: 27/06/2013
%% Author: Vanshdeep Singh
%% Description: This file defines the scheme for storing users,
%% assets and issued assets

-record(users, {user_id, user_name, contact, email, user_type}).
-record(assets, {asset_id, asset_name, description, location, state}).
-record(issued_assets, {asset_id, user_id, issue_date, return_date}).
