%% Table 'phone' is the location database. One entry for every phone
%% (or, more correctly, User Agent) that REGISTERs for one of our
%% users.
-record(phone, {number, flags, class, expire, address, requristr}).
%% Table 'user' is the user database if you are using sipuserdb_mnesia.
-record(user, {user, password, number, flags, classes}).
%% Table 'numbers' maps extra addresses (besides the first one -
%% ('number' in table 'user') to users. Note well that sipuserdb_mnesia
%% also has implicit mappings of addresses to users through
%% lookup:lookup_url_to_addresses.
-record(numbers, {user, number}).
