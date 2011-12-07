-module(sql_migration).

-export([behaviour_info/1]).

-export([migrations/1, migrate/3]).

behaviour_info(callbacks) ->
    [{upgrade,1},
     {downgrade, 1}];
behaviour_info(_Other) ->
    undefined.

migrations(App) ->
    {ok, Ms} = application:get_key(App, modules),
    Migrations = [ M || M <- Ms,
                       lists:member(sql_migration,
                                    proplists:get_value(behaviour, M:module_info(attributes), [])) ],
    lists:usort(Migrations).

migrate(Conn, Version, Migrations) ->
    BinVersion = atom_to_binary(Version, latin1),
    case pgsql:squery(Conn, "SELECT id FROM migrations ORDER BY id DESC") of
        {error,{error,error,<<"42P01">>,_,_}} ->
            %% init migrations and restart
            init_migrations(Conn),
            migrate(Conn, Version, Migrations);
        {ok, _, [{BinVersion}|_]} ->
            up_to_date;
        {ok, _, [{Top}|_]} when Top < BinVersion ->
            %% upgrade path
            TopAtom = binary_to_atom(Top, latin1),
            Upgrade = lists:dropwhile(fun (V) -> V =< TopAtom end, Migrations),
            [ begin 
                  M:upgrade(Conn),
                  pgsql:equery(Conn,
                               "INSERT INTO migrations (id) "
                               "VALUES ($1)", [atom_to_binary(M, latin1)])
              end || M <- Upgrade ],
            {upgrade, Upgrade};
        {ok, _, [{Top}|_]} when Top > BinVersion ->
            %% downgrade path
            TopAtom = binary_to_atom(Top, latin1),
            Downgrade = lists:takewhile(fun (V) -> V >= TopAtom end, lists:reverse(Migrations)),
            [ begin 
                  M:downgrade(Conn),
                  pgsql:equery(Conn,
                               "DELETE FROM migrations WHERE id = $1",
                               [atom_to_binary(M, latin1)])
              end || M <- Downgrade ],
            {downgrade, Downgrade};
        {ok, _, []} ->
            %% full upgrade path
            Upgrade = Migrations,
            [ begin 
                  M:upgrade(Conn),
                  pgsql:equery(Conn,
                               "INSERT INTO migrations (id) "
                               "VALUES ($1)", [atom_to_binary(M, latin1)])
              end || M <- Upgrade ],
            {upgrade, Upgrade}
    end.


%% Private
init_migrations(Conn) ->
    {ok, _, _} = pgsql:squery(Conn,
                              "CREATE TABLE migrations ("
                              "id VARCHAR(255) PRIMARY KEY,"
                              "datetime TIMESTAMP DEFAULT CURRENT_TIMESTAMP"
                              ")").
