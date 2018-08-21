-module(records).
%% -include("records.hrl").
-compile(export_all).
-record(included, {some_field, some_default = "yeah!", unimaginative_name}).

-record(robot, {name,
                type=industrial,
                hobbies,
                details=[]}).

-record(user, {id,name,group,age}).

first_robot() ->
    #robot{name="Merchatron",
           type=handemade,
           details=["Moved by a small inside man"]}.

car_factory(CorpName) ->
    #robot{name=CorpName, hobbies="building cars"}.

admin_panel(#user{name=Name, group=admin}) ->
    Name ++ " is allowed!";
admin_panel(#user{name=Name}) ->
    Name ++ " is not allowed".

adult_section(U = #user{}) when U#user.age >= 18 ->
    allowed;
adult_section(_) ->
    forbidden.

included() -> #included{some_field="Some value"}.
