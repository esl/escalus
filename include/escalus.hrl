%%==============================================================================
%% Copyright 2010 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================


-record(jid, {
        user :: binary(),
        server :: binary(),
        resource = <<"">> :: binary()}).

-record(client, {
        jid :: binary() | undefined,
        module :: atom(),
        rcv_pid :: pid(),
        event_client :: any(),
        props :: list()
       }).

-record(extaddress, {
          type = to :: to | cc | bcc | replyto | replyroom | noreply | ofrom,
          desc :: binary() | undefined,
          jid :: binary() | undefined,
          uri :: binary() | undefined,
          node :: binary() | undefined
         }).
