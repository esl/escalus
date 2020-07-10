%%%----------------------------------------------------------------------
%%% File    : scram.erl
%%% Author  : Stephen Röttger <stephen.roettger@googlemail.com>
%%% Purpose : SCRAM (RFC 5802)
%%% Created : 7 Aug 2011 by Stephen Röttger <stephen.roettger@googlemail.com>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2013   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(scram).

-author('stephen.roettger@googlemail.com').

%% External exports
-export([salted_password/4,
         stored_key/2,
         server_key/2,
         server_signature/3,
         client_signature/3,
         client_key/2,
         client_proof_signature/2
        ]).

-type hash_type() :: crypto:sha1() | crypto:sha2().
-export_type([hash_type/0]).

-spec salted_password(hash_type(), binary(), binary(), non_neg_integer()) -> binary().
salted_password(Hash, Password, Salt, IterationCount) ->
    fast_scram:hi(Hash, Password, Salt, IterationCount).

-spec client_key(hash_type(), binary()) -> binary().
client_key(Hash, SaltedPassword) ->
    crypto_hmac(Hash, SaltedPassword, <<"Client Key">>).

-spec stored_key(hash_type(), binary()) -> binary().
stored_key(Hash, ClientKey) -> crypto:hash(Hash, ClientKey).

-spec server_key(hash_type(), binary()) -> binary().
server_key(Hash, SaltedPassword) ->
    crypto_hmac(Hash, SaltedPassword, <<"Server Key">>).

-spec client_signature(hash_type(), binary(), binary()) -> binary().
client_signature(Hash, StoredKey, AuthMessage) ->
    crypto_hmac(Hash, StoredKey, AuthMessage).

-spec client_proof_signature(binary(), binary()) -> binary().
client_proof_signature(ClientProof, ClientSignature) ->
    crypto:exor(ClientProof, ClientSignature).

-spec server_signature(hash_type(), binary(), binary()) -> binary().
server_signature(Hash, ServerKey, AuthMessage) ->
    crypto_hmac(Hash, ServerKey, AuthMessage).

-spec crypto_hmac(hash_type(), binary(), binary()) -> binary() | no_return().
crypto_hmac(SHA, Key, Data) ->
    crypto:hmac(SHA, Key, Data).
