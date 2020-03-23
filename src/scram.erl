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

-spec salted_password(hash_type(), binary(), binary(), non_neg_integer()) -> binary().
salted_password(Hash, Password, Salt, IterationCount) ->
    hi(Hash, Password, Salt, IterationCount).

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
    mask(ClientProof, ClientSignature).

-spec server_signature(hash_type(), binary(), binary()) -> binary().
server_signature(Hash, ServerKey, AuthMessage) ->
    crypto_hmac(Hash, ServerKey, AuthMessage).

-spec hi(hash_type(), binary(), binary(), non_neg_integer()) -> binary().
hi(Hash, Password, Salt, IterationCount) ->
    U1 = crypto_hmac(Hash, Password, <<Salt/binary, 0, 0, 0, 1>>),
    mask(U1, hi_round(Hash, Password, U1, IterationCount - 1)).

-spec hi_round(hash_type(), binary(), binary(), non_neg_integer()) -> binary().
hi_round(Hash, Password, UPrev, 1) ->
    crypto_hmac(Hash, Password, UPrev);
hi_round(Hash, Password, UPrev, IterationCount) ->
    U = crypto_hmac(Hash, Password, UPrev),
    mask(U, hi_round(Hash, Password, U, IterationCount - 1)).

-spec mask(binary(), binary()) -> binary().
mask(Key, Data) ->
    KeySize = size(Key) * 8,
    <<A:KeySize>> = Key,
    <<B:KeySize>> = Data,
    C = A bxor B,
    <<C:KeySize>>.

-spec crypto_hmac(hash_type(), binary(), binary()) -> binary() | no_return().
crypto_hmac(SHA, Key, Data) ->
    crypto:hmac(SHA, Key, Data).
