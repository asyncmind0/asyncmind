#!/bin/sh -x
export IPFS_HASH="QmXmvMzFPbi5Jokts7sRK6xtQRdK3qV2CfdHaGEfm8zX5w"
cd ~/.config/
curl -o asyncmind.tgz 'http://stevenjoseph.in/ipfs/'${IPFS_HASH}
mkdir -p asyncmind
tar xf asyncmind.tgz -C asyncmind
cd asyncmind


