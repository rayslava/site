#!/bin/sh
# Prepare the git checkout used by the admin pull/refresh actions, then
# start the server.
#
# /opt/site.git is the repository the image was built from, with the built
# commit as branch "image" (absent when the build had no git directory).
# /site/var/src is a clone of it on the persistent volume, tracking GitHub
# master; the server runs inside it, so the admin "pull" action updates it
# and "refresh" recompiles it into the running server. On start:
#   checkout ahead of the image  - load the checkout (pulled posts survive
#                                  restarts and redeploys)
#   image ahead of the checkout  - fast-forward the checkout to the image
#   diverged                     - keep serving the image code

src=/site/var/src
baked=/opt/site.git
repo=https://github.com/rayslava/site.git

log() { echo "entrypoint: $*" >&2; }

prepare() {
    if [ ! -d "$src/.git" ]; then
        rm -rf "$src"
        if [ -f "$baked/HEAD" ]; then
            git clone -q "$baked" "$src" \
                && git -C "$src" checkout -q -B master origin/image \
                && git -C "$src" remote set-url origin "$repo" \
                && git -C "$src" config branch.master.remote origin \
                && git -C "$src" config branch.master.merge refs/heads/master \
                || return 1
        else
            git clone -q "$repo" "$src" || return 1
        fi
        printf '/static/jscl.js\n/activitypub\n' >> "$src/.git/info/exclude"
    fi
    cp /site/static/jscl.js "$src/static/jscl.js" || return 1
    # The server reads its keys from ./activitypub
    ln -sfn /site/activitypub "$src/activitypub" || return 1

    if [ ! -f "$baked/HEAD" ]; then
        export SITE_LOAD_SOURCE=1
        return 0
    fi
    git -C "$src" fetch -q "$baked" image || return 1
    image=$(git -C "$src" rev-parse FETCH_HEAD)
    head=$(git -C "$src" rev-parse HEAD)
    if [ "$head" = "$image" ]; then
        return 0
    elif git -C "$src" merge-base --is-ancestor "$image" "$head"; then
        log "checkout at $head is ahead of the image ($image), loading it"
        export SITE_LOAD_SOURCE=1
    elif git -C "$src" merge-base --is-ancestor "$head" "$image"; then
        log "fast-forwarding checkout to the image commit $image"
        git -C "$src" merge -q --ff-only "$image"
    else
        log "checkout at $head has diverged from the image ($image), serving the image code"
    fi
}

if [ -w "$(dirname "$src")" ]; then
    if prepare; then
        cd "$src"
    else
        log "could not prepare $src, admin pull/refresh will not work"
    fi
fi

exec /usr/local/bin/site-server "$@"
