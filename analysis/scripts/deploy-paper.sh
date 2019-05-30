#!/usr/bin/env bash
set -o errexit

PROJDIR=$(git rev-parse --show-toplevel)
cd "$PROJDIR"

DEPLOY_DIR=analysis/paper/_deploy
mkdir -p "$DEPLOY_DIR"
cp -Rf analysis/paper/paper{.md,.html,.pdf,_files} "$DEPLOY_DIR"

cd "$DEPLOY_DIR"

if [[ ! -f index.html ]]; then
    ln -sf paper.html index.html
fi

BRANCH=$(git rev-parse --abbrev-ref HEAD)
if [[ "$BRANCH" != "gh-pages" ]]; then
    echo "Creating the gh-pages branch."
    git init
    git checkout -b gh-pages
    git remote add origin git@github.com:ashiklom/hector_permafrost_emit
fi

if [[ -n $(git status --short) ]]; then
    git add .
    MSG=`date "+Update paper <%Y-%m-%d %H:%M>"`
    git commit -a -m "$MSG"
    git push --force origin gh-pages
else
    echo "Nothing to commit."
fi
