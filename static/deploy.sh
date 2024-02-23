#!/usr/bin/env bash

set -eux

outputDir=$(mktemp -d)
deployTime=$(date +"%Y-%m-%d %H:%M")
workingDir=$()

# Before doing anything lets check we are
# on the static/ dir of the master branch
cd "$(dirname "$0")"
git checkout master

function build_site() {
  # first build frontend js
  cd ../frontend/
  yarn
  yarn build
  # now copy output to static/js
  mkdir -p ../static/js
  cp ./output/js/* ../static/js

  # return to static/ dir run Hakyll through stack
  cd -
  stack build
  stack exec site build
  mv _site $outputDir
}

function deploy() {
  # go to project root and switch to server branch
  cd ..
  git checkout server
  git pull origin server

  # replace existing files with new output
  rm -rf _site
  mv "$outputDir/_site" . 
  # bring source files that should also be deployed
  git checkout master -- .circleci/config.yml .gitignore nginx api 

# only push to server if something did change
  if [[ `git status --porcelain` ]]; then
    git add .
    git commit -m "deploy at $deployTime"
    git push origin server
  else
    echo "No changes to deploy. Bye!"
  fi
}

function handle_deploy_error()  {
  git checkout -- .
  git reset --hard origin/server
  git checkout master
}

build_site
deploy || handle_deploy_error

# finally clean up
rm -rf $outputDir
git checkout master
