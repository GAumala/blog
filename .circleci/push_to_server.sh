if [[ -n "$CIRCLE_BRANCH" && "$CIRCLE_BRANCH" != "master" ]]; then
  echo "Only master branch can deploy. Bye!"
  exit 0
fi

mv ./static/_site /tmp
git checkout server
rm -rf _site
mv /tmp/_site . 
# bring source files that should also be deployed
git checkout master -- .circleci/config.yml .gitignore nginx api 

# Only push to server if something did change
if [[ `git status --porcelain` ]]; then
  git add .

  if [ -n "$CIRCLE_BUILD_NUM" ]; then
    git config --global user.email job@circleci.com
    git config --global user.name CircleCI
    git commit -m "deploy #$CIRCLE_BUILD_NUM"
  else
    git commit -m "deploy manually"

  git push origin server
else
  echo "No changes to deploy. Bye!"
fi
