set -eo pipefail

git fetch origin server:server
cd static
stack exec site build
cd ../

bash .circleci/push_to_server.sh
