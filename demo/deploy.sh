#!/bin/bash

# From https://gist.github.com/domenic/ec8b0fc8ab45f39403dd

set -e # Exit with nonzero exit code if anything fails

TARGET_BRANCH="gh-pages"
DEMO_FOLDER="${TRAVIS_BUILD_DIR}/demo"
TEMP_FOLDER="temp"
ENCRYPTION_LABEL="2e639d5f7f73"
UGLIFYJS="node ${TRAVIS_BUILD_DIR}/node_modules/uglify-js/bin/uglifyjs"
REPO=`git config remote.origin.url`
SSH_REPO=${REPO/https:\/\/github.com\//git@github.com:}
SHA=`git rev-parse --verify HEAD`

# Clone the existing gh-pages for this repo into TEMP_FOLDER
# Create a new empty branch if gh-pages doesn't exist yet (should only happen on first deploy)
echo "Building and deploying demo"
echo "Target: ${TARGET_BRANCH} branch"


# Compile
cd $DEMO_FOLDER

## elm.js
$SYSCONFCPUS $ELM_MAKE Main.elm --optimize --output $TRAVIS_BUILD_DIR/elm.js
$UGLIFYJS $TRAVIS_BUILD_DIR/elm.js --compress "pure_funcs='F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9',pure_getters=true,keep_fargs=false,unsafe_comps=true,unsafe=true,passes=2" --output $TRAVIS_BUILD_DIR/elm.js
$UGLIFYJS $TRAVIS_BUILD_DIR/elm.js --mangle --output $TRAVIS_BUILD_DIR/elm.js
echo "elm.js done"

## index.html
cp $DEMO_FOLDER/index.html $TRAVIS_BUILD_DIR/index.html
# sed -i -e 's/\/_compile\/Main.elm/elm.js/g' $TRAVIS_BUILD_DIR/index.html
echo "index.html done"


# Git commands
echo "Deploying..."
cd $TRAVIS_BUILD_DIR
git checkout -b $TARGET_BRANCH
git config user.name "Travis CI"
git config user.email "pablohirafuji@gmail.com"
git add index.html elm.js -f
git commit -m "Deploy to GitHub Pages: ${SHA}"


# Get the deploy key by using Travis's stored variables to decrypt deploy_key.enc
ENCRYPTED_KEY_VAR="encrypted_${ENCRYPTION_LABEL}_key"
ENCRYPTED_IV_VAR="encrypted_${ENCRYPTION_LABEL}_iv"
ENCRYPTED_KEY=${!ENCRYPTED_KEY_VAR}
ENCRYPTED_IV=${!ENCRYPTED_IV_VAR}
openssl aes-256-cbc -K $ENCRYPTED_KEY -iv $ENCRYPTED_IV -in ../demo/deploy-key.enc -out deploy-key -d

chmod 600 deploy-key
eval `ssh-agent -s`
ssh-add deploy-key

# Now that we're all set up, we can push.
git push $SSH_REPO $TARGET_BRANCH

echo "Deployed successfully."
exit 0