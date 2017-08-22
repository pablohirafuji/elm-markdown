#!/bin/bash

# From https://gist.github.com/domenic/ec8b0fc8ab45f39403dd

set -e # Exit with nonzero exit code if anything fails

SOURCE_BRANCH="master"
TARGET_BRANCH="gh-pages"
TEMP_FOLDER="temp"
ENCRYPTION_LABEL="2e639d5f7f73"


# Save some useful information
REPO=`git config remote.origin.url`
SSH_REPO=${REPO/https:\/\/github.com\//git@github.com:}
SHA=`git rev-parse --verify HEAD`

# Clone the existing gh-pages for this repo into TEMP_FOLDER
# Create a new empty branch if gh-pages doesn't exist yet (should only happen on first deploy)
echo "Starting deployment"
echo "Target: ${TARGET_BRANCH} branch"
git clone $REPO $TEMP_FOLDER
cd $TEMP_FOLDER
git checkout $TARGET_BRANCH || git checkout --orphan $TARGET_BRANCH

# Clean gh-pages existing contents
echo "Removing old static content"
git rm -rf . || exit 1
cd ..

# Run our compile script
echo "Compiling into ${TEMP_FOLDER}/"
npm install uglify-js -g
cd demo
elm package install --yes
$TRAVIS_BUILD_DIR/sysconfcpus/bin/sysconfcpus -n 2 elm make Main.elm --output ../$TEMP_FOLDER/main.js
cd ..
cp demo/index.html $TEMP_FOLDER/index.html
cd $TEMP_FOLDER
uglifyjs main.js --output main.js
sed -i -e 's/\/_compile\/Main.elm/main.js/g' index.html

# Now let's go have some fun with the cloned repo
git config user.name "Travis CI"
git config user.email "pablohirafuji@gmail.com"

# If there are no changes to the compiled gh-pages (e.g. this is a README update) then just bail.
if [ $(git status --porcelain | wc -l) -lt 1 ]; then
    echo "No changes to the output on this push; exiting deploy."
    exit 0
fi

# Commit the "changes", i.e. the new version.
# The delta will show diffs between new and old versions.
git add --all
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

echo "Cleaning up temp files"
cd ..
rm -Rf $TEMP_FOLDER

echo "Deployed successfully."
exit 0