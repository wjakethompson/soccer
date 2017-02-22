git config --global user.email "wjakethompson@gmail.com"
git config --global user.name "Jake Thompson"

# clone the repository to the book-output directory
git clone -b gh-pages \
  https://${GITHUB_PAT}@github.com/${TRAVIS_REPO_SLUG}.git \
  book-output
cd book-output
cp -r ../_book/* ./
git add --all *
git commit -m "Update the book"
git push origin gh-pages
