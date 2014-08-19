BASE=.
run=grunt --base $(BASE) --gruntfile node_modules/pixbi-grunt/Gruntfile.coffee

dev:
	$(run) dev


check:
	$(run) check


patch: build
	$(run) bump:patch
	make commit

minor: build
	$(run) bump:minor
	make commit

major: build
	$(run) bump:major
	make commit


build: prebuild
	$(run) build


# Separated from `build` for flexibility
prebuild:
	git stash
	git checkout develop


commit:
	# Commit
	git commit -m 'Bump version'
	# Merge into master
	git checkout master
	git merge develop
	# Apply tag
	$(run) tag
	# Sync with Github
	git push origin develop:develop
	git push origin master
	git push origin --tags
	# Go back to develop
	git checkout develop
