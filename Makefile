run=grunt --base . --gruntfile node_modules/pixbi-grunt/Gruntfile.coffee

dev:
	$(run) watch


check:
	$(run) check


patch: prebuild build
	$(run) bump:patch
	make commit


minor: prebuild build
	$(run) bump:minor
	make commit


major: prebuild build
	$(run) bump:major
	make commit


build:
	$(run) build


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
	# Go back to develop
	git checkout develop
