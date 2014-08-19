grunt = grunt --base . --gruntfile node_modules/pixbi-grunt/Gruntfile.coffee

dev:
	$(grunt) watch


patch: prebuild build
	$(grunt) bump:patch
	make commit


minor: prebuild build
	$(grunt) bump:minor
	make commit


major: prebuild build
	$(grunt) bump:major
	make commit


build:
	$(grunt) build


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
	$(grunt) tag
	# Go back to develop
	git checkout develop
