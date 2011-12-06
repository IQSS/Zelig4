TOP=../..
PKG=${shell cd ${TOP};pwd}
SUITE=doRUnit.R
R=R

all: inst test demos describe

inst: # Install package
	cd ${TOP}/..;\
	${R} CMD INSTALL ${PKG}

test: # Run unit tests
	export RCMDCHECK=FALSE;\
	cd ${TOP}/tests;\
	${R} --vanilla --slave < ${SUITE}

demos: # Run all demos
	export RCMDCHECK=FALSE;\
	cd ${TOP}/tests;\
	${R} --vanilla --slave < testdemos.R

describe: # test check.describe functions
	export RCMDCHECK=FALSE;\
	cd ${TOP}/tests;\
	${R} --vanilla --slave < check.describe.R

