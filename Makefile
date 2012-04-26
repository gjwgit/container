help:
	@echo "Manage the container R package\n\
	==============================\n\n\
	Targets\n\
	-------\n\n\
	check\tCheck for issues with hte packaging\n\
	build\tGenerate source package (.tar.gz file)\n\
	install\tInstall on the local machine\n\
	update\tBuild and install\n\
	u1\tUpload to Ubuntu One Cloud\n\
	"

.PHONY: check
check:
	R CMD check package

.PHONY: build
build: 
	R CMD build package

.PHONY: install
install:
	R CMD INSTALL container_1.0.tar.gz

.PHONY: update
update: build install

########################################################################
# Backup to the cloud

.PHONY: u1
u1: 
	rsync -a package $(HOME)/Ubuntu\ One/container
