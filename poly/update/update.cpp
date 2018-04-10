//============================================================================
// Name        : update.cpp
// Author      : sa
// Version     : 0.1
// Copyright   : public domain
// Description : Takes a directory and updates the source files
// Build	   : g++ -o update -lboost_filesystem update.cpp
//============================================================================
#include "boost/filesystem.hpp"
#include <iostream>
#include <string>
//#include <cstdio>

using namespace std;
using namespace boost::filesystem;

string DEFAULT_DIR = "/home/sa/src";

class Application {

public:

	void update_directory(string topdir) {

		path root(topdir);

		if (exists(root) && is_directory(root)) {
			cout << "root: " << root << endl;

			// project level
			directory_iterator end_proj_itr;
			path project;

			for (directory_iterator proj_itr(root); proj_itr != end_proj_itr; ++proj_itr) {
				project = proj_itr->path();
				cout << "\nproj:" << project << endl;

				// directory level
				directory_iterator end_dir_itr;
				path dir;

				for (directory_iterator dir_itr(proj_itr->path()); dir_itr
						!= end_dir_itr; ++dir_itr) {
					dir = dir_itr->path();
					if (is_directory(dir)) {

						//cout << "dir:" << dir << endl;

						if (dir.filename().compare(".bzr") == 0) {
							current_path(project);
							system("bzr pull");
							system("bzr update");
						} else if (dir.filename().compare(".hg") == 0) {
							current_path(project);
							system("hg pull");
							system("hg update");
						} else if (dir.filename().compare(".svn") == 0) {
							current_path(project);
							system("svn update");
						} else if (dir.filename().compare(".git") == 0) {
							current_path(project);
							system("git pull");
						} else
							continue;
					}
				}
			}
		}
	}
};

int main(int argc, char **argv) {
	int i;
	Application app;

	if (argc < 2) {
		app.update_directory(DEFAULT_DIR);
	} else {
		for (i = 0; i < argc; i++) {
			//printf("argv[%d] = \"%s\"\n", i, argv[i]);
			app.update_directory(argv[i]);
		}
	}
	return 0;
}
