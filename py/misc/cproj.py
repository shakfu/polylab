#!/usr/bin/env python3

"""
This is a little script which generates C or C++ project templates.
Examples:
    To create project named hello_world, which uses make to build:
    cproj.py -b make hello_world
    To create c++ library named libworld, which uses make to build:
    cproj.py -xlb make world
    To create c library named libworld, which uses cmake as a build system:
    cproj.py -lb cmake world
"""

import argparse
import os
import sys

def main():
    args = parse_arguments()

    language = get_language(args)

    project = createproject(language, args.name, args.library)

    project.create_directory_structure()
    project.create_source_files()

    build_system = create_build_system(args.build_system, project)
    build_system.create_makefile()


def parse_arguments():
    parser = argparse.ArgumentParser(
            description=__doc__,
            formatter_class=argparse.RawDescriptionHelpFormatter
            )

    parser.add_argument("-x", "--cpp", 
            help="Pass this option if this is a C++ project", 
            action="store_true"
            )

    parser.add_argument("-l", "--library", 
            help="Pass this option if this project is a library",
            action="store_true"
            )

    parser.add_argument("-b", "--build-system",
            help="Which build system the project uses (cmake, make or no build system)",
            default="none",
            choices=["make", "cmake", "none"]
            )

    parser.add_argument("name", 
            help="Project name. If it is a library, pass a name without 'lib' prefix"
            )

    return parser.parse_args()


def get_language(args):
    return "cpp" if args.cpp else "c"


def createproject(language, name, library):
    if language == "c" and not library:
        return CProject(name)
    elif language == "cpp" and not library:
        return CPPProject(name)
    elif language == "c" and library:
        return CLibraryProject(name)
    elif language == "cpp" and library:
        return CPPLibraryProject(name)


def create_build_system(build_system, project):
    if build_system == "make":
        return MakeBuildSystem(project)
    elif build_system == "cmake":
        return CMakeBuildSystem(project)
    else:
        return BuildSystem(project)


class Project:
    def __init__(self, name):
        self.name = name

    def create_directory_structure(self):
        os.mkdir(self.name)
        os.mkdir(self.name + "/include")
        os.mkdir(self.name + "/src")


class CProject(Project):
    compiler = "gcc"
    extension = "c"
    is_library = False
    file = "main"

    def create_source_files(self):
        with open(self.name + "/src/main.c", "w") as file:
          file.write(C_SOURCE.format(project_name=self.name))  


class CPPProject(Project):
    compiler = "g++"
    extension = "cpp"
    file = "main"
    is_library = False

    def create_source_files(self):
        with open(self.name + "/src/main.cpp", "w") as file:
          file.write(CPP_SOURCE.format(project_name=self.name))  


class CLibraryProject(Project):
    compiler = "gcc"
    extension = "c"
    is_library = True

    def __init__(self, name):
        super().__init__(name)
        self.file = name

    def create_source_files(self):
        with open(self.name + "/src/" + self.file + ".c", "w") as file:
            file.write(C_LIBRARY_SOURCE.format(project_name=self.name))  
        with open(self.name + "/include/" + self.file + ".h", "w") as file:
            file.write(C_LIBRARY_HEADER.format(project_name=self.name))


class CPPLibraryProject(Project):
    compiler = "g++"
    extension = "cpp"
    is_library = True
    
    def __init__(self, name):
        super().__init__(name)
        self.file = name

    def create_source_files(self):
        with open(self.name + "/src/" + self.file +".cpp", "w") as file:
            file.write(CPP_LIBRARY_SOURCE.format(project_name=self.name))  
        with open(self.name + "/include/" + self.file + ".h", "w") as file:
            file.write(CPP_LIBRARY_HEADER.format(project_name=self.name))


class BuildSystem:
    def __init__(self, project):
        self.project = project

    def create_makefile(self):
        pass


class MakeBuildSystem(BuildSystem):

    def create_makefile(self):
        build_folder = self.project.name + "/build"
        makefile_path = build_folder + "/Makefile"
        os.mkdir(build_folder)
        with open(makefile_path, "w") as file:
            vars = {}
            vars["project"] = self.project.name
            vars["cc"] = self.project.compiler
            vars["cflags"] = "-Wall -g -I../include"
            vars["ldflags"] = ""
            vars["ext"] = self.project.extension
            vars["file"] = self.project.file

            if self.project.is_library:
                vars["cflags"] += " -fPIC" 
                vars["ldflags"] += "-shared"
                vars["project"] = "lib" + vars["project"] + ".so"

            file.write(MAKEFILE_SOURCE.format(**vars))


class CMakeBuildSystem(BuildSystem):

    def create_makefile(self):
        with open(self.project.name + "/CMakeLists.txt", "w") as file:
            file.write(CMAKELISTS_SOURCE.format(
                project=self.project.name,
                ext=self.project.extension))


C_SOURCE = """#include <stdio.h>
int main(int argc, char ** argv)
{{
  printf("Hello {project_name}\\n");
  return 0;
}}
"""


C_LIBRARY_SOURCE = """#include <stdio.h>
#include "{project_name}.h"
int {project_name}(void)
{{
  printf("Hello {project_name}\\n");
  return 0;
}}
"""


CPP_SOURCE = """#include <iostream>
int main(int argc, char ** argv)
{{
  std::cout << "Hello {project_name}" << std::endl;
  return 0;
}}
"""

CPP_LIBRARY_SOURCE = """#include <iostream>
#include "{project_name}.h"
int {project_name}()
{{
  std::cout << "Hello {project_name}" << std::endl;
  return 0;
}}
"""

CPP_LIBRARY_HEADER = """#ifndef __{project_name}_h__
#define __{project_name}_h__
int {project_name}();
#endif
"""


C_LIBRARY_HEADER = """#ifndef __{project_name}_h__
#define __{project_name}_h__
int {project_name}(void);
#endif
"""


MAKEFILE_SOURCE = """CC={cc}
TARGET={project}
LDFLAGS+={ldflags}
CFLAGS+={cflags}
SOURCES=../src/{file}.{ext}
OBJECTS=$(SOURCES:.{ext}=.o)
all: $(TARGET)
$(TARGET): $(OBJECTS)
\t$(CC) $(OBJECTS) $(LDFLAGS) -o $(TARGET)
%.o: $.{ext}
\t$(CC) -c $(CFLAGS) $< -o $@
clean:
\trm -f $(TARGET)
\trm -f $(OBJECTS)
"""


CMAKELISTS_SOURCE = """cmake_minimum_required(VERSION 2.8.11)
project({project})
file(GLOB files src/*.{ext})
include_directories(include)
add_executable({project} ${{files}})
"""
    

if __name__ == "__main__":
    main();
