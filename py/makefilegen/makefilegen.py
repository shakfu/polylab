"""makefilegen: makefile generator

Makefile generator / direct compilation tool,
extracted from my work in shedskin.makefile in the shedskin project,
as this generic part of the code may be generally useful.
https://github.com/shedskin/shedskin
"""

import os
import platform
import re
import shutil
import subprocess
import sys
import sysconfig
from pathlib import Path
from typing import Any, Callable, Optional, TypeAlias

# type aliases
PathLike: TypeAlias = Path | str
TestFunc: TypeAlias = Callable[[str], bool]

# constants
PLATFORM = platform.system()


# -----------------------------------------------------------------------------
# utility functions]


def always_true(_: Any) -> bool:
    """dummy test function always returns True"""
    return True


def env_var(name: str) -> str:
    """return environment variable"""
    return f"${{{name}}}"


def check_output(cmd: str) -> Optional[str]:
    """Run a command and return its output, or None if command not found"""
    try:
        return subprocess.check_output(cmd.split(), encoding="utf8").strip()
    except FileNotFoundError:
        return None


# -----------------------------------------------------------------------------
# main classes


class MakefileWriter:
    """Handles writing Makefile contents"""

    def __init__(self, path: PathLike):
        self.makefile = open(path, "w", encoding="utf8")

    def write(self, line: str = "") -> None:
        """Write a line to the Makefile"""
        print(line, file=self.makefile)

    def close(self) -> None:
        """Close the Makefile"""
        self.makefile.close()


class PythonSystem:
    """Python system information"""

    def __init__(self):
        self.name = "Python"
        self.version_info = sys.version_info

    def __str__(self):
        return self.version

    @property
    def version(self) -> str:
        """semantic version of python: 3.11.10"""
        return f"{self.major}.{self.minor}.{self.patch}"

    @property
    def ver(self) -> str:
        """short major.minor python version: 3.11"""
        return f"{self.major}.{self.minor}"

    @property
    def ver_nodot(self) -> str:
        """concat major and minor version components: 311 in 3.11.7"""
        return self.ver.replace(".", "")

    @property
    def major(self) -> int:
        """major component of semantic version: 3 in 3.11.7"""
        return self.version_info.major

    @property
    def minor(self) -> int:
        """minor component of semantic version: 11 in 3.11.7"""
        return self.version_info.minor

    @property
    def patch(self) -> int:
        """patch component of semantic version: 7 in 3.11.7"""
        return self.version_info.micro

    @property
    def name_version(self) -> str:
        """return <name>-<fullversion>: e.g. Python-3.11.7"""
        return f"{self.name}-{self.version}"

    @property
    def name_ver(self) -> str:
        """return <name.lower><ver>: e.g. python3.11"""
        return f"{self.name.lower()}{self.ver}"

    @property
    def executable_name(self) -> str:
        """executable name"""
        name = self.name.lower()
        if PLATFORM == "Windows":
            name = f"{self.name}.exe"
        return name

    @property
    def libname(self) -> str:
        """library name prefix"""
        return f"lib{self.name}"

    @property
    def linklib(self) -> str:
        """name of library for linking"""
        return f"-l{self.name_ver}"

    @property
    def staticlib_name(self) -> str:
        """static libname"""
        suffix = ".a"
        if PLATFORM == "Windows":
            suffix = ".lib"
        return f"{self.libname}{suffix}"

    @property
    def dylib_name(self) -> str:
        """dynamic link libname"""
        if PLATFORM == "Windows":
            return f"{self.libname}.dll"
        if PLATFORM == "Darwin":
            return f"{self.libname}.dylib"
        return f"{self.libname}.so"

    @property
    def dylib_linkname(self) -> str:
        """symlink to dylib"""
        if PLATFORM == "Darwin":
            return f"{self.libname}.dylib"
        return f"{self.libname}.so"

    @property
    def prefix(self) -> str:
        """python system prefix"""
        return sysconfig.get_config_var("prefix")

    @property
    def include_dir(self) -> str:
        """python include directory"""
        return sysconfig.get_config_var("INCLUDEPY")

    @property
    def config_h_dir(self) -> str:
        """directory of config.h file"""
        return os.path.dirname(sysconfig.get_config_h_filename())

    @property
    def base_cflags(self) -> str:
        """python base cflags"""
        return sysconfig.get_config_var("BASECFLAGS")

    @property
    def libs(self) -> str:
        """python libs to link to"""
        return sysconfig.get_config_var("LIBS")

    @property
    def syslibs(self) -> str:
        """python system libs to link to"""
        return sysconfig.get_config_var("SYSLIBS")

    @property
    def is_shared(self) -> bool:
        """python system was built with enable_shared option"""
        return bool(sysconfig.get_config_var("Py_ENABLE_SHARED"))

    @property
    def libpl(self) -> str:
        """directory of python dependencies"""
        return sysconfig.get_config_var("LIBPL")

    @property
    def extension_suffix(self) -> str:
        """suffix of python extension"""
        if PLATFORM == "Windows":
            return ".pyd"
        return ".so"


class Builder:
    """Configure and execute compiler instructions."""

    def __init__(self, target: PathLike, strict: bool = False):
        self.target = target
        self.strict = strict  # raise error if entry already exists
        self._cc = "gcc"
        self._cxx = "g++"
        self._cppfiles: list[str] = []
        self._hppfiles: list[str] = []
        self._include_dirs: list[str] = []  # include directories
        self._cflags: list[str] = []  # c compiler flags
        self._cxxflags: list[str] = []  # c++ compiler flags
        self._link_dirs: list[str] = []  # link directories
        self._ldlibs: list[str] = []  # link libraries
        self._ldflags: list[str] = []  # linker flags + link_dirs
        self._cleanup_patterns: list[str] = []  # post-build cleanup by glob pattern
        self._cleanup_targets: list[str] = []  # post-build cleanup by path

    @property
    def cc(self) -> str:
        """c compiler"""
        return self._cc

    @cc.setter
    def cc(self, value: str) -> None:
        """set c compiler"""
        self._cc = value

    @property
    def cxx(self) -> str:
        """c++ compiler"""
        return self._cxx

    @cxx.setter
    def cxx(self, value: str) -> None:
        """set c++ compiler"""
        self._cxx = value

    @property
    def cppfiles(self) -> list[str]:
        """c++ files"""
        return self._cppfiles

    @cppfiles.setter
    def cppfiles(self, value: list[str]) -> None:
        """set c++ files"""
        self._cppfiles = value

    @property
    def hppfiles(self) -> list[str]:
        """hpp files"""
        return self._hppfiles

    @hppfiles.setter
    def hppfiles(self, value: list[str]) -> None:
        """set hpp files"""
        self._hppfiles = value

    @property
    def include_dirs(self) -> list[str]:
        """include directories"""
        return self._include_dirs

    @include_dirs.setter
    def include_dirs(self, value: list[str]) -> None:
        """set include directories"""
        self._include_dirs = value

    @property
    def cflags(self) -> list[str]:
        """c compiler flags"""
        return self._cflags

    @cflags.setter
    def cflags(self, value: list[str]) -> None:
        """set c compiler flags"""
        self._cflags = value

    @property
    def cxxflags(self) -> list[str]:
        """c++ compiler flags"""
        return self._cxxflags

    @cxxflags.setter
    def cxxflags(self, value: list[str]) -> None:
        """set c++ compiler flags"""
        self._cxxflags = value

    @property
    def link_dirs(self) -> list[str]:
        """link directories"""
        return self._link_dirs

    @link_dirs.setter
    def link_dirs(self, value: list[str]) -> None:
        """set link directories"""
        self._link_dirs = value

    @property
    def ldlibs(self) -> list[str]:
        """link libraries"""
        return self._ldlibs

    @ldlibs.setter
    def ldlibs(self, value: list[str]) -> None:
        """set link libraries"""
        self._ldlibs = value

    @property
    def ldflags(self) -> list[str]:
        """linker flags"""
        return self._ldflags

    @ldflags.setter
    def ldflags(self, value: list[str]) -> None:
        """set linker flags"""
        self._ldflags = value

    @property
    def cleanup_patterns(self) -> list[str]:
        """cleanup post-build by glob pattern"""
        return self._cleanup_patterns

    @cleanup_patterns.setter
    def cleanup_patterns(self, value: list[str]) -> None:
        """set cleanup post-build by glob pattern"""
        self._cleanup_patterns = value

    @property
    def cleanup_targets(self) -> list[str]:
        """cleanup post-build by path"""
        return self._cleanup_targets

    @cleanup_targets.setter
    def cleanup_targets(self, value: list[str]) -> None:
        """set cleanup post-build by path"""
        self._cleanup_targets = value

    @property
    def build_cmd(self) -> str:
        """Get the executable or extension build command"""
        return f"{self.CXX} {self.CXXFLAGS} {self.CPPFILES} {self.LDLIBS} {self.LDFLAGS} -o {self.TARGET}"

    @property
    def TARGET(self) -> str:
        """compilation product"""
        return str(self.target)

    @property
    def CPPFILES(self) -> str:
        """c++ files"""
        return " ".join(self.cppfiles)

    @property
    def HPPFILES(self) -> str:
        """hpp files"""
        return " ".join(self.hppfiles)

    @property
    def CXX(self) -> str:
        """c++ compiler"""
        return self.cxx

    @property
    def CFLAGS(self) -> str:
        """c compiler flags"""
        return " ".join(self.cflags)

    @property
    def CXXFLAGS(self) -> str:
        """c++ compiler flags"""
        _flags = " ".join(self.cxxflags)
        return f"{_flags} {self.INCLUDEDIRS}"

    @property
    def INCLUDEDIRS(self) -> str:
        """include directories"""
        return " ".join(self.include_dirs)

    @property
    def LINKDIRS(self) -> str:
        """link directories"""
        return " ".join(self.link_dirs)

    @property
    def LDFLAGS(self) -> str:
        """linker flags"""
        _flags = " ".join(self.ldflags)
        return f"{_flags} {self.LINKDIRS}"

    @property
    def LDLIBS(self) -> str:
        """link libraries"""
        return " ".join(self.ldlibs)

    def _add_config_entries(
        self,
        attr: str,
        prefix: str = "",
        test_func: Optional[TestFunc] = None,
        *entries,
    ) -> None:
        """Add an entry to the configuration"""
        assert hasattr(self, attr), f"Invalid attribute: {attr}"
        _list = getattr(self, attr)
        if not test_func:
            test_func = always_true
        for entry in entries:
            assert test_func(entry), f"Invalid entry: {entry}"
            if entry in _list:
                if self.strict:
                    raise ValueError(f"entry: {entry} already exists in {attr} list")
                continue
            _list.append(f"{prefix}{entry}")

    def _execute(self, cmd: str) -> None:
        """Execute a command"""
        print(cmd)
        os.system(cmd)

    def _remove(self, path: PathLike) -> None:
        """Remove a target"""
        path = Path(path)
        if path.is_dir():
            shutil.rmtree(path, ignore_errors=False)
        else:
            try:
                path.unlink()
            except FileNotFoundError:
                pass

    def configure(self) -> None:
        """Configure the builder"""
        self._setup_defaults()

    def build(self, dry_run: bool = False) -> None:
        """configure, then build executable or extension"""
        self.configure()
        if dry_run:
            print(self.build_cmd)
        else:
            print()
            self._execute(self.build_cmd)
            if self.cleanup_patterns or self.cleanup_targets:
                self.clean()

    def clean(self) -> None:
        """Clean up build artifacts"""
        for pattern in self.cleanup_patterns:
            for path in Path(".").glob(pattern):
                self._remove(path)
        for target in self.cleanup_targets:
            self._remove(target)

    def run_executable(self) -> None:
        """Run the executable"""
        print(f"Running {self.target}")
        self._execute(f"./{self.target}")

    def add_cppfiles(self, *entries: str) -> None:
        """Add c++ files to the configuration"""
        self._add_config_entries("_cppfiles", "", None, *entries)

    def add_hppfiles(self, *entries: str) -> None:
        """Add hpp files to the configuration"""
        self._add_config_entries("_hppfiles", "", None, *entries)

    def add_include_dirs(self, *entries):
        """Add include directories to the configuration"""
        self._add_config_entries("_include_dirs", "-I", os.path.isdir, *entries)

    def add_cflags(self, *entries):
        """Add compiler flags to the configuration"""
        self._add_config_entries("_cflags", "", None, *entries)

    def add_cxxflags(self, *entries):
        """Add c++ compiler flags to the configuration"""
        self._add_config_entries("_cxxflags", "", None, *entries)

    def add_link_dirs(self, *entries):
        """Add link directories to the configuration"""
        self._add_config_entries("_link_dirs", "-L", os.path.isdir, *entries)

    def add_ldlibs(self, *entries):
        """Add link libraries to the configuration"""
        self._add_config_entries("_ldlibs", "", None, *entries)

    def add_ldflags(self, *entries):
        """Add linker flags to the configuration"""
        self._add_config_entries("_ldflags", "", None, *entries)

    def add_cleanup_patterns(self, *entries):
        """Add cleanup patterns to the configuration"""
        self._add_config_entries("_cleanup_patterns", "", None, *entries)

    def add_cleanup_targets(self, *entries):
        """Add cleanup targets to the configuration"""
        self._add_config_entries("_cleanup_targets", "", None, *entries)

    def _setup_defaults(self):
        """Setup default model configuration"""
        self.add_include_dirs(os.getcwd())


class MakefileGenerator:
    """Generates Makefile for C/C++ code"""

    def __init__(self, path: PathLike, strict: bool = False):
        self.path = path
        self.strict = strict  # raise error if variable or entry already exists
        self.cxx = "g++"
        self.vars: dict[str, PathLike] = {}  # variables
        self.var_order: list[str] = []  # write order of variables
        self.include_dirs: list[str] = []  # include directories
        self.cflags: list[str] = []  # c compiler flags
        self.cxxflags: list[str] = []  # c++ compiler flags
        self.link_dirs: list[str] = []  # link directories
        self.ldlibs: list[str] = []  # link libraries
        self.ldflags: list[str] = []  # linker flags + link_dirs
        self.targets: list[str] = []  # targets
        self.phony: list[str] = []  # phony targets
        self.clean: list[str] = []  # clean target
        # writer
        self.writer = MakefileWriter(path)

    def write(self, text: Optional[str] = None) -> None:
        """Write a line to the Makefile"""
        if not text:
            self.writer.write("")
        else:
            self.writer.write(text)

    def close(self) -> None:
        """Close the Makefile"""
        self.writer.close()

    def check_dir(self, path: PathLike) -> bool:
        """Check if a path is a valid directory"""
        defaults = {"HOME": "$(HOME)", "PWD": "$(PWD)", "CURDIR": "$(CURDIR)"}
        str_path = str(path)
        # check if path contains a variable
        # FIXME: should check for multiple variables
        if str(path) in defaults.values():
            return True
        match = re.match(r".*\$+\((.+)\).*", str_path)
        if match:
            key = match.group(1)
            if key in defaults:
                return True
            assert key in self.vars, f"Invalid variable: {key}"
            assert os.path.isdir(
                self.vars[key]
            ), f"Value of variable {key} is not a directory: {self.vars[key]}"
            return True
        return os.path.isdir(str_path)

    def _normalize_path(self, path: str) -> str:
        """Normalize a path"""
        cwd = os.getcwd()
        home = os.path.expanduser("~")
        return path.replace(cwd, "$(CURDIR)").replace(home, "$(HOME)")

    def _normalize_paths(self, filenames: list[str]) -> list[str]:
        """Replace filenames with current directory"""
        cwd = os.getcwd()
        home = os.path.expanduser("~")
        return [f.replace(cwd, "$(CURDIR)").replace(home, "$(HOME)") for f in filenames]

    def _add_entry_or_variable(
        self,
        attr: str,
        prefix: str = "",
        test_func: Optional[TestFunc] = None,
        *entries,
        **kwargs,
    ) -> None:
        """Add an entry or variable to the Makefile"""
        assert hasattr(self, attr), f"Invalid attribute: {attr}"
        _list = getattr(self, attr)
        if not test_func:
            test_func = always_true
        for entry in entries:
            assert test_func(entry), f"Invalid entry: {entry}"
            if entry in _list:
                if self.strict:
                    raise ValueError(f"entry: {entry} already exists in {attr} list")
                continue
            _list.append(f"{prefix}{entry}")
        for key, value in kwargs.items():
            assert test_func(value), f"Invalid value: {value}"
            if key in self.vars:
                if self.strict:
                    raise ValueError(f"variable: {key} already exists in vars dict")
                continue
            self.vars[key] = value
            _list.append(f"{prefix}$({key})")
            self.var_order.append(key)

    def add_variable(self, key: str, value: str) -> None:
        """Add a variable to the Makefile"""
        self.vars[key] = value
        self.var_order.append(key)

    def add_include_dirs(self, *entries, **kwargs):
        """Add include directories to the Makefile"""
        self._add_entry_or_variable(
            "include_dirs", "-I", self.check_dir, *entries, **kwargs
        )

    def add_cflags(self, *entries, **kwargs):
        """Add compiler flags to the Makefile"""
        self._add_entry_or_variable("cflags", "", None, *entries, **kwargs)

    def add_cxxflags(self, *entries, **kwargs):
        """Add c++ compiler flags to the Makefile"""
        self._add_entry_or_variable("cxxflags", "", None, *entries, **kwargs)

    def add_link_dirs(self, *entries, **kwargs):
        """Add link directories to the Makefile"""
        self._add_entry_or_variable(
            "link_dirs", "-L", self.check_dir, *entries, **kwargs
        )

    def add_ldlibs(self, *entries, **kwargs):
        """Add link libraries to the Makefile"""
        self._add_entry_or_variable("ldlibs", "", None, *entries, **kwargs)

    def add_ldflags(self, *entries, **kwargs):
        """Add linker flags to the Makefile"""
        self._add_entry_or_variable("ldflags", "", None, *entries, **kwargs)

    def add_target(
        self, name: str, body: Optional[str] = None, deps: Optional[list[str]] = None
    ):
        """Add targets to the Makefile"""
        if body and deps:
            _deps = " ".join(deps)
            self.targets.append(f"{name}: {_deps}\n\t{body}")
        elif body and not deps:
            self.targets.append(f"{name}:\n\t{body}")
        elif not body and deps:
            _deps = " ".join(deps)
            self.targets.append(f"{name}: {_deps}")
        else:  # no body or dependencies
            raise ValueError("Either body or dependencies must be provided")

    def add_phony(self, *entries):
        """Add phony targets to the Makefile"""
        for entry in entries:
            if entry and entry not in self.phony:
                self.phony.append(entry)

    def add_clean(self, *entries):
        """Add clean target to the Makefile"""
        for entry in entries:
            if entry and entry not in self.clean:
                self.clean.append(entry)

    def _setup_defaults(self):
        """Setup default model configuration"""
        self.add_include_dirs(
            "$(CURDIR)"
        )  # CURDIR is absolute path to the current directory

    def _write_filelist(self, name: str, files: list[str]) -> None:
        """Write a file list to the Makefile"""
        if not files:
            return
        if len(files) == 1:
            self.write(f"{name}={files[0]}")
        else:
            filelist = " \\\n\t".join(files)
            self.write(f"{name}=\\\n\t{filelist}\n")

    def _write_variables(self) -> None:
        """Write variables to the Makefile"""
        self.write("# project variables")
        for key in self.var_order:
            value = self.vars[key]
            self.write(f"{key}={value}")
        self.write()

        # write includes
        if self.include_dirs:
            include_dirs = " ".join(self.include_dirs)
            self.write(f"INCLUDEDIRS={include_dirs}")
        # write link_dirs
        if self.link_dirs:
            link_dirs = " ".join(self.link_dirs)
            self.write(f"LINKDIRS={link_dirs}")
        self.write()

        # write cxx compiler
        self.write(f"CXX={self.cxx}")
        # write cflags
        if self.cflags:
            cflags = " ".join(self.cflags)
            self.write(f"CFLAGS+={cflags} $(INCLUDEDIRS)")
        # write cxxflags
        if self.cxxflags:
            cxxflags = " ".join(self.cxxflags)
            self.write(f"CXXFLAGS+={cxxflags} $(INCLUDEDIRS)")
        # write ldflags / link_dirs
        if self.ldflags or self.link_dirs:
            ldflags = " ".join(self.ldflags)
            self.write(f"LDFLAGS+={ldflags} $(LINKDIRS)")
        # write ldlibs
        if self.ldlibs:
            ldlibs = " ".join(self.ldlibs)
            self.write(f"LDLIBS={ldlibs}")
        self.write()

    def _write_phony(self) -> None:
        """Write phony targets to the Makefile"""
        if self.phony:
            phone_targets = " ".join(self.phony)
            self.write()
            self.write(f".PHONY: {phone_targets}")
            self.write()

    def _write_targets(self) -> None:
        """Write targets to the Makefile"""
        for target in sorted(self.targets):
            self.write(target)
            self.write()

    def _write_clean(self) -> None:
        """Write clean target to the Makefile"""
        if self.clean:
            clean_targets = " ".join(self.clean)
            self.write(f"clean:\n\t@rm -rf {clean_targets}")
            self.write()

    def generate(self) -> None:
        """Generate the Makefile"""
        self._setup_defaults()
        self._write_variables()
        self._write_phony()
        self._write_targets()
        self._write_clean()
        self.close()


if __name__ == "__main__":

    def test_builder() -> None:
        """Test Builder"""
        b = Builder("product")
        b.add_include_dirs("/opt/homebrew/include")
        b.add_cxxflags()
        b.add_cxxflags("-Wall", "-Wextra", "-std=c++11", "-O3")
        b.add_ldflags("-shared", "-Wl,-rpath,/usr/local/lib", "-fPIC")
        b.add_link_dirs("/usr/lib", "/usr/local/lib")
        b.add_ldlibs("-lpthread")
        b.build(dry_run=True)

    def test_makefile_generator() -> None:
        """Test MakefileGenerator"""
        m = MakefileGenerator("Makefile")
        m.add_variable("TEST", "test")
        m.add_include_dirs("/opt/homebrew/include")
        m.add_cflags("-Wall", "-Wextra")
        m.add_cxxflags("-Wall", "-Wextra", "-std=c++11", "-O3")
        m.add_ldflags("-shared", "-Wl,-rpath,/usr/local/lib", "-fPIC")
        m.add_link_dirs("/usr/lib", "/usr/local/lib")
        m.add_ldlibs("-lpthread")
        m.add_target("all", deps=["build", "test"])
        m.add_target("build", deps=["tool.exe"])
        m.add_target(
            "tool.exe",
            "$(CXX) $(CPPFILES) $(CXXFLAGS) $(LDFLAGS) -o $@ $^",
            deps=["a.o", "b.o"],
        )
        m.add_target("test", "echo $(TEST)", deps=["test.o"])
        m.add_phony("all", "build", "test")
        m.add_clean("test.o", "*.o")
        m.generate()

    test_builder()
    test_makefile_generator()
