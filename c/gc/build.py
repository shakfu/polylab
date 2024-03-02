#!/usr/bin/env python3
"""buildpy.py - builds python from source

features:

- Single script which downloads, builds python from source
- Different build configurations (static, dynamic) possible
- Trims python builds and zips site-packages by default.

"""

import datetime
import logging
import os
import platform
import shutil
import stat
import subprocess
import sys
import tarfile
import tempfile
from fnmatch import fnmatch
from pathlib import Path
from typing import Callable, Optional, Union
from urllib.request import urlretrieve


__version__ = "0.0.1"

# ----------------------------------------------------------------------------
# type aliases

Pathlike = Union[str, Path]
MatchFn = Callable[[Path], bool]
ActionFn = Callable[[Path], None]

# ----------------------------------------------------------------------------
# env helper

def getenv(key: str, default: bool = False) -> bool:
    """convert '0','1' env values to bool {True, False}"""
    return bool(int(os.getenv(key, default)))

def setenv(key: str, default: str):
    """get environ variable if it is exists else set default"""
    if key in os.environ:
        return os.getenv(key, default)
    else:
        os.environ[key] = default
        return default

# ----------------------------------------------------------------------------
# constants

PYTHON = sys.executable
PLATFORM = platform.system()
ARCH = platform.machine()
PY_VER_MINOR = sys.version_info.minor
if PLATFORM == "Darwin":
    MACOSX_DEPLOYMENT_TARGET = setenv("MACOSX_DEPLOYMENT_TARGET", "12.6")
DEFAULT_PY_VERSION = "3.11.7"
DEBUG = getenv('DEBUG', default=True)
COLOR = getenv('COLOR', default=True)

# ----------------------------------------------------------------------------
# logging config

class CustomFormatter(logging.Formatter):
    """custom logging formatting class"""

    white = "\x1b[97;20m"
    grey = "\x1b[38;20m"
    green = "\x1b[32;20m"
    cyan = "\x1b[36;20m"
    yellow = "\x1b[33;20m"
    red = "\x1b[31;20m"
    bold_red = "\x1b[31;1m"
    reset = "\x1b[0m"
    fmt = "%(delta)s - %(levelname)s - %(name)s.%(funcName)s - %(message)s"
    cfmt = (f"{white}%(delta)s{reset} - "
            f"{{}}%(levelname)s{{}} - "
            f"{white}%(name)s.%(funcName)s{reset} - "
            f"{grey}%(message)s{reset}")

    FORMATS = {
        logging.DEBUG: cfmt.format(grey, reset),
        logging.INFO: cfmt.format(green, reset),
        logging.WARNING: cfmt.format(yellow, reset),
        logging.ERROR: cfmt.format(red, reset),
        logging.CRITICAL: cfmt.format(bold_red, reset),
    }

    def __init__(self, use_color=COLOR):
        self.use_color = use_color

    def format(self, record):
        """custom logger formatting method"""
        if not self.use_color:
            log_fmt = self.fmt
        else:
            log_fmt = self.FORMATS.get(record.levelno)
        if PY_VER_MINOR > 10:
            duration = datetime.datetime.fromtimestamp(
                record.relativeCreated / 1000, datetime.UTC
            )
        else:
            duration = datetime.datetime.utcfromtimestamp(
                record.relativeCreated / 1000)
        record.delta = duration.strftime("%H:%M:%S")
        formatter = logging.Formatter(log_fmt)
        return formatter.format(record)


strm_handler = logging.StreamHandler()
strm_handler.setFormatter(CustomFormatter())
# file_handler = logging.FileHandler("log.txt", mode='w')
# file_handler.setFormatter(CustomFormatter(use_color=False))
logging.basicConfig(
    level=logging.DEBUG if DEBUG else logging.INFO,
    handlers=[strm_handler],
    # handlers=[strm_handler, file_handler],
)


# ----------------------------------------------------------------------------
# utility classes

class ShellCmd:
    """Provides platform agnostic file/folder handling."""

    log: logging.Logger

    def cmd(self, shellcmd: str, cwd: Pathlike = "."):
        """Run shell command within working directory"""
        self.log.info(shellcmd)
        subprocess.call(shellcmd, shell=True, cwd=str(cwd))

    def download(self, url: str, tofolder: Optional[Pathlike] = None) -> Pathlike:
        """Download a file from a url to an optional folder"""
        _path = Path(os.path.basename(url))
        if tofolder:
            _path = Path(tofolder).joinpath(_path)
            if _path.exists():
                return _path
        filename, _ = urlretrieve(url, filename=_path)
        return Path(filename)

    def extract(self, archive: Pathlike, tofolder: Pathlike = "."):
        """extract a tar archive"""
        if tarfile.is_tarfile(archive):
            with tarfile.open(archive) as f:
                f.extractall(tofolder)
        # elif zipfile.is_zipfile(archive):
        #     with zipfile.ZipFile(archive) as f:
        #         f.extractall(tofolder)
        else:
            raise TypeError("cannot extract from this file.")

    def fail(self, msg, *args):
        """exits the program with an error msg."""
        self.log.critical(msg, *args)
        sys.exit(1)

    def git_clone(
        self,
        url: str,
        recurse: bool = False,
        branch: Optional[str] = None,
        cwd: Pathlike = ".",
    ):
        """git clone a repository source tree from a url"""
        _cmds = ["git clone --depth 1"]
        if branch:
            _cmds.append(f"--branch {branch}")
        if recurse:
            _cmds.append("--recurse-submodules --shallow-submodules")
        _cmds.append(url)
        self.cmd(" ".join(_cmds), cwd=cwd)

    def getenv(self, key: str, default: bool = False) -> bool:
        """convert '0','1' env values to bool {True, False}"""
        self.log.info("checking env variable: %s", key)
        return bool(int(os.getenv(key, default)))

    def chdir(self, path: Pathlike):
        """Change current workding directory to path"""
        self.log.info("changing working dir to: %s", path)
        os.chdir(path)

    def chmod(self, path: Pathlike, perm=0o777):
        """Change permission of file"""
        self.log.info("change permission of %s to %s", path, perm)
        os.chmod(path, perm)

    def get(self, shellcmd, cwd: Pathlike = ".", shell: bool = False) -> str:
        """get output of shellcmd"""
        if not shell:
            shellcmd = shellcmd.split()
        return subprocess.check_output(
            shellcmd, encoding="utf8", shell=shell, cwd=str(cwd)
        ).strip()

    def makedirs(self, path: Pathlike, mode: int = 511, exist_ok: bool = True):
        """Recursive directory creation function"""
        self.log.info("making directory: %s", path)
        os.makedirs(path, mode, exist_ok)

    def move(self, src: Pathlike, dst: Pathlike):
        """Move from src path to dst path."""
        self.log.info("move path %s to %s", src, dst)
        shutil.move(src, dst)

    def copy(self, src: Pathlike, dst: Pathlike):
        """copy file or folders -- tries to be behave like `cp -rf`"""
        self.log.info("copy %s to %s", src, dst)
        src, dst = Path(src), Path(dst)
        if src.is_dir():
            shutil.copytree(src, dst)
        else:
            shutil.copy2(src, dst)

    def remove(self, path: Pathlike, silent: bool = False):
        """Remove file or folder."""

        # handle windows error on read-only files
        def remove_readonly(func, path, excptn):
            "Clear the readonly bit and reattempt the removal"
            if func not in (os.unlink, os.rmdir) or excptn.winerror != 5:
                raise excptn
            os.chmod(path, stat.S_IWRITE)
            func(path)

        path = Path(path)
        if path.is_dir():
            if not silent:
                self.log.info("remove folder: %s", path)
            shutil.rmtree(path, ignore_errors=not DEBUG, onexc=remove_readonly)
        else:
            if not silent:
                self.log.info("remove file: %s", path)
            try:
                path.unlink()
            except FileNotFoundError:
                if not silent:
                    self.log.warning("file not found: %s", path)

    def walk(
        self,
        root: Pathlike,
        match_func: MatchFn,
        action_func: ActionFn,
        skip_patterns: list[str],
    ):
        """general recursive walk from root path with match and action functions"""
        for root_, dirs, filenames in os.walk(root):
            _root = Path(root_)
            if skip_patterns:
                for skip_pat in skip_patterns:
                    if skip_pat in dirs:
                        dirs.remove(skip_pat)
            for _dir in dirs:
                current = _root / _dir
                if match_func(current):
                    action_func(current)

            for _file in filenames:
                current = _root / _file
                if match_func(current):
                    action_func(current)

    def glob_remove(self, root: Pathlike, patterns: list[str], skip_dirs: list[str]):
        """applies recursive glob remove using a list of patterns"""

        def match(entry: Path) -> bool:
            # return any(fnmatch(entry, p) for p in patterns)
            return any(fnmatch(entry.name, p) for p in patterns)

        def remove(entry: Path):
            self.remove(entry)

        self.walk(root, match_func=match, action_func=remove,
                  skip_patterns=skip_dirs)

    def pip_install(
        self,
        *pkgs,
        reqs: Optional[str] = None,
        upgrade: bool = False,
        pip: Optional[str] = None,
    ):
        """Install python packages using pip"""
        _cmds = []
        if pip:
            _cmds.append(pip)
        else:
            _cmds.append("pip3")
        _cmds.append("install")
        if reqs:
            _cmds.append(f"-r {reqs}")
        else:
            if upgrade:
                _cmds.append("--upgrade")
            _cmds.extend(pkgs)
        self.cmd(" ".join(_cmds))

    def apt_install(self, *pkgs, update: bool = False):
        """install debian packages using apt"""
        _cmds = []
        _cmds.append("sudo apt install")
        if update:
            _cmds.append("--upgrade")
        _cmds.extend(pkgs)
        self.cmd(" ".join(_cmds))

    def brew_install(self, *pkgs, update: bool = False):
        """install using homebrew"""
        _pkgs = " ".join(pkgs)
        if update:
            self.cmd("brew update")
        self.cmd(f"brew install {_pkgs}")

    def cmake_config(self, src_dir: Pathlike, build_dir: Pathlike, *scripts, **options):
        """activate cmake configuration / generation stage"""
        _cmds = [f"cmake -S {src_dir} -B {build_dir}"]
        if scripts:
            _cmds.append(" ".join(f"-C {path}" for path in scripts))
        if options:
            _cmds.append(" ".join(f"-D{k}={v}" for k, v in options.items()))
        self.cmd(" ".join(_cmds))

    def cmake_build(self, build_dir: Pathlike, release: bool = False):
        """activate cmake build stage"""
        _cmd = f"cmake --build {build_dir}"
        if release:
            _cmd += " --config Release"
        self.cmd(_cmd)

    def cmake_install(self, build_dir: Pathlike, prefix: Optional[str] = None):
        """activate cmake install stage"""
        _cmds = ["cmake --install", str(build_dir)]
        if prefix:
            _cmds.append(f"--prefix {prefix}")
        self.cmd(" ".join(_cmds))

    def install_name_tool(self, src: Pathlike, dst: Pathlike, mode: str = "id"):
        """change dynamic shared library install names"""
        _cmd = f"install_name_tool -{mode} {src} {dst}"
        self.log.info(_cmd)
        self.cmd(_cmd)

# ----------------------------------------------------------------------------
# main classes

class Project:
    """Utility class to hold project directory structure"""

    def __init__(self):
        self.cwd = Path.cwd()
        self.build = self.cwd / 'build'
        self.downloads = self.build / "downloads"
        self.src = self.build / "src"
        self.install = self.build / 'install'

    def setup(self):
        """create main project directories"""
        self.build.mkdir(exist_ok=True, parents=True)
        self.downloads.mkdir(exist_ok=True)
        self.src.mkdir(exist_ok=True)
        print("src: %s", self.src)
        self.install.mkdir(exist_ok=True)


class Shedskin(Project):
    """Project structure for shedskin with local build caching"""

    def __init__(self):
        self.cwd = Path.home() / '.cache'
        self.build = self.cwd / 'shedskin'
        self.downloads = self.build / "downloads"
        self.src = Path(tempfile.gettempdir())
        self.install = self.build / 'install'


class Builder(ShellCmd):
    """Abstract builder class with additional methods common to subclasses."""

    name: str
    version: str
    url_template: str
    libs_static: list[str]
    depends_on: list[type["Builder"]]

    def __init__(
        self, version: Optional[str] = None, project: Optional[Project] = None
    ):
        self.version = version or self.version
        self.project = project or Project()
        self.log = logging.getLogger(self.__class__.__name__)

    def __repr__(self):
        return f"<{self.__class__.__name__} '{self.name}-{self.version}'>"

    def __iter__(self):
        for dependency in self.depends_on:
            yield dependency
            yield from iter(dependency)

    @property
    def ver(self):
        """short python version: 3.11"""
        return ".".join(self.version.split(".")[:2])

    @property
    def ver_major(self):
        """major compoent of semantic version: 3 in 3.11.7"""
        return self.version.split(".")[0]

    @property
    def ver_minor(self):
        """minor compoent of semantic version: 11 in 3.11.7"""
        return self.version.split(".")[1]

    @property
    def ver_patch(self):
        """patch compoent of semantic version: 7 in 3.11.7"""
        return self.version.split(".")[2]

    @property
    def ver_nodot(self):
        """concat major and minor version components: 311 in 3.11.7"""
        return self.ver.replace(".", "")

    @property
    def name_version(self):
        """return name-<fullversion>: e.g. Python-3.11.7"""
        return f"{self.name}-{self.version}"

    @property
    def name_ver(self):
        """return name.lower-<ver>: e.g. python3.11"""
        return f"{self.name.lower()}{self.ver}"

    @property
    def url(self):
        """return download url with version interpolated"""
        return self.url_template

    @property
    def src_path(self):
        """return extracted source folder of build target"""
        return self.project.src / self.name

    @property
    def build_dir(self):
        """return 'build' folder src dir of build target"""
        return self.src_path / "build"

    @property
    def executable_name(self):
        """executable name of buld target"""
        name = self.name
        if PLATFORM == "Windows":
            name = f"{self.name}.exe"
        return name

    @property
    def executable(self):
        """executable path of buld target"""
        return self.project.bin / self.executable_name

    @property
    def libname(self):
        """library name suffix"""
        return f"lib{self.name}"

    @property
    def staticlib_name(self):
        """static libname"""
        suffix = ".a"
        if PLATFORM == "Windows":
            suffix = ".lib"
        return f"{self.libname}{suffix}"

    @property
    def dylib_name(self):
        """dynamic link libname"""
        if PLATFORM == "Darwin":
            return f"{self.libname}.dylib"
        if PLATFORM == "Linux":
            return f"{self.libname}.so"
        if PLATFORM == "Windows":
            return f"{self.libname}.dll"
        return self.fail("platform not supported")

    @property
    def dylib_linkname(self):
        """symlink to dylib"""
        if PLATFORM == "Darwin":
            return f"{self.libname}.dylib"
        if PLATFORM == "Linux":
            return f"{self.libname}.so"
        return self.fail("platform not supported")

    @property
    def dylib(self):
        """dylib path"""
        return self.project.lib / self.dylib_name

    @property
    def dylib_link(self):
        """dylib link path"""
        return self.project.lib / self.dylib_linkname

    @property
    def staticlib(self):
        """staticlib path"""
        return self.project.lib_static / self.staticlib_name

    @property
    def prefix(self):
        """builder prefix path"""
        return self.project.install / self.name.lower()

    def libs_static_exist(self):
        """check if all built stati libs already exist"""
        return all((self.prefix / "lib" / lib).exists()
                    for lib in self.libs_static)

    def pre_process(self):
        """override by subclass if needed"""

    def setup(self):
        """setup build environment"""
        self.project.setup()
        self.git_clone(self.url, recurse=True, cwd=self.project.src)
        self.log.info("cloned %s", self.url)
        assert self.src_path.exists(), f"git clone {self.name} failed"

    def configure(self):
        """configure build"""

    def build(self):
        """build target"""

    def install(self):
        """install target"""

    def clean(self):
        """clean build"""

    def post_process(self):
        """override by subclass if needed"""

    def process(self):
        """main builder process"""
        self.pre_process()
        self.setup()
        self.configure()
        self.build()
        self.install()
        self.clean()
        self.post_process()


class PcreBuilder(Builder):
    """pcre builder class"""

    name = "pcre"
    version = "8.45"
    url_template = "https://github.com/luvit/pcre.git"
    depends_on = []
    libs_static = ["libpcre.a", "libpcrecpp.a", "libpcreposix.a"]

    def build(self):
        """main build method"""
        self.build_dir.mkdir(exist_ok=True)
        self.cmake_config(self.src_path, self.build_dir,
            BUILD_SHARED_LIBS=False,
            PCRE_SHOW_REPORT=False,
            PCRE_BUILD_PCREGREP=False,
            PCRE_BUILD_TESTS=False,
        )
        self.cmake_build(self.build_dir, release=True)
        self.cmake_install(self.build_dir, prefix=self.prefix)
        if not self.libs_static_exist():
            self.fail("did not find %s static libs", self.name)
        self.log.info("DONE")


class BdwgcBuilder(Builder):
    """bdwgc builder class"""

    name = "bdwgc"
    version = "1.0.8"
    url_template = "https://github.com/ivmai/bdwgc.git"
    depends_on = []
    libs_static = ["libgc.a", "libgccpp.a"]

    def build(self):
        """main build method"""
        self.build_dir.mkdir(exist_ok=True)
        self.cmake_config(self.src_path, self.build_dir,
            enable_cplusplus=True,
            build_cord=False,
            enable_docs=False,
            BUILD_SHARED_LIBS=False,
        )
        self.cmake_build(self.build_dir, release=True)
        self.cmake_install(self.build_dir, prefix=self.prefix)
        if not self.libs_static_exist():
            self.fail("did not find %s static libs", self.name)
        self.log.info("DONE")


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(
        prog="build.py",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        description="A builder",
    )
    opt = parser.add_argument

    opt("-g", "--gc",   help="build bdwgc", action="store_true")
    opt("-p", "--pcre", help="build pcre", action="store_true")

    args = parser.parse_args()
    if args.gc:
        gc = BdwgcBuilder()
        gc.process()
    elif args.pcre:
        pc = PcreBuilder()
        pc.process()
    else:
        for builder_class in [BdwgcBuilder, PcreBuilder]:
            b = builder_class()
            b.process()



