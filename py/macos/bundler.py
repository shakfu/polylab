#!/usr/bin/env python3
"""bundler.py

Provides functional tools to make an .app bundle

- make_bundle() requires macholib
- get_deps() recursively returns dependencies

"""
import os
import re
import shutil
import stat
import subprocess
from pathlib import Path
from typing import Set, Optional

from macholib import macho_standalone


PATTERNS = [
    "/opt/local/",
    "/usr/local/",
    "/Users/",
    "/tmp/",
]


INFO_PLIST_TMPL = """\
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>CFBundleDevelopmentRegion</key>
    <string>English</string>
    <key>CFBundleExecutable</key>
    <string>{executable}</string>
    <key>CFBundleGetInfoString</key>
    <string>{versioned_bundle_name}</string>
    <key>CFBundleIconFile</key>
    <string>app.icns</string>
    <key>CFBundleIdentifier</key>
    <string>{bundle_identifier}</string>
    <key>CFBundleInfoDictionaryVersion</key>
    <string>6.0</string>
    <key>CFBundleName</key>
    <string>{bundle_name}</string>
    <key>CFBundlePackageType</key>
    <string>APPL</string>
    <key>CFBundleShortVersionString</key>
    <string>{versioned_bundle_name}</string>
    <key>CFBundleSignature</key>
    <string>????</string>
    <key>CFBundleVersion</key>
    <string>{bundle_version}</string>
    <key>NSAppleScriptEnabled</key>
    <string>YES</string>
    <key>NSMainNibFile</key>
    <string>MainMenu</string>
    <key>NSPrincipalClass</key>
    <string>NSApplication</string>
</dict>
</plist>
"""


class BundleFolder:
    def __init__(self, path: Path | str):
        self.path = Path(path)

    def create(self):
        """create bundle folder"""
        if not self.path.exists():
            self.path.mkdir(exist_ok=True, parents=True)
        assert self.path.is_dir(), f"{self.path} is not a directory"

    def copy(self, src: Path | str):
        """recursive copy from src to bundle folder"""
        src = Path(src)
        shutil.copytree(src, self.path / src.name)


class Bundle:
    """Makes a macos bundle.

    :param      target:   The target executable
    :type       target:   str
    :param      version:  The version; defaults to '1.0'
    :type       version:  str
    :param      prefix:   The prefix of the bundle id; defaults to 'org.me'
    :type       prefix:   str
    :param      prefix:   The suffix of the bundle; defaults to '.app'
    :type       prefix:   str
    """

    def __init__(
        self,
        target: Path | str,
        version: str = "1.0",
        add_to_resources: Optional[list[str | Path]] = None,
        base_id: str = "org.me",
        extension: str = ".app",
    ):
        self.target = Path(target)
        self.version = version
        self.add_to_resources = add_to_resources or []
        self.base_id = base_id
        self.extension = extension
        # folders
        self.bundle = self.target.parent / (self.target.stem + extension)
        self.contents = self.bundle / "Contents"
        self.macos = self.contents / "MacOS"
        # special bundle folders
        self.frameworks = BundleFolder(self.contents / "Frameworks")
        self.resources = BundleFolder(self.contents / "Resources")
        # files
        self.info_plist = self.contents / "Info.plist"
        self.pkg_info = self.contents / "PkgInfo"
        self.executable = self.macos / self.target.name

    def create_executable(self):
        """create bundle executable"""
        shutil.copy(self.target, self.executable)
        oldmode = os.stat(self.executable).st_mode
        os.chmod(self.executable, oldmode | stat.S_IXUSR | stat.S_IXGRP | stat.S_IXOTH)

    def create_info_plist(self):
        """create info.plist file"""
        with open(self.info_plist, "w", encoding="utf-8") as fopen:
            fopen.write(
                INFO_PLIST_TMPL.format(
                    executable=self.target.name,
                    bundle_name=self.target.stem,
                    bundle_identifier=f"{self.base_id}.{self.target.stem}",
                    bundle_version=self.version,
                    versioned_bundle_name=f"{self.target.stem} {self.version}",
                )
            )

    def create_pkg_info(self):
        """create pkg_info file"""
        with open(self.pkg_info, "w", encoding="utf-8") as fopen:
            fopen.write("APPL????")

    def create_resources(self):
        """create and populate  bundle `Resources` folder"""
        if self.add_to_resources:
            self.resources.create()
            for resource in self.add_to_resources:
                self.resources.copy(resource)

    def create_frameworks(self):
        """create and populate  bundle `Frameworks` folder"""
        self.frameworks.create()
        macho_standalone.standaloneApp(self.bundle)

    def create(self):
        """create the bundle"""

        self.macos.mkdir(exist_ok=True, parents=True)
        self.create_executable()
        self.create_info_plist()
        self.create_pkg_info()
        self.create_resources()
        self.create_frameworks()


def make_bundle(
    target: Path | str,
    version: str = "1.0",
    add_to_resources: Optional[list[str]] = None,
    prefix: str = "org.me",
    suffix: str = ".app",
):
    target = Path(target)
    bundle = target.parent / (target.stem + suffix)
    bundle_contents = bundle / "Contents"

    bundle_info_plist = bundle_contents / "Info.plist"
    bundle_pkg_info = bundle_contents / "PkgInfo"

    bundle_macos = bundle_contents / "MacOS"
    bundle_frameworks = bundle_contents / "Frameworks"
    bundle_resources = bundle_contents / "Resources"

    bundle_subdirs = [bundle_macos, bundle_frameworks]

    bundle_executable = bundle_macos / target.name

    for subdir in bundle_subdirs:
        subdir.mkdir(exist_ok=True, parents=True)

    shutil.copy(target, bundle_executable)

    with open(bundle_info_plist, "w", encoding="utf-8") as fopen:
        fopen.write(
            INFO_PLIST_TMPL.format(
                executable=target.name,
                bundle_name=target.stem,
                bundle_identifier=f"{prefix}.{target.stem}",
                bundle_version=version,
                versioned_bundle_name=f"{target.stem} {version}",
            )
        )

    with open(bundle_pkg_info, "w", encoding="utf-8") as fopen:
        fopen.write("APPL????")

    oldmode = os.stat(bundle_executable).st_mode
    os.chmod(bundle_executable, oldmode | stat.S_IXUSR | stat.S_IXGRP | stat.S_IXOTH)

    if add_to_resources:
        bundle_resources.mkdir(exist_ok=True, parents=True)
        for resource in add_to_resources:
            _resource = Path(resource)
            shutil.copytree(_resource, bundle_resources / _resource.name)

    macho_standalone.standaloneApp(bundle)


class DependencyTree:
    PATTERNS = [
        "/opt/local/",
        "/usr/local/",
        "/Users/",
        "/tmp/",
    ]

    def __init__(self, target: str):
        self.target = target
        self.install_names: dict[str, set[tuple[str, str]]] = {}
        self.dependencies: list[str] = []

    def get_dependencies(self, target: Optional[str] = None):
        """get dependencies in tree structure and as a list of paths"""
        if not target:
            target = self.target
        key = os.path.basename(target)
        self.install_names[key] = set()
        result = subprocess.check_output(["otool", "-L", target], text=True)
        entries = [line.strip() for line in result.splitlines()]
        for entry in entries:
            match = re.match(r"\s*(\S+)\s*\(compatibility version .+\)$", entry)
            if match:
                # print(entry)
                path = match.group(1)
                print(path)
                dep_path, dep_filename = os.path.split(path)
                if any(dep_path.startswith(p) for p in PATTERNS) or dep_path == "":
                    item = (path, "@rpath/" + dep_filename)
                    self.install_names[key].add(item)
                    if path not in self.dependencies:
                        self.dependencies.append(path)
                        self.get_dependencies(path)


def get_dependencies(
    target: str,
    names: Optional[dict[str, Set]] = None,
    deps: Optional[list[str]] = None,
):
    """get dependencies in tree structure and as a list of paths"""
    key = os.path.basename(target)
    _deps = [] if not deps else deps
    _names = {} if not names else names
    _names[key] = set()
    result = subprocess.check_output(["otool", "-L", target], text=True)
    entries = [line.strip() for line in result.splitlines()]
    for entry in entries:
        match = re.match(r"\s*(\S+)\s*\(compatibility version .+\)$", entry)
        if match:
            path = match.group(1)
            dep_path, dep_filename = os.path.split(path)
            if any(dep_path.startswith(p) for p in PATTERNS) or dep_path == "":
                item = (path, "@rpath/" + dep_filename)
                _names[key].add(item)
                if path not in _deps:
                    _deps.append(path)
                    get_dependencies(path, _names, _deps)
    return _names, _deps


if __name__ == "__main__":
    # tree, dependencies = get_dependencies('libguile-3.0.1.dylib')
    # tree = DependencyTree()
    # tree.get_dependencies('libguile-3.0.1.dylib')

    # tree = DependencyTree('libsndfile.1.0.35.dylib')
    # tree.get_dependencies()
    # from pprint import pprint
    # pprint(tree.install_names)

    # tree, dependencies = get_dependencies('libsndfile.1.0.35.dylib')
    tree, dependencies = get_dependencies("libsndfile.dylib")
